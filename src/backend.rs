use inkwell::{
    OptimizationLevel,
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    types::{BasicTypeEnum, FloatType, IntType},
    values::{BasicValueEnum, FloatValue, IntValue, PointerValue},
};
use std::collections::HashMap;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum LangType {
    I32,
    I64,
    F32,
    F64,
}

fn llvm_type_for<'ctx>(ctx: &'ctx Context, lang_type: LangType) -> BasicTypeEnum<'ctx> {
    match lang_type {
        LangType::I32 => ctx.i32_type().into(),
        LangType::I64 => ctx.i64_type().into(),
        LangType::F32 => ctx.f32_type().into(),
        LangType::F64 => ctx.f64_type().into(),
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BackendValue<'ctx> {
    Int(IntValue<'ctx>),
    Float(FloatValue<'ctx>),
}

impl<'ctx> BackendValue<'ctx> {
    // Helper to convert BackendValue into LLVM's BasicValueEnum.
    pub fn as_basic_value(self) -> BasicValueEnum<'ctx> {
        match self {
            BackendValue::Int(val) => val.into(),
            BackendValue::Float(val) => val.into(),
        }
    }
}

pub type MainType = unsafe extern "C" fn() -> i64;

pub trait Backend {
    type ExprType;
    type Error;
    fn new() -> Result<Self, Self::Error>
    where
        Self: Sized;
    fn generate(&mut self, ast: &[frontend::ast::Statement]) -> Result<(), Self::Error>;
    fn eval_expr(
        &mut self,
        expr: &frontend::ast::Expression,
    ) -> Result<Self::ExprType, Self::Error>;
    fn get_main(&self) -> MainType;
}

pub struct BackendScopeManager<T> {
    scopes: Vec<HashMap<String, T>>,
}

impl<T> BackendScopeManager<T> {
    pub fn new() -> Self {
        Self { scopes: vec![] }
    }

    pub fn new_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }
    pub fn top_scope(&self) -> &HashMap<String, T> {
        self.scopes.last().expect("No scope available")
    }
    pub fn top_scope_mut(&mut self) -> &mut HashMap<String, T> {
        self.scopes.last_mut().expect("No scope available")
    }

    pub fn add_variable(&mut self, name: String, info: T) {
        self.top_scope_mut().insert(name, info);
    }
    pub fn get_variable(&self, name: &str) -> Option<&T> {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Some(val);
            }
        }
        None
    }
    pub fn get_variable_mut(&mut self, name: &str) -> Option<&mut T> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(val) = scope.get_mut(name) {
                return Some(val);
            }
        }
        None
    }
}

type VariableInfo<'ctx> = (PointerValue<'ctx>, BasicTypeEnum<'ctx>, LangType);

use crate::frontend;

#[derive(Debug, Error)]
pub enum InkwellError {
    #[error("Failed to initialize Inkwell Backend: {0}")]
    FailedToInitialize(String),
    #[error("Failed to build LLVM IR: {0}")]
    BuilderError(#[from] inkwell::builder::BuilderError),
    #[error("Undefined reference: {0}")]
    UndefinedReference(String),
    #[error("Type mismatch or unsupported operation")]
    TypeError,
}

pub struct InkwellBackend {
    ctx: &'static Context,
    pub module: Module<'static>,
    exec_engine: ExecutionEngine<'static>,
    builder: Builder<'static>,
    scopes: BackendScopeManager<VariableInfo<'static>>,
}

impl Backend for InkwellBackend {
    type ExprType = BackendValue<'static>;
    type Error = InkwellError;

    fn new() -> Result<Self, Self::Error> {
        let ctx = Box::leak(Box::new(Context::create()));
        let module = ctx.create_module("main");
        let exec_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .map_err(|e| InkwellError::FailedToInitialize(e.to_string()))?;
        let builder = ctx.create_builder();

        Ok(Self {
            ctx,
            module,
            exec_engine,
            builder,
            scopes: BackendScopeManager::new(),
        })
    }

    fn generate(&mut self, ast: &[frontend::ast::Statement]) -> Result<(), Self::Error> {
        let main_fn_type = self.ctx.i64_type().fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_fn_type, None);
        let block = self.ctx.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(block);
        self.scopes.new_scope();

        for st in ast {
            match st {
                frontend::ast::Statement::Return(expr) => {
                    let value = self.eval_expr(expr)?;
                    self.builder.build_return(Some(&value.as_basic_value()))?;
                }
                frontend::ast::Statement::VariableCreate { name, value, _type } => {
                    let computed = self.eval_expr(value)?;
                    let lang_type = match _type.as_str() {
                        "i32" => LangType::I32,
                        "i64" => LangType::I64,
                        "f32" => LangType::F32,
                        "f64" => LangType::F64,
                        t => unimplemented!("Type not implemented: {}", t),
                    };
                    let llvm_ty = llvm_type_for(self.ctx, lang_type.clone());
                    let mem = self.builder.build_alloca(llvm_ty, name)?;
                    self.builder.build_store(mem, computed.as_basic_value())?;
                    self.scopes
                        .add_variable(name.to_string(), (mem, llvm_ty, lang_type));
                }
                frontend::ast::Statement::Assign { name, value } => {
                    let computed = self.eval_expr(value)?;
                    let (ptr, _llvm_ty, _) = self
                        .scopes
                        .get_variable(name)
                        .ok_or_else(|| InkwellError::UndefinedReference(name.clone()))?;
                    self.builder.build_store(*ptr, computed.as_basic_value())?;
                }
                s => {
                    unimplemented!("Statement codegen not implemented: {:?}", s);
                }
            }
        }

        self.scopes.pop_scope();
        Ok(())
    }

    fn eval_expr(
        &mut self,
        expr: &frontend::ast::Expression,
    ) -> Result<Self::ExprType, Self::Error> {
        match expr {
            frontend::ast::Expression::Number(n) => {
                Ok(BackendValue::Int(self.ctx.i64_type().const_int(*n, false)))
            }
            frontend::ast::Expression::Variable(name) => {
                let (ptr, llvm_ty, _lang_ty) = self
                    .scopes
                    .get_variable(name)
                    .ok_or_else(|| InkwellError::UndefinedReference(name.clone()))?;
                let loaded = self.builder.build_load(*llvm_ty, *ptr, name)?;
                if llvm_ty.is_int_type() {
                    Ok(BackendValue::Int(loaded.into_int_value()))
                } else if llvm_ty.is_float_type() {
                    Ok(BackendValue::Float(loaded.into_float_value()))
                } else {
                    Err(InkwellError::TypeError)
                }
            }
            frontend::ast::Expression::Binary { left, op, right } => {
                let left_val = self.eval_expr(left)?;
                let right_val = self.eval_expr(right)?;

                match (left_val, right_val) {
                    (BackendValue::Int(l), BackendValue::Int(r)) => match op {
                        frontend::token::Token::Plus => {
                            Ok(BackendValue::Int(self.builder.build_int_add(l, r, "add")?))
                        }
                        frontend::token::Token::Minus => {
                            Ok(BackendValue::Int(self.builder.build_int_sub(l, r, "sub")?))
                        }
                        frontend::token::Token::Star => {
                            Ok(BackendValue::Int(self.builder.build_int_mul(l, r, "mul")?))
                        }
                        frontend::token::Token::Slash => Ok(BackendValue::Int(
                            self.builder.build_int_signed_div(l, r, "div")?,
                        )),
                        _ => Err(InkwellError::TypeError),
                    },
                    (BackendValue::Float(l), BackendValue::Float(r)) => match op {
                        frontend::token::Token::Plus => Ok(BackendValue::Float(
                            self.builder.build_float_add(l, r, "fadd")?,
                        )),
                        frontend::token::Token::Minus => Ok(BackendValue::Float(
                            self.builder.build_float_sub(l, r, "fsub")?,
                        )),
                        frontend::token::Token::Star => Ok(BackendValue::Float(
                            self.builder.build_float_mul(l, r, "fmul")?,
                        )),
                        frontend::token::Token::Slash => Ok(BackendValue::Float(
                            self.builder.build_float_div(l, r, "fdiv")?,
                        )),
                        _ => Err(InkwellError::TypeError),
                    },
                    _ => Err(InkwellError::TypeError),
                }
            }
        }
    }

    fn get_main(&self) -> MainType {
        unsafe { self.exec_engine.get_function("main").unwrap().as_raw() }
    }
}
