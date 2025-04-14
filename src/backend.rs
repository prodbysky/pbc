pub type MainType = unsafe extern "C" fn() -> i64;

use thiserror::Error;

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
    scopes: Vec<std::collections::HashMap<String, T>>,
}

impl<T> BackendScopeManager<T> {
    pub fn new() -> Self {
        Self { scopes: vec![] }
    }

    pub fn new_scope(&mut self) {
        self.scopes.push(std::collections::HashMap::new());
    }
    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }
    pub fn top_scope(&self) -> &std::collections::HashMap<String, T> {
        self.scopes.last().expect("Ran out of scopes? sus")
    }
    pub fn top_scope_mut(&mut self) -> &mut std::collections::HashMap<String, T> {
        self.scopes.last_mut().expect("Ran out of scopes? sus")
    }

    pub fn add_variable(&mut self, name: String, ident: T) {
        self.top_scope_mut().insert(name, ident);
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

pub struct InkwellBackend {
    ctx: &'static inkwell::context::Context,
    pub module: inkwell::module::Module<'static>,
    exec_engine: inkwell::execution_engine::ExecutionEngine<'static>,
    builder: inkwell::builder::Builder<'static>,
    i64_type: inkwell::types::IntType<'static>,
    scopes: BackendScopeManager<(
        inkwell::values::PointerValue<'static>,
        inkwell::types::IntType<'static>,
    )>,
}

use crate::frontend;

#[derive(Debug, Error)]
pub enum InkwellError {
    #[error("Failed to initialize Inkwell Backend: {0}")]
    FailedToInitialize(String),
    #[error("Failed to build LLVM IR: {0}")]
    BuilderError(#[from] inkwell::builder::BuilderError),
    #[error("Tried to get variable that doesn't exist: {0}")]
    UndefinedReference(String),
}

impl Backend for InkwellBackend {
    type ExprType = inkwell::values::IntValue<'static>;
    type Error = InkwellError;
    fn new() -> Result<Self, Self::Error> {
        let ctx = Box::leak(Box::new(inkwell::context::Context::create()));
        let module = ctx.create_module("main");
        let exec_engine = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .map_err(|e| InkwellError::FailedToInitialize(e.to_string()))?;

        let builder = ctx.create_builder();

        let i64_type = ctx.i64_type();
        Ok(Self {
            ctx,
            module,
            exec_engine,
            builder,
            i64_type,
            scopes: BackendScopeManager::new(),
        })
    }

    fn generate(&mut self, ast: &[frontend::ast::Statement]) -> Result<(), Self::Error> {
        let main_fn_type = self.i64_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_fn_type, None);
        let block = self.ctx.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(block);
        self.scopes.new_scope();

        for st in ast {
            match st {
                frontend::ast::Statement::Return(expr) => {
                    let value = self.eval_expr(expr);
                    self.builder.build_return(Some(&value?))?;
                }
                frontend::ast::Statement::VariableCreate {
                    name,
                    value,
                    _type: t,
                } => {
                    let value = self.eval_expr(value);
                    let real_type = match t.as_str() {
                        "i64" => self.ctx.i64_type(),
                        t => unimplemented!("goofy new type not implemented: {t}"),
                    };
                    let mem = self.builder.build_alloca(real_type, name)?;
                    self.builder.build_store(mem, value?)?;
                    self.scopes.add_variable(name.to_string(), (mem, real_type));
                }
                frontend::ast::Statement::Assign { name, value } => {
                    let val = self.eval_expr(value)?;
                    let Some(&(ptr, t)) = self.scopes.get_variable(name) else {
                        return Err(InkwellError::UndefinedReference(name.clone()));
                    };
                    self.scopes.add_variable(name.to_string(), (ptr, t));
                    self.builder.build_store(ptr, val)?;
                }
                s => {
                    unimplemented!("Not implemented statement code gen: {s:?}");
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
            frontend::ast::Expression::Number(n) => Ok(self.i64_type.const_int(*n, false)),
            frontend::ast::Expression::Variable(name) => {
                let Some(&(ptr, t)) = self.scopes.get_variable(name) else {
                    return Err(InkwellError::UndefinedReference(name.clone()));
                };

                let val = self.builder.build_load(t, ptr, name)?;
                match t {
                    inkwell::types::IntType { .. } => Ok(val.into_int_value()),
                }
            }
            frontend::ast::Expression::Binary { left, op, right } => {
                let left_val = self.eval_expr(left);
                let right_val = self.eval_expr(right);

                match op {
                    frontend::token::Token::Plus => {
                        Ok(self.builder.build_int_add(left_val?, right_val?, "add")?)
                    }
                    frontend::token::Token::Minus => {
                        Ok(self.builder.build_int_sub(left_val?, right_val?, "sub")?)
                    }
                    frontend::token::Token::Star => {
                        Ok(self.builder.build_int_mul(left_val?, right_val?, "mul")?)
                    }
                    frontend::token::Token::Slash => Ok(self
                        .builder
                        .build_int_signed_div(left_val?, right_val?, "div")?),
                    _ => panic!("Unsupported operator"),
                }
            }
        }
    }

    fn get_main(&self) -> MainType {
        unsafe { self.exec_engine.get_function("main").unwrap().as_raw() }
    }
}
