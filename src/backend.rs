pub type MainType = unsafe extern "C" fn() -> i64;
pub trait Backend {
    type ExprType;
    fn new() -> Self;
    fn generate(&mut self, ast: &[frontend::ast::Statement]);
    fn eval_expr(&mut self, expr: &frontend::ast::Expression) -> Self::ExprType;
    fn get_main(&self) -> MainType;
}

pub struct InkwellBackend {
    ctx: &'static inkwell::context::Context,
    pub module: inkwell::module::Module<'static>,
    exec_engine: inkwell::execution_engine::ExecutionEngine<'static>,
    builder: inkwell::builder::Builder<'static>,
    i64_type: inkwell::types::IntType<'static>,
    variables: std::collections::HashMap<String, inkwell::values::PointerValue<'static>>,
}

use crate::frontend;

impl Backend for InkwellBackend {
    type ExprType = inkwell::values::IntValue<'static>;
    fn new() -> Self {
        let ctx = Box::leak(Box::new(inkwell::context::Context::create()));
        let module = ctx.create_module("main");
        let exec_engine = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();

        let builder = ctx.create_builder();

        let i64_type = ctx.i64_type();
        Self {
            ctx,
            module,
            exec_engine,
            builder,
            i64_type,
            variables: std::collections::HashMap::new(),
        }
    }

    fn generate(&mut self, ast: &[frontend::ast::Statement]) {
        let main_fn_type = self.i64_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_fn_type, None);
        let block = self.ctx.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(block);

        for st in ast {
            match st {
                frontend::ast::Statement::Return(expr) => {
                    let value = self.eval_expr(expr);
                    self.builder.build_return(Some(&value)).unwrap();
                }
                frontend::ast::Statement::ConstantAssign(name, value) => {
                    let value = self.eval_expr(value);
                    let mem = self.builder.build_alloca(self.i64_type, name).unwrap();
                    self.builder.build_store(mem, value).unwrap();
                    self.variables.insert(name.to_string(), mem);
                }
                s => {
                    eprintln!("{s:?}");
                    return;
                }
            }
        }
    }

    fn eval_expr(&mut self, expr: &frontend::ast::Expression) -> Self::ExprType {
        match expr {
            frontend::ast::Expression::Number(n) => self.i64_type.const_int(*n, false),
            frontend::ast::Expression::Variable(name) => self
                .builder
                .build_load(self.i64_type, *self.variables.get(name).unwrap(), name)
                .unwrap()
                .into_int_value(),
            frontend::ast::Expression::Binary { left, op, right } => {
                let left_val = self.eval_expr(left);
                let right_val = self.eval_expr(right);

                match op {
                    frontend::token::Token::Plus => self
                        .builder
                        .build_int_add(left_val, right_val, "add")
                        .unwrap(),
                    frontend::token::Token::Minus => self
                        .builder
                        .build_int_sub(left_val, right_val, "sub")
                        .unwrap(),
                    frontend::token::Token::Star => self
                        .builder
                        .build_int_mul(left_val, right_val, "mul")
                        .unwrap(),
                    frontend::token::Token::Slash => self
                        .builder
                        .build_int_signed_div(left_val, right_val, "div")
                        .unwrap(),
                    frontend::token::Token::Ident(name) => {
                        let var = self.variables.get(name).unwrap();
                        self.builder
                            .build_load(self.i64_type, *var, "name")
                            .unwrap()
                            .into_int_value()
                    }
                    _ => panic!("Unsupported operator"),
                }
            }
        }
    }

    fn get_main(&self) -> MainType {
        unsafe { self.exec_engine.get_function("main").unwrap().as_raw() }
    }
}
