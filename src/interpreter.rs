use crate::parser::{ExprNode};
use std::collections::HashMap;
use std::f32::consts::PI;

fn pow(interp: &mut Interpreter, args: &Vec<Box<ExprNode>>) -> Result<f32, &'static str> {
  if args.len() != 2 {
    return Err("Incorrect arguments for pow function: expected pow(num, exp)")
  } 
  
  Ok(interp.eval(&*args[0]).powf(interp.eval(&*args[1])))
}

fn sqrt(interp: &mut Interpreter, args: &Vec<Box<ExprNode>>) -> Result<f32, &'static str> {
  if args.len() != 1 {
    return Err("Incorrect arguments for sqrt function: expected sqrt(num)")
  }

  Ok(interp.eval(&*args[0]).sqrt())
}

fn pi(_: &mut Interpreter, args: &Vec<Box<ExprNode>>) -> Result<f32, &'static str> {
  if args.len() != 0 {
    return Err("Incorrect arguments for PI function: expected no arguments")
  }
  
  Ok(PI)
}

type InterpreterFunction = fn(&mut Interpreter, &Vec<Box<ExprNode>>) -> Result<f32, &'static str>;
pub struct Interpreter {
  bifs: HashMap<String, InterpreterFunction>,
  vars: HashMap<String, f32>,
  errors: Vec<String>,
}

impl Interpreter {
  pub fn new() -> Self {
    let mut bifs: HashMap<String, InterpreterFunction> = HashMap::new();
    bifs.insert(String::from("pow"), pow);
    bifs.insert(String::from("sqrt"), sqrt);
    bifs.insert(String::from("pi"), pi);

    Interpreter {
      bifs,
      vars: HashMap::new(),
      errors: vec![],
    }
  }

  pub fn eval(&mut self, expr: &ExprNode) -> f32 {
    match expr {
      ExprNode::Number(val) => *val,
      ExprNode::Add(expr1, expr2) => self.eval(expr1) + self.eval(expr2),
      ExprNode::Sub(expr1, expr2) => self.eval(expr1) - self.eval(expr2),
      ExprNode::Mul(expr1, expr2) => self.eval(expr1) * self.eval(expr2),
      ExprNode::Div(expr1, expr2) => self.eval(expr1) / self.eval(expr2),
      ExprNode::FunctionCall(name, args) => match self.bifs.get(name) {
        Some(func) => match func(self, &args) {
          Ok(res) => res,
          Err(msg) => {
            self.errors.push(msg.to_string());
            0.
          },
        },
        None => {
          self.errors.push(format!("Unsupported function: {}", name));
          0.
        },
      },
      ExprNode::Assign(name, res) => {
        let expr_value = self.eval(res);
        match self.vars.get_mut(name) {
          Some(val) => {
            *val = expr_value;
          },
          None => {
            self.vars.insert(name.clone(), expr_value);
          }
        };
        expr_value
      },
      ExprNode::Var(name) => 
        match self.vars.get(name) {
          Some(val) => *val,
          None => {
            self.errors.push(format!("Undeclared variable: {}", name));
            0.
          },
        },
    }
  }

  pub fn print_errors(&mut self) -> bool {
    if self.errors.len() > 0 {
      for err in &self.errors {
        println!("{}", err);
      }
      self.errors.clear();
      return true
    }

    false
  }
}

#[cfg(test)]
mod tests {
  use crate::interpreter::{Interpreter};
  use crate::parser::{ExprNode};
  use std::f32::consts::{PI};
  
  #[test]
  fn number() {
    assert_eq!(
      Interpreter::new().eval(
        &ExprNode::Number(5.),
      ),
      5.,
    )
  }

  #[test]
  fn add() {
    assert_eq!(
      Interpreter::new().eval(
        &ExprNode::Add(
          Box::new(
            ExprNode::Add(
              Box::new(ExprNode::Number(1.)),
              Box::new(ExprNode::Number(1.)),
            ),
          ),
          Box::new(ExprNode::Number(2.)),
        ),
      ),
      4.,
    )
  }

  #[test]
  fn sub() {
    assert_eq!(
      Interpreter::new().eval(
        &ExprNode::Sub(
          Box::new(ExprNode::Number(2.)),
          Box::new(
            ExprNode::Sub(
              Box::new(ExprNode::Number(1.)),
              Box::new(ExprNode::Number(0.5)),
            ),
          ),
        ),
      ),
      1.5,
    )
  }

  #[test]
  fn mul() {
    assert_eq!(
      Interpreter::new().eval(
        &ExprNode::Mul(
          Box::new(
            ExprNode::Mul(
              Box::new(ExprNode::Number(2.)),
              Box::new(ExprNode::Number(2.)),
            ),
          ),
          Box::new(ExprNode::Number(4.)),
        ),
      ),
      16.,
    )
  }

  #[test]
  fn div() {
    assert_eq!(
      Interpreter::new().eval(
        &ExprNode::Div(
          Box::new(
            ExprNode::Div(
              Box::new(ExprNode::Number(4.)),
              Box::new(ExprNode::Number(2.)),
            ),
          ),
          Box::new(ExprNode::Number(0.5)),
        ),
      ),
      4.
    )
  }

  #[test]
  fn pow_func() {
    assert_eq!(
      Interpreter::new().eval(
        &ExprNode::FunctionCall(
          String::from("pow"),
          vec![
            Box::new(ExprNode::Number(2.)),
            Box::new(ExprNode::Number(4.)),
          ],
        ),
      ),
      16.,
    )
  }

  #[test]
  fn sqrt_func() {
    assert_eq!(
      Interpreter::new().eval(
        &ExprNode::FunctionCall(
          String::from("sqrt"),
          vec![
            Box::new(ExprNode::Number(25.)),
          ],
        ),
      ),
      5.,
    )
  }

  #[test]
  fn pi_func() {
    assert_eq!(
      Interpreter::new().eval(
        &ExprNode::FunctionCall(
          String::from("pi"),
          vec![],
        ),
      ),
      PI,
    )
  }

  #[test]
  fn assign() {
    let mut interp = Interpreter::new();
    assert_eq!(
      interp.eval(
        &ExprNode::Assign(
          String::from("x"),
          Box::new(ExprNode::Number(2.)),
        ),
      ),
      2.,
    );

    assert_eq!(
      *interp.vars.get("x").unwrap(),
      2.,
    )
  }

  #[test]
  fn var() {
    let mut interp = Interpreter::new();
    interp.vars.insert(String::from("x"), 5.);
    assert_eq!(
      interp.eval(
        &ExprNode::Add(
          Box::new(ExprNode::Number(5.)),
          Box::new(ExprNode::Var(String::from("x"))),
        ),
      ),
      10.,
    )
  }

  #[test]
  fn all() {
    let mut interp = Interpreter::new();
    interp.vars.insert(String::from("y"), 2.);

    assert_eq!(
      interp.eval(
        &ExprNode::Assign(
          String::from("x"),
          // x = 1 + (3 * 4 - 5) / 2 + pow(3, y)
          Box::new(
            ExprNode::Add(
              Box::new(
                ExprNode::Add(
                  Box::new(ExprNode::Number(1.)),
                  Box::new(
                    ExprNode::Div(
                      Box::new(
                        ExprNode::Sub(
                          Box::new(
                            ExprNode::Mul(
                              Box::new(ExprNode::Number(3.)),
                              Box::new(ExprNode::Number(4.)),
                            ),
                          ),
                          Box::new(ExprNode::Number(5.)),
                        ),
                      ),
                      Box::new(ExprNode::Number(2.)),
                    ),
                  ),
                ),
              ),
              Box::new(
                ExprNode::FunctionCall(
                  String::from("pow"),
                  vec![
                    Box::new(ExprNode::Number(3.)),
                    Box::new(ExprNode::Var(String::from("y"))),
                  ],
                ),
              ),
            ),
          ),
        ),
      ),
      13.5,
    )
  }
}