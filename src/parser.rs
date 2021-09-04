extern crate nom;

use nom::branch::{alt};
use nom::bytes::complete::{tag};
use nom::character::complete::{alpha1, multispace0};
use nom::combinator::{map, opt};
use nom::multi::{many0};
use nom::number::complete::{float};
use nom::sequence::{delimited, pair, preceded, tuple, separated_pair};
use nom::IResult;

#[derive(Debug, PartialEq, Clone)]
pub enum ExprNode {
  Number(f32),
  Add(Box<ExprNode>, Box<ExprNode>),
  Sub(Box<ExprNode>, Box<ExprNode>),
  Mul(Box<ExprNode>, Box<ExprNode>),
  Div(Box<ExprNode>, Box<ExprNode>),
  FunctionCall(String, Vec<Box<ExprNode>>),
  Assign(String, Box<ExprNode>),
  Var(String),
}

fn ws<F, I, O, E>(f: F) -> impl FnMut(I) -> IResult<I, O, E>
where
  F: FnMut(I) -> IResult<I, O, E>,
  I: nom::InputTakeAtPosition,
  <I as nom::InputTakeAtPosition>::Item: nom::AsChar + Clone,
  E: nom::error::ParseError<I>,
{
  delimited(multispace0, f, multispace0)
}

fn parse_number(input: &str) -> IResult<&str, ExprNode> {
  ws(
    map(
      float,
      |num| ExprNode::Number(num),
    ),
  )(input)
}

fn parse_negative_number(input: &str) -> IResult<&str, ExprNode> {
  ws(
    map(
      preceded(
        tag("-"),
        float,
      ),
      |num| ExprNode::Number(num * -1.0),
    ),
  )(input)
}

fn parse_value(input: &str) -> IResult<&str, ExprNode> {
  ws(
    alt((
      parse_negative_number,
      parse_number,
    )),
  )(input)
}

fn parse_nested_expr(input: &str) -> IResult<&str, ExprNode> {
  ws(
    delimited(
      tag("("),
      parse_addsub,
      tag(")"),
    ),
  )(input)
}

fn parse_function_call(input: &str) -> IResult<&str, ExprNode> {
  map(
    pair(
      alpha1,
      delimited(
        tag("("),
          opt(
            tuple((
              opt(
                parse_addsub,
              ),
              many0(
                preceded(
                  tag(","),
                  parse_addsub,
                ),
              ),
            )),
          ),
        tag(")"),
      ),
    ),
    |(name, args)| 
      ExprNode::FunctionCall(
        name.to_string(),
        match args {
          Some((first_argument, rest)) => 
            match first_argument {
              Some(arg) => [
                vec![Box::new(arg)], 
                rest.into_iter()
                  .map(|expr| Box::new(expr))
                  .collect()
              ].concat(),
              None => vec![],
            },
          None => vec![],
        },
      ),
  )(input)
}

fn parse_assign(input: &str) -> IResult<&str, ExprNode> {
  map(
    separated_pair(
      ws(alpha1),
      tag("="),
      parse_addsub,
    ),
    |(identifier, expr)| ExprNode::Assign(identifier.to_string(), Box::new(expr)),
  )(input)
}

fn parse_var(input: &str) -> IResult<&str, ExprNode> {
  map(
    ws(alpha1),
    |s: &str| ExprNode::Var(s.to_string()),
  )(input)
}

fn parse_expr(input: &str) -> IResult<&str, ExprNode> {
  ws(
    alt((
      parse_assign,
      parse_function_call,
      parse_var,
      parse_nested_expr,
      parse_value,
    )),
  )(input)
}

fn exp_helper(initial: ExprNode, exprs: Vec<(&str, ExprNode)>) -> ExprNode {
  exprs.into_iter().fold(initial, |prev, curr| {
    let (operator, val) = curr;
    let (first, second) = (Box::new(prev), Box::new(val.clone()));
    match operator {
      "+" => ExprNode::Add(first, second),
      "-" => ExprNode::Sub(first, second),
      "*" => ExprNode::Mul(first, second),
      "/" => ExprNode::Div(first, second),
      _ => panic!("Unsupported operator in expression"),
    }
  })
}

fn parse_addsub(input: &str) -> IResult<&str, ExprNode> {
  let (input, value) = parse_muldiv(input)?;
  let (input, rest) = ws(
    many0(
      pair(
        alt((
          tag("+"),
          tag("-",)
        )),
        parse_muldiv,
      ),
    ),
  )(input)?;

  Ok((input, exp_helper(value, rest)))
}

fn parse_muldiv(input: &str) -> IResult<&str, ExprNode> {
  let (input, value) = parse_expr(input)?;
  let (input, rest) = ws(
    many0(
      pair(
        alt((
          tag("*"),
          tag("/"),
        )),
        parse_expr,
      ),
    ),
  )(input)?;

  Ok((input, exp_helper(value, rest)))
}

pub fn parse(input: &str) -> IResult<&str, ExprNode> {
  ws(parse_addsub)(input)
}

#[cfg(test)]
mod tests {
  use crate::parser::{
    ExprNode,
    parse_number,
    parse_negative_number,
    parse_value,
    parse_nested_expr,
    parse_expr,
    parse_addsub,
    parse_muldiv,
    parse_function_call,
    parse_assign,
    parse_var,
    parse,
  };

  #[test]
  fn number_int() {
    assert_eq!(
      parse_number("5"),
      Ok(("", ExprNode::Number(5.))),
    )
  }

  #[test]
  fn number_float() {
    assert_eq!(
      parse_number("5.125"),
      Ok(("", ExprNode::Number(5.125))),
    )
  }

  #[test]
  fn negative_number() {
    assert_eq!(
      parse_negative_number("-20"),
      Ok(("", ExprNode::Number(-20.))),
    )
  }

  #[test]
  fn value() {
    assert_eq!(
      parse_value("123"),
      Ok(("", ExprNode::Number(123.))),
    );
    assert_eq!(
      parse_value("-123"),
      Ok(("", ExprNode::Number(-123.))),
    )
  }

  #[test]
  fn nested_expr() {
    assert_eq!(
      parse_nested_expr("(2 + 3)"),
      Ok((
        "",
        ExprNode::Add(
          Box::new(ExprNode::Number(2.)),
          Box::new(ExprNode::Number(3.)),
        ),
      ))
    )
  }

  #[test]
  fn expr() {
    assert_eq!(
      parse_expr("(1 * (2 + 3))"),
      Ok((
        "",
        ExprNode::Mul(
          Box::new(ExprNode::Number(1.)),
          Box::new(
            ExprNode::Add(
              Box::new(ExprNode::Number(2.)),
              Box::new(ExprNode::Number(3.)),
            ),
          ),
        ),
      ))
    )
  }

  #[test]
  fn addsub() {
    assert_eq!(
      parse_addsub("1 + 2 - 3 + 4"),
      Ok((
        "",
        ExprNode::Add(
          Box::new(
            ExprNode::Sub(
              Box::new(
                ExprNode::Add(
                  Box::new(ExprNode::Number(1.)),
                  Box::new(ExprNode::Number(2.)),
                ),
              ),
              Box::new(ExprNode::Number(3.)),
            ),
          ),
          Box::new(ExprNode::Number(4.)),
        ),
      )),
    )
  }

  #[test]
  fn muldiv() {
    assert_eq!(
      parse_muldiv("1 * 2 / 3 * 4"),
      Ok((
        "",
        ExprNode::Mul(
          Box::new(
            ExprNode::Div(
              Box::new(
                ExprNode::Mul(
                  Box::new(ExprNode::Number(1.)),
                  Box::new(ExprNode::Number(2.)),
                ),
              ),
              Box::new(ExprNode::Number(3.)),
            ),
          ),
          Box::new(ExprNode::Number(4.)),
        ),
      )),
    )
  }

  #[test]
  fn function_call() {
    assert_eq!(
      parse_function_call("pow((2 + 2) * 3, 6 / 2)"),
      Ok((
        "",
        ExprNode::FunctionCall(
          String::from("pow"),
          vec![
            Box::new(
              ExprNode::Mul(
                Box::new(ExprNode::Add(
                  Box::new(ExprNode::Number(2.)),
                  Box::new(ExprNode::Number(2.)),
                )),
                Box::new(ExprNode::Number(3.)),
              ),
            ),
            Box::new(
              ExprNode::Div(
                Box::new(ExprNode::Number(6.)),
                Box::new(ExprNode::Number(2.)),
              ),
            ),
          ],
        ),
      )),
    )
  }

  #[test]
  fn assign() {
    assert_eq!(
      parse_assign("x = 5"),
      Ok((
        "",
        ExprNode::Assign(
          String::from("x"),
          Box::new(ExprNode::Number(5.)),
        ),
      )),
    )
  }

  #[test]
  fn var() {
    assert_eq!(
      parse_var("x"),
      Ok((
        "",
        ExprNode::Var(String::from("x"),)
      )),
    )
  }

  #[test]
  fn all() {
    assert_eq!(
      parse("x = 1 + (3 * 4 - 5) / 2 + pow(3, y)"),
      Ok((
        "",
        ExprNode::Assign(
          String::from("x"),
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
      )),
    )
  }
}