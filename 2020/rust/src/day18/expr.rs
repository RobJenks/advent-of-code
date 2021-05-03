type Val = isize;

#[derive(Copy, Clone, Debug)]
pub enum Op {
    Add,
    Mul
}

#[derive(Clone, Debug)]
pub enum Expr {
    Primitive(Val),
    Compound(Box<Expr>, Vec<(Op, Box<Expr>)>)
}

impl Op {
    pub fn eval(&self, lhs: Val, rhs: Val) -> Val {
        match self {
            Op::Add => lhs + rhs,
            Op::Mul => lhs * rhs
        }
    }

    fn parse(ch: char) -> Self {
        match ch {
            '+' => Op::Add,
            '*' => Op::Mul,
            _ => panic!("Unrecognised operator '{}'", ch)
        }
    }
}

#[derive(Debug, PartialEq)]
enum ParseState {
    InExpr,
    InOp,
    InNested
}

impl Expr {
    pub fn eval(&self) -> Val {
        match self {
            Expr::Primitive(n) => *n,
            Expr::Compound(init, ops) => ops.iter()
                .fold(init.eval(), |acc, (op, expr)| op.eval(acc, expr.eval()))
        }
    }

    pub fn parse(expr_str: &str) -> Self {
        let mut ops = vec![];
        let mut started = 0usize;
        let mut nest_level = 0usize;
        let mut next_op = Op::Add;
        let mut state = ParseState::InExpr;

        let str = format!("{} ", expr_str);
        for (i, ch) in str.chars().enumerate() {
            match (i, ch) {
                // Closing bracket; take action when it closes the nested expression we began at 'started'
                (i, ')') => {
                    assert!(nest_level > 0 && state == ParseState::InNested);
                    nest_level -= 1;
                    if nest_level == 0 {
                        let nested_expr = Self::parse(&str[started+1..i]);
                        ops.push((next_op, Box::new(nested_expr)));
                    }
                }

                // Opening bracket; start parsing a new nested expression
                (i, '(') => {
                    assert!(state == ParseState::InExpr || state == ParseState::InNested);
                    nest_level += 1;
                    if nest_level == 1 {
                        assert_ne!(state, ParseState::InNested); // Can't already be in nested expr parsing mode since this is root-level nesting
                        state = ParseState::InNested;
                        started = i;
                    }
                }

                // Whitespace; commit components if required
                (i, ' ') => {
                    if state == ParseState::InExpr {
                        let value = str[started..i].parse::<Val>()
                            .unwrap_or_else(|e| panic!("Failed to parse expr component '{}' ({})", &str[started..i], e));
                        ops.push((next_op, Box::new(Expr::Primitive(value))));
                        state = ParseState::InOp;
                        started = i + 1;
                    }
                    else if state == ParseState::InOp {
                        assert_eq!(started, (i - 1));   // All ops are single-char
                        next_op = Op::parse(str.chars().nth(started).unwrap_or_else(|| panic!("Failed to get operator at {}", started)));
                        state = ParseState::InExpr;
                        started = i + 1;
                    }
                    else if state == ParseState::InNested {
                        if nest_level == 0 {            // If level == 0 then nested expression has already been committed
                            state = ParseState::InOp;
                            started = i + 1;
                        }
                    }
                }

                // Parsing numeric value
                (_, _) if state == ParseState::InExpr => {}

                // Parsing operator
                (_, _) if state == ParseState::InOp => {}

                // Moving over nested expression
                (_, _) if state == ParseState::InNested => {}

                (i, ch) => panic!("Unexpected parser state at {} ({}) for string '{}' (state={:?}, start={}, nest={}, next_op={:?}",
                                             i, ch, str, state, started, nest_level, next_op)
            }
        }

        Expr::Compound(Box::new(Expr::Primitive(0)), ops)
    }
}

#[cfg(test)]
mod tests {
    use crate::day18::expr::Expr;
    use crate::day18::expr::Expr::*;
    use crate::day18::expr::Op::*;

    #[test]
    fn test_expr_eval() {
        assert_eq!(48, Compound(Box::new(Primitive(12)), vec![(Add, Box::new(Primitive(36)))]).eval());
        assert_eq!(156, Compound(Box::new(Primitive(12)), vec![(Add, Box::new(
                        Compound(Box::new(Primitive(36)), vec![(Mul, Box::new(Primitive(4)))])))]).eval());
    }

    #[test]
    fn test_flat_expr_parsing() {
        assert_eq!(3, Expr::parse("1 + 2").eval());
        assert_eq!(6, Expr::parse("1 + 2 + 3").eval());
        assert_eq!(20, Expr::parse("1 * 2 + 3 * 4").eval());
    }

    #[test]
    fn test_basic_nested_expr_parsing() {
        assert_eq!(3, Expr::parse("(1 + 2)").eval());
        assert_eq!(6, Expr::parse("(1 + 2) + 3").eval());
        assert_eq!(7, Expr::parse("1 + (2 * 3)").eval());
        assert_eq!(28, Expr::parse("1 + (2 * 3) * 4").eval());
    }

    #[test]
    fn test_multiple_nested_expr_parsing() {
        assert_eq!(60, Expr::parse("1 + (2 * (3 + 4)) * 4").eval());
        assert_eq!(113, Expr::parse("1 + ((2 * 2) * (3 + 4) * 4)").eval());
    }
}