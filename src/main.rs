pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;
fn main() {
    use ast::node::{Statement, Str};
    use lexer::Lexer;
    use parser::Parser;
    let mut parser = Parser::new(Lexer::new(
        "
        if (x == 3) {
            let x = 0;
            let z = 0;
            return x + z;
        } else if x {
            return x + !!zzzz;
        } else {
            return f() + fn(x,y) { x + y }(1, 2);
        }
        let x = 3;
        return if x == 3 {
            4;
        } else {
            3;
        };
    ",
    ));
    let program = if let Statement::Program(program) = parser.parse_program().unwrap().node {
        program
    } else {
        vec![]
    };
    for stmt in program.iter() {
        println!("{}", stmt.str());
    }
}
