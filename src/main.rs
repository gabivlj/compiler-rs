pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;
fn main() {
    use ast::node::{Statement, Str};
    use lexer::Lexer;
    use parser::Parser;
    let s = "
    let fibonacci = fn(x) {
      if (x == 0) {
        0
      } else {
        if (x == 1) {
          return 1;
        } else {
          fibonacci(x - 1) + fibonacci(x - 2);
        }
      }
    };
    fibonacci(35);
"
    .repeat(1000);
    let elapsed = std::time::Instant::now();
    for i in 0..1000 {
        let mut l = Lexer::new(&s);
        loop {
            if l.next_token() != token::TokenType::EOF {
            } else {
                break;
            }
        }
    }
    println!("elapsed: {}", elapsed.elapsed().as_millis());
}
