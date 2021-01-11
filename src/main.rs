pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;

#[cfg(not(target_env = "msvc"))]
use jemallocator::Jemalloc;

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

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
        let p = Parser::new(l).parse_program();
        p.unwrap();
    }
    println!("elapsed: {}", elapsed.elapsed().as_millis());
}
