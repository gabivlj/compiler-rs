#![feature(maybe_uninit_uninit_array)]
#[macro_use]
extern crate lazy_static;
pub mod ast;
pub mod hash_undo;
pub mod lexer;
pub mod parser;
pub mod semantic;
pub mod string_interning;
pub mod token;

#[cfg(not(target_env = "msvc"))]
use jemallocator::Jemalloc;

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

fn main() {
    use lexer::Lexer;
    use parser::Parser;
    let s = "
    let fibonacci: fib = fn(x) {
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
    for _ in 0..10 {
        let l = Lexer::new(&s);
        let p = Parser::new(l).parse_program();
        p.unwrap();
    }
    println!("elapsed: {}", elapsed.elapsed().as_millis());
}
