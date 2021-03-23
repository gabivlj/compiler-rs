#![feature(maybe_uninit_uninit_array)]
#[macro_use]
extern crate lazy_static;

#[macro_use]
macro_rules! string_id {
    ($s:expr) => {
        StringInternal::add_string($s)
    };
}

#[macro_use]
macro_rules! string {
    ($s:expr) => {
        StringInternal::get_id($s)
    };
}

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
    let fibonacci: int = fn(x: int) {
      if (x == 0) {
        return 0;
      } else {
        if (x == 1) {
          return 1;
        } else {
          return fibonacci(x - 1) + fibonacci(x - 2);
        }
      }
    };
    fibonacci(35);
";
    let elapsed = std::time::Instant::now();
    for _ in 0..1_000_000 {
        let l = Lexer::new(&s);
        let p = Parser::new(l).parse_program();
        let mut p = p.unwrap();
        semantic::SemanticAnalysis::new()
            .type_check_statement(&mut p.node)
            .unwrap();
    }
    println!("elapsed: {}", elapsed.elapsed().as_millis());
}
