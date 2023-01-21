#![deny(missing_debug_implementations)]

mod error;
mod parser;

fn main() {
    let prog = include_str!("../program.lll");
    let parser = parser::LLLParser::new();
    let res = parser.parse(prog);
    println!("{:#?}", res);
}
