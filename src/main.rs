#![deny(missing_debug_implementations)]

mod ast;
mod error;
mod misc;
mod parser;
mod type_check;

fn main() {
    let prog = include_str!("../program.lll");
    let parser = parser::LLLParser::new();
    let res = parser.parse(prog);
    println!("{:#?}", res);
}
