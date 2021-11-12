extern crate toys_rs;

use clap::Parser;
use std::fs;
use std::io::Result;

fn main() -> Result<()> {
    let opts = Options::parse();

    let code = match opts {
        Options {
            file_path: None,
            eval: None,
        } => panic!("Target toys code is required. Pass toys code with argument or -e option."),
        Options {
            file_path: Some(file_path),
            ..
        } => fs::read_to_string(&file_path).expect(&format!("Failed to read file {}", file_path)),
        Options {
            eval: Some(code), ..
        } => code,
    };

    match toys_rs::run(&code) {
        Ok(exit_status) => std::process::exit(exit_status),
        Err(e) => {
            println!("[Runtime Error] {}", e.to_string());
            std::process::exit(1);
        }
    }
}

#[derive(Debug, Parser)]
struct Options {
    #[clap(name = "File path")]
    file_path: Option<String>,

    #[clap(short, long)]
    eval: Option<String>,
}
