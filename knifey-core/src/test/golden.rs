use crate::parse::parse_expr;
use std::path::PathBuf;

pub fn golden<A: std::fmt::Debug>(name: &str, value: A) -> String {
    let mut buffer = String::new();
    buffer += &format!("{}\n", name);
    buffer += "---\n";
    buffer += &format!("{:#?}\n", value);
    buffer
}

pub fn golden_expr(input: &str) -> String {
    golden(input, parse_expr(input).expect("parse"))
}

pub fn golden_name(input: &str) -> String {
    let name = blake3::hash(input.as_bytes());
    let name = &*name.to_hex();
    name.to_string()
}

pub fn path_to_golden_file(input: &str) -> PathBuf {
    let name = golden_name(input);
    PathBuf::from("src/golden").join(name)
}

pub fn read_golden(input: &str) -> Result<String, std::io::Error> {
    let path = path_to_golden_file(input);
    let buffer = std::fs::read_to_string(path)?;
    Ok(buffer)
}

pub fn write_golden(input: &str) -> Result<(), std::io::Error> {
    let content = golden_expr(input);
    let path = path_to_golden_file(input);
    std::fs::write(path, content)
}

pub fn check_golden_expr(input: &str) {
    if path_to_golden_file(input).exists() {
        let new = golden_expr(input);
        let old = read_golden(input).expect("golden file read");
        assert_eq!(
            new,
            old,
            "{} does not match the AST for `{}`",
            golden_name(input),
            input
        );
    } else {
        write_golden(input).expect("golden file write");
    }
}
