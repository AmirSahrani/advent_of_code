use std::fs;
use std::time::{Duration, Instant};

pub fn read_file(filename: &str) -> String {
    fs::read_to_string(filename).expect("Reading file failed")
}
pub fn benchmark<'input, Str: ?Sized, F>(f: F, input: &'input Str) -> Duration
where
    F: Fn(&'input Str),
{
    let now = Instant::now();
    f(input);
    let elapsed = now.elapsed();
    elapsed
}
