use googletest::prelude::*;
use std::io::Write;
use std::process::{Command, Stdio};

const APP_NAME: &str = "target/debug/rlox";

#[gtest]
fn test_number_expression() {
    let mut child = Command::new(APP_NAME)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("Could not start child command");
    if let Some(stdin) = child.stdin.as_mut() {
        writeln!(stdin, "321").expect("Error writing to child process");
        // stdin is dropped here, sending an EOF. Also, dropping implicitly flushes.
    }

    let output = child.wait_with_output().expect("Error waiting for child");
    let stdout = String::from_utf8_lossy(&output.stdout);
    expect_that!(
        stdout.lines().collect::<Vec<&str>>(),
        elements_are!(&"> Result: 321", &"> ", &"Ok, bye!")
    );
}

#[gtest]
fn test_empty() {
    let mut child = Command::new(APP_NAME)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("Could not start child command");
    if let Some(stdin) = child.stdin.as_mut() {
        writeln!(stdin).expect("Error writing to child process");
        // stdin is dropped here, sending an EOF. Also, dropping implicitly flushes.
    }

    let output = child.wait_with_output().expect("Error waiting for child");
    let stdout = String::from_utf8_lossy(&output.stdout);
    expect_that!(
        stdout.lines().collect::<Vec<&str>>(),
        elements_are!(&"> No input.", &"> ", &"Ok, bye!")
    );
}
