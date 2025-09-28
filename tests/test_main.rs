use googletest::prelude::*;
use std::io::Write;
use std::process::{Command, Stdio};

const APP_NAME: &str = "target/debug/rlox";

fn run_repl_with_input(lines: &[&str]) -> Vec<String> {
    let mut child = Command::new(APP_NAME)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("Could not start child command");

    if let Some(stdin) = child.stdin.as_mut() {
        for line in lines {
            writeln!(stdin, "{line}").expect("Error writing to child process");
        }
        // Dropping stdin sends EOF to the child process.
    }

    let output = child
        .wait_with_output()
        .expect("Error waiting for child process");
    assert!(
        output.status.success(),
        "Child process exited with status {:?}",
        output.status
    );

    String::from_utf8_lossy(&output.stdout)
        .lines()
        .map(str::to_owned)
        .collect()
}

#[gtest]
fn test_number_expression() {
    expect_that!(
        run_repl_with_input(&["321"]),
        elements_are!(&"> 321", &"> ", &"Ok, bye!")
    );
}

#[gtest]
fn test_empty() {
    expect_that!(
        run_repl_with_input(&[""]),
        elements_are!(&"> No input.", &"> ", &"Ok, bye!")
    );
}

#[gtest]
fn test_simple_calculation() {
    expect_that!(
        run_repl_with_input(&["2 + 3*5"]),
        elements_are!(&"> (2 + (3 * 5))", &"> ", &"Ok, bye!")
    );
}
