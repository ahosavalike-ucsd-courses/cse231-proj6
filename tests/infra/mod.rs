use std::{
    path::{Path, PathBuf},
    process::Command,
};

pub(crate) enum TestKind {
    Success,
    RuntimeError,
    StaticError,
}

#[macro_export]
macro_rules! success_tests {
    ($($tt:tt)*) => {
        mod comp_succ { $crate::compile_tests!(Success => $($tt)*); }
        mod eval_succ { $crate::eval_tests!(Success => $($tt)*); }
    }
}

#[macro_export]
macro_rules! runtime_error_tests {
    ($($tt:tt)*) => {
        mod comp_run { $crate::compile_tests!(RuntimeError => $($tt)*); }
        mod eval_run { $crate::eval_tests!(RuntimeError => $($tt)*); }
    }
}

#[macro_export]
macro_rules! static_error_tests {
    ($($tt:tt)*) => {
        mod comp_stat { $crate::compile_tests!(StaticError => $($tt)*); }
        mod eval_stat { $crate::eval_tests!(StaticError => $($tt)*); }
    }
}

#[macro_export]
macro_rules! success_compile_tests {
    ($($tt:tt)*) => { mod comp_succ { $crate::compile_tests!(Success => $($tt)*); }}
}

#[macro_export]
macro_rules! runtime_error_compile_tests {
    ($($tt:tt)*) => { mod comp_run { $crate::compile_tests!(RuntimeError => $($tt)*); }}
}

#[macro_export]
macro_rules! static_error_compile_tests {
    ($($tt:tt)*) => { mod comp_stat { $crate::compile_tests!(StaticError => $($tt)*); }}
}

#[macro_export]
macro_rules! success_eval_tests {
    ($($tt:tt)*) => { mod eval_succ { $crate::eval_tests!(Success => $($tt)*); }}
}

#[macro_export]
macro_rules! runtime_error_eval_tests {
    ($($tt:tt)*) => { mod eval_run { $crate::eval_tests!(RuntimeError => $($tt)*); }}
}

#[macro_export]
macro_rules! static_error_eval_tests {
    ($($tt:tt)*) => { mod eval_stat { $crate::eval_tests!(StaticError => $($tt)*); }}
}

#[macro_export]
macro_rules! compile_tests {
    ($kind:ident =>
        $(
            {
                name: $name:ident,
                file: $file:literal,
                $(input: $input:literal,)?
                expected: $expected:literal $(,)?
                $(" $(tt:$tt)* ")?
            }
        ),*
        $(,)?
    ) => {
        $(
            #[test]
            fn $name() {
                #[allow(unused_assignments, unused_mut)]
                let mut input = None;
                $(input = Some($input);)?
                let kind = $crate::infra::TestKind::$kind;
                $crate::infra::run_test_compile(stringify!($name), $file, input, $expected, kind);
            }
        )*
    };
}

#[macro_export]
macro_rules! eval_tests {
    ($kind:ident =>
        $(
            {
                name: $name:ident,
                file: $file:literal,
                $(input: $input:literal,)?
                expected: $expected:literal $(,)?
                $(" $(tt:$tt)* ")?
            }
        ),*
        $(,)?
    ) => {
        $(
            #[test]
            fn $name() {
                #[allow(unused_assignments, unused_mut)]
                let mut input = None;
                $(input = Some($input);)?
                let kind = $crate::infra::TestKind::$kind;
                $crate::infra::run_test_eval($file, input, $expected, kind);
            }
        )*
    };
}

pub(crate) fn run_test_compile(
    name: &str,
    file: &str,
    input: Option<&str>,
    expected: &str,
    kind: TestKind,
) {
    let file = Path::new("tests").join(file);

    // Compile Test
    match kind {
        TestKind::Success => run_success_test(name, &file, expected, input),
        TestKind::RuntimeError => run_runtime_error_test(name, &file, expected, input),
        TestKind::StaticError => run_static_error_test(name, &file, expected),
    }
}

pub(crate) fn run_test_eval(file: &str, input: Option<&str>, expected: &str, kind: TestKind) {
    let file = Path::new("tests").join(file);

    // Eval Test
    match kind {
        TestKind::Success => run_success_test_eval(&file, expected, input),
        TestKind::RuntimeError => run_runtime_error_test_eval(&file, expected, input),
        TestKind::StaticError => run_static_error_test_eval(&file, expected, input),
    }
}

fn run_success_test_eval(file: &Path, expected: &str, input: Option<&str>) {
    match eval(file, input) {
        Err(err) => {
            panic!("eval: expected a successful execution, but got an error: `{err}`");
        }
        Ok(actual_output) => {
            println!("eval:");
            diff(expected, actual_output);
        }
    }
}

fn run_success_test(name: &str, file: &Path, expected: &str, input: Option<&str>) {
    if let Err(err) = compile(name, file) {
        panic!("expected a successful compilation, but got an error: `{err}`");
    }
    match run(name, input) {
        Err(err) => {
            panic!("expected a successful execution, but got an error: `{err}`");
        }
        Ok(actual_output) => {
            diff(expected, actual_output);
        }
    }
}

fn run_runtime_error_test_eval(file: &Path, expected: &str, input: Option<&str>) {
    match eval(file, input) {
        Ok(out) => {
            panic!("eval: expected a runtime error, but program executed succesfully: `{out}`");
        }
        Err(err) => {
            println!("eval:");
            check_error_msg(&err, expected)
        }
    }
}

fn run_runtime_error_test(name: &str, file: &Path, expected: &str, input: Option<&str>) {
    if let Err(err) = compile(name, file) {
        panic!("expected a successful compilation, but got an error: `{err}`");
    }
    match run(name, input) {
        Ok(out) => {
            panic!("expected a runtime error, but program executed succesfully - expected error: `{expected}`, output: `{out}`");
        }
        Err(err) => check_error_msg(&err, expected),
    }
}

fn run_static_error_test_eval(file: &Path, expected: &str, input: Option<&str>) {
    match eval(file, input) {
        Ok(s) => panic!("eval: expected a failure, but compilation succeeded: {s}"),
        Err(err) => {
            println!("eval:");
            check_error_msg(&err, expected);
        }
    }
}

fn run_static_error_test(name: &str, file: &Path, expected: &str) {
    match compile(name, file) {
        Ok(()) => {
            panic!(
                "expected a static error, but compilation succeeded - expected error: `{expected}`"
            )
        }
        Err(err) => check_error_msg(&err, expected),
    }
}

fn compile(name: &str, file: &Path) -> Result<(), String> {
    // Run the compiler
    let compiler: PathBuf = ["target", "debug", env!("CARGO_PKG_NAME")].iter().collect();
    let output = Command::new(&compiler)
        .arg(file)
        .arg(&mk_path(name, Ext::Asm))
        .output()
        .expect("could not run the compiler");
    if !output.status.success() {
        return Err(String::from_utf8(output.stderr).unwrap());
    }

    // Assemble and link
    let output = Command::new("make")
        .arg(&mk_path(name, Ext::Run))
        .output()
        .expect("could not run make");
    assert!(output.status.success(), "linking failed");

    Ok(())
}

fn eval(file: &Path, input: Option<&str>) -> Result<String, String> {
    // Run the compiler
    let arch = "x86_64-apple-darwin";
    let mut cmd = Command::new(
        [
            "target",
            arch,
            "debug",
            env!("CARGO_PKG_NAME"),
        ]
        .iter()
        .collect::<PathBuf>(),
    );
    cmd.args(["-e", &format!("{}", file.to_str().unwrap())]);

    if let Some(input) = input {
        cmd.arg(input);
    }

    println!("{:?} {:?}", cmd.get_program(), cmd.get_args());

    let output = cmd.output().expect("eval: could not run the compiler");

    if output.status.success() {
        Ok(String::from_utf8(output.stdout).unwrap().trim().to_string())
    } else {
        Err(String::from_utf8(output.stderr).unwrap().trim().to_string())
    }
}

fn run(name: &str, input: Option<&str>) -> Result<String, String> {
    let mut cmd = Command::new(&mk_path(name, Ext::Run));
    if let Some(input) = input {
        cmd.arg(input);
    }
    let output = cmd.output().unwrap();
    if output.status.success() {
        Ok(String::from_utf8(output.stdout).unwrap().trim().to_string())
    } else {
        Err(String::from_utf8(output.stderr).unwrap().trim().to_string())
    }
}

fn check_error_msg(found: &str, expected: &str) {
    let lower_found = found.trim().to_lowercase();
    let lower_expected = expected.trim().to_lowercase();
    assert!(
        lower_found.contains(&lower_expected),
        "the reported error message does not contain the expected subtring - found: `{found}`, expected: `{expected}`",
    );
}

fn diff(expected: &str, found: String) {
    let expected = expected.trim();

    let expected_lines: Vec<&str> = expected.lines().collect();
    let actual_lines: Vec<&str> = found.lines().collect();
    if expected_lines != actual_lines {
        eprintln!(
            "output differed!\n{}",
            prettydiff::diff_lines(&found, expected)
        );
        panic!("test failed");
    }
}

fn mk_path(name: &str, ext: Ext) -> PathBuf {
    Path::new("tests").join(format!("{name}.{ext}"))
}

#[derive(Copy, Clone)]
enum Ext {
    Asm,
    Run,
}

impl std::fmt::Display for Ext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ext::Asm => write!(f, "s"),
            Ext::Run => write!(f, "run"),
        }
    }
}
