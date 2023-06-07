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
        mod comp_succ { $crate::compile_tests!(Success, true => $($tt)*); }
        mod eval_succ { $crate::eval_tests!(Success => $($tt)*); }
    }
}

#[macro_export]
macro_rules! runtime_error_tests {
    ($($tt:tt)*) => {
        mod comp_run { $crate::compile_tests!(RuntimeError, true => $($tt)*); }
        mod eval_run { $crate::eval_tests!(RuntimeError => $($tt)*); }
    }
}

#[macro_export]
macro_rules! static_error_tests {
    ($($tt:tt)*) => {
        mod comp_stat { $crate::compile_tests!(StaticError, false => $($tt)*); }
        mod eval_stat { $crate::eval_tests!(StaticError => $($tt)*); }
    }
}

#[macro_export]
macro_rules! success_compile_tests {
    ($($tt:tt)*) => { mod comp_succ { $crate::compile_tests!(Success, true => $($tt)*); }}
}

#[macro_export]
macro_rules! runtime_error_compile_tests {
    ($($tt:tt)*) => { mod comp_run { $crate::compile_tests!(RuntimeError, true => $($tt)*); }}
}

#[macro_export]
macro_rules! static_error_compile_tests {
    ($($tt:tt)*) => { mod comp_stat { $crate::compile_tests!(StaticError, false => $($tt)*); }}
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
    ($kind:ident, $profile:literal =>
        $(
            {
                name: $name:ident,
                file: $file:literal,
                $(input: $input:literal,)?
                $(heap_size: $heap_size:literal,)?
                $(time_trials: $time_trials:literal,)?
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
                #[allow(unused_assignments, unused_mut)]
                let mut heap_size = None;
                $(heap_size = Some($heap_size);)?
                #[allow(unused_assignments, unused_mut)]
                let mut time_trials = None;
                $(time_trials = Some($time_trials);)?
                let kind = $crate::infra::TestKind::$kind;
                $crate::infra::run_test_compile(stringify!($name), $file, input, heap_size, time_trials, $expected, kind, $profile);
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
                $(heap_size: $heap_size:literal,)?
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
                #[allow(unused_assignments, unused_mut)]
                let mut heap_size = None;
                $(heap_size = Some($heap_size);)?
                let kind = $crate::infra::TestKind::$kind;
                $crate::infra::run_test_eval($file, input, heap_size, $expected, kind);
            }
        )*
    };
}

#[allow(dead_code)]
fn profile(name: &str, input: Option<&str>, heap_size: Option<usize>, time_trials: Option<u32>) {
    println!("Profiling: {name}");
    if cfg!(windows) {
        eprintln!("The profiling tools being used do not work on your platform.");
        return;
    }

    let mut program_str = mk_path(name, Ext::Run).to_str().unwrap().to_owned();
    if let Some(input) = input {
        program_str.push_str(" ");
        program_str.push_str(input);
    }
    if let Some(heap_size) = heap_size {
        program_str.push_str(" ");
        program_str.push_str(&heap_size.to_string());
    }

    profile_dynamic_instr_count(&program_str);
    profile_static_instr_count(mk_path(name, Ext::Obj).to_str().unwrap());
    profile_time_taken(&program_str, time_trials);
}

fn profile_dynamic_instr_count(program_str: &str) {
    let cmd = if cfg!(target_os = "linux") {
        format!("valgrind --tool=callgrind --callgrind-out-file=tests/callgrind.out {program_str} >/dev/null 2>&1 && grep \"^summary:\" tests/callgrind.out | awk '{{print $2}}'")
    } else {
        eprintln!("valgrind is only available on Linux. Using usr/bin/time to gauge instructions instead (use it only for relative comparisons on your system)");
        format!(
            "/usr/bin/time -l {program_str} 2>&1 | grep -i \"instructions\" | awk '{{print $1}}'"
        )
    };

    let out = Command::new("sh").args(["-c", &cmd]).output().unwrap();
    if out.status.success() {
        let out_str = String::from_utf8(out.stdout).unwrap().trim().to_string();
        println!("Instructions executed: {out_str}");
    } else {
        eprintln!("Failed to measure instructions executed");
    }
    println!();
}

fn profile_static_instr_count(program_str: &str) {
    let cmd = if cfg!(target_os = "linux") {
        format!("objdump -d -M intel \"{program_str}\" | awk -F'\\t' '{{if ($3 != \"\") {{count++}}}} END {{print count}}'")
    } else {
        format!( "objdump -d --x86-asm-syntax=intel {program_str} | grep '^\\ ' | expand | cut -c41- | sed 's/ .*//' | wc -l")
    };

    let out = Command::new("sh").args(["-c", &cmd]).output().unwrap();
    if out.status.success() {
        let out_str = String::from_utf8(out.stdout).unwrap().trim().to_string();
        println!("Instructions in generated .s: {out_str}");
    } else {
        eprintln!("Failed to get static instruction count");
    }

    let cmd = if cfg!(target_os = "linux") {
        format!("objdump -d -M intel \"{program_str}\" | awk -F'\\t' '{{if ($3 != \"\") {{split($3, instr, /[[:space:]]/); print instr[1]}}}}' | sort | uniq -c | sort -nr")
    } else {
        format!( "objdump -d --x86-asm-syntax=intel {program_str} | grep '^\\ ' | expand | cut -c41- | sed 's/ .*//' | sed '/^$/d' | sort | uniq -c | sort -nr")
    };

    let out_counts = Command::new("sh").args(["-c", &cmd]).output().unwrap();
    if out_counts.status.success() {
        let out_str = String::from_utf8(out_counts.stdout).unwrap().to_string();
        println!("Instructions by type in your generated assembly before linking is:\n{out_str}");
    } else {
        eprintln!("Failed to get static instruction types counts");
    }
    println!();
}

fn profile_time_taken(program_str: &str, trials: Option<u32>) {
    let cmd = if cfg!(target_os = "linux") {
        format!(
            "perf stat -e task-clock:u {program_str} 2>&1 | grep -oP '(\\d+\\.\\d+)' | head -n 1"
        )
    } else {
        eprintln!("perf is only available on Linux. Using usr/bin/time instead.");
        format!("/usr/bin/time -p {program_str} 2>&1 | grep 'user' | awk '{{print $2}}'")
    };

    println!("Time taken in ms (seconds on MacOS):");
    for i in 1..(trials.unwrap_or(5) + 1) {
        let out = Command::new("sh").args(["-c", &cmd]).output().unwrap();
        if out.status.success() {
            let out_str = String::from_utf8(out.stdout).unwrap().trim().to_string();
            println!("{i} {out_str}");
        } else {
            eprintln!("{i} Failed to measure time taken");
        }
    }
    println!();
}

#[allow(dead_code)]
pub(crate) fn run_test_compile(
    name: &str,
    file: &str,
    input: Option<&str>,
    heap_size: Option<usize>,
    time_trials: Option<u32>,
    expected: &str,
    kind: TestKind,
    profile: bool,
) {
    let file = Path::new("tests").join(file);

    // Compile Test
    match kind {
        TestKind::Success => run_success_test(name, &file, expected, input, heap_size, profile, time_trials),
        TestKind::RuntimeError => run_runtime_error_test(name, &file, expected, input, heap_size, profile, time_trials),
        TestKind::StaticError => run_static_error_test(name, &file, expected),
    }
}

#[allow(dead_code)]
pub(crate) fn run_test_eval(
    file: &str,
    input: Option<&str>,
    heap_size: Option<usize>,
    expected: &str,
    kind: TestKind,
) {
    let file = Path::new("tests").join(file);

    // Eval Test
    match kind {
        TestKind::Success => run_success_test_eval(&file, expected, input, heap_size),
        TestKind::RuntimeError => run_runtime_error_test_eval(&file, expected, input, heap_size),
        TestKind::StaticError => run_static_error_test_eval(&file, expected, input),
    }
}

#[allow(dead_code)]
fn run_success_test_eval(
    file: &Path,
    expected: &str,
    input: Option<&str>,
    heap_size: Option<usize>,
) {
    match eval(file, input, heap_size) {
        Err(err) => {
            panic!("eval: expected a successful execution, but got an error: `{err}`");
        }
        Ok(actual_output) => {
            println!("eval:");
            diff(expected, actual_output);
        }
    }
}

#[allow(dead_code)]
fn run_success_test(
    name: &str,
    file: &Path,
    expected: &str,
    input: Option<&str>,
    heap_size: Option<usize>,
    prof: bool,
    time_trials: Option<u32>,
) {
    if let Err(err) = compile(name, file) {
        panic!("expected a successful compilation, but got an error: `{err}`");
    }
    match run(name, input, heap_size) {
        Err(err) => {
            panic!("expected a successful execution, but got an error: `{err}`");
        }
        Ok(actual_output) => {
            diff(expected, actual_output);
        }
    }
    if prof {
        profile(name, input, heap_size, time_trials);
    }
}

#[allow(dead_code)]
fn run_runtime_error_test_eval(
    file: &Path,
    expected: &str,
    input: Option<&str>,
    heap_size: Option<usize>,
) {
    match eval(file, input, heap_size) {
        Ok(out) => {
            panic!("eval: expected a runtime error, but program executed succesfully: `{out}`");
        }
        Err(err) => {
            println!("eval:");
            check_error_msg(&err, expected)
        }
    }
}

#[allow(dead_code)]
fn run_runtime_error_test(
    name: &str,
    file: &Path,
    expected: &str,
    input: Option<&str>,
    heap_size: Option<usize>,
    prof: bool,
    time_trials: Option<u32>,
) {
    if let Err(err) = compile(name, file) {
        panic!("expected a successful compilation, but got an error: `{err}`");
    }
    match run(name, input, heap_size) {
        Ok(out) => {
            panic!("expected a runtime error, but program executed succesfully - expected error: `{expected}`, output: `{out}`");
        }
        Err(err) => check_error_msg(&err, expected),
    }
    if prof {
        profile(name, input, heap_size, time_trials);
    }
}

#[allow(dead_code)]
fn run_static_error_test_eval(file: &Path, expected: &str, input: Option<&str>) {
    match eval(file, input, None) {
        Ok(s) => panic!("eval: expected a failure, but compilation succeeded: {s}"),
        Err(err) => {
            println!("eval:");
            check_error_msg(&err, expected);
        }
    }
}

#[allow(dead_code)]
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

#[allow(dead_code)]
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

#[allow(dead_code)]
fn eval(file: &Path, input: Option<&str>, heap_size: Option<usize>) -> Result<String, String> {
    // Run the compiler
    let arch = "x86_64-apple-darwin";
    let mut cmd = Command::new(
        ["target", arch, "debug", env!("CARGO_PKG_NAME")]
            .iter()
            .collect::<PathBuf>(),
    );
    cmd.args(["-e", &format!("{}", file.to_str().unwrap())]);

    if let Some(input) = input {
        cmd.arg(input);
    }
    if let Some(heap_size) = heap_size {
        if input.is_none() {
            cmd.arg("false");
        }
        cmd.arg(heap_size.to_string());
    }

    println!("{:?} {:?}", cmd.get_program(), cmd.get_args());

    let output = cmd.output().expect("eval: could not run the compiler");

    if output.status.success() {
        Ok(String::from_utf8(output.stdout).unwrap().trim().to_string())
    } else {
        Err(String::from_utf8(output.stderr).unwrap().trim().to_string())
    }
}

#[allow(dead_code)]
fn run(name: &str, input: Option<&str>, heap_size: Option<usize>) -> Result<String, String> {
    let mut cmd = Command::new(&mk_path(name, Ext::Run));
    if let Some(input) = input {
        cmd.arg(input);
    }
    if let Some(heap_size) = heap_size {
        if input.is_none() {
            cmd.arg("false");
        }
        cmd.arg(heap_size.to_string());
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
    Obj,
}

impl std::fmt::Display for Ext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ext::Asm => write!(f, "s"),
            Ext::Run => write!(f, "run"),
            Ext::Obj => write!(f, "o"),
        }
    }
}
