//! Basic tests for the `run` subcommand

use anyhow::{bail, Context};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use wasmer_integration_tests_cli::{get_wasmer_path, ASSET_PATH, C_ASSET_PATH};

fn wasi_test_python_path() -> PathBuf {
    Path::new(C_ASSET_PATH).join("python-0.1.0.wasmer")
}

fn wasi_test_wasm_path() -> PathBuf {
    Path::new(C_ASSET_PATH).join("qjs.wasm")
}

fn test_no_imports_wat_path() -> PathBuf {
    Path::new(ASSET_PATH).join("fib.wat")
}

fn test_no_start_wat_path() -> PathBuf {
    Path::new(ASSET_PATH).join("no_start.wat")
}

/// Ignored on Windows because running vendored packages does not work
/// since Windows does not allow `::` characters in filenames (every other OS does)
///
/// The syntax for vendored package atoms has to be reworked for this to be fixed, see
/// https://github.com/wasmerio/wasmer/issues/3535
// FIXME: Re-enable. See https://github.com/wasmerio/wasmer/issues/3717
#[ignore]
#[cfg_attr(target_os = "windows", ignore)]
#[test]
fn test_run_customlambda() -> anyhow::Result<()> {
    let bindir = String::from_utf8(
        Command::new(get_wasmer_path())
            .arg("config")
            .arg("--bindir")
            .output()
            .expect("failed to run wasmer config --bindir")
            .stdout,
    )
    .expect("wasmer config --bindir stdout failed");

    // /Users/fs/.wasmer/bin
    let checkouts_path = Path::new(&bindir)
        .parent()
        .expect("--bindir: no parent")
        .join("checkouts");
    println!("checkouts path: {}", checkouts_path.display());
    let _ = std::fs::remove_dir_all(&checkouts_path);

    let output = Command::new(get_wasmer_path())
        .arg("run")
        .arg("https://wapm.io/ciuser/customlambda")
        // TODO: this argument should not be necessary later
        // see https://github.com/wasmerio/wasmer/issues/3514
        .arg("customlambda.py")
        .arg("55")
        .output()?;

    let stdout_output = std::str::from_utf8(&output.stdout).unwrap();
    let stderr_output = std::str::from_utf8(&output.stderr).unwrap();

    println!("first run:");
    println!("stdout: {stdout_output}");
    println!("stderr: {stderr_output}");
    assert_eq!(stdout_output, "139583862445\n");

    // Run again to verify the caching
    let output = Command::new(get_wasmer_path())
        .arg("run")
        .arg("https://wapm.io/ciuser/customlambda")
        // TODO: this argument should not be necessary later
        // see https://github.com/wasmerio/wasmer/issues/3514
        .arg("customlambda.py")
        .arg("55")
        .output()?;

    let stdout_output = std::str::from_utf8(&output.stdout).unwrap();
    let stderr_output = std::str::from_utf8(&output.stderr).unwrap();

    println!("second run:");
    println!("stdout: {stdout_output}");
    println!("stderr: {stderr_output}");
    assert_eq!(stdout_output, "139583862445\n");

    Ok(())
}

#[allow(dead_code)]
fn assert_tarball_is_present_local(target: &str) -> Result<PathBuf, anyhow::Error> {
    let wasmer_dir = std::env::var("WASMER_DIR").expect("no WASMER_DIR set");
    let directory = match target {
        "aarch64-darwin" => "wasmer-darwin-arm64.tar.gz",
        "x86_64-darwin" => "wasmer-darwin-amd64.tar.gz",
        "x86_64-linux-gnu" => "wasmer-linux-amd64.tar.gz",
        "aarch64-linux-gnu" => "wasmer-linux-aarch64.tar.gz",
        "x86_64-windows-gnu" => "wasmer-windows-gnu64.tar.gz",
        _ => return Err(anyhow::anyhow!("unknown target {target}")),
    };
    let libwasmer_cache_path = Path::new(&wasmer_dir).join("cache").join(directory);
    if !libwasmer_cache_path.exists() {
        return Err(anyhow::anyhow!(
            "targz {} does not exist",
            libwasmer_cache_path.display()
        ));
    }
    println!("using targz {}", libwasmer_cache_path.display());
    Ok(libwasmer_cache_path)
}

// FIXME: Fix and re-enable this test
// See https://github.com/wasmerio/wasmer/issues/3615
// #[test]
#[allow(dead_code)]
fn test_cross_compile_python_windows() -> anyhow::Result<()> {
    let temp_dir = tempfile::TempDir::new()?;

    #[cfg(not(windows))]
    let targets = &[
        "aarch64-darwin",
        "x86_64-darwin",
        "x86_64-linux-gnu",
        "aarch64-linux-gnu",
        "x86_64-windows-gnu",
    ];

    #[cfg(windows)]
    let targets = &[
        "aarch64-darwin",
        "x86_64-darwin",
        "x86_64-linux-gnu",
        "aarch64-linux-gnu",
    ];

    // MUSL has no support for LLVM in C-API
    #[cfg(target_env = "musl")]
    let compilers = &["cranelift", "singlepass"];
    #[cfg(not(target_env = "musl"))]
    let compilers = &["cranelift", "singlepass", "llvm"];

    // llvm-objdump  --disassemble-all --demangle ./objects/wasmer_vm-50cb118b098c15db.wasmer_vm.60425a0a-cgu.12.rcgu.o
    // llvm-objdump --macho --exports-trie ~/.wasmer/cache/wasmer-darwin-arm64/lib/libwasmer.dylib
    let excluded_combinations = &[
        ("aarch64-darwin", "llvm"), // LLVM: aarch64 not supported relocation Arm64MovwG0 not supported
        ("aarch64-linux-gnu", "llvm"), // LLVM: aarch64 not supported relocation Arm64MovwG0 not supported
        // https://github.com/ziglang/zig/issues/13729
        ("x86_64-darwin", "llvm"), // undefined reference to symbol 'wasmer_vm_raise_trap' kind Unknown
        ("x86_64-windows-gnu", "llvm"), // unimplemented symbol `wasmer_vm_raise_trap` kind Unknown
    ];

    for t in targets {
        for c in compilers {
            if excluded_combinations.contains(&(t, c)) {
                continue;
            }
            println!("{t} target {c}");
            let python_wasmer_path = temp_dir.path().join(format!("{t}-python"));

            let tarball = match std::env::var("GITHUB_TOKEN") {
                Ok(_) => Some(assert_tarball_is_present_local(t)?),
                Err(_) => None,
            };
            let mut output = Command::new(get_wasmer_path());

            output.arg("create-exe");
            output.arg(wasi_test_python_path());
            output.arg("--target");
            output.arg(t);
            output.arg("-o");
            output.arg(python_wasmer_path.clone());
            output.arg(format!("--{c}"));
            if std::env::var("GITHUB_TOKEN").is_ok() {
                output.arg("--debug-dir");
                output.arg(format!("{t}-{c}"));
            }

            if t.contains("x86_64") && *c == "singlepass" {
                output.arg("-m");
                output.arg("avx");
            }

            if let Some(t) = tarball {
                output.arg("--tarball");
                output.arg(t);
            }

            println!("command {:?}", output);

            let output = output.output()?;

            let stdout = std::str::from_utf8(&output.stdout)
                .expect("stdout is not utf8! need to handle arbitrary bytes");

            let stderr = std::str::from_utf8(&output.stderr)
                .expect("stderr is not utf8! need to handle arbitrary bytes");

            if !output.status.success() {
                bail!("linking failed with: stdout: {stdout}\n\nstderr: {stderr}");
            }

            println!("stdout: {stdout}");
            println!("stderr: {stderr}");

            if !python_wasmer_path.exists() {
                let p = std::fs::read_dir(temp_dir.path())
                    .unwrap()
                    .filter_map(|e| Some(e.ok()?.path()))
                    .collect::<Vec<_>>();
                panic!(
                    "target {t} was not compiled correctly {stdout} {stderr}, tempdir: {:#?}",
                    p
                );
            }
        }
    }

    Ok(())
}

#[test]
fn run_whoami_works() -> anyhow::Result<()> {
    // running test locally: should always pass since
    // developers don't have access to WAPM_DEV_TOKEN
    if std::env::var("GITHUB_TOKEN").is_err() {
        return Ok(());
    }

    let ciuser_token = std::env::var("WAPM_DEV_TOKEN").expect("no CIUSER / WAPM_DEV_TOKEN token");
    // Special case: GitHub secrets aren't visible to outside collaborators
    if ciuser_token.is_empty() {
        return Ok(());
    }

    let output = Command::new(get_wasmer_path())
        .arg("login")
        .arg("--registry")
        .arg("wapm.dev")
        .arg(ciuser_token)
        .output()?;

    if !output.status.success() {
        bail!(
            "wasmer login failed with: stdout: {}\n\nstderr: {}",
            std::str::from_utf8(&output.stdout)
                .expect("stdout is not utf8! need to handle arbitrary bytes"),
            std::str::from_utf8(&output.stderr)
                .expect("stderr is not utf8! need to handle arbitrary bytes")
        );
    }

    let output = Command::new(get_wasmer_path())
        .arg("whoami")
        .arg("--registry")
        .arg("wapm.dev")
        .output()?;

    let stdout = std::str::from_utf8(&output.stdout)
        .expect("stdout is not utf8! need to handle arbitrary bytes");

    if !output.status.success() {
        bail!(
            "linking failed with: stdout: {}\n\nstderr: {}",
            stdout,
            std::str::from_utf8(&output.stderr)
                .expect("stderr is not utf8! need to handle arbitrary bytes")
        );
    }

    assert_eq!(
        stdout,
        "logged into registry \"https://registry.wapm.dev/graphql\" as user \"ciuser\"\n"
    );

    Ok(())
}

#[test]
fn run_wasi_works() -> anyhow::Result<()> {
    let output = Command::new(get_wasmer_path())
        .arg("run")
        .arg(wasi_test_wasm_path())
        .arg("--")
        .arg("-e")
        .arg("print(3 * (4 + 5))")
        .output()?;

    if !output.status.success() {
        bail!(
            "linking failed with: stdout: {}\n\nstderr: {}",
            std::str::from_utf8(&output.stdout)
                .expect("stdout is not utf8! need to handle arbitrary bytes"),
            std::str::from_utf8(&output.stderr)
                .expect("stderr is not utf8! need to handle arbitrary bytes")
        );
    }

    let stdout_output = std::str::from_utf8(&output.stdout).unwrap();
    assert_eq!(stdout_output, "27\n");

    Ok(())
}

/// TODO: on linux-musl, the packaging of libwasmer.a doesn't work properly
/// Tracked in https://github.com/wasmerio/wasmer/issues/3271
#[cfg(not(any(target_env = "musl", target_os = "windows")))]
#[cfg(feature = "webc_runner")]
#[test]
fn test_wasmer_create_exe_pirita_works() -> anyhow::Result<()> {
    // let temp_dir = Path::new("debug");
    // std::fs::create_dir_all(&temp_dir);

    use wasmer_integration_tests_cli::get_repo_root_path;
    let temp_dir = tempfile::TempDir::new()?;
    let temp_dir = temp_dir.path().to_path_buf();
    let python_wasmer_path = temp_dir.join("python.wasmer");
    std::fs::copy(wasi_test_python_path(), &python_wasmer_path)?;
    let python_exe_output_path = temp_dir.join("python");

    let native_target = target_lexicon::HOST;
    let tmp_targz_path = get_repo_root_path().unwrap().join("link.tar.gz");

    println!("compiling to target {native_target}");

    let mut cmd = Command::new(get_wasmer_path());
    cmd.arg("create-exe");
    cmd.arg(&python_wasmer_path);
    cmd.arg("--tarball");
    cmd.arg(&tmp_targz_path);
    cmd.arg("--target");
    cmd.arg(format!("{native_target}"));
    cmd.arg("-o");
    cmd.arg(&python_exe_output_path);
    // change temp_dir to a local path and run this test again
    // to output the compilation files into a debug folder
    //
    // cmd.arg("--debug-dir");
    // cmd.arg(&temp_dir);
    println!("running: {cmd:?}");

    let output = cmd
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .output()?;

    if !output.status.success() {
        let stdout = std::str::from_utf8(&output.stdout)
            .expect("stdout is not utf8! need to handle arbitrary bytes");

        bail!(
            "running wasmer create-exe {} failed with: stdout: {}\n\nstderr: {}",
            python_wasmer_path.display(),
            stdout,
            std::str::from_utf8(&output.stderr)
                .expect("stderr is not utf8! need to handle arbitrary bytes")
        );
    }

    println!("compilation ok!");

    if !python_exe_output_path.exists() {
        return Err(anyhow::anyhow!(
            "python_exe_output_path {} does not exist",
            python_exe_output_path.display()
        ));
    }

    println!("invoking command...");

    let mut command = Command::new(&python_exe_output_path);
    command.arg("-c");
    command.arg("print(\"hello\")");

    let output = command
        .output()
        .map_err(|e| anyhow::anyhow!("{e}: {command:?}"))?;

    let stdout = std::str::from_utf8(&output.stdout)
        .expect("stdout is not utf8! need to handle arbitrary bytes");

    if stdout != "hello\n" {
        bail!(
            "1 running python.wasmer failed with: stdout: {}\n\nstderr: {}",
            stdout,
            std::str::from_utf8(&output.stderr)
                .expect("stderr is not utf8! need to handle arbitrary bytes")
        );
    }

    Ok(())
}

// FIXME: Re-enable. See https://github.com/wasmerio/wasmer/issues/3717
#[cfg(feature = "webc_runner")]
#[test]
#[ignore]
fn test_wasmer_run_pirita_works() -> anyhow::Result<()> {
    let temp_dir = tempfile::TempDir::new()?;
    let python_wasmer_path = temp_dir.path().join("python.wasmer");
    std::fs::copy(wasi_test_python_path(), &python_wasmer_path)?;

    let output = Command::new(get_wasmer_path())
        .arg("run")
        .arg(python_wasmer_path)
        .arg("--")
        .arg("-c")
        .arg("print(\"hello\")")
        .output()?;

    let stdout = std::str::from_utf8(&output.stdout)
        .expect("stdout is not utf8! need to handle arbitrary bytes");

    if stdout != "hello\n" {
        bail!(
            "1 running python.wasmer failed with: stdout: {}\n\nstderr: {}",
            stdout,
            std::str::from_utf8(&output.stderr)
                .expect("stderr is not utf8! need to handle arbitrary bytes")
        );
    }

    Ok(())
}

// FIXME: Re-enable. See https://github.com/wasmerio/wasmer/issues/3717
#[cfg(feature = "webc_runner")]
#[test]
#[ignore]
fn test_wasmer_run_pirita_url_works() -> anyhow::Result<()> {
    let output = Command::new(get_wasmer_path())
        .arg("run")
        .arg("https://wapm.dev/syrusakbary/python")
        .arg("--")
        .arg("-c")
        .arg("print(\"hello\")")
        .output()?;

    let stdout = std::str::from_utf8(&output.stdout)
        .expect("stdout is not utf8! need to handle arbitrary bytes");

    if stdout != "hello\n" {
        bail!(
            "1 running python.wasmer failed with: stdout: {}\n\nstderr: {}",
            stdout,
            std::str::from_utf8(&output.stderr)
                .expect("stderr is not utf8! need to handle arbitrary bytes")
        );
    }

    Ok(())
}

#[test]
fn test_wasmer_run_works_with_dir() -> anyhow::Result<()> {
    let temp_dir = tempfile::TempDir::new()?;
    let qjs_path = temp_dir.path().join("qjs.wasm");

    std::fs::copy(wasi_test_wasm_path(), &qjs_path)?;
    std::fs::copy(
        format!("{}/{}", C_ASSET_PATH, "qjs-wasmer.toml"),
        temp_dir.path().join("wasmer.toml"),
    )?;

    assert!(temp_dir.path().exists());
    assert!(temp_dir.path().join("wasmer.toml").exists());
    assert!(temp_dir.path().join("qjs.wasm").exists());

    // test with "wasmer qjs.wasm"
    let output = Command::new(get_wasmer_path())
        .arg(temp_dir.path())
        .arg("--")
        .arg("--quit")
        .output()?;

    let stdout = std::str::from_utf8(&output.stdout)
        .expect("stdout is not utf8! need to handle arbitrary bytes");

    if !output.status.success() {
        bail!(
            "running {} failed with: stdout: {}\n\nstderr: {}",
            qjs_path.display(),
            stdout,
            std::str::from_utf8(&output.stderr)
                .expect("stderr is not utf8! need to handle arbitrary bytes")
        );
    }

    // test again with "wasmer run qjs.wasm"
    let output = Command::new(get_wasmer_path())
        .arg("run")
        .arg(temp_dir.path())
        .arg("--")
        .arg("--quit")
        .output()?;

    let stdout = std::str::from_utf8(&output.stdout)
        .expect("stdout is not utf8! need to handle arbitrary bytes");

    if !output.status.success() {
        bail!(
            "running {} failed with: stdout: {}\n\nstderr: {}",
            qjs_path.display(),
            stdout,
            std::str::from_utf8(&output.stderr)
                .expect("stderr is not utf8! need to handle arbitrary bytes")
        );
    }

    Ok(())
}

// FIXME: Re-enable. See https://github.com/wasmerio/wasmer/issues/3717
#[ignore]
#[cfg(not(target_env = "musl"))]
#[test]
fn test_wasmer_run_works() -> anyhow::Result<()> {
    let output = Command::new(get_wasmer_path())
        .arg("https://wapm.io/python/python")
        .arg(format!("--mapdir=.:{}", ASSET_PATH))
        .arg("test.py")
        .output()?;

    let stdout = std::str::from_utf8(&output.stdout)
        .expect("stdout is not utf8! need to handle arbitrary bytes");

    if stdout != "hello\n" {
        bail!(
            "1 running python/python failed with: stdout: {}\n\nstderr: {}",
            stdout,
            std::str::from_utf8(&output.stderr)
                .expect("stderr is not utf8! need to handle arbitrary bytes")
        );
    }

    // same test again, but this time with "wasmer run ..."
    let output = Command::new(get_wasmer_path())
        .arg("run")
        .arg("https://wapm.io/python/python")
        .arg(format!("--mapdir=.:{}", ASSET_PATH))
        .arg("test.py")
        .output()?;

    let stdout = std::str::from_utf8(&output.stdout)
        .expect("stdout is not utf8! need to handle arbitrary bytes");

    if stdout != "hello\n" {
        bail!(
            "2 running python/python failed with: stdout: {}\n\nstderr: {}",
            stdout,
            std::str::from_utf8(&output.stderr)
                .expect("stderr is not utf8! need to handle arbitrary bytes")
        );
    }

    // set wapm.io as the current registry
    let _ = Command::new(get_wasmer_path())
        .arg("login")
        .arg("--registry")
        .arg("wapm.io")
        // will fail, but set wapm.io as the current registry regardless
        .arg("öladkfjasöldfkjasdölfkj")
        .output()?;

    // same test again, but this time without specifying the registry
    let output = Command::new(get_wasmer_path())
        .arg("run")
        .arg("python/python")
        .arg(format!("--mapdir=.:{}", ASSET_PATH))
        .arg("test.py")
        .output()?;

    let stdout = std::str::from_utf8(&output.stdout)
        .expect("stdout is not utf8! need to handle arbitrary bytes");

    if stdout != "hello\n" {
        bail!(
            "3 running python/python failed with: stdout: {}\n\nstderr: {}",
            stdout,
            std::str::from_utf8(&output.stderr)
                .expect("stderr is not utf8! need to handle arbitrary bytes")
        );
    }

    // same test again, but this time with only the command "python" (should be looked up locally)
    let output = Command::new(get_wasmer_path())
        .arg("run")
        .arg("_/python")
        .arg(format!("--mapdir=.:{}", ASSET_PATH))
        .arg("test.py")
        .output()?;

    let stdout = std::str::from_utf8(&output.stdout)
        .expect("stdout is not utf8! need to handle arbitrary bytes");

    if stdout != "hello\n" {
        bail!(
            "4 running python/python failed with: stdout: {}\n\nstderr: {}",
            stdout,
            std::str::from_utf8(&output.stderr)
                .expect("stderr is not utf8! need to handle arbitrary bytes")
        );
    }

    Ok(())
}

#[test]
fn run_no_imports_wasm_works() -> anyhow::Result<()> {
    let output = Command::new(get_wasmer_path())
        .arg("run")
        .arg(test_no_imports_wat_path())
        .output()?;

    if !output.status.success() {
        bail!(
            "linking failed with: stdout: {}\n\nstderr: {}",
            std::str::from_utf8(&output.stdout)
                .expect("stdout is not utf8! need to handle arbitrary bytes"),
            std::str::from_utf8(&output.stderr)
                .expect("stderr is not utf8! need to handle arbitrary bytes")
        );
    }

    Ok(())
}

#[test]
fn run_wasi_works_non_existent() -> anyhow::Result<()> {
    let output = Command::new(get_wasmer_path())
        .arg("run")
        .arg("does/not/exist")
        .output()?;

    let stderr = std::str::from_utf8(&output.stderr).unwrap();

    let stderr_lines = stderr.lines().map(|s| s.to_string()).collect::<Vec<_>>();

    assert_eq!(
        stderr_lines,
        vec!["error: Could not find local file does/not/exist".to_string()]
    );

    Ok(())
}

// FIXME: Re-enable. See https://github.com/wasmerio/wasmer/issues/3717
#[ignore]
#[test]
fn run_test_caching_works_for_packages() -> anyhow::Result<()> {
    // set wapm.io as the current registry
    let _ = Command::new(get_wasmer_path())
        .arg("login")
        .arg("--registry")
        .arg("wapm.io")
        // will fail, but set wapm.io as the current registry regardless
        .arg("öladkfjasöldfkjasdölfkj")
        .output()?;

    let output = Command::new(get_wasmer_path())
        .arg("python/python")
        .arg(format!("--mapdir=.:{}", ASSET_PATH))
        .arg("test.py")
        .output()?;

    if output.stdout != b"hello\n".to_vec() {
        panic!("failed to run https://wapm.io/python/python for the first time: stdout = {}, stderr = {}", 
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        );
    }

    let time = std::time::Instant::now();

    let output = Command::new(get_wasmer_path())
        .arg("python/python")
        .arg(format!("--mapdir=.:{}", ASSET_PATH))
        .arg("test.py")
        .output()?;

    if output.stdout != b"hello\n".to_vec() {
        panic!("failed to run https://wapm.io/python/python for the second time");
    }

    // package should be cached
    assert!(std::time::Instant::now() - time < std::time::Duration::from_secs(1));

    Ok(())
}

#[test]
fn run_test_caching_works_for_packages_with_versions() -> anyhow::Result<()> {
    // set wapm.io as the current registry
    let _ = Command::new(get_wasmer_path())
        .arg("login")
        .arg("--registry")
        .arg("wapm.io")
        // will fail, but set wapm.io as the current registry regardless
        .arg("öladkfjasöldfkjasdölfkj")
        .output()?;

    let output = Command::new(get_wasmer_path())
        .arg("python/python@0.1.0")
        .arg(format!("--mapdir=.:{}", ASSET_PATH))
        .arg("test.py")
        .output()?;

    if output.stdout != b"hello\n".to_vec() {
        panic!("failed to run https://wapm.io/python/python for the first time");
    }

    let time = std::time::Instant::now();

    let output = Command::new(get_wasmer_path())
        .arg("python/python@0.1.0")
        .arg(format!("--mapdir=.:{}", ASSET_PATH))
        .arg("test.py")
        .output()?;

    if output.stdout != b"hello\n".to_vec() {
        panic!("failed to run https://wapm.io/python/python for the second time");
    }

    // package should be cached
    assert!(std::time::Instant::now() - time < std::time::Duration::from_secs(1));

    Ok(())
}

// FIXME: Re-enable. See https://github.com/wasmerio/wasmer/issues/3717
#[ignore]
#[test]
fn run_test_caching_works_for_urls() -> anyhow::Result<()> {
    let output = Command::new(get_wasmer_path())
        .arg("https://wapm.io/python/python")
        .arg(format!("--mapdir=.:{}", ASSET_PATH))
        .arg("test.py")
        .output()?;

    if output.stdout != b"hello\n".to_vec() {
        panic!("failed to run https://wapm.io/python/python for the first time");
    }

    let time = std::time::Instant::now();

    let output = Command::new(get_wasmer_path())
        .arg("https://wapm.io/python/python")
        .arg(format!("--mapdir=.:{}", ASSET_PATH))
        .arg("test.py")
        .output()?;

    if output.stdout != b"hello\n".to_vec() {
        panic!("failed to run https://wapm.io/python/python for the second time");
    }

    // package should be cached
    assert!(std::time::Instant::now() - time < std::time::Duration::from_secs(1));

    Ok(())
}

// This test verifies that "wasmer run --invoke _start module.wat"
// works the same as "wasmer run module.wat" (without --invoke).
#[test]
fn run_invoke_works_with_nomain_wasi() -> anyhow::Result<()> {
    // In this example the function "wasi_unstable.arg_sizes_get"
    // is a function that is imported from the WASI env.
    let wasi_wat = "
    (module
        (import \"wasi_unstable\" \"args_sizes_get\"
          (func $__wasi_args_sizes_get (param i32 i32) (result i32)))
        (func $_start)
        (memory 1)
        (export \"memory\" (memory 0))
        (export \"_start\" (func $_start))
      )
    ";

    let random = rand::random::<u64>();
    let module_file = std::env::temp_dir().join(&format!("{random}.wat"));
    std::fs::write(&module_file, wasi_wat.as_bytes()).unwrap();
    let output = Command::new(get_wasmer_path())
        .arg("run")
        .arg(&module_file)
        .output()?;

    let stderr = std::str::from_utf8(&output.stderr).unwrap().to_string();
    let success = output.status.success();
    if !success {
        println!("ERROR in 'wasmer run [module.wat]':\r\n{stderr}");
        panic!();
    }

    let output = Command::new(get_wasmer_path())
        .arg("run")
        .arg("--invoke")
        .arg("_start")
        .arg(&module_file)
        .output()?;

    let stderr = std::str::from_utf8(&output.stderr).unwrap().to_string();
    let success = output.status.success();
    if !success {
        println!("ERROR in 'wasmer run --invoke _start [module.wat]':\r\n{stderr}");
        panic!();
    }

    std::fs::remove_file(&module_file).unwrap();
    Ok(())
}

#[test]
fn run_no_start_wasm_report_error() -> anyhow::Result<()> {
    let output = Command::new(get_wasmer_path())
        .arg("run")
        .arg(test_no_start_wat_path())
        .output()?;

    assert_eq!(output.status.success(), false);
    let result = std::str::from_utf8(&output.stderr).unwrap().to_string();
    assert_eq!(result.contains("Can not find any export functions."), true);
    Ok(())
}

// Test that wasmer can run a complex path
#[test]
fn test_wasmer_run_complex_url() -> anyhow::Result<()> {
    let wasm_test_path = wasi_test_wasm_path();
    let wasm_test_path = wasm_test_path.canonicalize().unwrap_or(wasm_test_path);
    let mut wasm_test_path = format!("{}", wasm_test_path.display());
    if wasm_test_path.starts_with(r#"\\?\"#) {
        wasm_test_path = wasm_test_path.replacen(r#"\\?\"#, "", 1);
    }
    #[cfg(target_os = "windows")]
    {
        wasm_test_path = wasm_test_path.replace("D:\\", "D://");
        wasm_test_path = wasm_test_path.replace("C:\\", "C://");
        wasm_test_path = wasm_test_path.replace("c:\\", "c://");
        wasm_test_path = wasm_test_path.replace("\\", "/");
        // wasmer run used to fail on c:\Users\username\wapm_packages\ ...
        assert!(
            wasm_test_path.contains("://"),
            "wasm_test_path path is not complex enough"
        );
    }

    let mut cmd = Command::new(get_wasmer_path());
    cmd.arg("run");
    cmd.arg(wasm_test_path);
    cmd.arg("--");
    cmd.arg("-q");

    let cmd_str = format!("{cmd:?}");
    let output = cmd.output().with_context(|| {
        anyhow::anyhow!(
            "failed to run {cmd_str} with {}",
            get_wasmer_path().display()
        )
    })?;

    if !output.status.success() {
        bail!(
            "wasmer run qjs.wasm failed with: stdout: {}\n\nstderr: {}",
            std::str::from_utf8(&output.stdout)
                .expect("stdout is not utf8! need to handle arbitrary bytes"),
            std::str::from_utf8(&output.stderr)
                .expect("stderr is not utf8! need to handle arbitrary bytes")
        );
    }

    Ok(())
}
