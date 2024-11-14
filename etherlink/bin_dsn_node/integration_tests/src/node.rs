// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! A helper class for spawning RPC nodes in integration tests.

use ethers::prelude::Http;
use ethers::providers::Provider;
use std::{
    env,
    fs::{create_dir_all, File},
    net::TcpListener,
    path::{Path, PathBuf},
    process::{Child, Command, ExitStatus, Stdio},
    time::Duration,
};
use tokio::net::TcpStream;
use url::Url;

const MIN_PORT: u16 = 49_152;
const MAX_PORT: u16 = 65_535;
const CONNECTION_ATTEMPTS: usize = 360;
const CONNECTION_ATTEMPT_DELAY_MS: u64 = 500;

#[derive(Debug)]
/// A wrapper over the RPC node process handle
///
/// When this struct goes out of scope, it's `Drop` impl
/// will take care of killing the node process.
pub struct RpcNode {
    process: Child,
    address: String,
}

impl Drop for RpcNode {
    fn drop(&mut self) {
        let mut kill = Command::new("kill")
            .args(["-s", "TERM", &self.process.id().to_string()])
            .spawn()
            .expect("Failed to kill");
        kill.wait().expect("Failed to kill the process");
    }
}

fn get_free_port() -> u16 {
    for port in MIN_PORT..=MAX_PORT {
        if let Ok(listener) = TcpListener::bind(("127.0.0.1", port)) {
            return listener.local_addr().expect("No local addr").port();
        }
        // otherwise port is occupied
    }
    panic!("No free ports available");
}

fn get_repository_root() -> PathBuf {
    let manifest_path = Path::new(&env!("CARGO_MANIFEST_DIR"));
    let repository_root = manifest_path
        .parent()
        .expect("Failed to get parent directory of CARGO_MANIFEST_DIR");
    repository_root.to_path_buf()
}

impl RpcNode {
    fn cargo_run(root_dir: &Path, binary: &str, args: Vec<&str>) -> Child {
        let arguments = [vec!["run", "--bin", binary, "--release", "--"], args].concat();

        let logs_dir = Path::join(root_dir, Path::new("target/dsn-node-logs"));
        create_dir_all(logs_dir.clone()).expect("Failed to create logs dir");

        let stdout =
            Stdio::from(File::create(logs_dir.join(format!("{}-stdout.txt", binary))).unwrap());
        let stderr =
            Stdio::from(File::create(logs_dir.join(format!("{}-stderr.txt", binary))).unwrap());

        Command::new("cargo")
            .stdout(stdout)
            .stderr(stderr)
            .args(arguments)
            .spawn()
            .expect("Could not run DSN node")
    }

    pub fn run(binary: &str, args: Vec<&str>) -> Self {
        let port = get_free_port();
        let address = format!("127.0.0.1:{}", port);
        let repository_root = &get_repository_root();

        std::env::set_current_dir(repository_root).expect("Failed to change working directory");

        let args = [args, vec!["--rpc-address", address.as_str()]].concat();

        let process = Self::cargo_run(repository_root.as_path(), binary, args);

        Self { process, address }
    }

    pub fn endpoint(&self) -> Url {
        Url::parse(&format!("http://{}", self.address)).unwrap()
    }

    pub fn as_provider(&self) -> Provider<Http> {
        Provider::new(Http::new(self.endpoint()))
    }

    pub fn has_exited(&mut self) -> Option<ExitStatus> {
        self.process
            .try_wait()
            .expect("Failed to get DSN node exit status")
    }

    pub async fn wait_till_started(&mut self) {
        let mut attempts = CONNECTION_ATTEMPTS;
        loop {
            match TcpStream::connect(&self.address).await {
                Ok(_) => return,
                Err(err) => {
                    if let Some(status) = self.has_exited() {
                        panic!("DSN node exited early with {}", status);
                    }
                    if attempts == 0 {
                        panic!("Failed to connect to {}: {}", self.address, err);
                    }
                }
            };

            attempts -= 1;
            tokio::time::sleep(Duration::from_millis(CONNECTION_ATTEMPT_DELAY_MS)).await;
        }
    }
}
