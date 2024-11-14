use clap::Parser;
use std::net::SocketAddr;

use jsonrpc_http_server::{
    jsonrpc_core::{IoHandler, Value},
    ServerBuilder,
};

#[derive(Parser)]
pub struct Args {
    #[arg(short, long)]
    pub rpc_address: SocketAddr,
}

pub fn main() {
    let args = Args::parse();

    std::env::set_var("RUST_LOG", "trace");
    env_logger::init();

    let mut io = IoHandler::default();
    io.add_method("eth_sendRawTransaction", |_| async {
        Ok(Value::String(
            "0x73a5b33d17d3d1d78a5133a01870717af2f96d93cf8047de7c52ba184d2a29cf".into(),
        ))
    });

    ServerBuilder::new(io)
        .start_http(&args.rpc_address)
        .unwrap()
        .wait();
}
