use cassis_core::{HopAck, HopInstruction};

pub const ALPN_PROTOCOL: &str = "cassis-hop/1";

#[derive(thiserror::Error, Debug)]
pub enum IrohError {
    #[error("io error: {0}")]
    Io(String),
    #[error("protocol error: {0}")]
    Protocol(String),
    #[error("unimplemented")]
    Unimplemented,
}

#[derive(Clone, Debug)]
pub struct IrohClient {
    pub peer: String,
}

impl IrohClient {
    pub fn new(peer: String) -> Self {
        Self { peer }
    }

    pub async fn send_instruction(&self, _instruction: HopInstruction) -> Result<HopAck, IrohError> {
        Err(IrohError::Unimplemented)
    }
}

pub struct IrohServer;

impl IrohServer {
    pub fn new() -> Self {
        Self
    }

    pub async fn handle_instruction(
        &self,
        _instruction: HopInstruction,
    ) -> Result<HopAck, IrohError> {
        Err(IrohError::Unimplemented)
    }
}
