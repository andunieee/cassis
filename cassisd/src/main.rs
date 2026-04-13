use cassis_core::{
    HopAck, HopInstruction, HopReject, NetworkAdapter, NetworkId, WatchError,
};
use cassis_onchain::validate_timelock_delta;
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};
use tokio::sync::Mutex;

type PendingMap = Arc<Mutex<HashMap<[u8; 32], HopInstruction>>>;

#[tokio::main]
async fn main() {
    println!("cassisd scaffold");
}

pub struct CassisDaemon {
    adapters: HashMap<NetworkId, Arc<dyn NetworkAdapter>>,
    pending: PendingMap,
}

impl CassisDaemon {
    pub fn new(adapters: HashMap<NetworkId, Arc<dyn NetworkAdapter>>) -> Self {
        Self {
            adapters,
            pending: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub async fn handle_instruction(&self, instruction: HopInstruction) -> Result<HopAck, HopReject> {
        self.validate_instruction(&instruction)?;
        {
            let mut pending = self.pending.lock().await;
            pending.insert(instruction.payment_hash, instruction.clone());
        }

        let adapters = self.adapters.clone();
        let pending = Arc::clone(&self.pending);
        tokio::spawn(async move {
            watch_instruction(instruction, adapters, pending).await;
        });

        Ok(HopAck {
            payment_hash: instruction.payment_hash,
            accepted: true,
            signature: None,
        })
    }

    fn validate_instruction(&self, instruction: &HopInstruction) -> Result<(), HopReject> {
        if instruction.payment_hash.iter().all(|byte| *byte == 0) {
            return Err(HopReject {
                payment_hash: instruction.payment_hash,
                reason: "zero payment hash".to_string(),
            });
        }

        if instruction.amount_msat == 0 {
            return Err(HopReject {
                payment_hash: instruction.payment_hash,
                reason: "amount must be positive".to_string(),
            });
        }

        let incoming = self
            .adapters
            .get(&instruction.incoming_network)
            .ok_or_else(|| HopReject {
                payment_hash: instruction.payment_hash,
                reason: "incoming network unsupported".to_string(),
            })?;
        if !self.adapters.contains_key(&instruction.outgoing_network) {
            return Err(HopReject {
                payment_hash: instruction.payment_hash,
                reason: "outgoing network unsupported".to_string(),
            });
        }

        let now = unix_now();
        if instruction.incoming_deadline < now.saturating_add(incoming.incoming_delta_secs()) {
            return Err(HopReject {
                payment_hash: instruction.payment_hash,
                reason: "incoming deadline too soon".to_string(),
            });
        }

        Ok(())
    }
}

async fn watch_instruction(
    instruction: HopInstruction,
    adapters: HashMap<NetworkId, Arc<dyn NetworkAdapter>>,
    pending: PendingMap,
) {
    let incoming_adapter = match adapters.get(&instruction.incoming_network) {
        Some(adapter) => adapter,
        None => {
            remove_pending(&pending, instruction.payment_hash).await;
            return;
        }
    };
    let outgoing_adapter = match adapters.get(&instruction.outgoing_network) {
        Some(adapter) => adapter,
        None => {
            remove_pending(&pending, instruction.payment_hash).await;
            return;
        }
    };

    let incoming = match incoming_adapter
        .watch_incoming_htlc(
            instruction.payment_hash,
            instruction.amount_msat,
            instruction.incoming_deadline,
        )
        .await
    {
        Ok(htlc) => htlc,
        Err(_) => {
            remove_pending(&pending, instruction.payment_hash).await;
            return;
        }
    };

    if incoming.amount_msat < instruction.amount_msat {
        remove_pending(&pending, instruction.payment_hash).await;
        return;
    }

    if validate_timelock_delta(
        incoming.expiry,
        instruction.outgoing_expiry,
        incoming_adapter.incoming_delta_secs(),
    )
    .is_err()
    {
        remove_pending(&pending, instruction.payment_hash).await;
        return;
    }

    let outgoing = match outgoing_adapter
        .create_outgoing_htlc(
            instruction.payment_hash,
            instruction.amount_msat,
            instruction.outgoing_expiry,
            &instruction.recipient,
        )
        .await
    {
        Ok(htlc) => htlc,
        Err(_) => {
            remove_pending(&pending, instruction.payment_hash).await;
            return;
        }
    };

    match outgoing_adapter
        .watch_preimage(&outgoing, instruction.outgoing_expiry)
        .await
    {
        Ok(preimage) => {
            let _ = incoming_adapter.claim_incoming(&incoming, preimage).await;
        }
        Err(WatchError::DeadlineExceeded) => {
            let _ = outgoing_adapter.refund_outgoing(&outgoing).await;
        }
        Err(_) => {
            let _ = outgoing_adapter.refund_outgoing(&outgoing).await;
        }
    }

    remove_pending(&pending, instruction.payment_hash).await;
}

async fn remove_pending(pending: &PendingMap, payment_hash: [u8; 32]) {
    let mut pending = pending.lock().await;
    pending.remove(&payment_hash);
}

fn unix_now() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|duration| duration.as_secs())
        .unwrap_or(0)
}
