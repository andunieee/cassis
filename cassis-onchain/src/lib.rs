use rand::RngCore;
use sha2::{Digest, Sha256};

#[derive(thiserror::Error, Debug)]
pub enum TimelockError {
    #[error("insufficient delta")]
    InsufficientDelta,
}

pub fn hash_preimage(preimage: [u8; 32]) -> [u8; 32] {
    let mut hasher = Sha256::new();
    hasher.update(preimage);
    let result = hasher.finalize();
    let mut out = [0u8; 32];
    out.copy_from_slice(&result);
    out
}

pub fn generate_preimage() -> [u8; 32] {
    let mut preimage = [0u8; 32];
    rand::thread_rng().fill_bytes(&mut preimage);
    preimage
}

pub fn validate_timelock_delta(
    incoming_expiry: u64,
    outgoing_expiry: u64,
    delta_secs: u64,
) -> Result<(), TimelockError> {
    if incoming_expiry >= outgoing_expiry.saturating_add(delta_secs) {
        Ok(())
    } else {
        Err(TimelockError::InsufficientDelta)
    }
}

pub fn compute_timelock_cascade(final_expiry: u64, deltas: &[u64]) -> Vec<u64> {
    let mut expiries = Vec::with_capacity(deltas.len() + 1);
    let mut current = final_expiry;
    expiries.push(current);
    for delta in deltas.iter().rev() {
        current = current.saturating_add(*delta);
        expiries.push(current);
    }
    expiries.reverse();
    expiries
}
