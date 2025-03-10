use sha::sha256::Sha256;
use sha::utils::{Digest, DigestExt};
use std::convert::TryInto;
use std::default::Default;

/// Compute the SHA-256 of `bytes` and return it
pub fn sha256(bytes: &[u8]) -> [u8; 32] {
    let sha = Sha256::default().digest(bytes).to_bytes();
    sha.try_into().unwrap()
}

