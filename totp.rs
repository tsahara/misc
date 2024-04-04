use data_encoding::BASE32;
use std::time::{SystemTime, UNIX_EPOCH};
use totp_lite::{totp_custom, Sha1, DEFAULT_STEP};

fn main() {
    // Negotiated between you and the authenticating service.
    let secret: &str = "HXDMVJECJJWSRB3HWIZR4IFUGFTMXBOZ";
    let upstr: String = secret.to_uppercase();
    let bytes: &[u8] = upstr.as_bytes();
    let password: Vec<u8> = BASE32.decode(bytes).unwrap();

    // The number of seconds since the Unix Epoch.
    let seconds: u64 = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs();

    // Uses the default step of 30 seconds, but asks for 10 result digits instead.
    let result: String = totp_custom::<Sha1>(DEFAULT_STEP, 6, &password, seconds);
    println!("TOTP: {}", result);
}
