//! Intent CLI - Contract-driven API testing
//!
//! Main entry point for the Intent CLI application.

use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

fn main() {
    // Initialize tracing
    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer())
        .with(tracing_subscriber::EnvFilter::from_default_env())
        .init();

    tracing::info!("Intent CLI starting...");

    // TODO: Implement CLI commands
    println!("Intent CLI v0.1.0 - Contract-driven API testing");
}
