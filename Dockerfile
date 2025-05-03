# Use the latest stable Rust version
FROM rust:latest as builder

WORKDIR /usr/src/app

COPY . .

# Build with release profile
RUN cargo build --release --manifest-path mips_app/Cargo.toml

# Runtime container
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y ca-certificates && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=builder /usr/src/app/target/release/mips_app .
COPY --from=builder /usr/src/app/assets ./assets

ENV RUST_BACKTRACE=1
CMD ["./mips_app"]