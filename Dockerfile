FROM rust:1.60.0-bullseye AS builder

COPY . .
RUN cargo build --release

FROM debian:bullseye

COPY --from=builder target/release/knifey-service /usr/local/bin/knifey-service

CMD ["/usr/local/bin/knifey-service"]
