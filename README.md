# Pingu - a fast, safe and flexible HTTP implementation

Features:
- Minimal & low-level - there is no functionality expect writing and reading HTTP requests and responses.
- Fast - because of the minimalistic nature of this library, it is fast, and I mean very fast.
- Safe - this library uses `#![forbid(unsafe_code)]`.
- Flexible - this implementation does not spawn or need spawning of, additional tasks, so we don't depend on any runtime. Also, because we don't include any server or client implementation, you are free to do whatever you want with the protocol. Unlike other libraries, this one is only the protocol, so I don't need to hide it away behind some client or server implementation.

This library provides reading and writing HTTP/1.1 requests and responses.
