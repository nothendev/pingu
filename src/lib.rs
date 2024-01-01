#![forbid(unsafe_code)]
use std::{io::ErrorKind, str::FromStr};

use bytes::Bytes;
use futures::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt};
use http::{HeaderValue, Method, StatusCode, Uri, Version};

pub use http;

pub type Response<B = Body> = http::Response<B>;
pub type Request<B = Body> = http::Request<B>;

#[derive(Debug, PartialEq, Eq)]
pub struct Body(BodyInner);

impl Body {
    pub fn full(bytes: Bytes) -> Self {
        Self(BodyInner::Full(bytes))
    }

    async fn write(&mut self, writer: &mut (impl AsyncWriteExt + Unpin)) -> std::io::Result<()> {
        match &self.0 {
            BodyInner::Full(full) => writer.write_all(&full).await,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum BodyInner {
    Full(Bytes),
}

#[macro_export]
macro_rules! async_write {
    ($writer:expr, $($fmt:tt)*) => {
        $writer.write_all(&format!($($fmt)*).into_bytes())
    };
    (bytes $writer:expr, $($fmt:tt)*) => {
        $writer.write_all(&format_bytes::format_bytes!($($fmt)*))
    };
}

pub async fn write_request(
    mut request: Request,
    writer: &mut (impl AsyncWriteExt + Unpin),
) -> std::io::Result<()> {
    // Request-Line = Method SP Request-URI SP HTTP-Version CRLF
    async_write!(
        writer,
        "{method} {url} {version:?}\r\n",
        method = request.method().as_str(),
        url = request
            .uri()
            .path_and_query()
            .ok_or_else(|| std::io::Error::new(
                ErrorKind::InvalidInput,
                "No http scheme was present"
            ))?,
        version = request.version()
    )
    .await?;

    for (name, val) in request.headers() {
        async_write!(bytes writer, b"{}: {}\r\n", name.as_str().as_bytes(), val.as_bytes()).await?;
    }

    writer.write_all(b"\r\n").await?;

    request.body_mut().write(writer).await?;

    Ok(())
}

pub async fn read_request(
    reader: &mut (impl AsyncBufReadExt + Unpin),
) -> std::io::Result<Request> {
    let mut request_line = String::new();
    reader.read_line(&mut request_line).await?;
    #[cfg(feature = "tracing")]
    tracing::trace!(%request_line);

    fn read_request_line<'a>(
        mut line: impl Iterator<Item = &'a str>,
    ) -> Option<http::request::Builder> {
        let method = Method::from_str(line.next()?).ok()?;
        let uri = Uri::from_str(line.next()?).ok()?;
        let version = match line.next()?.trim_end_matches("\r\n") {
            "HTTP/1.0" => Version::HTTP_10,
            "HTTP/1.1" => Version::HTTP_11,
            "HTTP/2.0" => Version::HTTP_2,
            _ => return None,
        };

        Some(Request::builder().method(method).uri(uri).version(version))
    }

    let mut request = read_request_line(request_line.split(' '))
        .ok_or_else(|| std::io::Error::new(ErrorKind::UnexpectedEof, "Invalid request line"))?;

    let mut header_line = vec![];

    while async {
        header_line.clear();
        reader.read_until(b'\n', &mut header_line).await?;
        Ok::<_, std::io::Error>(header_line != b"\r\n")
    }
    .await?
    {
        let mut header = header_line.split(|x| *x == b':');

        #[cfg(feature = "tracing")]
        tracing::trace!(?header_line);

        request = request.header(
            header.next().unwrap(),
            if let Some(value) = header.next() {
                let val = std::str::from_utf8(value)
                    .expect("invalid header value (non-utf8)")
                    .trim_start_matches(' ')
                    .trim_end_matches("\r\n");
                HeaderValue::from_str(val)
                    .map_err(|e| std::io::Error::new(ErrorKind::InvalidData, e))?
            } else {
                HeaderValue::from_static("")
            },
        );
    }

    let mut buf = vec![];

    reader.read_to_end(&mut buf).await?;

    request
        .body(Body(BodyInner::Full(buf.into())))
        .map_err(|e| std::io::Error::new(ErrorKind::Other, e))
}

pub async fn write_response(
    mut response: Response,
    writer: &mut (impl AsyncWriteExt + Unpin),
) -> std::io::Result<()> {
    // StatusCode's Display impl includes the reason phrase so we don't output it ourselves
    async_write!(
        writer,
        "{version:?} {status_code}\r\n",
        version = response.version(),
        status_code = response.status()
    )
    .await?;

    for (name, val) in response.headers() {
        async_write!(bytes writer, b"{}: {}\r\n", name.as_str().as_bytes(), val.as_bytes()).await?;
    }

    writer.write_all(b"\r\n").await?;

    response.body_mut().write(writer).await?;

    Ok(())
}

pub async fn read_response(
    reader: &mut (impl AsyncBufReadExt + Unpin),
) -> std::io::Result<Response> {
    let mut status_line = String::new();
    reader.read_line(&mut status_line).await?;

    fn read_status_line<'a>(
        mut line: impl Iterator<Item = &'a str>,
    ) -> Option<http::response::Builder> {
        let version = match line.next()?.trim_end_matches("\r\n") {
            "HTTP/1.0" => Version::HTTP_10,
            "HTTP/1.1" => Version::HTTP_11,
            "HTTP/2.0" => Version::HTTP_2,
            _ => return None,
        };
        let status = StatusCode::from_str(line.next()?).ok()?;

        let _reason_phrase = line.next()?;

        Some(Response::builder().version(version).status(status))
    }

    let mut response = read_status_line(status_line.split(' '))
        .ok_or_else(|| std::io::Error::new(ErrorKind::UnexpectedEof, "Invalid status line"))?;

    let mut header_line = vec![];

    while async {
        header_line.clear();
        reader.read_until(b'\n', &mut header_line).await?;
        Ok::<_, std::io::Error>(header_line != b"\r\n")
    }
    .await?
    {
        let mut header = header_line.split(|x| *x == b':');

        response = response.header(
            header.next().unwrap(),
            if let Some(value) = header.next() {
                let val = std::str::from_utf8(value)
                    .expect("invalid header value (non-utf8)")
                    .trim_start_matches(' ')
                    .trim_end_matches("\r\n");
                HeaderValue::from_str(val)
                    .map_err(|e| std::io::Error::new(ErrorKind::InvalidData, e))?
            } else {
                HeaderValue::from_static("")
            },
        );
    }

    let mut buf = vec![];

    reader.read_to_end(&mut buf).await?;

    response
        .body(Body(BodyInner::Full(buf.into())))
        .map_err(|e| std::io::Error::new(ErrorKind::Other, e))
}

#[cfg(test)]
mod tests {
    use futures::io::BufReader;

    use super::*;
    #[tokio::test]
    async fn test_write() -> std::io::Result<()> {
        let request = Request::get(Uri::from_static("/magic"))
            .body(Body::full(Bytes::from(vec![])))
            .unwrap();

        let mut buf = futures::io::Cursor::new(Vec::<u8>::new());

        write_request(request, &mut buf).await?;

        let buf: Bytes = buf.into_inner().into();

        assert_eq!(buf, &b"GET /magic HTTP/1.1\r\n\r\n"[..]);

        Ok(())
    }

    #[tokio::test]
    async fn test_read() -> std::io::Result<()> {
        let request = || {
            Request::get(Uri::from_static("/magic"))
                .header("Authorization", "Bearer hehe")
                .body(Body::full(Bytes::from(vec![0xfa, 0xf7, 0x5a])))
                .unwrap()
        };

        let mut buf = futures::io::Cursor::new(Vec::<u8>::new());

        write_request(request(), &mut buf).await?;

        let buf: Bytes = buf.into_inner().into();

        assert_eq!(
            buf,
            &b"GET /magic HTTP/1.1\r\nauthorization: Bearer hehe\r\n\r\n\xfa\xf7\x5a"[..]
        );

        let mut buf_read = BufReader::new(futures::io::Cursor::new(buf.to_vec()));

        let read_request = read_request(&mut buf_read).await?;

        let request = request();
        assert!(read_request.uri() == request.uri());
        assert!(read_request.headers() == request.headers());
        assert!(read_request.version() == request.version());
        assert!(read_request.body() == request.body());

        Ok(())
    }

    #[tokio::test]
    async fn test_write_response_and_read_response() -> std::io::Result<()> {
        let response = || Response::builder()
            .status(200)
            .body(Body::full("Hello, world!".into()))
            .unwrap();

        let mut buffer = Vec::new();
        write_response(response(), &mut buffer).await?;

        let mut reader = BufReader::new(futures::io::Cursor::new(buffer));
        let read_response = read_response(&mut reader).await?;

        let response = response();
        assert_eq!(response.status(), read_response.status());
        assert_eq!(response.body(), read_response.body());

        Ok(())
    }
}
