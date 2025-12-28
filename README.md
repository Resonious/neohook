# neohook

[![Package Version](https://img.shields.io/hexpm/v/neohook)](https://hex.pm/packages/neohook)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/neohook/)

```sh
gleam add neohook@1
```
```gleam
import neohook

pub fn main() -> Nil {
  // TODO: An example of the project in use
}
```

Further documentation can be found at <https://hexdocs.pm/neohook>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```

## Deployment

Rough notes for now.

Certbot:

```bash
DOMAIN=snd.one
EMAIL=your-email@whatever.com

certbot certonly --webroot --webroot-path /var/www/public -d $DOMAIN --non-interactive --agree-tos --email $EMAIL
```
