# grafdhall

[![Hackage](https://img.shields.io/hackage/v/grafdhall.svg?logo=haskell)](https://hackage.haskell.org/package/grafdhall)
[![Apache-2.0 license](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](LICENSE)

`grafdhall` takes [Grafana][grafana] dashboards in Dhall format, and submits them to a grafana service.

The CLI expects the following environment variables:

- `GRAFANA_URL`  : the url of the service.
- `GRAFANA_PASS` : the password of the user.
- `GRAFANA_USER` : the name of the user, defaults to `admin`.

Uses the [dhall-grafana][dhall-grafana] bindings to write the dashboards, checkout the
[examples](./examples/).

## Contribute

Build the project:

```
$ cabal build
```

Run the cli:

```
$ cabal run grafdhall -- --help
```

[grafana]: https://grafana.com
[dhall-grafana]: https://github.com/weeezes/dhall-grafana
