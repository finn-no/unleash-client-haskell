# unleash-client-haskell

This is an opinionated Unleash Haskell client that uses `unleash-client-haskell`.

`Unleash.Client` exports:

- A data structure for client configuration.
- A function for client registration.
- A function for polling for feature toggles.
- A function for sending metrics.

## Build

```
nix-build
```

## Maintainers

- [Eirik Meland](mailto:eirik.meland@gmail.com)
- [Even Brenden](mailto:evenbrenden@gmail.com)

## Dependencies

- async (BSD-3-Clause)
- http-client (MIT)
- servant-client (BSD-3-Clause)
- text (BSD-2-Clause)
- time (BSD-2-Clause)
- unleash-client-haskell (MIT)
