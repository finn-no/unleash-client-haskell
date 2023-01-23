# unleash-client-haskell

This is an opinionated Unleash Haskell client that uses `unleash-client-haskell-core`.

`Unleash.Client` exports functions and types for:

- Client configuration
- Client registration
- Fetching feature toggle states
- Sending metrics

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
