# unleash-client-haskell

This is an [Unleash](https://www.getunleash.io/) client Haskell library.

`Unleash.Client` provides functions and types for:

- Client configuration
- Client registration
- Fetching feature toggle states
- Sending metrics

Core functionality is provided by [unleash-client-haskell-core](https://github.com/finn-no/unleash-client-haskell-core).

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
- unleash-client-haskell-core (MIT)
