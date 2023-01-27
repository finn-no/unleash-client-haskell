# unleash-client-haskell

This is an [Unleash](https://www.getunleash.io/) client SDK for Haskell.

`Unleash.Client` provides a stateful HTTP client with functions and types for:

- Client configuration
- Client registration
- Fetching feature toggles
- Fetching variants
- Sending metrics

You'll need to spawn threads and handle errors yourself. This is demonstrated in the [example application](example/Main.hs).

`unleash-client-haskell` is a (working) work in progress and the API is likely to change.

See [unleash-client-haskell-core](https://github.com/finn-no/unleash-client-haskell-core) for a bare-bones feature toggle evaluation library.

## Build

```
nix-build
```

## Maintainers

- [Eirik Meland](mailto:eirik.meland@gmail.com)
- [Even Brenden](mailto:evenbrenden@gmail.com)

## Dependencies

- aeson (BSD-3-Clause)
- containers (BSD-3-Clause)
- http-client-tls (MIT)
- http-media (MIT)
- servant (BSD-3-Clause)
- servant-client (BSD-3-Clause)
- text (BSD-2-Clause)
- time (BSD-2-Clause)
- unleash-client-haskell-core (MIT)
- unliftio (MIT)
