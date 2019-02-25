# front (work-in-progress)

A front-end web framework which aim is to bring the power of GHC on client side with as less JavaScript as possible by the Fay.

The goal is to have a tiny websockets-based bridge between client and server to propagate client events on server for further handling.

## Features

- Server-side rendering.
- both SPA and regular webapp (sharing state across multiple routes).
- Several communication models (single session, broadcast, etc.).
- Virtual DOM with full/partial rendering.
- "blaze-html" like extended markup tree for handling both DOM and JS events.

## Installation

For server:

```
cabal new-build
```

For client (issue: faylang/fay#459):

```
cabal sandbox init
cabal install # fay executable and libraries will be loaded
export HASKELL_PACKAGE_SANDBOX=`echo .cabal-sandbox/*-packages.conf.d/`
cabal-sandbox/bin/fay --no-rts --no-stdlib \
  --package fay-base,fay-dom,fay-websockets \
  --include shared,fay \
  -o bundle.js fay/Client.hs  
```

## Usage

Coming soon...

## Acknowledgement

This ongoing framework would not have happened without these people and technologies:

- @5HT for inspiration by **N2O** framework. The idea of transfer both data and events over websockets.
- @jaspervdj for **blaze-html**.
- @meiersi for **blaze-react** and the approach how to handle events in the same markup with html.
- @snoyberg for **yesod-fay** and the way how to embed generated assert into server
and to trigger dependent Fay compilation from Haskell code.
- @bergmark for **Fay** compiler.