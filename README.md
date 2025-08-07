# purescript-partykit

Purescript bindings for the PartyKit API.

## Goal

This library provides Purescript bindings for [PartyKit](partykit.io), a platform that simplifies creating real-time, multiplayer, and collaborative applications.

This allows you to write your PartyKit server and client logic in Purescript, a strongly-typed functional programming language that compiles to JavaScript.

## Usage

The library is split into two main modules: `PartyKit.Server` and `PartyKit.Client`.

### Server

The `PartyKit.Server` module provides bindings to the PartyKit server API. You can use it to create a PartyKit server that handles WebSocket connections, messages, and state.

```purescript
module Main where

import Prelude

import Effect (Effect)
import PartyKit.Server (PartyServer, onConnect, onMessage)

main :: Effect Unit
main = do
  let
    server :: PartyServer
    server =
      { onConnect: \conn -> do
          log "connected"
      , onMessage: \msg conn -> do
          log "messaged"
      }
  pure unit
```

### Client

The `PartyKit.Client` module provides bindings to the PartyKit client API. You can use it to connect to a PartyKit server and send and receive messages.

```purescript
module Main where

import Prelude

import Effect (Effect)
import PartyKit.Client (PartySocket)

main :: Effect Unit
main = do
  let
    socket :: PartySocket
    socket = new PartySocket({ host: "localhost:1999", room: "my-room" })
  pure unit
```

## Example

The `examples/tic_tac_toe` directory contains a complete example of a multiplayer tic-tac-toe game built with `purescript-partykit`.

## Roadmap / TODO
  - [ ] Write implementations for the partyserver library as well allowing deployments to cloudflare directly using wrangler.dev

