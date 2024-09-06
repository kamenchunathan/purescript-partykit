import { PartySocket } from "partysocket"

export function createPartySocketImpl(args) {
  return new PartySocket(args)
}

