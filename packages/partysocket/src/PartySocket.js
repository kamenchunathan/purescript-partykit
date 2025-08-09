import { PartySocket } from "partysocket"

export function createPartySocketImpl(args) {
  return new PartySocket(args)
}

export function sendImpl(sock, data) {
  sock.send(data);
}

