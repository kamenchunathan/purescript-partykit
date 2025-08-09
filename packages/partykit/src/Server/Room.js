export function broadcastImpl(room, message, exceptionIds) {
  room.broadcast(message, exceptionIds);
}
