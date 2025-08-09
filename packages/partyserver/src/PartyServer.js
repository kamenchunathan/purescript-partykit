import { Server } from "partyserver";

export function createImpl(properties) {
  return class extends Server {
    onStart() {
      if (properties.onStart) {
        properties.onStart(this);
      }
    }

    onConnect(connection, ctx) {
      if (properties.onConnect) {
        properties.onConnect(this, connection, ctx);
      }
    }

    async onMessage(connection, message) {
      if (properties.onMessage) {
        properties.onMessage(this, connection, message);
      }
    }

    onClose(connection, code, reason, wasClean) {
      if (properties.onClose) {
        properties.onClose(this, connection, code, reason, wasClean);
      }
    }

    onError(connection, error) {
      if (properties.onError) {
        properties.onError(this, connection, error);
      }
    }

    onRequest(request) {
      if (properties.onRequest) {
        return properties.onRequest(this, request);
      }
      return super.onRequest(request);
    }

    onException(error) {
      if (properties.onException) {
        properties.onException(this, error);
      }
    }

    onAlarm() {
      if (properties.onAlarm) {
        properties.onAlarm(this);
      }
    }
  };
}

export function broadcastImpl(server, msg, without) {
  server.broadcast(msg, without);
}

export function getConnectionImpl(server, id) {
  const conn = server.getConnection(id);
  return conn === undefined ? null : conn;
}

export function getConnectionsImpl(server, tag) {
  return Array.from(server.getConnections(tag ?? undefined));
}
