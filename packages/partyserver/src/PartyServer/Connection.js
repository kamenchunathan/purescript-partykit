export function request(context) {
  return context.request;
}

export function id(connection) {
  return connection.id;
}

export function server(connection) {
  return connection.server;
}

export function state(connection) {
  return connection.state;
}

export function setStateValueImpl(connection, value) {
  return connection.setState(value);
}

export function setStateFnImpl(connection, fn) {
  return connection.setState(fn);
}

export function sendImpl(connection, message) {
  connection.send(message);
}
