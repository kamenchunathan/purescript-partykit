export const broadcast = room => message => exceptionIds => {
  room.broadcast(message, exceptionIds);
}

export function getImpl(storage, key) {
  return storage.get(key);
}

export function putImpl(storage, key, value) {
  return storage.put(key, value);
}

export function deleteImpl(storage, key) {
  return storage.get(key);
}

export function deleteAllImpl(storage) {
  return storage.deleteAll;
}
