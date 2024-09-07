export function createImpl(properties) {
  let PartyServer = class {
    constructor(room) {
      properties.constructor(room)
    }
  }

  for (const [key, value] of Object.entries(properties)) {
    if (key == 'constructor') {
      continue;
    }
    // static functions
    else if (key === 'onBeforeRequest') {
      Object(PartyServer.prototype, key, { value });
      continue;
    }

    Object.defineProperty(PartyServer.prototype, key, { value });
  }

  return PartyServer;
}

export function eitherImpl(l) {
  return function(r) {
    return function(cb) {
      return function(val) {
        return cb((typeof (val) === 'string') ? l(val) : r(val));
      }
    }
  }
}

export function mkEffectMethod1(f) {
  return function() {
    f(this)();
  }
}

export function mkEffectMethod2(f) {
  return function(a) {
    f(this)(a)();
  }
}

export function mkEffectMethod3(f) {
  return function(a, b) {
    f(this)(a)(b)();
  }
}
