#!/usr/bin/env node

type CacheLoader<K, V> = (key: K) => Promise<V>;

interface Cache<K, V> {
  loader: CacheLoader<K, V>;
  get(key: K): Promise<V>;
}

class MyCache<K, V> implements Cache<K, V> {
  loader: CacheLoader<K, V>;
  private readonly store: Map<K, Promise<V>>;

  constructor(loader: CacheLoader<K, V>) {
    this.loader = loader;
    this.store = new Map();
  }

  get(key: K): Promise<V> {
    const val = this.store.get(key);
    if (val !== undefined) return val;
    const newVal = this.loader(key);
    this.store.set(key, newVal);
    newVal.catch((_) => this.del(key, newVal));
    return newVal;
  }

  del(key: K, val: Promise<V>): boolean {
    if (this.store.get(key) === val) {
      return this.store.delete(key);
    }
    return false;
  }
}

// ===  Test ===

let first = true;
function getLength(key: string): Promise<number> {
  console.log(`getLength key: ${key}`);
  if (key == "world" && first) {
    first = false;
    return Promise.reject("error: first world");
  }
  return new Promise(
    (resolve) => setTimeout(() => resolve(key.length), 1000),
  );
}

const cache = new MyCache<string, number>(getLength);

for (const key of ["hello", "world"]) {
  for (let i = 0; i < 3; i++) {
    try {
      const val = await cache.get(key);
      console.log(`${key}=>${val}`);
    } catch (e) {
      console.log(`Erorr: ${e}`);
    }
  }
}
