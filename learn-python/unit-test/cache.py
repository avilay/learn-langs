import numpy as np


class Cache:
    def __init__(self, capacity):
        self._cache = {}
        self._capacity = capacity

    def read(self, key):
        if key not in self._cache:
            keys = list(self._cache.keys())
            if len(keys) == self._capacity:
                del self._cache[keys[0]]
            self._cache[key] = np.random.randint(100)
        return self._cache[key]

    def read_all(self):
        return {
            'capacity': self._capacity,
            'cached_data': self._cache
        }


def use_cache():
    cache = Cache(3)
    print(cache.read('key one'))
    print(cache.read('key two'))
    print(cache.read('key three'))
    print('\n', cache.read_all())
    print(cache.read('key four'))
    cache._capacity = 10
    print('\n', cache.read_all())

