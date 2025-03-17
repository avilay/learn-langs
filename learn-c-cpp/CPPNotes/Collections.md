# Collections

Ref: [Containers library - cppreference.com](https://en.cppreference.com/w/cpp/container)

## Custom Objects

In order to add UDTs to containers we need two things -

* A way to check equality
* A way to get a unique hash value

It is of course very trivial to implement the `operator==` for any given struct or class.

The standard library contains various implementations of -

```c++
template<class Key>
struct hash;
```

This is a callable class so it can be used as follows -

```c++
std::hash<std::string> strHasher {};
size_t h { strHasher("Avilay") };
```

Alternately, I can use it in a single line like so -

```c++
size_t h { std::hash<std::string> {}("Avilay") };
```

It is pretty straightforward for me to implement my own `hash` for my UDT and insert it in the standard namespace.

Here is the full implementation if I want to make my UDT usable in collections -

```c++
struct Cookie {
    int calories;
    std::string flavor;
    bool operator==(const Cookie&) const = default;
};

template <>
struct std::hash<Cookie> {
    size_t operator()(const Cookie& cookie) const noexcept
    {
        size_t h { 7 };
        h = 31 * h + std::hash<int> {}(cookie.calories);
        h = 31 * h + std::hash<string> {}(cookie.flavor);
        return h;
    }
};
```

This implementation is from old-style Java classes. The C++ examples use raising a big number to the power of another big number and it can be a pretty expensive op.

[std::hash - cppreference.com](https://en.cppreference.com/w/cpp/utility/hash)

[Guide to hashCode() in Java | Baeldung](https://www.baeldung.com/java-hashcode)

## Maps

There are two implementations of maps in C++ -

* `map` which is implemented as a binary search tree.

* `unordered_map` which is implemented as a hash table and can only hold unique keys.

  * `unordered_multimap` which can hold duplicate keys.

Each data structure lives in its own header, so `unordered_map` lives in `<unordered_map>`.

### `unordered_map`

#### Initialization

Uses aggregate-style initialization. Any duplicate elements are ignored. The value of the first occurence of the key will be stored.

```c++
std::unordered_map<std::string, float> map {
  { "pi", 3.141 },
  { "euler", 2.718 },
  { "plank", 6.626 },
  { "pi", 3.141592 }  // Dups will be silently ignored
};
```

#### Iteration

Range based for loops work. The item is a key-value pair.

```c++
for (const auto& kv : map) {
  std::string key { kv.first };
  float val { kv.second };
}
```

#### Writes

##### Upsert

There are two ways to upsert the map -

* `operator[]`
* `.insert()`

As with vectors and arrays, `operator[]` is not strict. If the key exists, then its value is updated with the new value provided. If the key does not exist it is added to the map.

```c++
// avogadro => 6.022 is added to the map
map["avogadro"] = 6.022;

// pi is updated from its old value 3.141 to its new value 3.141592
map["pi"] = 3.141592;
```

`insert()` is not much better! If they key exists, then it will silently ignore the insertion. If the key does not exist it will be added to the map.

```c++
// avogadro => 6.022 is added to the map
map.insert({ "avogadro", 6.022 });

// this is silently ignored, pi retains its old value 3.141
map.insert({ "pi", 3.141592 });
```

##### Delete Key

`erase()` method is used to delete keys. If the key exists, it will be deleted. If it does not exist, this method does nothing.

```c++
// pi => 3.141 is deleted from the map
map.erase("pi");

// nothing happens
map.erase("avogadro");
```

#### Reads

##### Get Value

There are two ways to get values -

* `operator[]`
* `.at()`

Again `operator[]` is not strict. If the key exists, its value is read. If the key does not exist, this will insert this key with a default value initialization of the value type.

```c++
// x has the value 3.141
float x { map["pi"] };

// y has the value 0.0
// avogadro => 0.0 has been added to the map
float y { map["avogadro"] };
```

`at` works as expected. If the key exists, its value is read. If the key does not exist, it will throw `std::out_of_range` exception.

```c++
float x { map.at("pi") };

try {
  float y { map.at("avogadro") };
} catch (std::out_of_range err) {...}
```

##### Check Existence

Instead of a boolean method that checks the existence, I have to use the `count()` method to count the number of occurrence of the key.

```c++
map.count("pi") > 0 == true
map.count("avogadro") > 0 == false  
map.contains("pi")  // evaluates to true
```

## Sets

There are two implementations of maps in C++ -

* `set` which is implemented as a binary search tree.

* `unordered_set` which is implemented as a hash table and can only hold unique keys.
  * `unordered_set` which can hold duplicate keys.

Each data structure lives in its own header, so `unordered_set` lives in `<unordered_set>`.

```c++
// uniqs = {1, 2, 3, 4, 5}
std::unordered_set<int> uniqs { 3, 4, 1, 3, 2, 3, 5 };

// vec :: vector<int>
std::unordered_set<int> uniqs { vec.begin(), vec.end() };

// range based for-loop
for (const auto& x : uniqs) {
    std::cout << std::format("x={}", x) << std::endl;
}

auto len { std::ssize(uniqs) };

uniqs.insert(6);  // will add 6 to the set
uniqs.insert(1);  // ignored

uniqs.emplace(6);  // useful for creating the object directly inside the set.

uniqs.count(10) > 0 // is false
uniqs.count(1) > 0  // is true  
uniqs.contains(1) // evaluates to true
  
auto it { uniqs.find(3) };
if (it != uniqs.end()) {
  *it;
}

uniqs.erase(1);  // uniqs is now {2, 3, 4, 5}
uniqs.erase(10);  // ignored
```

## Deques

In `<deque>` header. Very similar to `std::vector`.

```c++
std::deque<int> q { 3, 1, 2, 4 };

// Typical usage
while (!q.empty()) {
    int& x = q.front();
    std::cout << std::format("Processing {}", x) << std::endl;
    q.pop_front();
    if (x == 3) {
        q.push_back(10);
    }
}
```

