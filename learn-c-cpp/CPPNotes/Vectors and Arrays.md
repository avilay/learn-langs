# Vectors and Arrays

Three styles of arrays, two are fixed-size, i.e., their size aka length must be known at compile time. And one is dynamic-size, i.e., it is resizable and its size can be changed after instantiation, so it is not needed at compile time.

* C-style arrays - fixed-size arrays
* `std::array` - fixed-size arrays
* `std::vector` - dynamic-size arrays

## Vectors

### Initialization

Need the `<vector>` header.

```c++
std::vector<int> primes { 2, 3, 5, 7 };
std::vector vowels { 'a', 'e', 'i', 'o', 'u' };
```

This kind of an intializer where I can provide a comma separated list of values is called a **list initializer**.

All the elements in the array are in contiguous memory -

```c++
std::cout << &(primes[0]) << std::endl;
std::cout << &(primes[1]) << std::endl;
std::cout << &(primes[2]) << std::endl;
std::cout << &(primes[3]) << std::endl;
// will prinout of adjacent memory locations 4 bytes (int length) apart.
```

To create a vector of objects, I don't have to give the object type inside the initializer, it is inferred and the right parameterized ctor for Cookie is called -

```c++
std::vector<Cookie> cookies {
  { 200, "Chocolate Chip" },
  { 180, "Oatmeal Raisin" },
  {220, "Snicker Doodle" }
};
```

To create a vector of some fixed length, I can use the old-style ctor, also called direct initialization, where I give the desired length as an argument. The vector is "value initialized", i.e., is filled with the default initializer of the contained element. Such a vector is **not** empty.

```c++
explicit std::vector<T>(int)
```

```c++
std::vector<int> nums(3);
// nums will be { 0, 0, 0 }
```

If I use the usual initializer, C++ always gives precendence to the list initializer, so it will create a single element vector for me.

This is a problem in an edge case when a vector is a member of a class/struct and I want to provide a default initializer for it, as opposed to initializing it in the ctor.

```c++
struct Foo {
  std::vector<int> v1(10);  // ERROR: Direct initialization not allowed.
  std::vector<int> v { std::vector<int>(10) };
}
```

I can add the `const` qualifier like so -

```c++
const std::vector<int> v1 { 1, 2, 3, 5 };
std::vector<const int> v2 { 1, 2, 3, 5 };
const std::vector<const int> v3 { 1, 2, 3, 5 };
```

I cannot declare a vector of lvalue references because elements must be assignable and lvalue references cannot be unseated.

```c++
std::vector<Cookie&> cookies {};  // ERROR: Cannot use lvalue references
```

### Length and Capacity

In C++ length of containers is called "size". The size of the vector is given as a `T::size_type` which in most cases defaults to `std::size_t` which in turn defaults to some big unsigned int. There are three ways to get the length of a vector -

```c++
// v :: std::vector
auto len = v.size();  // len :: size_t
len = std::size(v);  // Preferred in templated functions because works with C-style arrays.
auto len2 = std::ssize(v);  // len2 is some large **signed** integral type.
```

In case I want it to use this in a class or some such -

```c++
int len { static_cast<int>(v.size()) };
```

Length is different than the capacity, which is the allocated capacity that may not necessarily be filled up with elements.

```c++
v.capacity();
```

To check for 0 length use `.empty()` method -

```c++
if (v.empty()) {
  // v is 0 length
}
```

### Accessing Elements

In the examples below I am using the pattern `ClassType obj = v[i]` or something to that effect. This means that everytime I access anything inside the container I am paying the price to copy it to the `obj`. To avoid this I should read the value inside a reference type, e.g., `ClassType& obj = v[i]`.

To access an element - 

```c++
int x = primes[0];
x = primes.at(3);
```

* The `operator[]` does not do any sort of bounds checking.
* `.at` method does bounds checking, throws `std::out_of_range` exception.

 `operator[]` accepts a `std::size_t` index, I can still pass an `int` index to it without any static casts or any other warnings 🤔

```c++
int idx;
std::cout << "Enter index to access: " << " " << std::endl;
std::cin >> idx;

int val = primes[idx];  // No compiler warning.
std::size_t i { idx };  // WARN: Got a compiler warning
val = primes[i];
```

### As Function Parameter and Return Type

Like with any heavy class or data structure, to pass a vector to a function, use const references. To return vectors just return them as if by value, but internally they will be moved, thus avoiding any expensive copying.

```c++
std::vector<int> extend(const std::vector<int>& v1, const std::vector<int>& v2)
{
  std::vector<int> combined {...};
  :::
  return combined;
}
```

### Iterating & Iterators

Using standard index and the `operator[]` -

```c++
// v :: std::vector
long len { std::ssize(v) };
for (int i { 0 }; i < len; i++) {
  v[i];
}

// alternately use auto
auto len { std::ssize(v) };
for (auto i { 0 }; i < len; i++) {
  v[i];
}
```

Using range based loops -

```c++
// This will incur a copy cost of copying each element of v into x successively
for (auto x : v) {
  x;
}

// This will avoid the copy cost
for (const auto& x : v) {
  x;
}

// reverse iterator
for (const auto& x : std::views::reverse(v)) {
  x;
}
```

C++ has the concept of iterators -

```c++
// Points to the first element in the sequence
auto it = v.begin();
```

I can move the iterator to the next element using the `+` op -

```c++
// Points to the next element
it += 1;  
```

I can get to the current element that the iterator is pointing to using the derefrence operator -

```c++
std::cout << *it;
```

Reach the end like this -

```c++
// Points to the position after the last element in the sequence
// derefrencing it leads to undefined behavior
it = v.end();
```

Iterators are usually used with the `<alogirthm>` library which has functions like `for_each`, `accumulate`, etc. But here is a simple demo of using iterators to iterate through the vector -

```c++
// cookies :: std::vector<Cookie>
// std::string Cookie::repr();
for (auto it { cookies.begin() }; it != cookies.end(); it++) {
	std::cout << it->repr() << std::endl;  // will call Cookie::repr
  std::cout << (*it).repr() << std::endl;  // same effect
  Cookie c = *it;  // Incur a copy cost
}
```

I can start iterating at any position and end at any position. I just need to take care that I am not past the end. `v.begin() + n` corresponds to the element at **index** `n`.

```c++
for (auto it { cookies.begin() + 1 }; it != (cookies.end() - 1); it++) {
	std::cout << it->repr() << std::endl;
}
```

Iterators can also be used to modify the vector -

```c++
auto it = cookies.begin();
*it = { 140, "Soft Thin" };
```

### Resizing

When a vector is resized, any new elements are value initialized.

```c++
std::vector<int> v { 1, 2, 3 };
v.resize(5);
// Now v contains 1, 2, 3, 0, 0

std::vector<int> u { 1, 2, 3, 4, 5 };
v.resize(3);
// Now u contains 1, 2, 3
```

Resizing results in reallocation where all the existing elements are **copied** to their new location. While resizing definitely changes the length of the vector, it can optionally change the capacity as well. Usually when the size is reduced like in the second example above, the capacity of the vector might remains the same, only its length is reduced. However, I can give the CRT a hint that I am ok with reducing the capacity as well to reclaim space -

```c++
v.shrink_to_fit();
```

However, this is just a hint, it is **not** guaranteed to reduce the capacity and reclaim space.

If I want to "resize" the capacity without changing the length I can just reserve the capacity -

```c++
std::vector<int> v { 1, 2, 3 };  // Both length and capacity are 3
v.reserve(6);  // Length is still 3, capacity increased to 6
```

### Adding & Removing Elements

| Method           | Behavior                                                     |
| ---------------- | ------------------------------------------------------------ |
| `push_back()`    | Appends an element to the back of the vector.                |
| `pop_back()`     | Returns and removes the last element of the vector.          |
| `back()`         | Returns the last element of the vector **without** removing it. |
| `emplace_back()` | Same as push but can be more performant in some scenarios.   |
| `clear()`        | Empties the entire vector reducing the length to 0. May or may not reduce the capacity. |
| `erase()`        | Removes any arbitrary element or range of elements pointed to by the iterator. |
| `insert()`       | Inserts a single element or splices multiple elements at any arbitrary position(s) in the vector. |

When using these operations, I should be cognizant of direct initialization and resizing as they change the length of the vector and fill it with default values. Pushing back in this case will append a new element after the default ones.

```c++
std::vector<int> v(3);
v.push_back(1);
// v contains { 0, 0, 0, 1 }
```

#### Difference between `push_back` and `emplace_back`

> If I already have an object that I want to append to the vector, I am better off using `push_back`. If I am constructing the object as I am appending it to the vector, I am better off using emplace_back.

Lets say I have a UDT called `Cookie` and `cookies` is a vector of `Cookie` objects.

The below code will call the copy constructor to copy the cookie object inside the vector.

```c++
// cookie :: Cookie already exists
cookies.push_back(cookie);  // PREFERRED
```

The below code will first call the parameterized constructor to construct the cookie object, then call the move constructor to create the rvalue inside the parens, and finally call the copy ctor to copy this temporary object inside the vector.

```c++
cookies.push_back({ 200, "Chocolate Chip" });  // NOT PREFERRED
```

The below code will call the parameterized constructor once to construct the object directly inside the vector, so no copies or moves need to be made. Notice how when using `emplace_back` I use varargs to pass all the ctor arguments directly to the `emplace_back` method.

```c++
cookies.emplace_back(200, "Chocolate Chip");  // PREFERRED
```

The below code performes the most poorly, it calls the copy constructor three times for some reason.

```c++
// cookie :: Cookie already exists
cookies.emplace_back(cookie);
```

See more details [abseil / Tip of the Week #112: emplace vs. push_back](https://abseil.io/tips/112)

#### Inserting

`insert` can be used to insert a single element or a range of elements.

Lets say I have a cookies vector with the following cookies -

```
<Cookie(calories=200, flavor=Chocolate Chip)>
<Cookie(calories=180, flavor=Oatmeal Raisin)>
<Cookie(calories=220, flavor=Snicker Doodle)>
```

Then I insert a cookie at index [1], this will move Oatmeal Raisin and Snicker Doodle down.

```c++
cookies.insert(cookies.begin() + 1, { 240, "Double Chocolate Chip" });
```

The result will be -

```
<Cookie(calories=200, flavor=Chocolate Chip)>
<Cookie(calories=240, flavor=Double Chocolate Chip)>
<Cookie(calories=180, flavor=Oatmeal Raisin)>
<Cookie(calories=220, flavor=Snicker Doodle)>
```

I can also "splice" a range of values at any arbitrary position -

```c++
std::vector<int> nums { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
std::vector<int> moreNums { 100, 200, 300 };
nums.insert(nums.begin() + 2, moreNums.begin(), moreNums.end());
```

After this the `nums` vector will look like -

```
0 1 100 200 300 2 3 4 5 6 7 8 9
```

#### Erasing

`erase` can be used to remove a single element or a range of elements from $[i, j)$. It will move all the elements up thereby reducing the length, but it may or may not reduce capacity.

```c++
// cookies is a vector of 6 Cookie objects

// Will delete cookies[2]
cookies.erase(cookies.begin() + 2);  

// Will delete cookies[2], cookies[3], and cookies[4]
cookies.erase(cookies.begin() + 2, cookies.begin() + 5)
```

### References

In the normal course of things, I cannot have a vector of references, because the underlying type in containers has to be assignable and references are rvalues therefore not assignable. If I do want to use references I need to use the `std::reference_wrapper` and the helper functions `std::ref` and `std::cref`.

## Arrays

Part of the `<array>` library.

> Use std::array for constexpr arrays, and std::vector for everything else.

### Initialization

Need to know the size at compile time. Can use value initialization -

```c++
std::array<int, 5> a {};  // Filled with 5 zeros.
```

`std::array` does not have any constructors, it uses aggregate initialization -

```c++
std::array<int, 5> primes { 1, 2, 3, 5, 7 };
```

Unlike vectors, when I am initializing arrays of object, I have to mention the object type -

```c++
std::array<Cookie, 3> cookies {
  Cookie { 200, "Chocolate Chip" },
  Cookie { 180, "Oatmeal Raisin" },
  Cookie {220, "Snicker Doodle" }
};

// As opposed to vectors where I could do -
std::vector<Cookie> cookies {
  { 200, "Chocolate Chip" },
  { 180, "Oatmeal Raisin" },
  {220, "Snicker Doodle" }
};
```

Alternately, if I don't want to mention the object type, I need to use double braces like so -

```c++
std::array<Cookie, 3> cookies {
    { { 200, "Chocolate Chip" },
        { 180, "Oatmeal Raisin" },
        { 220, "Snicker Doodle" } }
};
```

All structs have something called **brace elision** where the compiler gets rid of extra braces if not needed. This is why sometimes folks will always use double braces when initializing arrays, even if they are not needed.

```c++
std::array<int, 3> nums {{ 1, 2, 3 }};
```

I can make the array const, this means that the elements are also const even though they are not explicitly marked as such -

```c++
const std::array<Cookie, 3> cookies {...};
// Now I can only call const methods on the elements.
```

The main benefit of using `std::array` over `std::vector` is that it can be a `constexpr` which means all sorts of compiler optimizations can be applied to it.

```c++
constexpr std::array<int, 3> nums { 1, 2, 3 };
constexpr std::array { 1, 2, 3 };  // Type inference works kinda/sorta
```

If I don't want to provide both the template args - the type and size, I can use `to_array` along with type inference -

```c++
constexpr auto nums { std::to_array({ 1, 2, 3 })};
```

This will incur a copy cost.



For the most part, whatever works for vectors will work for arrays, except the stuff related to dynamic sizing, like `resize()`, `insert()`, etc. Here are a few things that arrays have that vectors don't.

### Compile time bounds checking 

`operator[]` does no bounds checking, and `at()` only does runtime bounds checking. But given arrays are mostly `constexpr`, we can do compile time bounds checking using `get`.

```c++
std::get<3>(nums);  // will return the element at index 3
```

### Function Parameters

For arrays we need to pass in both the type and size when passed in as an argument. When templatizing we can do a number of different things as shown below -

```c++
void foo(const std::array<int, 3>& nums);

template<typename T, auto N>
void foo(const std::array<T, N>& arr);

template<auto N>
void foo(const std::array<int, N>& integers);

template<typename T>
void foo(const std::array<T, 3>& triples);
```

### Return Type

Unlike vectors, arrays are not move capable, so returning them by value will incur a copy cost. If I want to avoid that I can use an out param.



## C-Style Array

### Initialization

The length must be constexpr when the array is being allocated on the stack. Just like std::array, C-style arrays are also aggregates, which means we can use aggregate initialization, there are no ctors.

```c++
int ary[3] {};  // All elements are value initialized, i.e., 0 in case of ints.
int ary[] { 1, 2, 3 };  // No need to specify length with aggregate initialization.
const int primes[] { 2, 3, 5, 7, 11 }; // an array of const int
int primes[n] = {};  // n is a runtime value and the array will be value initialized.
```

To initialize a variable number of elements I need to allocate them on the heap -

```c++
Cookie* cookies { new Cookie[n] };
```

This will call the default ctor n times and allocate n cookies on the heap, with `cookies` pointing at the first element. Unlike the `ary` array which had knowledge of its size, `cookies` does not. However the heap where it is allocated has a header where n is recorded. To free the entire chunk of contiguous memory all I have to do is -

```c++
delete[] cookies;
```

without specifying the number of elements. The system will get n from the header and free all n objects.

#### Multidimensional

```c++
int matrix[3][5]
{
  {1, 2},
  {6, 7, 8},
  {11, 12, 13, 14}
};
```

The above will result in this matrix -
$$
\begin{bmatrix}
1 & 2 & 0 & 0 & 0 \\
6 & 7 & 8 & 0 & 0 \\
11 & 12 & 13 & 14 & 0
\end{bmatrix}
$$
The missing elements are filled with the value initializer, 0 in case of ints.

I can omit the dimension of the first axis, but I have to provide the dimensions of all other axes.

```c++
int matrix[][2]
{
  {1, 2},
  {3, 4}
};
```

The first axis' dimension is inferred as 2.

Internally multidimensional arrays are stored in row-major form.

### Accessing Elements

Just the simple `operator[]` sans any bounds checking.

```c++
nums[0];
```

#### Length

The `sizeof` operator gives the size of the entire array.

```c++
int ary[] { 1, 2, 3 };
auto sz { sizeof(ary) };  // sz = 12 (bytes) assuming 4 byte ints.
```

Instead of doing weird things like `sizeof(ary) / sizeof(int)` or anything like that I can just use the `std::ssize` function that I have been using for vectors and arrays -

```c++
auto len { std::ssize(ary) };  // len = 3
```

### Adding Elements

```c++
ary[1] = 200;  // OK
ary = { 100, 200, 300 };  // ERROR: C-style arrays don't support assignment.
```

### Pointer Arithmetic

Arrays are allocated a contiguous chunk of memory divided up in blocks where each block is the size of the containing element. I can have a pointer point to the first element of the array. Incrementing or decrementing a pointer works in blocks. The block size is whatever is the size of the element being pointed to is. That way the pointer is always pointing to a single element from the beginning of its entry. Assuming ints are 4 bytes -

![ptr_arithmetic](/Users/avilay/projects/bitbucket/learn/learn-lang/learn-c-cpp/CPPNotes/imgs/ptr_arithmetic.png)

Thus `*(ptr + n)` $\equiv$ `ary[n]` where `int* ptr { ary }`. 

Any pointer itself is 8 bytes long, regardless of what type of object it is pointing to. So incrementing a pointer to a pointer will increment it by 8.

### Array Decay to Pointers

When C-style arrays are used in an expression, they will be implicitly converted to a pointer of the containing type, initialized with the address of the first element. This behavior is called array decay. This is called "decay", because the pointer loses the length information. The actual array (type `int[]` say) knows its length. The pointer (`int*`) does not.

In the code below, being inside an expression will cause ary to decay to a pointer, which means that the type of `ptr` is `int*` and its value is the address of the first element.

```c++
int ary[] { 1, 2, 3 };
int* ptr { ary };
```

When used with the `operator[]`, the array decays to a pointer.

```c++
ary[2];
// above is equivalent to -
int* ptr { ary };
*((ptr) + (2));
// which in turn is equivalent to -
*(ptr + 2);
```

Because of this behavior I can also do  `n[ptr]`, the compiler is going to translate it to `*((n) + (ptr))` which is `ary[2]`. 

> Favor subscripting when indexing from the start of the array. Favor pointer arithmetic when doing relative positioning from some arbitrary element.

The semantics of the array and the pointer are pretty much interchangeable -

```c++
int arr[]{0, 1, 2, 3};
int* ptr { arr };

// All the following are equivalent and will evaluate to 0
*ptr;
*arr;
arr[0];
ptr[0];
```

While not obvious, but C++ supports negative indices, though they don't work the way they work in Python. Here it will just go to the previous element.

```c++
int ary[] { 0, 1, 2, 3, 4 };
const int* ptr { ary + 2 };
ptr[-1];  // This will evaluate to 1
```

### Function Parameters

Even if I defined a function as accepting an array and pass the array to this function, inside the function the array has decayed to a pointer.

```c++
void foo(const int ary[])
{
  // ary is actually int*
}

// Equivalent to -
void foo(const int* ptr) {...}

// Even if I give the size in the argument, it is ignored
// Follow is equivalent to above -
void foo(const int ary[5]) {...}
```

This means that I no longer have the length info available. E.g., if I do `std::ssize(ary)` I'll get a compile time error. I cannot do range based for loops inside the function. I should treat `ary` as a pointer inside the function. This is why it customary to pass the length of the array along with the array -

```c++
void foo(const int[] ary, int len) {...}
void foo(const int* ptr, int len) {...}
```

It is good practice to use the first style of function signature as it makes it clear that the function is working with an array, and not any old int pointer. The downside is that it hides the fact that the param is really just an int pointer.

### Pointer Arithmetic & Subscripting

See Pointers.md.

### Iterating

The most basic style of iterating where the size is known separately -

```c++
// ary :: int[3], n = 3
for (int i {0}; i < n; i++) {
  ary[i];
}
```

When the size is known, i.e., when the array has not decayed to a pointer, I can use range based for loops -

```c++
int main()
{
  int ary[] { 1, 2, 3 };
  for (auto x : ary) {
    x;
  }
}

void foo(int[] ary)
{
  // ERROR: will not work because length is not known
  for (auto x : ary) {
    x;
  }
}
```

I can create my own iterators using pointers -

```c++
int primes[] { 1, 2, 3, 5, 7 };
auto begin { primes };
auto end { begin + std::ssize(primes) };
for (auto it { begin }; it < end; it++) {
    std::cout << *it;
}

// Works with functions
int main()
{
  int primes[] { 1, 2, 3, 5, 7 };
  auto begin { primes };
  auto end { begin + std::ssize(primes) };
  foo(begin, end);
}

void foo(const int* begin, const int* end)
{
  for (auto it { begin }; it < end; it++) {
    *it;
  }
}
```

#### Iterating multidimensional arrays

Just have a range based for loop for each axis -

```c++
int matrix[][5] {
    { 1, 2 },
    { 3, 4, 5 },
    { 6, 7, 8, 9 }
};

for (const auto& row : matrix) {
    for (const auto& val : row) {
        val;
    }
}
```

