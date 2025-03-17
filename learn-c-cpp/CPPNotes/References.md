# References

## Lvalue References

### Basic Facts

References can be both const i.e., immutable and modifiable. Immutable references are more versatile than modifiable ones. Here is the basic syntax immutable refs -

```c++
// x :: int, y :: const int
const int& ref;  // ERROR: refs have to be initialized upon declaration
const int& cr1 { x };  // can refer to modifiable int
const int& cr { y };  // can also refer to const int
const int& cr2 { cr1 };  // cr1 is substitued by x, so cr2 is just a reference to x
int y { cr1 };  // again cr1 is substitued by x, so x is just copied to y
const int& cr3 { 10 };  // rvalues work, see temporary anonymous section below
const double& cr4 { 10 };  // rvalues of compatible types are also ok
const double& cr5 { x }; // ERROR: cannot reference lvalues of different types
```

Here is what I can and cannot do with modifiable refs -

```c++
// x :: int, y :: const int
int& ref;  // ERROR
int& r1 { x };  // can only refer to a modifiable int
int& r { y };  // ERROR: cannot have a modifiable ref to a const
int& r2 { r1 };
int y { r1 };
int& r3 { 10 }; // ERROR: cannot have a modifiable ref to an rvalue
double& r4 { 10 };  // ERROR: cannot have a modifiable ref to different types
const double& r5 { x };  //ERROR
```

Once a reference has been successfully created, it is said that it **bind**s to a concrete lvalue. In the below example, `ref` is bound to `x`. It can now be used anywhere the concrete object could be used. And if the reference is modifiable, then it can also change the value of its concrete lvalue.

```c++
int x { 10 };
int& ref { x };
cout << ref << endl;  // Will print 10
ref = 20;
cout << x << " " << ref << endl;  // Will print 20 20
```

One thing to be careful about is that references cannot be reseated. In the code below, the last line can be mistakenly be assumed to have ref reference y, but that is not possible. ref will now always reference x. That line just serves to change the value of x to whatever is in y.

```c++
int x { 10 };
int& ref { x };  // ref refers to x
int y { 20 };
ref = y;  // x got assigned the value of y, i.e., 20.
```

If a const ref references a modifiable lvalue, then we cannot change the value via the ref, but the lvalue can change its own value.

```c++
int x { 10 };
const int& ref { x };
ref = 20;  // ERROR: ref is const
x = 20;  // OK: Now both ref and x have the value 20
```

### Temporary Anonymous

Lets consider the `const int& ref { 10 }` invocation. Here is what happens -

1. A temporary anonymous const int lvalue is created and its value is set to 10.

   ```c++
   const int __anon = 10;
   ```

2. `ref` then references this anonymous lvalue.

   ```c++
   const int& ref { __anon };
   ```

Now this temporary anonymous variable has the same lifetime as `ref`. I think the temporary anonymous variable has to be const because there is no direct way of changing it. This is why it can only be referenced by an immutable reference. Also see [c++ - Why is it illegal to take the address of an rvalue temporary? - Stack Overflow](https://stackoverflow.com/questions/8763398/why-is-it-illegal-to-take-the-address-of-an-rvalue-temporary/) discussion.

### Reference Passing

> Unless there is a specific need to, prefer using const by-ref params over anything else.

A reference parameter means that when the function called, the argument does not have to be copied onto the functions's stack. It can be referenced directly from the function's stack while living on the caller's stack. The caller calls the function with the same syntax it would've used for a copy-by-value parameter.

```c++
void bake(Cookie& cookie) {}

Cookie c;
bake(c);  // same syntax as copy-by-value
```

In the code above, `cookie` references `c`. And because it is a modifiable reference, `bake` can make changes to it. But a modifiable reference parameter has the same limitations as a modifiable reference, i.e., it cannot accept a const argument, or a literal rvalue argument.

```c++
int calcDistance(stirng& src, string& dst) {}

string ap { "Avilay" };
string akp { "Anika" };
int distance = calcDistance(ap, akp);  // OK

const string mks { "Manjit" };
int distance = calcDistance(ap, mks);  // ERROR: dst is a modifiable lvalue ref

int distance = calcDistance("Avilay", "avilay");  // ERROR: dst cannot accept rvalues
```

All of this can be mitigated if we use const by-ref params.

```c++
void print(const int& ref) {}

int x { 5 };
print(x);  // OK

print(5);  // OK

const int y { 6 };
print(y);  // OK

print(5.5);  // OK
```

By-ref params can also be used as "out" or "in/out" params. In general avoid doing this, it makes the code very hard to reason about later.

### Returning References

* The only good reason for returning a modifiable reference is to return one of the by-ref input params. See Returning By-Ref Input Params section for examples.
* The only good reason for returning a const reference is when returning a reference to an immutable static variable. See Returning Static Reference for examples.

A "normal" function that returns a concrete object by value, evaluates to an rvalue.

```c++
Cookie bake() {}

Cookie cookie { bake() };
```

Here calling `bake()` evaluates to an rvalue and the move ctor will be called.

However, a function that returns an lvalue ref does **not** evaluate to an rvalue. To confirm this try to get the address `&bake()` is legit code that will compile and return a (hopefully) valid address.

```c++
Cookie& bake() {}

Cookie cookie { bake() };
```

Here `bake()` invocation evaluates to an lvalue ref. This is similar to the `int y { ref }` statement.

```
Cookie cookie { ref :: Cookie& }
```

Then `ref` is substitued by the concrete lvalue -

```
Cookie cookie { bakedCookie :: Cookie }
```

And now the copy ctor is called to initialize `cookie` object.

I can accept the return of bake into another ref -

```c++
Cookie& cookie { bake() };
```

This is same as `int& ref1 { ref2 }`, where `cookie` references the same concrete lvalue that `bake()` references.

### Dangling References

In all of the following cases the function will return a dangling reference. In case of modifiable ref types, we get a compiler warning, in case of const ref types we only get a warning. But the code that calls these functions will not work as expected.

```c++
// error: non-const lvalue reference to type 'basic_string<...>' cannot bind to a temporary of type 'basic_string<...>'
string& whoami()
{
  string user { "aptg" };
  return user;
}

// error: non-const lvalue reference to type 'string' (aka 'basic_string<char>') cannot bind to a value of unrelated type 'const char[5]'
string& whoami()
{
  return "aptg";  // 
}

// error: non-const lvalue reference to type 'string' (aka 'basic_string<char>') cannot bind to an initializer list temporary
string& whoami()
{
  string& user { "aptg" };  // Cannot use rvalue to initialize modifiable lvalue ref
  return user;
}
```

Even though the following code compiles fine, it is still returning a dangling reference.

```c++
// warning: reference to stack memory associated with local variable 'user' returned
const string& whoami()
{
  string user { "aptg" };
  return user;
}

// warning: returning reference to local temporary object
const string& whoami()
{
  return "aptg"; 
}

// warning: returning reference to local temporary object
const string& whoami()
{
  const string& user { "aptg" };
  return user;
}
```

#### Returning Static References

It is ok to return a reference to an immutable static variable because it will still be available when the function ends.

```c++
const string& getProgramName()
{
  static const string programName { "C++" };
  return programName;
}

string name { getProgramName() };  // OK: name is just a ref to the static variable programName
```

However, care must be taken to ensure that the concrete object is immutable, i.e., const. Otherwise strange bugs will start creeping in.

```c++
const int& getNextId()
{
  static int id { 0 };
  id += 1;
  return id;
}
const int& x { getNextId() };  // x = 1
const int& y { getNextId() };  // y = 2 and x = 2
```

Both `x` and `y` are lvalue refs to `id`. Even though both `x` and `y` are consts, `id` isn't and changes its value from underneath `x` and `y`. But they still refelect the latest value of `id` at all times. This can be mitigated if `x` and `y` are concrete ints. Here the lvalue ref returned by `getNextId` is implicitly converted to an rvalue and then copied into `x`. Now `x` holds a copy of the static variable `id`. Even if `id` changes, `x` will not. But this is not very sensible, because the right thing to do would've been for `getNextId` to return a copy in the first place.

```c++
int x { getNextId() };  // x = 1, but the value was copied into the concrete int
int y { getNextId() };  // y = 2, but x does not change because it is a copy
```

#### Returning By-Ref Input Params

Lets consider this example -

```c++
const string& foo(const string& ref)
{
    return ref;
}

int main()
{
	string name { foo("aptg") };  // name = "aptg"
} 
```

Here is what happens -

1. In main scope, "aptg" is put in an anon temp variable.

```
[main] string __anon { "aptg" };
```

2. In main/foo(?) scope, a reference to `__anon` is created using the perfectly legit and "first-principle" way of creating a reference `int& ref { x }`.

```
[main] const string& __ref { __anon };
```

3. This reference is passed to `foo`, now `ref` is also referencing `__anon`.

```
[foo] const string& ref { __ref };
```

4. `foo` returns this same reference.

```
[main] string name { __ref };
```

5. This is still a reference to `__anon`, which is still in scope.

```
[main] string name { __anon };
```

6. The copy ctor is called to instantiate `name`.

Because these functions return lvalue references, I can actually modify the value of the referenced concrete lvalue in weird ways. Consider -

```c++
int& max(int& x, int& y)
{
    return (x > y) ? x : y;
}

int a { 5 };
int b { 6 };
max(a, b) = 7;  // b will be assigned the value 7
```

## Rvalue References

Here is how to initialize rvalue references. Recall that usually rvalues don't exist beyond the expression in which they are defined, but in this case the rvalue will live as long as its reference `rref` is alive.

```c++
int&& rref { 5 };
```

I can also change it (weird) but that is because the above triggers the creation of a temporary object that is holding the value. And in the code below I am just changing the value held by this temp variable.

```c++
rref = 10;
```

Here is the really weired part, the symbol `rref` is an **lvalue** of type `int&&`, i.e., it holds a reference to an rvalue, but its own value catgory is an lvalue 🤯. It is important to keep in mind that an object's value category is different from its type.

| symbol          | value category | type    |
| --------------- | -------------- | ------- |
| `5`             | rvalue         | `int`   |
| `x :: int`      | lvalue         | `int`   |
| `ref :: int&`   | lvalue         | `int&`  |
| `rref :: int&&` | lvalue         | `int&&` |



Now ordinarily the following function would work for both rvalues and lvalues because it takes a const lvalue ref.

```c++
void fun(const int& ref) {...}

int x { 5 };
fun(x);
fun(5);
```

As opposed to `void fun(int& ref)` which would've only worked for the lvalue and not the literal rvalue. However, if I define another overload of this function that explicitly takes in rvalues then the rvalue invocation will go there.

```c++
void fun(const int& ref)
{
  std::cout << "lvalue overload: " << ref << std::endl;
}

void fun(int&& rref)
{
  std::cout << "rvalue overload: " << rref << std::endl;
}

int x { 5 };
fun(x);  // invokes the lvalue overload
fun(5);  // invokes the rvalue overload

int&& rref { 5 };
fun(rref);  // invokes the **lvalue** overload
```

But based on the previous discussion, if I have `int&& rref { 5 }` and I call `fun(rref)`, it will go to the lvalue overload. This is because even though `rref` is a reference to an rvalue, it itself is an lvalue. The functions take in refrences to either lvalue or rvalue. In the second function, an rvalue reference cannot bind to an lvalue like `rref`.

There is never a good reason to return an rvalue reference from a function.

If I have a function that expects an rvalue but I only have the lvalue, then I can use `std::move` from `<utility>` header to cast the lvalue into an rvalue.

