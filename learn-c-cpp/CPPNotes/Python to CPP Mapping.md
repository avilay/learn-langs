### Python

```
all :: [bool] -> bool
any :: [bool] -> bool
filter :: (a -> bool) -> [a] -> [a]
takewhile :: (a -> bool) -> [a] -> [a]
dropwhile :: (a -> bool) -> [a] -> [a]
compress :: [a] -> [bool] -> [a]
groupby :: [a] -> (a -> b) -> [(b, [a])]
product :: [a] -> [b] -> [c] -> [(a, b, c)]  
-- can have n
accumulate :: [a] -> (b -> a -> b) -> b -> [b]
reduce :: (b -> a -> b) -> [a] -> b -> b 
-- similar to foldl :: (b -> a -> b) ->b -> [a]  -> b
map :: (a -> b -> c) -> [a] -> [b] -> [c] --> can have n
-- (simplified) map :: (a -> b) -> [a] -> [b]
reversed :: [a] -> [a]
sorted :: [a] -> (a -> b) -> bool -> [a]
zip :: [a] -> [b] -> bool -> [(a, b)]
pairwise :: [a] -> [(a, a)]
```



In C++ where I am saying `[a]` I mean I have to give the start and end iterator. It is just more convenient to write it as a `[a]` because even in Python it represents the iterator and not an actual list.


| Python                   | C++                                                         |
| ------------------------ | ----------------------------------------------------------- |
| `all :: [bool] -> bool`  | `all_of :: [a] -> (a -> bool) -> bool`                      |
| `any :: [bool ] -> bool` | `any_of :: [a] -> (a -> bool) -> bool`                      |
|                          | `none_of :: [a] -> (a -> bool) -> bool`                     |
|                          | `find :: [a] -> a -> a`                                     |
|                          | `find_if :: [a] -> (a -> bool) -> a`                        |
|                          | `find_if_not`, `find_end`, `find_first_of`, `adjacent_find` |
|                          | `count :: [a] -> a -> int`                                  |
|                          | `count_if :: [a] -> (a -> bool) -> int`                     |
|                          |                                                             |

