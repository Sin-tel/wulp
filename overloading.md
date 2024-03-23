## operator overloading
Binary operators
| Operator | Function |
|----------|----------|
| `+`  | `__add`    |
| `-`  | `__sub`    |
| `*`  | `__mul`    |
| `/`  | `__div`    |
| `%`  | `__mod`    |
| `^`  | `__pow`    |
| `..` | `__concat` |

Binary comparison operators: (T, T) -> bool

| Operator | Function |
|----------|----------|
| `==` | `__eq` |
| `<`  | `__lt` |
| `<=` | `__le` |

Unary operator

| Operator | Function |
|----------|----------|
| `-`  | `__unm` |

notes:
* all binary operators currently assume that the two operands are of the same type
* everything implements `==`, by pointer comparison, so override it if it makes sense to do so
* we dont expose `__index`, `__newindex`, `__len`, `__call`
* the boolean operators `not`, `and`, `or` are not overloadable
* you can not implement `!=`, `>` or `>=`: `a != b` is equivalent to `not (a == b)` and the comparisons just get swapped
