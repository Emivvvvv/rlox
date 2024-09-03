# rlox

Rust implementations of the jlox and clox interpreters from the book [Crafting Interpreters](https://craftinginterpreters.com/).

> jlox is a tree-walk interpreter written in Java and clox is a bytecode interpreter written in C.

# rlox-ast roadmap
Rust implementation of the jlox interpreter from the second chapter of "Crafting Interpreters".
<br>

|        Chapter         | Status |
|:----------------------:|:------:|
|        Scanning        |   ✅    |
|   Representing Code    |   ✅    |
|  Parsing Expressions   |   ✅    |
| Evaluating Expressions |   ✅    |
|  Statements and State  |   ✅    |
|      Control Flow      |   ✅    |
|       Functions        |   ✅    |
| Resolving and Binding  |   ✅    |
|        Classes         |   ✅    |
|      Inheritance       |   ✅    |

# rlox-bytecode roadmap
Rust implementation of the clox interpreter from the third chapter of "Crafting Interpreters".
<br>

> [!IMPORTANT]
> The foundation of rclox was inspired by this [implementation](https://github.com/jeschkies/lox-rs/blob/master/bytecode/).
> This implementation is more idiomatic that rjlox, and has some unsafe code to enhance performance.

<br>

|         Chapter          | Status |
|:------------------------:|:------:|
|    Chunks of Bytecode    |   ✅    |
|    A Virtual Machine     |   ✅    |
|    Scanning on Demand    |   ⏳    |
|  Compiling Expressions   |   ⏳    |
|     Types of Values      |   ⏳    |
|         Strings          |   ⏳    |
|       Hash Tables        |   ⏳    |
|     Global Variables     |   ⏳    |
|     Local Variables      |   ⏳    |
|  Jumping Back and Forth  |   ⏳    |
|   Calls and Functions    |   ⏳    |
|         Closures         |   ⏳    |
|    Garbage Collection    |   ⏳    |
|  Classes and Instances   |   ⏳    |
| Methods and Initializers |   ⏳    |
|       Superclasses       |   ⏳    |
|       Optimization       |   ⏳    |

# benchmark

The benchmark was conducted on a Macbook M1 Pro, while plugged in and with the fans running at maximum capacity.

```lox
fun fib(n) {
  if (n < 2) return n;
  return fib(n - 1) + fib(n - 2);
}

var before = clock();
print fib(40);
var after = clock();
print after - before;
```

rlox-ast: 
```shell
cargo run --release -- test.rlox
    Finished `release` profile [optimized] target(s) in 0.06s
     Running `target/release/rlox-ast test.rlox`
102334155
101.38370704650879
```
rlox-bytecode:
```shell
STILL ON DEVELOPMENT.
```
