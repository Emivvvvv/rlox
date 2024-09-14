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

### rlox-ast benchmark

```shell
sh run_benchmark.sh
binary_trees.lox,16.811022996902466
equality.lox,6.28954291343689
fib.lox,66.90174412727356
instantiation.lox,4.425313949584961
invocation.lox,2.363626003265381
method_call.lox,1.3553078174591064
properties.lox,3.2026171684265137
```

# rlox-bytecode roadmap
Rust implementation of the clox interpreter from the third chapter of "Crafting Interpreters".
<br>

> [!NOTE]
> The foundation of rclox was inspired by [jeschkies's lox-rs implementation](https://github.com/jeschkies/lox-rs/blob/master/bytecode/).

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
