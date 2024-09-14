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
```

|        File         |      Took (s)      |
|:-------------------:|:------------------:|
|  binary_trees.lox   | 11.058765888214111 |
|    equality.lox     | 5.993005990982056  |
|       fib.lox       | 66.72664785385132  |
|  instantiation.lox  | 4.426839828491211  |
|   invocation.lox    | 2.3550169467926025 |
|   method_call.lox   | 1.3441059589385986 |
|   properties.lox    | 3.2077040672302246 |
| string_equality.lox | 4.504413843154907  |
|      trees.lox      | 15.673010110855103 |
|       zoo.lox       | 2.396976947784424  |

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
|    Scanning on Demand    |   ✅    |
|  Compiling Expressions   |   ✅    |
|     Types of Values      |   ✅    |
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
