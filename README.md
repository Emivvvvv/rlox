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

> [!NOTE]
> The foundation of rlox-bytecode was inspired by [jeschkies's lox-rs implementation](https://github.com/jeschkies/lox-rs/blob/master/bytecode/).

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
