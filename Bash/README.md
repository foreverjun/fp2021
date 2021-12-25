### An implementation of Bash mini-language

This is a homework for a functional programming course.

License: LGPL

Author: Timofey Pushkin, pushkin.td@gmail.com

Features done (append only):

- AST
- Parser

Features in progress (and TODOs):

- Commands
    - Simple commands
    - Pipelines
    - Command lists
    - Compound commands
        - `while`
        - `for` (two syntactic variants, without positional parameters)
        - `if`, `case`
        - `((...))`
- Functions (with recursion)
    - Because of `{ ... }` not being supported, function body may contain a simple command
- Variables and parameters (out of positional and special parameters only numerical positional parameters are supported)
- Expansions (expansions are not allowed to be nested)
    - Brace expansion
    - Shell Parameter Expansion
    - Command Substitution (`$(...)` form) only
    - Word Splitting (partial, on <space>, <tab>, <newline> only)
    - Arithmetical expansion (with variables)
    - Filename Expansion
- Pattern Matching
- Redirections (`<`, `>`, `>>`, `<&`, `>&`, no descriptors from variables)
- Arrays (indexed and associative in `name=(key1 value1 key2 value2 … )` form)
