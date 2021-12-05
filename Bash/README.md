### An implementation of Bash mini-language

This is a homework for functional programming course.

License: LGPL

Author: Timofey Pushkin, pushkin.td@gmail.com

Features done (append only):

- None

Features in progress (and TODOs):

- Quotes (`'`, `"` and escape character `\ `)
- Commands
    - Simple commands
    - Pipelines
    - Command lists
    - Compound commands
        - `while`
        - `for` (two syntactic variants)
        - `if`, `case`
        - `((...))`
- Functions (two syntactic variants; with recursion)
- Variables and parameters (without positional and special parameters)
- Expansions
    - Brace expansion
    - Shell Parameter Expansion
    - Command Substitution (with backtick version)
    - Word Splitting (only on <space>, <tab>, <newline>)
    - Filename Expansion
    - Quote Removal
- Pattern Matching
- Redirections (`<`, `>`, `>>`, `<&`, `>&`, no custom descriptors)
- Arrays (indexed and associative as `name=(key1 value1 key2 value2 … )`)
