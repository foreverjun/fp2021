### An implementation of Bash mini-language

This is a homework for a functional programming course.

License: LGPL

Author: Timofey Pushkin, pushkin.td@gmail.com

Features done (for AST, parser and interpreter):

- Quoting
- Commands
    - Simple commands
    - Pipelines
    - Command lists (`&&` and `||`)
    - Compound commands
        - `while`
        - `for` (two syntactic variants)
        - `if`
        - `case`
        - `((...))`
        - `{...}`
- Functions (with recursion)
- Variables and parameters
    - Out of positional and special parameters only numerical positional parameters are supported
- Expansions (expansions are not allowed to be nested)
    - Brace expansion
    - Shell Parameter Expansion
    - Command Substitution (both forms behave in the new-fashioned way)
    - Word Splitting (partial, on <space>, <tab>, <newline> only)
    - Arithmetical expansion (with variables)
    - Filename Expansion
- Pattern Matching
- Redirections (`<`, `>`, `>>`, `<&`, `>&`)
    - No checking for input/output availability
    - No descriptors from variables
- Arrays
    - Indexed: `name=(value1 value2 value3 ...)`
    - Associative: `name=(key1=value1 key2=value2 ...)`
- External executables
