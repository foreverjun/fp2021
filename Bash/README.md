### An implementation of Bash mini-language

This is a homework for functional programming course.

License: LGPL

Author: Timofey Pushkin, pushkin.td@gmail.com

Features done (append only):

- None

Features in progress (and TODOs):

- Кавычки
    - Escape character `\ `
    - Одинарные
    - Двойные
    - **Нужны ли** [ANSI-C Quoting](https://www.gnu.org/software/bash/manual/bash.html#ANSI_002dC-Quoting) и [Locale-Specific Translation](https://www.gnu.org/software/bash/manual/bash.html#Locale-Translation)?
- Комментарии
- Команды
    - Простые команды
    - Пайплайны
        - **Нужна ли** [поддержка](https://www.gnu.org/software/bash/manual/bash.html#Pipelines) `[time [-p]]` и `[!]`?
    - Листы команд
        - **Нужна ли** [поддержка](https://www.gnu.org/software/bash/manual/bash.html#Lists) `&` и subshell'ы вообще?
    - [Compound команды](https://www.gnu.org/software/bash/manual/bash.html#Compound-Commands)
        - `until`, `while`
        - `for` (возможно, два варианта синтаксиса)
            - **Нужно ли** поддерживать вариант `for (( expr1 ; expr2 ; expr3 )) ; do commands ; done`?
        - `if`, `case`
            - **Нужна ли** [поддержка](https://www.gnu.org/software/bash/manual/bash.html#index-select) `select`?
        - `((...))`
            - Влечет за собой поддержку [Shell Arithmetic](https://www.gnu.org/software/bash/manual/bash.html#Shell-Arithmetic)
                - **Нужна ли** поддержка не-десятичных систем счисления?
        - `[[...]]`
            - Влечет за собой поддержку [Conditional Expressions](https://www.gnu.org/software/bash/manual/bash.html#Bash-Conditional-Expressions)
                - **Можно ли** поддерживать лишь "C локаль"?
        - `{}`
            - **Нужна ли** [поддержка](https://www.gnu.org/software/bash/manual/bash.html#Command-Grouping) `()`? Если да, то нужны и subshell'ы
        - **Нужна ли** [поддержка](https://www.gnu.org/software/bash/manual/bash.html#Coprocesses) `coproc`? Если да, то нужны и subshell'ы
- Функции (два варианта синтаксиса)
    - С рекурсией
- Параметры
    - Переменные
        - С поддержкой `+=`
    - **Нужна ли** [поддержка positional parameters](https://www.gnu.org/software/bash/manual/bash.html#Positional-Parameters)? Нужно будет думать, откуда их брать
    - **Нужна ли** [поддержка special parameters](https://www.gnu.org/software/bash/manual/bash.html#Special-Parameters)?
        - `*`, `@`, `#` зависят от ответа про positional parameters
        - `-` вряд ли, так как требуется разбираться с флагами
        - `?`, `$`, `0` более-менее понятны
        - `!` вряд ли, так как требуется разбираться с [Job Control](https://www.gnu.org/software/bash/manual/bash.html#Job-Control)
- Expansion'ы *(выглядит как самая страшная часть)*
    - Brace expansion
    - **Нужно ли** [поддерживать Tilde expansion](https://www.gnu.org/software/bash/manual/bash.html#Tilde-Expansion)?
        - **Если да, то** только простую форму или кастомные логины, `~+`, `~-`, `~N` и т.д. — тоже?
    - Shell Parameter Expansion
        - Много разных вариантов, **буду ориентироваться на [это](https://www.thegeekstuff.com/2010/07/bash-string-manipulation/)** из предыдущего года ([в мануале](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion) вариантов ещё больше)
    - Command Substitution
        - **Нужно ли** [поддерживать](https://www.gnu.org/software/bash/manual/bash.html#Command-Substitution) ` `` `?
        - Видимо, нужны subshell'ы
        - *Если правильно понимаю эту подстановку, то её нужно будет как-то рекуррентно парсить и интерпретировать*
    - Arithmetic Expansion
    - **Нужно ли** [поддерживать Process Substitution](https://www.gnu.org/software/bash/manual/bash.html#Process-Substitution)? В оригинальном Bash оно поддерживается не на всех системах
    - Word Splitting
        - Используется `$IFS` — нужно будет подумать, откуда брать
    - Filename Expansion
    - Quote Removal
- [Pattern Matching](https://www.gnu.org/software/bash/manual/bash.html#Pattern-Matching)
    - **Нужно ли** поддерживать `**` как особый случай?
    - **Нужно ли** поддерживать особые случаи в `[...]`?
        - **Можно ли** при `-` использовать лишь стандартную "C локаль"?
        - **Нужно ли** поддерживать `[:class:]`, `[=c=]`, `[.symbol.]`?
    - **Нужно ли** поддерживать Composite patterns?
- [Перенаправления](https://www.gnu.org/software/bash/manual/bash.html#Redirections)
    - `<`, `>`, `>>`
    - **Нужно ли** поддерживать кастомные дескрипторы?
        - **Если да, то нужно ли** поддерживать замену кастомных дескрипторов переменными?
        - **Если да, то нужно ли** поддерживать [дублирования](https://www.gnu.org/software/bash/manual/bash.html#Duplicating-File-Descriptors) и [перемещения](https://www.gnu.org/software/bash/manual/bash.html#Moving-File-Descriptors) дескрипторов?
    - **Нужно ли** [поддерживать разницу между](https://www.gnu.org/software/bash/manual/bash.html#Redirecting-Output) `>` и `>|`?
    - **Нужно ли** [поддерживать](https://www.gnu.org/software/bash/manual/bash.html#Redirecting-Standard-Output-and-Standard-Error) `>&` или `&>`? По мануалу, последнее предпочтительнее, но первое принадлежит к более широкому классу "дублирований", о которых выше
    - **Нужно ли** поддерживать [Here Documents](https://www.gnu.org/software/bash/manual/bash.html#Here-Documents) и [Here Strings](https://www.gnu.org/software/bash/manual/bash.html#Here-Strings)?
    - **Нужно ли** [поддерживать](https://www.gnu.org/software/bash/manual/bash.html#Opening-File-Descriptors-for-Reading-and-Writing) `<>`?
- **Нужно ли** [поддерживать](https://www.gnu.org/software/bash/manual/bash.html#Shell-Scripts) `#!`? Есть в этом смысл, раз подразумевается лишь Bash?
- [Массивы](https://www.gnu.org/software/bash/manual/bash.html#Arrays)
    - Индексируемые
    - **Нужно ли** поддерживать ассоциативные?
        - **Если да, то нужно ли** поддерживать оба способа compound assignments (`name=(key1 value1 key2 value2 … )` и `name=( [key1]=value1 [key2]=value2 … )`)?
    - **Нужно ли** поддерживать отрицательные индексы (индексы "с конца")?
    - **Нужно ли** поддерживать `*` и `@` как индексы массивов?
