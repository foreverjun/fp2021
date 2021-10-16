## An implementaion of asm

This is a homework for functional programming course.

License: LGPL for implementation code + WTFPL for test examles in miniLanguage

Author: Alina Shlegel, https://github.com/shlegelal

Features done (append only):

- Parser  (for example)
- interpreter of non-recursive functions (for example)
- ...

Features in progress (and TODOs):

- Interpreter of recursive functions is not yet ready  (for example)
- TODO: make pretty-printing less memory consuming (for example)
- ...

##### Замечания по стилю кодирования

- Если merge request не проходит CI -- проверяться не будет
- Замечания должны быть откомментированы, иначе проверяться не будет.
    - Если исправлены, должны быть поменчены как "исправлены"
    - Если непонятны/некорректны, то это должно быть откомментировано соответствующим образом.

  Такие суровые ограничения вводятся, чтобы замечания не игнорировались.

- Имена типов и функций -- snake_case
- Имена типов модулей и модулей -- CamelCase
- Ворнинги должны быть пофикшены
- Не стесняйтесь писать `if ... then ... else` вместо `match ... with true -> .. | false -> ...`
- Не стесняйтесь писать гварды в мэтчинге, например

```ocaml
match ... with
| x when f x -> ...
| x          -> ...
| ...
```

вместо

```ocaml
match ... with
| x -> if f x then ... else ...
| ...
```

- Вместо `fun x y -> match y with` лучше писать короче: `fun x -> function`
- Используйте quoted string literals в тестах, чтобы не экранировать руками

```
─( 11:21:01 )─< command 1 >────────────────────────────
utop # {|
  int main () {
    return 0;
  }
  |};;
- : string = "\n  int main () {\n    return 0;\n  }\n  "
```

- Не надо писать

```ocaml
match ... with
| x ->
    Hashtbl.replace tbl key value |> fun () -> ...
```

Лучше

```ocaml
match ... with
| x ->
    let () = Hashtbl.replace tbl key value in
    ...
```

или

```ocaml
match ... with
| x -> (
    Hashtbl.replace tbl key value;
    ...
  )
```

или даже

```ocaml
match ... with
| x -> begin
    Hashtbl.replace tbl key value;
    ...
  end
```

- Не надо писать

```ocaml
let x = if long_expression then true else false in ...
```

лучше

```ocaml
let x = long_expression in ...
```

## Дополнительно

TODO: перевести не англ

### Здесь будут комментарии по работе с репозиторием

- Pазработка велась при помощи `IntelliJ IDEA` с
  плагином [`ReasonML`](https://plugins.jetbrains.com/plugin/9440-reasonml)
- Дистрибутив: `Fedora`

### Подготовка среды

- Установка и настройка [__opam__](https://ocaml.org/docs/install.html) (2.0.8):

```shell 
dnf install opam 
opam init
```

тут предложат сделать файл конфиг для окружения, я сделала (иначе придется постоянно писать `eval $(opam env)`)

- Установка [__OCaml__](https://ocaml.org/docs/install.html) (4.13.1) from [__releases__](https://ocaml.org/releases/)

```shell 
opam switch create 4.13.1+flambda --package=ocaml-variants.4.13.1+options,ocaml-option-flambda
opam install ocamlformat 
opam install ocamlformat-rpc-lib 
```

- Переключение switch и удаление дефолтного (или других)

```shell 
opam switch 4.13.1+flambda
opam switch remove default
```

тут попросят поменять конфиг файл, сделайте это

- Проверка

```shell 
opam switch
opam search ocamlformat
ocaml -version 
/home/alan/.opam/4.13.1+flambda/bin/ocaml
```

- Установка `dune`

```shell 
opam install dune
```

Настройка плагина в `IntelliJ IDEA` инструкция
по [ссылке](https://giraud.github.io/reasonml-idea-plugin/docs/language-support/ocaml)

+ установка либ, которые предложет dune при запуске `dune build` в директории `/Lambda`
    + stdio
    + те, что в зависимостях в файле `asm.opam`

### Работа с `dune`

+ Собрать проект

```sh 
dune build 
``` 

в рабочей директории -> появится `_build`

+ Запуск

```sh 
dune exec ./demoes/demoAO.exe
``` 

(любой файл из деректории `/demoes`, но с расширением `.exe`)

+ Тесты

    - `inline tests` (в файлах `dune` включены `(inline_tests))`)

    - `cram tests` (в файлах `dune-project` включены `(cram enable))`)
        - эти тесты находятся в `/demos` с расширением `.t`
        - они содержат строку вызова, например
      ```  
        $ (cd ../../../../default && demos/demoAO.exe) 
      ```
      (как при запуске + до `$` обязательно 2 пробела)

      далее должен идти ожидаемый вывод
        + Записать актуальный вывод вывод в файл теста как ожидаемый
      ```shell
      dune promote
      ```

  запускаются при помощи

    ```sh 
    dune runtest
    ```



