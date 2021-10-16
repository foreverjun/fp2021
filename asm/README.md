## An implementaion of asm

This is a homework for functional programming course.

License: LGPL for implementation code + WTFPL for test examles in my asm

Author: Alina Shlegel, https://github.com/shlegelal

Features done (append only):

- Parser  (for example)
- interpreter of non-recursive functions (for example)
- ...

Features in progress (and TODOs):

- Interpreter of recursive functions is not yet ready  (for example)
- TODO: make pretty-printing less memory consuming (for example)
- ...

## How to work with repo 
// TODO: перевести на англ

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
/home/$(whoami)/.opam/4.13.1+flambda/bin/ocaml
```

- Установка `dune`

```shell 
opam install dune
```

Настройка плагина в `IntelliJ IDEA` инструкция
по [ссылке](https://giraud.github.io/reasonml-idea-plugin/docs/language-support/ocaml)

+ установка либ, которые предложет dune при запуске `dune build` в директории `/Lambda`
    + stdio
    + ocamlformat-rpc-lib
    + те, что в зависимостях в файле `asm.opam`

### Работа с `dune`

+ Собрать проект

```sh 
dune build 
``` 

в рабочей директории -> появится `_build`

+ Запуск

```sh 
dune exec ./demos/demoAO.exe
``` 

(любой файл из деректории `/demos`, но с расширением `.exe`)

+ Тесты

    - `inline tests` (в файлах `dune` включены `(inline_tests)`)

    - `cram tests` (в файлах `dune-project` включены `(cram enable)`)
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



