### An implementaion of Kotlin mini-language

This is a homework for functional programming course.

License: LGPL for implementation code + WTFPL for test examles in miniLanguage

Author: Trefilov Stepan, trefilov.stepan@yandex.ru

Features done (append only):

- AST
- Parser
- Тесты на парсер, интерпретатор и примеры программ в demos.ml
- Стандартные конструкции потока управления: ветвление if ... else, цикл while
- Примитивные типы: целые числа, строки, булевы значения, null
- Поддержка функций (в том числе рекурсивных)
- ООП: классы, публичные\приватные\protected методы\поля
- ООП: наследование от open классов
- REPL
- Flow-sensitive typing: проверка может ли значение быть null-ом
    * Запрет делать присваивания null в типы, не подразумевающие хранения null
    <pre>
    Kotlin REPL
    Type @help for getting further information
    >> val x: Int = null;;
    Kotlin REPL # (Utils.ExpectedToBeNotNull (Const NullValue))
    </pre>
    * Запрет использовать nullable типы в качестве их обычных аналогов
    <pre>
    Kotlin REPL
    Type @help for getting further information
    >> fun fact(n: Int): Int {
    >>      if(n > 1) return n * fact(n - 1)
    >>      else return 1
    >> };;
    Kotlin REPL # &lt;REPL empty answer&gt;
    >> val nullableVariable: Int? = 1;;
    Kotlin REPL # &lt;REPL empty answer&gt;
    >> fact(nullableVariable);;
    Kotlin REPL # (Utils.ExpectedToBeNotNull (VarIdentifier "nullableVariable"))
    </pre>
    * Можно проверить nullable переменную на null (внутри if), и если его там нет, использовать переменную nullable типа в качестве not-nullable
    <pre>
    Kotlin REPL
    Type @help for getting further information
    >> fun fact(n: Int): Int {
    >>      if(n > 1) return n * fact(n - 1)
    >>      else return 1
    >> };;
    Kotlin REPL # &lt;REPL empty answer&gt;
    >> val notNullVariable: Int? = 1;;
    Kotlin REPL # &lt;REPL empty answer&gt;
    >> if(notNullVariable != null) println(fact(notNullVariable));;
    1
    </pre>

Features in progress (and TODOs):

- Строгая типизация анонимных функций
- Upcasting объектов
- Pretty-printer

Пример работы с REPL:
<pre>
Kotlin REPL
Type @help for getting further information
>> fun fact(n: Int): Int { 
>>      if(n > 1) return n * fact(n - 1)
>>      else return 1
>> };;
Kotlin REPL # &lt;REPL empty answer&gt;
>> fact(5);;
Kotlin REPL # IntValue (120)
</pre>
