### An implementaion of Lambda mini-language

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

Features in progress (and TODOs):

- Flow-sensitive typing: проверка может ли значение быть null-ом
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