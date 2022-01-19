Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

  $ ./demoBFS.exe
  answer
  0
  1
  1
  2

  $ ./demoIteratorMap.exe
  Initial collection
  1
  2
  3
  4
  5
  IteratorMap-ed collection
  1
  4
  9
  16
  25

  $ ./demoFactorial.exe
  Factorial of number 5:
  120
  120
  120
  120

  $ ./demoFlowSensitiveTyping.exe
  |-> test flow_sensitive_typing1
  Test failed: (Typing (ExpressionExpectedToBeNotNullable (VarIdentifier "nullVariable")))
  |-> test flow_sensitive_typing2
  value of notNullVariable
  1
  |-> test flow_sensitive_typing3
  Test failed: (Typing (ExpressionExpectedToBeNotNullable (VarIdentifier "nullVariable")))
  |-> test flow_sensitive_typing4
  120
  |-> test LOOP
  1
  |-> test plus via many +1
  13

  $ ./demoInvalidProgramParse.exe
  Test failed: (Parser InvalidProgram)
