External executable call
  $ cat <<-"EOF" > another_script.sh
  > #!/bin/sh
  > echo "hello from another script"
  > EOF
  $ ./demoInterpret.exe <<-"EOF"
  > echo $( ./another_script.sh )
  > EOF
  hello from another script
  Interpretation finished with return code: 0

Interruption with Ctrl-C while running an external executable:
(1) An external script with an infinite loop is written into another_script.sh
(2) demoInterrupt.exe starts the script with an another_script.sh call in a forked process
and waits for any output on stderr (which will be available after the infinite loop is
started)
(3) Ctrl-C signal is sent to the script
(4) The another_script.sh call finishes and the whole script resumes its execution 
  $ cat <<-"EOF" > another_script.sh
  > #!/bin/sh
  > echo "[external script] starting an infinite loop..."
  > while (( 1 )); do echo "stderr message to signal the start of the loop" 1>&2; done
  > EOF
  $ ./demoInterrupt.exe <<-"EOF"
  > echo "[main script] about to call the external script"
  > echo $( ./another_script.sh )
  > echo "[main script] finishing"
  [main script] about to call the external script
  [external script] starting an infinite loop...
  [main script] finishing
  Interpretation finished with return code: 0

Pipelines and redirections:
(1) echo's output is redirected to the first cat's input
(2) the first cat prints its input to testfile
(3) the second cat gets nothing on its stdin as everything was printed to testfile
  $ ./demoInterpret.exe <<-"EOF"
  > echo printing | cat >testfile | cat
  > EOF
  Interpretation finished with return code: 0
The first echo call doesn't print to stdout but writes to testfile
  $ ./demoInterpret.exe <<-"EOF"
  > cat <testfile
  > echo appending >>testfile
  > EOF
  printing
  Interpretation finished with return code: 0
The second echo call doesn't print to stdout but appends to testfile
  $ ./demoInterpret.exe <<-"EOF"
  > cat <testfile
  > EOF
  printing
  appending
  Interpretation finished with return code: 0
cat receives an argument (which is not yet supported) and produces a error message which
is redirected from stderr to another_testfile (return code 1 is intentional)
  $ ./demoInterpret.exe <<-"EOF"
  > cat some_nonexistent_file >another_testfile 2>&1
  > EOF
  Interpretation finished with return code: 1
Another cat call shows the contents of another_testfile that were produced by the previous
cat call
  $ ./demoInterpret.exe <<-"EOF"
  > cat <another_testfile
  > EOF
  cat: some_nonexistent_file: No such file or directory
  Interpretation finished with return code: 0

Quoting
  $ ./demoInterpret.exe <<-"EOF"
  > echo "$( echo this should be a command substitution )"
  > echo "\$( echo this should be just a string in parentheses )"
  > echo '$( echo this should be just a string in parentheses too )'
  > 
  > echo -------------------
  > 
  > X="$((2 + 2))"
  > echo "X equals $X"
  > echo "X doesn't equal \$X"
  > echo 'X does not equal $X neither'
  > EOF
  this should be a command substitution
  $( echo this should be just a string in parentheses )
  $( echo this should be just a string in parentheses too )
  -------------------
  X equals 4
  X doesn't equal $X
  X does not equal $X neither
  Interpretation finished with return code: 0

Pipeline lists: with && the left side is computed only if the left side is true, with
|| the right side is computed only if the left side is false
  $ ./demoInterpret.exe <<-"EOF"
  > (( 2 > 3 )) && echo should not be printed
  > (( 2 > 3 )) || echo should be printed 1
  > 
  > (( 2 < 3 )) && echo should be printed 2
  > (( 2 < 3 )) || echo should not be printed
  > 
  > if (( 2 > 3 )) || (( 2 + 2 == 4 )) && (( 1 + 1 == 2 ))
  > then echo should be printed 3
  > else echo should not be printed
  > fi
  > EOF
  should be printed 1
  should be printed 2
  should be printed 3
  Interpretation finished with return code: 0

Compounds and pattern matching
  $ ./demoInterpret.exe <<-"EOF"
  > ABC=5
  > while (( ABC < 10)); do {
  >   echo ABC equals $ABC
  >   ABC=$(( ABC + 1 ))
  > }; done
  > 
  > echo -------------------
  > 
  > for (( i = 0; i < 3; i = i + 1 )); do {
  >   for j in a b; do {
  >     echo i is $i and j is $j
  >   }; done
  > }; done
  > 
  > echo -------------------
  > 
  > if (( ABC == 10 )) && (( i == 3 )) && (( j == b )); then echo yes; else echo no; fi
  > 
  > echo -------------------
  > 
  > case bash in
  >   sh | zch | fish) echo wrong shell;;
  >   ?sh) echo wrong as bash has two letters before sh;;
  >   [c-z]?sh) echo wrong as bash starts with b;;
  >   [a-c]?s*) echo right!;;
  >   *) echo wrong as the previous one was true;;
  > esac
  > EOF
  ABC equals 5
  ABC equals 6
  ABC equals 7
  ABC equals 8
  ABC equals 9
  -------------------
  i is 0 and j is a
  i is 0 and j is b
  i is 1 and j is a
  i is 1 and j is b
  i is 2 and j is a
  i is 2 and j is b
  -------------------
  yes
  -------------------
  right!
  Interpretation finished with return code: 0

Functions with parameters:
1) Recursive factorial computation
  $ ./demoInterpret.exe <<-"EOF"
  > factorial () {
  >   n=$1
  >   if (( n <= 1 )); then
  >     echo 1
  >   else {
  >     last=$( factorial $(( n - 1 )) )
  >     echo $(( n * last ))
  >   }; fi
  > }
  > 
  > factorial 5
  > EOF
  120
  Interpretation finished with return code: 0
2) Iterative fibonacci series computation
  $ ./demoInterpret.exe <<-"EOF"
  > frst=0
  > scnd=1
  > 
  > function fib {
  >   a=$frst
  >   b=$scnd
  >   n=$1
  >   
  >   echo $a
  >   
  >   for (( i = 1; i < n; i = i + 1 )); do {
  >     a=$(( a + b ))
  >     echo $a
  >     b=$(( a - b ))
  >   }; done
  > }
  > 
  > fib 10
  > EOF
  0
  1
  1
  2
  3
  5
  8
  13
  21
  34
  Interpretation finished with return code: 0

Variables
  $ ./demoInterpret.exe <<-"EOF"
  > X="first value"
  > echo $X
  > X="second value"
  > echo $X
  > X=(now it is "an array")
  > echo "the third value is" ${X[3]}
  > echo "the zero value is" $X
  > X[10]="let's assign it another value"
  > echo "and here it is:"  ${X[10]}
  > 
  > echo -------------------
  > 
  > echo_Y () { echo "Y equals \"$Y\"" }
  > 
  > Y="this will only be visible in this command call" echo_Y
  > echo "Y equals \"$Y\""
  > 
  > Z="and this won't be visible here" echo "Z equals \"$Z\""
  > 
  > EOF
  first value
  second value
  the third value is an array
  the zero value is now
  and here it is: let's assign it another value
  -------------------
  Y equals "this will only be visible in this command call"
  Y equals ""
  Z equals ""
  Interpretation finished with return code: 0

Expansions
  $ touch a_file_with_a_rare_name.abacab a_file_with_another_rare_name.abacab
  $ ./demoInterpret.exe <<-"EOF"
  > echo brace expansion 1: prefix{_1_,_2_,_3_}postfix
  > echo brace expansion 2: a{2..-3..2}b
  > echo brace expansion 3: {e..a..-2}sh
  > 
  > echo -------------------
  > 
  > X=something-long
  > echo parameter expansion: $X ${#X} ${X:2+2:10/2} ${X#s*e} ${X//?o/_} ...
  > 
  > echo -------------------
  > 
  > echo command substitution 1: $(if (( 1 == 1 )); then echo result 1; fi)
  > echo command substitution 2: `if (( 1 == 1 )); then echo result 2; fi`
  > 
  > echo -------------------
  > 
  > X=3
  > echo arithmetic expansion: $(( 9 * 5 - X ))
  > 
  > echo -------------------
  > 
  > echo filename expansion: a_file_w?th_a*_name.[a-c]ba?ab
  > EOF
  brace expansion 1: prefix_1_postfix prefix_2_postfix prefix_3_postfix
  brace expansion 2: a2b a0b a-2b
  brace expansion 3: esh csh ash
  -------------------
  parameter expansion: something-long 14 thing thing-long _mething-_ng ...
  -------------------
  command substitution 1: result 1
  command substitution 2: result 2
  -------------------
  arithmetic expansion: 42
  -------------------
  filename expansion: a_file_with_a_rare_name.abacab a_file_with_another_rare_name.abacab
  Interpretation finished with return code: 0

Arrays: indexed and associative
  $ ./demoInterpret.exe <<-"EOF"
  > IND_ARR=(1 2 abc)
  > echo ${IND_ARR[0]}
  > echo ${IND_ARR[1]}
  > echo ${IND_ARR[2]}
  > 
  > echo -------------------
  > 
  > ASSOC_ARR=(num=123 key=word)
  > echo ${ASSOC_ARR[num]}
  > echo ${ASSOC_ARR[key]}
  > 
  > echo -------------------
  > 
  > echo $(( ASSOC_ARR[num] + IND_ARR[1] ))
  > EOF
  1
  2
  abc
  -------------------
  123
  word
  -------------------
  125
  Interpretation finished with return code: 0
