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
