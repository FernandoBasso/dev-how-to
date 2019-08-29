
function usage () {
  name=${0##*/}

cat << EOF
This program takes two numbers as argument.

Example:

  $name 5 10
EOF
}

if [[ $# < 2 ]] ; then
  usage
fi

function sum () {
  printf '%12s: %4d\n' 'sum' $(( $1 + $2 ))
}

function difference () {
  printf '%12s: %4d\n' 'difference' $(( $1 - $2 ))
}

function product () {
  printf '%12s: %4d\n' 'product' $(( $1 * $2 ))
}

function quotient () {
  printf '%12s: %4d\n' 'quotient' $(( $1 / $2 ))
}

function remainder () {
  printf '%12s: %4d\n' 'remainder' $(( $1 % $2 ))
}

sum $1 $2
difference $1 $2
product $1 $2
quotient $1 $2
remainder $1 $2


