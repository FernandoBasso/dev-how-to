#
# http://mywiki.wooledge.org/BashFAQ/001
#

#
# help read tells me it "reads a single line from stdin". So, even using
# IFS=$'\n' I would not be able to do something like IFS=$'\n' read x y
# <<<$'10\n20'
#
# mapfile -t array <<< $'10\n20\n30'; declare -p array
#

in="${1:-/dev/stdin}"


read x <&0
read y <&0

printf '%d\n' $(( $x + $y ))
printf '%d\n' $(( $x - $y ))
printf '%d\n' $(( $x * $y ))
printf '%d\n' $(( $x / $y ))
