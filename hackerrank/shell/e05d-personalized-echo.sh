#
# A Personalized Echo
# ===================
#
# https://www.hackerrank.com/challenges/bash-tutorials---a-personalized-echo
#
#
# Running
# -------
#
#     bash script.sh <<<'Master Yoda'
#

sed -r 's/(.*)/Welcome \1/g'
# → Welcome Master Yoda

