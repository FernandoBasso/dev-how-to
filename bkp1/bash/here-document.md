# Bash Here Documents

## Here Document To Variable

Assign two lines to the variable `input`.

```shell-session
$ read -r -d '' input << 'EOF'
> int j = [int]3.45;
> [ 22 + 5 ] * 0.5;
> EOF

$ echo "$input"
int j = [int]3.45;
[ 22 + 5 ] * 0.5;
```

