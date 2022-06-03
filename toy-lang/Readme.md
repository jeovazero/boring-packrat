# Toy-Lang

## Version ZERO

Example:

```
f x = x
g a b = a + b
fat n = fat' n 1
fat' 1 acc = acc
fat' n acc = fat (n - 1) (n * acc)
expr a b = g 1 b - a * f 8

run (fat 5 + expr 4 9)
```

1. Tail call recursion

```
fat' 1 acc = acc
fat' n acc = fat (n - 1) (n * acc)
```

2. Light Pattern Matching

```
g 1 = 2
g 2 = 4
g 3 = 6
g x = 99999
```

3. Arithmetic 

```
add a b = a + b
mult a b = a * b
sub a b = a - b
expr a b c d = add (sub a b) (mult c d)
```

