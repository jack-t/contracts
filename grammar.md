Expression Grammar
```
expression : bexpression [= expression];
bexpression : nbexpression [binary_op (?b)expression];
nbexpression : [unary_op] (lit | ( expression ) | id | id( [expression [,]] ))
```