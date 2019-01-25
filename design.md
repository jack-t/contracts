```
type T < * // alias for *
type string < array[char, n]

fn at(str: string, n: int[0, str.length]) -> char {
	str[n] // built in
}

fn find(str: string, x: char) -> int[-1, str.length] {
	i = 0
	while(i < str.length) {
		if(at(str, i) == x) {
			return i
		}
		i = i + 1
	}
	return -1
}

```

Types:
The universal type is `*`. Any object, including some hypothetical nil, is a member of the set described by `*`.

In order to specify a type, you begin with `*` and then constrain it. An `int` is `*` constrained by membership in the integers. Floats, doubles, etc., are treated the same way.
Constraints are specified in brackets:
int[0, 5] is an integer on the interval (0, 5]
float[0.5, 1.0] is a float on the interval (0.5, 1.0]
array[char['a', 'e'], 5] is an array of 5 chars with values between a and e 

It's a dynamically typed language, but if you provide a type when you declaration a variable (e.g., in a paramter, optionally on locals), then it'll be checked whenever it's assigned.

## Structs

```
type string < array[char]

struct point {
	name: string
	x: float
	y: float
}

```

### Contracts
```
contract parity[value: bit](this: int) {
	return (parity == 0 && value % 2 == 0) || (parity == 1 && value % 2 != 0);
}

contract bounds[lower: int, upper: int](this: int) {
	return this >= lower && this < upper;
}

type special_int int|parity[got_parity]|bounds[0, 10]; // has a parity check (got_parity being the parity value you just read) and a bounds check
```

# Tokens

identifier
integer

## Keywords
type
int
float
char
array
struct
contract
fn
return
while
if

## Operators
+
-
*
/
%
==
!=
<
>
<=
>=
(
)

## Other Symbols
"
'
;
,
|
[
]