# The goal #
A simple statically typed language that should be easily compiled to dynamically typed languages
# Occam's Razor #

> Entities should not be multiplied unnecessarily.
>
> -- <cite>William of Ockham</cite>

For example, many languages has these entites
* variables
	```
	var foo = 1;
	var bar = 2;
	```
* parameters
	```
	function (foo,bar) { return foo + bar; }
	```
* objects
	```
	{foo : 1, bar : 2}
	```
* tuples
	```
	(1,2)
	```
Oczor uses records instead of all these entities

# Example of generated JavaScript code #

<table> 
<thead><tr><th>Oczor</th><th>JavaScript</th></tr></thead>
<tbody> <tr> <td><pre lang='haskell'><code>

x1 = 1

x2 = 
  foo = 1
  bar = 1


func x y = x + y + 1



x3 = func 1 2

func2 x = 
  temp = x == 1
  in not (temp && false)


</code></pre></td><td ><pre lang='javascript'><code>
  var x1 = 1;

  var x2 = {
    foo : 1,
    bar : 1
  };

  var func = function(x,y){
    return x + y + 1;
  };

  var x3 = func(1,2);

  var func2 = function(x){
    var temp = x === 1;
    return !(temp && false);
  };
</code></pre></td></tr></tbody></table>


# Other languages #

* [Lua](https://github.com/ptol/oczor/wiki/Languages#lua)
* [Ruby](https://github.com/ptol/oczor/wiki/Languages#ruby)
* [Emacs Lisp](https://github.com/ptol/oczor/wiki/Languages#emacs-lisp)

# Syntax #
Most syntax constructions have two options - comma or indent separation

<table>
<thead><tr><th>Comma</th><th>Indentation</th></tr></thead>
<tbody> <tr> <td ><pre lang='haskell'><code>
x = (foo = 1, bar = 2)



type Cat = name : String, age : Int




</code></pre></td><td ><pre lang='haskell'><code>
x = 
  foo = 1
  bar = 2

type Cat = 
  name : String
  age : Int
  
</code></pre></td></tr></tbody></table>


# Records #

```haskell
x = (foo = 1, bar = 2)
```

#### labels ####

```haskell
print x.foo
print x.bar
```

#### record update ####

```haskell
y = x with (foo = 2)
```

#### tuples ####

Tuple is a record with labels `itemN`

```haskell
x = (2,3)
```
same as

```haskell
x = (item1 = 2, item2 = 3)
```
Destructuring assignment
```haskell
x = (2,3)
(y,z) = x
print y
```
```haskell
x = (foo = 1, bar = 2)
 
(foo = y, bar = z) = x
print y
```

# Functions #

#### anonymouse ####

```haskell
x y => (x,y) or \x y => (x,y) -- \ is optional
```

#### a definition ####

a function definition is a syntax sugar for label with anonymouse function
```haskell
foo x y = (x,y)
```
same as
```haskell
foo = \x y => (x,y)
```
```haskell
z = foo 1 2
```
For example, it's possible to group functions like this
```haskell
bar = 
  foo x y = (x,y)
  id z = z
```
```haskell
bar.foo 1 2
bar.id 1
```

#### Pattern matching ####

Parameters can be matched with literals, tuples, records or external variables

```haskell
x = 1
foo = case
  1 => 2
  (x,2) => 3
  (foo = x, bar = 3) => 4
  ^x => 5
```

#### partial application ####

```haskell
inc : Int => Int
inc = add _ 1

x = inc 1
```

# Foreign function interface #

```javascript
function eqAny(x,y) {
  return x === y;
}
```

```haskell
ffi eqAny : a, a => Bool
```

# Operators #

```haskell
infix == 4 eqAny
x = if 1 == 2 then 1 else 2 
```

  
# Types #

## Record Type ##

```haskell
type Cat = 
  name : String
  age : Int
```
```haskell
cat = (name = "Tiger", age = 25)
```
## Union Type ##

```haskell
type Number = Int | Double

x : Array Number
x = [1, 2.0, 3]
```




# Ad-hoc polymorphism  #

```haskell
ffi eqInt : Int, Int => Bool
ffi eqString : String, String => Bool

class equal a : a => Bool

instance Int equal x y = eqInt x y
instance String equal x y = eqString x y

infix == 4 equal

x = 1 == 1
y = "foo" == "foo"
```
# Development state #
Alpha/Experimental

# Installation #

1. Install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

2. clone or download this repository

3. In project directory run
	```
	stack setup
	stack install
	```
	`stack install` will install the occ executable into ~/.local/bin, which you should add to your PATH. 

# How to use #

1. create a directory `hello`
2. copy `std` directory from `libs` directory
3. create a file `hello.oc`

	```
	import std
	print "hi"
	```
4. run `occ` to compile `hello.oc`

	```
	occ hello.oc
	```
	`hello.js` should be in `output` directory
5. `hello.js` can be run with node

	```
	node output/hello.js
	```
	should print `hi`
6. `occ --help` to display the compiler options

