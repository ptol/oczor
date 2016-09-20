print = -> (x) {puts x}

addAny = -> (x,y) {x + y}

mulAny = -> (x,y) {x * y}

subAny = -> (x,y) {x - y}
divAny = -> (x,y) {x / y}

eqAny = -> (x,y) {x == y}

showAny = -> (x) {x.to_s}

unit = {}
emptyObject = {}

eqInt = eqAny
eqDouble = eqAny
eqString = eqAny
eqChar = eqAny
eqBool = eqAny

showInt = showAny
showDouble = showAny
showString = showAny
showChar = showAny
showBool = showAny

addInt = addAny
mulInt = mulAny
subInt = subAny
divInt = divAny

addDouble = addAny
mulDouble = mulAny
subDouble = subAny
divDouble = divAny

addBool = -> (x,y) {x || y}
mulBool = -> (x,y) {x && y}

_not = -> (x) {!x}
orBool = -> (x,y) {x || y}
andBool = -> (x,y) {x && y}

appendString = addAny
