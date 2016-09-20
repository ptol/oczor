function print(x){ console.log(x); }

function addAny(x,y){ return x + y; }

function mulAny(x,y){ return x * y; }

function subAny(x,y){ return x - y; }

function eqAny(x,y){ return x === y; }

function showAny(x){ return x + ""; }

var unit = {}
var emptyObject = {}

var eqInt = eqAny;
var eqDouble = eqAny;
var eqString = eqAny;
var eqChar = eqAny;
var eqBool = eqAny;

var showInt = showAny;
var showDouble = showAny;
var showString = showAny;
var showChar = showAny;
var showBool = showAny;

var addInt = addAny
var mulInt = mulAny
var subInt = subAny
var divInt = function(x,y){return x / y | 0;}

var addDouble = addAny
var mulDouble = mulAny
var subDouble = subAny
var divDouble = function(x,y){return x / y;}

var addBool = function(x,y){return x || y}
var mulBool = function(x,y){return x && y}

var not = function(x){return !x}
var orBool = function(x,y){return x || y}
var andBool = function(x,y){return x && y}

var appendString = addAny

