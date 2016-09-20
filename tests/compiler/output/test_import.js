var oc = {};
oc.instances = {};

oc.cloneObject = function(obj){
	var result = {};
	for (var key in obj) {
		if (obj.hasOwnProperty(key)) result[key] = obj[key];
	}
	return result;
}

if(oc.std == null){
  oc.std = {};
}
oc.std.ff = (function(){
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


  return {
    unit : unit,
    showInt : showInt,
    showDouble : showDouble,
    showString : showString,
    showChar : showChar,
    showBool : showBool,
    eqAny : eqAny,
    eqInt : eqInt,
    eqDouble : eqDouble,
    eqString : eqString,
    eqChar : eqChar,
    eqBool : eqBool,
    not : not,
    andBool : andBool,
    orBool : orBool,
    addInt : addInt,
    mulInt : mulInt,
    subInt : subInt,
    divInt : divInt,
    addDouble : addDouble,
    mulDouble : mulDouble,
    subDouble : subDouble,
    divDouble : divDouble,
    appendString : appendString,
    print : print
  }
})();
if(oc.std == null){
  oc.std = {};
}
oc.std.numbers = (function(){
  oc.instances.add = {};
  var add = function(x,p1,p2){
    return x(p1,p2);
  };
  oc.instances.zero = {};
  var zero = function(x){
    return x;
  };
  oc.instances.mul = {};
  var mul = function(x,p1,p2){
    return x(p1,p2);
  };
  oc.instances.one = {};
  var one = function(x){
    return x;
  };
  oc.instances.sub = {};
  var sub = function(x,p1,p2){
    return x(p1,p2);
  };
  oc.instances.div = {};
  var div = function(x,p1,p2){
    return x(p1,p2);
  };
  oc.instances.add.Int = oc.std.ff.addInt;
  oc.instances.mul.Int = oc.std.ff.mulInt;
  oc.instances.zero.Int = 0;
  oc.instances.one.Int = 1;
  oc.instances.sub.Int = oc.std.ff.subInt;
  oc.instances.div.Int = oc.std.ff.divInt;
  oc.instances.add.Double = oc.std.ff.addDouble;
  oc.instances.mul.Double = oc.std.ff.mulDouble;
  oc.instances.zero.Double = 0.0;
  oc.instances.one.Double = 1.0;
  oc.instances.sub.Double = oc.std.ff.subDouble;
  oc.instances.div.Double = oc.std.ff.divDouble;
  var negate = function(_nzero,_nsub,x){
    return _nsub(zero(_nzero),x);
  };
  return {
    add : add,
    zero : zero,
    mul : mul,
    one : one,
    sub : sub,
    div : div,
    negate : negate
  }
})();
oc.testModule = (function(){
  oc.instances.zero = {};
  var zero = function(x){
    return x;
  };
  var x = 1;
  return {
    zero : zero,
    x : x
  }
})();
oc.test_import = (function(){
  oc.instances.zero.Int = 0;
  return {}
})();
