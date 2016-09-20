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
if(oc.std == null){
  oc.std = {};
}
oc.std.prelude = (function(){
  var flip = function(f){
    return function(a,b){
      return f(b,a);
    };
  };
  var cnst = function(x){
    return function(y){
      return x;
    };
  };
  var id = function(x){
    return x;
  };
  var fst = function(x,y){
    return x;
  };
  var snd = function(x,y){
    return y;
  };
  oc.instances.show = {};
  var show = function(x,p1){
    return x(p1);
  };
  oc.instances.eq = {};
  var eq = function(x,p1,p2){
    return x(p1,p2);
  };
  oc.instances.mempty = {};
  var mempty = function(x){
    return x;
  };
  oc.instances.append = {};
  var append = function(x,p1,p2){
    return x(p1,p2);
  };
  oc.instances.map = {};
  var map = function(x,p1,p2){
    return x(p1,p2);
  };
  oc.instances.apply = {};
  var apply = function(x,p1,p2){
    return x(p1,p2);
  };
  oc.instances.pure = {};
  var pure = function(x,p1){
    return x(p1);
  };
  oc.instances.bind = {};
  var bind = function(x,p1,p2){
    return x(p1,p2);
  };
  oc.instances.traverse = {};
  var traverse = function(x,p1,p2){
    return x(p1,p2);
  };
  oc.instances.show.Int = oc.std.ff.showInt;
  oc.instances.show.Double = oc.std.ff.showDouble;
  oc.instances.show.String = oc.std.ff.showString;
  oc.instances.show.Char = oc.std.ff.showChar;
  oc.instances.show.Bool = oc.std.ff.showBool;
  oc.instances.eq.Int = oc.std.ff.eqInt;
  oc.instances.eq.Double = oc.std.ff.eqDouble;
  oc.instances.eq.String = oc.std.ff.eqString;
  oc.instances.eq.Char = oc.std.ff.eqChar;
  oc.instances.eq.Bool = oc.std.ff.eqBool;
  oc.instances.mempty.String = "";
  oc.instances.append.String = oc.std.ff.appendString;
  return {
    flip : flip,
    cnst : cnst,
    id : id,
    fst : fst,
    snd : snd,
    show : show,
    eq : eq,
    mempty : mempty,
    append : append,
    map : map,
    apply : apply,
    pure : pure,
    bind : bind,
    traverse : traverse
  }
})();
oc.testing = (function(){
  var check = function(b){
    return oc.std.ff.print((b ? "Pass" : "FAIL!!!"));
  };
  var checkEq = function(_neq,x,y){
    return check(_neq(x,y));
  };
  return {
    check : check,
    checkEq : checkEq
  }
})();
oc.test_numbers = {
  item1 : oc.testing.checkEq(oc.instances.eq.Bool,(1 === 2),false),
  item2 : oc.testing.checkEq(oc.instances.eq.Int,((1 + 2) * 3),9),
  item3 : oc.testing.checkEq(oc.instances.eq.Int,(1 + 2),3),
  item4 : oc.testing.checkEq(oc.instances.eq.Int,(2 * 3),6),
  item5 : oc.testing.checkEq(oc.instances.eq.Int,(9 - 5),4),
  item6 : oc.testing.checkEq(oc.instances.eq.Int,oc.instances.div.Int(12,4),3),
  item7 : oc.testing.checkEq(oc.instances.eq.Double,(1.0 + 2.0),3.0),
  item8 : oc.testing.checkEq(oc.instances.eq.Double,(2.0 * 3.0),6.0),
  item9 : oc.testing.checkEq(oc.instances.eq.Double,(9.0 - 5.0),4.0),
  item10 : oc.testing.checkEq(oc.instances.eq.Double,(12.0 / 4.0),3.0)
};
