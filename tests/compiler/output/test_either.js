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
if(oc.std == null){
  oc.std = {};
}
oc.std.eithers = (function(){
  var pureEither = function(x){
    return {
      right : x
    };
  };
  var either = function(fl,fr,m){
    return (function(_a1){
      if((function(_a){
        return _a.left !== undefined;
      })(_a1)){
        return (function(_a){
          var x = _a.left;
          return fl(x);
        })(_a1);
      }else{
        if((function(_b){
          return _b.right !== undefined;
        })(_a1)){
          return (function(_b){
            var x = _b.right;
            return fr(x);
          })(_a1);
        }else{
          throw "cases error";
        }
      }
    })(m);
  };
  var isLeft = function(x){
    return either(oc.std.prelude.cnst(true),oc.std.prelude.cnst(false),x);
  };
  var isRight = function(x){
    return either(oc.std.prelude.cnst(false),oc.std.prelude.cnst(true),x);
  };
  var fromEither = function(x,m){
    return either(oc.std.prelude.cnst(x),oc.std.prelude.id,m);
  };
  var mapEither = function(f,z){
    return (function(_a1){
      if((function(_c){
        return _c.left !== undefined;
      })(_a1)){
        return (function(_c){
          var x = _c.left;
          return {
            left : x
          };
        })(_a1);
      }else{
        if((function(_d){
          return _d.right !== undefined;
        })(_a1)){
          return (function(_d){
            var x = _d.right;
            return {
              right : f(x)
            };
          })(_a1);
        }else{
          throw "cases error";
        }
      }
    })(z);
  };
  var bindEither = function(f,z){
    return (function(_a1){
      if((function(_e){
        return _e.left !== undefined;
      })(_a1)){
        return (function(_e){
          var x = _e.left;
          return {
            left : x
          };
        })(_a1);
      }else{
        if((function(_f){
          return _f.right !== undefined;
        })(_a1)){
          return (function(_f){
            var x = _f.right;
            return f(x);
          })(_a1);
        }else{
          throw "cases error";
        }
      }
    })(z);
  };
  var showEither = function(_doshow,_dpshow,x){
    return either(function(p1){
      return oc.std.prelude.show(_doshow,p1);
    },function(p1){
      return oc.std.prelude.show(_dpshow,p1);
    },x);
  };
  var eqEither = function(_exeq,_eyeq,_a1,_a2){
    if((function(_g,_h){
      return (_g.left
      !==
      undefined && _h.left
      !==
      undefined);
    })(_a1,_a2)){
      return (function(_g,_h){
        var x = _g.left;
        var y = _h.left;
        return _exeq(x,y);
      })(_a1,_a2);
    }else{
      if((function(_i,_j){
        return (_i.right
        !==
        undefined && _j.right
        !==
        undefined);
      })(_a1,_a2)){
        return (function(_i,_j){
          var x = _i.right;
          var y = _j.right;
          return _eyeq(x,y);
        })(_a1,_a2);
      }else{
        return (function(_k,_l){
          return false;
        })(_a1,_a2);
      }
    }
  };
  oc.instances.pure.Either = pureEither;
  oc.instances.map.Either = mapEither;
  oc.instances.bind.Either = bindEither;
  oc.instances.show.Either = showEither;
  oc.instances.eq.Either = eqEither;
  return {
    pureEither : pureEither,
    either : either,
    isLeft : isLeft,
    isRight : isRight,
    fromEither : fromEither,
    mapEither : mapEither,
    bindEither : bindEither,
    showEither : showEither,
    eqEither : eqEither
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
oc.test_either = (function(){
  var m = oc.instances.pure.Either(2);
  var m2 = oc.instances.pure.Either(3);
  var ml = {
    left : "foo"
  };
  return {
    m : m,
    m2 : m2,
    ml : ml,
    item1 : oc.testing.checkEq(oc.instances.eq.Int,oc.std.eithers.fromEither(3,m),2),
    item2 : oc.testing.checkEq(oc.instances.eq.Int,oc.std.eithers.fromEither(3,ml),3),
    item3 : oc.testing.checkEq(oc.instances.eq.Bool,oc.std.eithers.isLeft(ml),true),
    item4 : oc.testing.checkEq(oc.instances.eq.Bool,oc.std.eithers.isRight(m),true),
    item5 : oc.testing.checkEq(function(p1,p2){
      return oc.instances.eq.Either(oc.instances.eq.String,oc.instances.eq.Int,p1,p2);
    },oc.instances.map.Either(function(x){
      return (x + 1);
    },m),m2),
    item6 : oc.testing.checkEq(function(p1,p2){
      return oc.instances.eq.Either(oc.instances.eq.String,oc.instances.eq.Int,p1,p2);
    },m,m),
    item7 : oc.testing.checkEq(function(p1,p2){
      return oc.instances.eq.Either(oc.instances.eq.String,oc.instances.eq.Int,p1,p2);
    },ml,ml),
    item8 : oc.testing.checkEq(oc.instances.eq.Bool,(function(p1,p2){
      return oc.instances.eq.Either(oc.instances.eq.String,oc.instances.eq.Int,p1,p2);
    })(m,m2),false),
    item9 : oc.testing.checkEq(oc.instances.eq.String,(function(p1){
      return oc.instances.show.Either(oc.instances.show.String,oc.instances.show.Int,p1);
    })(ml),"foo")
  }
})();
