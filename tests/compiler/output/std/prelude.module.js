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