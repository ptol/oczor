if(oc.std == null){
  oc.std = {};
}
oc.std.foldable = (function(){
  oc.instances.foldl = {};
  var foldl = function(x,p1,p2,p3){
    return x(p1,p2,p3);
  };
  var any = function(_qfoldl,f,l){
    return _qfoldl(function(a,e){
      return (a || f(e));
    },false,l);
  };
  var all = function(_aefoldl,f,l){
    return _aefoldl(function(a,e){
      return (a && f(e));
    },true,l);
  };
  var or = function(_amfoldl,l){
    return any(_amfoldl,oc.std.prelude.id,l);
  };
  var and = function(_aufoldl,l){
    return all(_aufoldl,oc.std.prelude.id,l);
  };
  var sum = function(_azfoldl,_bmzero,_bmadd,l){
    return _azfoldl(_bmadd,oc.std.numbers.zero(_bmzero),l);
  };
  var product = function(_brfoldl,_ceone,_cemul,l){
    return _brfoldl(_cemul,oc.std.numbers.one(_ceone),l);
  };
  var contains = function(_cteq,_cufoldl,x,l){
    return any(_cufoldl,function(y){
      return _cteq(x,y);
    },l);
  };
  var count = function(_czfoldl,l){
    return _czfoldl(function(a,e){
      return (a + 1);
    },0,l);
  };
  var foldMap = function(_dymempty,_dyappend,_dlfoldl,f,l){
    return _dlfoldl(function(a,e){
      return _dyappend(a,f(e));
    },oc.std.prelude.mempty(_dymempty),l);
  };
  var concat = function(_ecfoldl,_ehmempty,_ehappend,l){
    return foldMap(_ehmempty,_ehappend,_ecfoldl,oc.std.prelude.id,l);
  };
  return {
    foldl : foldl,
    any : any,
    all : all,
    or : or,
    and : and,
    sum : sum,
    product : product,
    contains : contains,
    count : count,
    foldMap : foldMap,
    concat : concat
  }
})();