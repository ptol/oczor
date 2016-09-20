oc.test_maybe = (function(){
  var m = oc.instances.pure.Maybe(2);
  var m2 = oc.instances.pure.Maybe(3);
  var testNone = oc.std.maybes.none;
  var maybeBindFunc = function(_a1){
    if((function(x){
      return oc.std.ff.eqInt(x,1);
    })(_a1)){
      return (function(x){
        return oc.std.maybes.none;
      })(_a1);
    }else{
      if((function(x){
        return oc.std.ff.eqInt(x,2);
      })(_a1)){
        return (function(x){
          return {
            just : 3
          };
        })(_a1);
      }else{
        throw "cases error";
      }
    }
  };
  return {
    m : m,
    m2 : m2,
    testNone : testNone,
    maybeBindFunc : maybeBindFunc,
    item1 : oc.testing.checkEq(oc.instances.eq.Bool,oc.std.maybes.isJust(m),true),
    item2 : oc.testing.checkEq(oc.instances.eq.Bool,oc.std.maybes.isNone(testNone),true),
    item3 : oc.testing.check((oc.std.maybes.maybe(1,oc.std.prelude.cnst(2),oc.std.maybes.none) === 1)),
    item4 : oc.testing.check((oc.std.maybes.maybe(1,function(x){
      return (x + 1);
    },m2) === 4)),
    item5 : oc.testing.check((oc.std.maybes.fromMaybe(0,oc.std.maybes.none) === 0)),
    item6 : oc.testing.check((oc.std.maybes.fromMaybe(0,oc.std.maybes.pureMaybe(2)) === 2)),
    item7 : oc.testing.check((oc.std.maybes.fromMaybe(0,m) === 2)),
    item8 : oc.testing.check((oc.std.maybes.fromMaybe(0,oc.instances.map.Maybe(function(x){
      return (x + 1);
    },m)) === 3)),
    item9 : oc.testing.check((oc.std.maybes.fromMaybe(0,oc.instances.bind.Maybe(maybeBindFunc,m)) === 3)),
    item10 : oc.testing.check((oc.instances.foldl.Maybe(function(a,e){
      return (a + e);
    },1,m) === 3)),
    item11 : oc.testing.check((oc.std.maybes.showMaybe(oc.instances.show.Int,testNone) === "none")),
    item12 : oc.testing.check(((function(p1){
      return oc.instances.show.Maybe(oc.instances.show.Int,p1);
    })(m) === "2")),
    item13 : oc.testing.check((function(p1,p2){
      return oc.instances.eq.Maybe(oc.instances.eq.Int,p1,p2);
    })(m,m))
  }
})();