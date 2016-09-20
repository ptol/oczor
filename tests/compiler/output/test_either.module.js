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