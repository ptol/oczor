oc.test_list = (function(){
  var l = oc.std.lists.range(1,3);
  return {
    l : l,
    item1 : oc.testing.checkEq(oc.instances.eq.Int,oc.std.lists.length(l),3),
    item2 : oc.testing.checkEq(function(p1,p2){
      return oc.instances.eq.List(oc.instances.eq.Int,p1,p2);
    },l,l)
  }
})();