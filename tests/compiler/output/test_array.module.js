oc.test_array = (function(){
  var arr = [1,2,3];
  return {
    arr : arr,
    item1 : oc.testing.checkEq(function(p1,p2){
      return oc.instances.eq.Array(oc.instances.eq.Int,p1,p2);
    },arr,[1,2,3]),
    item2 : oc.testing.checkEq(function(p1,p2){
      return oc.instances.eq.Array(oc.instances.eq.Int,p1,p2);
    },oc.instances.pure.Array(1),[1]),
    item3 : oc.testing.checkEq(function(p1,p2){
      return oc.instances.eq.Array(oc.instances.eq.Int,p1,p2);
    },oc.instances.append.Array([1,2],[3,4]),[1,2,3,4]),
    item4 : oc.testing.checkEq(function(p1,p2){
      return oc.instances.eq.Array(oc.instances.eq.Int,p1,p2);
    },oc.instances.map.Array(function(x){
      return (x + 1);
    },arr),[2,3,4]),
    item5 : oc.testing.checkEq(oc.instances.eq.Int,oc.instances.foldl.Array(function(a,e){
      return (a + e);
    },0,arr),6),
    item6 : oc.testing.checkEq(oc.instances.eq.Int,oc.std.arrays.arrayLength(arr),3)
  }
})();