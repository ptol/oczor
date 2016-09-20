oc.test_syntax = (function(){
  var testCases = function(_a1){
    if((function(x){
      return oc.std.ff.eqInt(x,1);
    })(_a1)){
      return (function(x){
        return 2;
      })(_a1);
    }else{
      if((function(x){
        return oc.std.ff.eqInt(x,2);
      })(_a1)){
        return (function(x){
          return 3;
        })(_a1);
      }else{
        if((function(x){
          return oc.std.ff.eqInt(x,3);
        })(_a1)){
          return (function(x){
            return 4;
          })(_a1);
        }else{
          throw "cases error";
        }
      }
    }
  };
  var x = {
    foo : 1
  };
  var y = (function(){
    var _obj = x;
    var _clone = oc.cloneObject(_obj);
    _clone.foo = 2;
    return _clone
  })();
  return {
    testCases : testCases,
    item1 : oc.testing.checkEq(oc.instances.eq.Int,testCases(2),3),
    x : x,
    y : y,
    item2 : oc.testing.checkEq(oc.instances.eq.Int,y.foo,2)
  }
})();