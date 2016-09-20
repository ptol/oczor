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