oc.test_code = (function(){
  var x1 = 1;
  var x2 = {
    foo : 1,
    bar : 1
  };
  var func = function(x,y){
    return ((x + y) + 1);
  };
  var x3 = func(1,2);
  var func2 = function(x){
    var temp = (x === 1);
    return (! (temp && false));
  };
  return {
    x1 : x1,
    x2 : x2,
    func : func,
    x3 : x3,
    func2 : func2,
    temp1 : x1,
    temp2 : x2,
    temp3 : x3
  }
})();