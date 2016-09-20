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