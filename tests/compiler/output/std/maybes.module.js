if(oc.std == null){
  oc.std = {};
}
oc.std.maybes = (function(){
  var none = {};
  oc.instances.eq.None = oc.std.ff.eqAny;
  var noneMaybe = none;
  var pureMaybe = function(x){
    return {
      just : x
    };
  };
  var maybe = function(_a1,_a2,_a3){
    if((function(x,f,_a){
      return oc.instances.eq.None(_a,none);
    })(_a1,_a2,_a3)){
      return (function(x,f,_a){
        return x;
      })(_a1,_a2,_a3);
    }else{
      if((function(x,f,z){
        return z.just !== undefined;
      })(_a1,_a2,_a3)){
        return (function(x,f,z){
          return f(z.just);
        })(_a1,_a2,_a3);
      }else{
        throw "cases error";
      }
    }
  };
  var isNone = function(x){
    return maybe(true,oc.std.prelude.cnst(false),x);
  };
  var isJust = function(x){
    return maybe(false,oc.std.prelude.cnst(true),x);
  };
  var fromMaybe = function(x,m){
    return maybe(x,oc.std.prelude.id,m);
  };
  var mapMaybe = function(f,x){
    return maybe(noneMaybe,function(y){
      return {
        just : f(y)
      };
    },x);
  };
  var bindMaybe = function(f,x){
    return maybe(none,f,x);
  };
  var applyMaybe = function(f,x){
    return maybe(none,function(y){
      return mapMaybe(y,x);
    },f);
  };
  var showMaybe = function(_cvshow,x){
    return maybe("none",function(p1){
      return oc.std.prelude.show(_cvshow,p1);
    },x);
  };
  var eqMaybe = function(_edeq,_a1,_a2){
    if((function(_b,_c){
      return (oc.instances.eq.None(_b,none) && oc.instances.eq.None(_c,none));
    })(_a1,_a2)){
      return (function(_b,_c){
        return true;
      })(_a1,_a2);
    }else{
      if((function(_d,y){
        return oc.instances.eq.None(_d,none);
      })(_a1,_a2)){
        return (function(_d,y){
          return false;
        })(_a1,_a2);
      }else{
        if((function(x,_e){
          return oc.instances.eq.None(_e,none);
        })(_a1,_a2)){
          return (function(x,_e){
            return false;
          })(_a1,_a2);
        }else{
          if((function(x,y){
            return (x.just
            !==
            undefined && y.just
            !==
            undefined);
          })(_a1,_a2)){
            return (function(x,y){
              return _edeq(x.just,y.just);
            })(_a1,_a2);
          }else{
            throw "cases error";
          }
        }
      }
    }
  };
  var foldlMaybe = function(f,i,x){
    return maybe(i,function(y){
      return f(i,y);
    },x);
  };
  oc.instances.show.Maybe = showMaybe;
  oc.instances.eq.Maybe = eqMaybe;
  oc.instances.foldl.Maybe = foldlMaybe;
  oc.instances.pure.Maybe = pureMaybe;
  oc.instances.map.Maybe = mapMaybe;
  oc.instances.apply.Maybe = applyMaybe;
  oc.instances.bind.Maybe = bindMaybe;
  oc.instances.mempty.Maybe = none;
  return {
    none : none,
    noneMaybe : noneMaybe,
    pureMaybe : pureMaybe,
    maybe : maybe,
    isNone : isNone,
    isJust : isJust,
    fromMaybe : fromMaybe,
    mapMaybe : mapMaybe,
    bindMaybe : bindMaybe,
    applyMaybe : applyMaybe,
    showMaybe : showMaybe,
    eqMaybe : eqMaybe,
    foldlMaybe : foldlMaybe
  }
})();