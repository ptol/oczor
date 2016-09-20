if(oc.std == null){
  oc.std = {};
}
oc.std.lists = (function(){
  var emptyList = {};
  oc.instances.eq.EmptyList = oc.std.ff.eqAny;
  var pureList = function(x){
    return {
      item1 : x,
      item2 : emptyList
    };
  };
  var range = function(_a1,_a2){
    if((function(x,y){
      return (x === y);
    })(_a1,_a2)){
      return (function(x,y){
        return pureList(x);
      })(_a1,_a2);
    }else{
      return (function(x,y){
        var z = range((x + 1),y);
        return {
          item1 : x,
          item2 : z
        };
      })(_a1,_a2);
    }
  };
  var ifEmpty = function(def,f,l){
    return (function(_a1){
      if((function(_a){
        return oc.instances.eq.EmptyList(_a,emptyList);
      })(_a1)){
        return (function(_a){
          return def;
        })(_a1);
      }else{
        return f(_a1);
      }
    })(l);
  };
  var isEmpty = function(l){
    return ifEmpty(true,function(x){
      return false;
    },l);
  };
  var length = function(_a1){
    if((function(_b){
      return oc.instances.eq.EmptyList(_b,emptyList);
    })(_a1)){
      return (function(_b){
        return 0;
      })(_a1);
    }else{
      if((function(_c){
        return (_c.item1
        !==
        undefined && _c.item2
        !==
        undefined);
      })(_a1)){
        return (function(_c){
          var h = _c.item1;
          var t = _c.item2;
          return (length(t) + 1);
        })(_a1);
      }else{
        throw "cases error";
      }
    }
  };
  var eqList = function(_dieq,_a1,_a2){
    if((function(_d,_e){
      return (oc.instances.eq.EmptyList(_d,emptyList) && oc.instances.eq.EmptyList(_e,emptyList));
    })(_a1,_a2)){
      return (function(_d,_e){
        return true;
      })(_a1,_a2);
    }else{
      if((function(_f,y){
        return oc.instances.eq.EmptyList(_f,emptyList);
      })(_a1,_a2)){
        return (function(_f,y){
          return false;
        })(_a1,_a2);
      }else{
        if((function(x,_g){
          return oc.instances.eq.EmptyList(_g,emptyList);
        })(_a1,_a2)){
          return (function(x,_g){
            return false;
          })(_a1,_a2);
        }else{
          if((function(_h,_i){
            return (_h.item1
            !==
            undefined && _h.item2
            !==
            undefined && _i.item1
            !==
            undefined && _i.item2
            !==
            undefined);
          })(_a1,_a2)){
            return (function(_h,_i){
              var h1 = _h.item1;
              var t1 = _h.item2;
              var h2 = _i.item1;
              var t2 = _i.item2;
              return (_dieq(h1,h2) && eqList(_dieq,t1,t2));
            })(_a1,_a2);
          }else{
            throw "cases error";
          }
        }
      }
    }
  };
  var appendList = function(_a1,_a2){
    if((function(_j,y){
      return oc.instances.eq.EmptyList(_j,emptyList);
    })(_a1,_a2)){
      return (function(_j,y){
        return y;
      })(_a1,_a2);
    }else{
      if((function(_k,y){
        return (_k.item1
        !==
        undefined && _k.item2
        !==
        undefined);
      })(_a1,_a2)){
        return (function(_k,y){
          var h = _k.item1;
          var t = _k.item2;
          return {
            item1 : h,
            item2 : appendList(t,y)
          };
        })(_a1,_a2);
      }else{
        throw "cases error";
      }
    }
  };
  var foldlList = function(f,i,l){
    return ifEmpty(i,function(h,t){
      return foldlList(f,f(i,h),t);
    },l);
  };
  var mapList = function(f,l){
    return ifEmpty(emptyList,function(h,t){
      return {
        item1 : f(h),
        item2 : mapList(f,t)
      };
    },l);
  };
  oc.instances.eq.List = eqList;
  return {
    emptyList : emptyList,
    pureList : pureList,
    range : range,
    ifEmpty : ifEmpty,
    isEmpty : isEmpty,
    length : length,
    eqList : eqList,
    appendList : appendList,
    foldlList : foldlList,
    mapList : mapList
  }
})();