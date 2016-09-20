oc = {}
oc[:instances] = {}

oc[:cloneObject] = -> (obj) do
	result = {};
	obj.each {|key, value|  
	  result[key] = value
	}
	result;
end

class UniqObject 
end

if oc[:std] == nil
  oc[:std] = {}
end
oc[:std][:ff] = (->() do
  print = -> (x) {puts x}

addAny = -> (x,y) {x + y}

mulAny = -> (x,y) {x * y}

subAny = -> (x,y) {x - y}
divAny = -> (x,y) {x / y}

eqAny = -> (x,y) {x == y}

showAny = -> (x) {x.to_s}

unit = {}
emptyObject = {}

eqInt = eqAny
eqDouble = eqAny
eqString = eqAny
eqChar = eqAny
eqBool = eqAny

showInt = showAny
showDouble = showAny
showString = showAny
showChar = showAny
showBool = showAny

addInt = addAny
mulInt = mulAny
subInt = subAny
divInt = divAny

addDouble = addAny
mulDouble = mulAny
subDouble = subAny
divDouble = divAny

addBool = -> (x,y) {x || y}
mulBool = -> (x,y) {x && y}

_not = -> (x) {!x}
orBool = -> (x,y) {x || y}
andBool = -> (x,y) {x && y}

appendString = addAny

  return {
    :unit => unit,
    :showInt => showInt,
    :showDouble => showDouble,
    :showString => showString,
    :showChar => showChar,
    :showBool => showBool,
    :eqAny => eqAny,
    :eqInt => eqInt,
    :eqDouble => eqDouble,
    :eqString => eqString,
    :eqChar => eqChar,
    :eqBool => eqBool,
    :_not => _not,
    :andBool => andBool,
    :orBool => orBool,
    :addInt => addInt,
    :mulInt => mulInt,
    :subInt => subInt,
    :divInt => divInt,
    :addDouble => addDouble,
    :mulDouble => mulDouble,
    :subDouble => subDouble,
    :divDouble => divDouble,
    :appendString => appendString,
    :print => print
  }
end).()
if oc[:std] == nil
  oc[:std] = {}
end
oc[:std][:numbers] = (->() do
  oc[:instances][:add] = {}
  add = ->(x,p1,p2) do
    return x.(p1, p2)
  end
  oc[:instances][:zero] = {}
  zero = ->(x) do
    return x
  end
  oc[:instances][:mul] = {}
  mul = ->(x,p1,p2) do
    return x.(p1, p2)
  end
  oc[:instances][:one] = {}
  one = ->(x) do
    return x
  end
  oc[:instances][:sub] = {}
  sub = ->(x,p1,p2) do
    return x.(p1, p2)
  end
  oc[:instances][:div] = {}
  div = ->(x,p1,p2) do
    return x.(p1, p2)
  end
  oc[:instances][:add][:Int] = oc[:std][:ff][:addInt]
  oc[:instances][:mul][:Int] = oc[:std][:ff][:mulInt]
  oc[:instances][:zero][:Int] = 0
  oc[:instances][:one][:Int] = 1
  oc[:instances][:sub][:Int] = oc[:std][:ff][:subInt]
  oc[:instances][:div][:Int] = oc[:std][:ff][:divInt]
  oc[:instances][:add][:Double] = oc[:std][:ff][:addDouble]
  oc[:instances][:mul][:Double] = oc[:std][:ff][:mulDouble]
  oc[:instances][:zero][:Double] = 0.0
  oc[:instances][:one][:Double] = 1.0
  oc[:instances][:sub][:Double] = oc[:std][:ff][:subDouble]
  oc[:instances][:div][:Double] = oc[:std][:ff][:divDouble]
  negate = ->(_nzero,_nsub,x) do
    return _nsub.(zero.(_nzero), x)
  end
  return {
    :add => add,
    :zero => zero,
    :mul => mul,
    :one => one,
    :sub => sub,
    :div => div,
    :negate => negate
  }
end).()
if oc[:std] == nil
  oc[:std] = {}
end
oc[:std][:prelude] = (->() do
  flip = ->(f) do
    return ->(a,b) do
      return f.(b, a)
    end
  end
  cnst = ->(x) do
    return ->(y) do
      return x
    end
  end
  id = ->(x) do
    return x
  end
  fst = ->(x,y) do
    return x
  end
  snd = ->(x,y) do
    return y
  end
  oc[:instances][:show] = {}
  show = ->(x,p1) do
    return x.(p1)
  end
  oc[:instances][:eq] = {}
  eq = ->(x,p1,p2) do
    return x.(p1, p2)
  end
  oc[:instances][:mempty] = {}
  mempty = ->(x) do
    return x
  end
  oc[:instances][:append] = {}
  append = ->(x,p1,p2) do
    return x.(p1, p2)
  end
  oc[:instances][:map] = {}
  map = ->(x,p1,p2) do
    return x.(p1, p2)
  end
  oc[:instances][:apply] = {}
  apply = ->(x,p1,p2) do
    return x.(p1, p2)
  end
  oc[:instances][:pure] = {}
  pure = ->(x,p1) do
    return x.(p1)
  end
  oc[:instances][:bind] = {}
  bind = ->(x,p1,p2) do
    return x.(p1, p2)
  end
  oc[:instances][:traverse] = {}
  traverse = ->(x,p1,p2) do
    return x.(p1, p2)
  end
  oc[:instances][:show][:Int] = oc[:std][:ff][:showInt]
  oc[:instances][:show][:Double] = oc[:std][:ff][:showDouble]
  oc[:instances][:show][:String] = oc[:std][:ff][:showString]
  oc[:instances][:show][:Char] = oc[:std][:ff][:showChar]
  oc[:instances][:show][:Bool] = oc[:std][:ff][:showBool]
  oc[:instances][:eq][:Int] = oc[:std][:ff][:eqInt]
  oc[:instances][:eq][:Double] = oc[:std][:ff][:eqDouble]
  oc[:instances][:eq][:String] = oc[:std][:ff][:eqString]
  oc[:instances][:eq][:Char] = oc[:std][:ff][:eqChar]
  oc[:instances][:eq][:Bool] = oc[:std][:ff][:eqBool]
  oc[:instances][:mempty][:String] = ""
  oc[:instances][:append][:String] = oc[:std][:ff][:appendString]
  return {
    :flip => flip,
    :cnst => cnst,
    :id => id,
    :fst => fst,
    :snd => snd,
    :show => show,
    :eq => eq,
    :mempty => mempty,
    :append => append,
    :map => map,
    :apply => apply,
    :pure => pure,
    :bind => bind,
    :traverse => traverse
  }
end).()
oc[:testing] = (->() do
  check = ->(b) do
    return oc[:std][:ff][:print].((b ? "Pass" : "FAIL!!!"))
  end
  checkEq = ->(_neq,x,y) do
    return check.(_neq.(x, y))
  end
  return {
    :check => check,
    :checkEq => checkEq
  }
end).()
if oc[:std] == nil
  oc[:std] = {}
end
oc[:std][:foldable] = (->() do
  oc[:instances][:foldl] = {}
  foldl = ->(x,p1,p2,p3) do
    return x.(p1, p2, p3)
  end
  any = ->(_qfoldl,f,l) do
    return _qfoldl.(->(a,e) do
      return (a || f.(e))
    end, false, l)
  end
  all = ->(_aefoldl,f,l) do
    return _aefoldl.(->(a,e) do
      return (a && f.(e))
    end, true, l)
  end
  _or = ->(_amfoldl,l) do
    return any.(_amfoldl, oc[:std][:prelude][:id], l)
  end
  _and = ->(_aufoldl,l) do
    return all.(_aufoldl, oc[:std][:prelude][:id], l)
  end
  sum = ->(_azfoldl,
  _bmzero,
  _bmadd,
  l) do
    return _azfoldl.(_bmadd, oc[:std][:numbers][:zero].(_bmzero), l)
  end
  product = ->(_brfoldl,
  _ceone,
  _cemul,
  l) do
    return _brfoldl.(_cemul, oc[:std][:numbers][:one].(_ceone), l)
  end
  contains = ->(_cteq,
  _cufoldl,
  x,
  l) do
    return any.(_cufoldl, ->(y) do
      return _cteq.(x, y)
    end, l)
  end
  count = ->(_czfoldl,l) do
    return _czfoldl.(->(a,e) do
      return (a + 1)
    end, 0, l)
  end
  foldMap = ->(_dymempty,
  _dyappend,
  _dlfoldl,
  f,
  l) do
    return _dlfoldl.(->(a,e) do
      return _dyappend.(a, f.(e))
    end, oc[:std][:prelude][:mempty].(_dymempty), l)
  end
  concat = ->(_ecfoldl,
  _ehmempty,
  _ehappend,
  l) do
    return foldMap.(_ehmempty, _ehappend, _ecfoldl, oc[:std][:prelude][:id], l)
  end
  return {
    :foldl => foldl,
    :any => any,
    :all => all,
    :_or => _or,
    :_and => _and,
    :sum => sum,
    :product => product,
    :contains => contains,
    :count => count,
    :foldMap => foldMap,
    :concat => concat
  }
end).()
if oc[:std] == nil
  oc[:std] = {}
end
oc[:std][:maybes] = (->() do
  none = UniqObject.new
  oc[:instances][:eq][:None] = oc[:std][:ff][:eqAny]
  noneMaybe = none
  pureMaybe = ->(x) do
    return {
      :just => x
    }
  end
  maybe = ->(_a1,_a2,_a3) do
    if (->(x,f,_a) do
      return oc[:instances][:eq][:None].(_a, none)
    end).(_a1, _a2, _a3)
      return (->(x,f,_a) do
        return x
      end).(_a1, _a2, _a3)
    else
      if (->(x,f,z) do
        return z[:just] != nil
      end).(_a1, _a2, _a3)
        return (->(x,f,z) do
          return f.(z[:just])
        end).(_a1, _a2, _a3)
      else
        raise "cases error"
      end
    end
  end
  isNone = ->(x) do
    return maybe.(true, oc[:std][:prelude][:cnst].(false), x)
  end
  isJust = ->(x) do
    return maybe.(false, oc[:std][:prelude][:cnst].(true), x)
  end
  fromMaybe = ->(x,m) do
    return maybe.(x, oc[:std][:prelude][:id], m)
  end
  mapMaybe = ->(f,x) do
    return maybe.(noneMaybe, ->(y) do
      return {
        :just => f.(y)
      }
    end, x)
  end
  bindMaybe = ->(f,x) do
    return maybe.(none, f, x)
  end
  applyMaybe = ->(f,x) do
    return maybe.(none, ->(y) do
      return mapMaybe.(y, x)
    end, f)
  end
  showMaybe = ->(_cvshow,x) do
    return maybe.("none", ->(p1) do
      return oc[:std][:prelude][:show].(_cvshow, p1)
    end, x)
  end
  eqMaybe = ->(_edeq,_a1,_a2) do
    if (->(_b,_c) do
      return (oc[:instances][:eq][:None].(_b, none) && oc[:instances][:eq][:None].(_c, none))
    end).(_a1, _a2)
      return (->(_b,_c) do
        return true
      end).(_a1, _a2)
    else
      if (->(_d,y) do
        return oc[:instances][:eq][:None].(_d, none)
      end).(_a1, _a2)
        return (->(_d,y) do
          return false
        end).(_a1, _a2)
      else
        if (->(x,_e) do
          return oc[:instances][:eq][:None].(_e, none)
        end).(_a1, _a2)
          return (->(x,_e) do
            return false
          end).(_a1, _a2)
        else
          if (->(x,y) do
            return (x[:just] != nil && 
            y[:just] != nil)
          end).(_a1, _a2)
            return (->(x,y) do
              return _edeq.(x[:just], y[:just])
            end).(_a1, _a2)
          else
            raise "cases error"
          end
        end
      end
    end
  end
  foldlMaybe = ->(f,i,x) do
    return maybe.(i, ->(y) do
      return f.(i, y)
    end, x)
  end
  oc[:instances][:show][:Maybe] = showMaybe
  oc[:instances][:eq][:Maybe] = eqMaybe
  oc[:instances][:foldl][:Maybe] = foldlMaybe
  oc[:instances][:pure][:Maybe] = pureMaybe
  oc[:instances][:map][:Maybe] = mapMaybe
  oc[:instances][:apply][:Maybe] = applyMaybe
  oc[:instances][:bind][:Maybe] = bindMaybe
  oc[:instances][:mempty][:Maybe] = none
  return {
    :none => none,
    :noneMaybe => noneMaybe,
    :pureMaybe => pureMaybe,
    :maybe => maybe,
    :isNone => isNone,
    :isJust => isJust,
    :fromMaybe => fromMaybe,
    :mapMaybe => mapMaybe,
    :bindMaybe => bindMaybe,
    :applyMaybe => applyMaybe,
    :showMaybe => showMaybe,
    :eqMaybe => eqMaybe,
    :foldlMaybe => foldlMaybe
  }
end).()
oc[:test_maybe] = (->() do
  m = oc[:instances][:pure][:Maybe].(2)
  m2 = oc[:instances][:pure][:Maybe].(3)
  testNone = oc[:std][:maybes][:none]
  maybeBindFunc = ->(_a1) do
    if (->(x) do
      return oc[:std][:ff][:eqInt].(x, 1)
    end).(_a1)
      return (->(x) do
        return oc[:std][:maybes][:none]
      end).(_a1)
    else
      if (->(x) do
        return oc[:std][:ff][:eqInt].(x, 2)
      end).(_a1)
        return (->(x) do
          return {
            :just => 3
          }
        end).(_a1)
      else
        raise "cases error"
      end
    end
  end
  return {
    :m => m,
    :m2 => m2,
    :testNone => testNone,
    :maybeBindFunc => maybeBindFunc,
    :item1 => oc[:testing][:checkEq].(oc[:instances][:eq][:Bool], oc[:std][:maybes][:isJust].(m), true),
    :item2 => oc[:testing][:checkEq].(oc[:instances][:eq][:Bool], oc[:std][:maybes][:isNone].(testNone), true),
    :item3 => oc[:testing][:check].((oc[:std][:maybes][:maybe].(1, oc[:std][:prelude][:cnst].(2), oc[:std][:maybes][:none]) == 1)),
    :item4 => oc[:testing][:check].((oc[:std][:maybes][:maybe].(1, ->(x) do
      return (x + 1)
    end, m2) == 4)),
    :item5 => oc[:testing][:check].((oc[:std][:maybes][:fromMaybe].(0, oc[:std][:maybes][:none]) == 0)),
    :item6 => oc[:testing][:check].((oc[:std][:maybes][:fromMaybe].(0, oc[:std][:maybes][:pureMaybe].(2)) == 2)),
    :item7 => oc[:testing][:check].((oc[:std][:maybes][:fromMaybe].(0, m) == 2)),
    :item8 => oc[:testing][:check].((oc[:std][:maybes][:fromMaybe].(0, oc[:instances][:map][:Maybe].(->(x) do
      return (x + 1)
    end, m)) == 3)),
    :item9 => oc[:testing][:check].((oc[:std][:maybes][:fromMaybe].(0, oc[:instances][:bind][:Maybe].(maybeBindFunc, m)) == 3)),
    :item10 => oc[:testing][:check].((oc[:instances][:foldl][:Maybe].(->(a,
    e) do
      return (a + e)
    end, 1, m) == 3)),
    :item11 => oc[:testing][:check].((oc[:std][:maybes][:showMaybe].(oc[:instances][:show][:Int], testNone) == "none")),
    :item12 => oc[:testing][:check].(((->(p1) do
      return oc[:instances][:show][:Maybe].(oc[:instances][:show][:Int], p1)
    end).(m) == "2")),
    :item13 => oc[:testing][:check].((->(p1,
    p2) do
      return oc[:instances][:eq][:Maybe].(oc[:instances][:eq][:Int], p1, p2)
    end).(m, m))
  }
end).()
