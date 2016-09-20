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
oc[:test_numbers] = {
  :item1 => oc[:testing][:checkEq].(oc[:instances][:eq][:Bool], (1 == 2), false),
  :item2 => oc[:testing][:checkEq].(oc[:instances][:eq][:Int], ((1 + 2) * 3), 9),
  :item3 => oc[:testing][:checkEq].(oc[:instances][:eq][:Int], (1 + 2), 3),
  :item4 => oc[:testing][:checkEq].(oc[:instances][:eq][:Int], (2 * 3), 6),
  :item5 => oc[:testing][:checkEq].(oc[:instances][:eq][:Int], (9 - 5), 4),
  :item6 => oc[:testing][:checkEq].(oc[:instances][:eq][:Int], (12 / 4), 3),
  :item7 => oc[:testing][:checkEq].(oc[:instances][:eq][:Double], (1.0 + 2.0), 3.0),
  :item8 => oc[:testing][:checkEq].(oc[:instances][:eq][:Double], (2.0 * 3.0), 6.0),
  :item9 => oc[:testing][:checkEq].(oc[:instances][:eq][:Double], (9.0 - 5.0), 4.0),
  :item10 => oc[:testing][:checkEq].(oc[:instances][:eq][:Double], (12.0 / 4.0), 3.0)
}
