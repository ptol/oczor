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
oc[:test_code] = (->() do
  x1 = 1
  x2 = {
    :foo => 1,
    :bar => 1
  }
  func = ->(x,y) do
    return ((x + y) + 1)
  end
  x3 = func.(1, 2)
  func2 = ->(x) do
    temp = (x == 1)
    return (! (temp && false))
  end
  return {
    :x1 => x1,
    :x2 => x2,
    :func => func,
    :x3 => x3,
    :func2 => func2,
    :temp1 => x1,
    :temp2 => x2,
    :temp3 => x3
  }
end).()
