oc = {}
oc.instances = {}

oc.cloneObject = function (t)
  local t2 = {}
  for k,v in pairs(t) do
    t2[k] = v
  end
  return t2
end

function print_r(arr, indentLevel)
    local str = ""
    local indentStr = "#"

    if(indentLevel == nil) then
        print(print_r(arr, 0))
        return
    end

    for i = 0, indentLevel do
        indentStr = indentStr.."\t"
    end

    for index,value in pairs(arr) do
        if type(value) == "table" then
            str = str..indentStr..index..": \n"..print_r(value, (indentLevel + 1))
        else 
            str = str..indentStr..index..": "..value.."\n"
        end
    end
    return str
end

if oc.std == nil then
  oc.std = {}
end
oc.std.ff = (function()
  local _print = function(x) print(x); end

function addAny(x,y) return x + y end

function mulAny(x,y) return x * y end

function subAny(x,y) return x - y end

function eqAny(x,y) return x == y end

function showAny(x) return tostring(x) end

local unit = {}
local emptyObject = {}
local eqAny = eqAny

local eqInt = eqAny
local eqDouble = eqAny
local eqString = eqAny
local eqChar = eqAny
local eqBool = eqAny

local showInt = showAny
local showDouble = showAny
local showString = showAny
local showChar = showAny
local showBool = showAny

local addInt = addAny
local mulInt = mulAny
local subInt = subAny
local divInt = function(x,y) return math.floor( x / y) end

local addDouble = addAny
local mulDouble = mulAny
local subDouble = subAny
local divDouble = function(x,y) return x / y end

local addBool = function(x,y) return x or y end
local mulBool = function(x,y) return x and y end

local _not = function(x) return not x end
local orBool = function(x,y) return x or y end
local andBool = function(x,y) return x and y end

local appendString = function(x,y) return x .. y end

  return {
    unit = unit,
    showInt = showInt,
    showDouble = showDouble,
    showString = showString,
    showChar = showChar,
    showBool = showBool,
    eqAny = eqAny,
    eqInt = eqInt,
    eqDouble = eqDouble,
    eqString = eqString,
    eqChar = eqChar,
    eqBool = eqBool,
    _not = _not,
    andBool = andBool,
    orBool = orBool,
    addInt = addInt,
    mulInt = mulInt,
    subInt = subInt,
    divInt = divInt,
    addDouble = addDouble,
    mulDouble = mulDouble,
    subDouble = subDouble,
    divDouble = divDouble,
    appendString = appendString,
    _print = _print
  }
end)()
if oc.std == nil then
  oc.std = {}
end
oc.std.numbers = (function()
  oc.instances.add = {}
  local add = function(x,p1,p2)
    return x(p1, p2)
  end
  oc.instances.zero = {}
  local zero = function(x)
    return x
  end
  oc.instances.mul = {}
  local mul = function(x,p1,p2)
    return x(p1, p2)
  end
  oc.instances.one = {}
  local one = function(x)
    return x
  end
  oc.instances.sub = {}
  local sub = function(x,p1,p2)
    return x(p1, p2)
  end
  oc.instances.div = {}
  local div = function(x,p1,p2)
    return x(p1, p2)
  end
  oc.instances.add.Int = oc.std.ff.addInt
  oc.instances.mul.Int = oc.std.ff.mulInt
  oc.instances.zero.Int = 0
  oc.instances.one.Int = 1
  oc.instances.sub.Int = oc.std.ff.subInt
  oc.instances.div.Int = oc.std.ff.divInt
  oc.instances.add.Double = oc.std.ff.addDouble
  oc.instances.mul.Double = oc.std.ff.mulDouble
  oc.instances.zero.Double = 0.0
  oc.instances.one.Double = 1.0
  oc.instances.sub.Double = oc.std.ff.subDouble
  oc.instances.div.Double = oc.std.ff.divDouble
  local negate = function(_nzero,
  _nsub,
  x)
    return _nsub(zero(_nzero), x)
  end
  return {
    add = add,
    zero = zero,
    mul = mul,
    one = one,
    sub = sub,
    div = div,
    negate = negate
  }
end)()
if oc.std == nil then
  oc.std = {}
end
oc.std.prelude = (function()
  local flip = function(f)
    return function(a,b)
      return f(b, a)
    end
  end
  local cnst = function(x)
    return function(y)
      return x
    end
  end
  local id = function(x)
    return x
  end
  local fst = function(x,y)
    return x
  end
  local snd = function(x,y)
    return y
  end
  oc.instances.show = {}
  local show = function(x,p1)
    return x(p1)
  end
  oc.instances.eq = {}
  local eq = function(x,p1,p2)
    return x(p1, p2)
  end
  oc.instances.mempty = {}
  local mempty = function(x)
    return x
  end
  oc.instances.append = {}
  local append = function(x,p1,p2)
    return x(p1, p2)
  end
  oc.instances.map = {}
  local map = function(x,p1,p2)
    return x(p1, p2)
  end
  oc.instances.apply = {}
  local apply = function(x,p1,p2)
    return x(p1, p2)
  end
  oc.instances.pure = {}
  local pure = function(x,p1)
    return x(p1)
  end
  oc.instances.bind = {}
  local bind = function(x,p1,p2)
    return x(p1, p2)
  end
  oc.instances.traverse = {}
  local traverse = function(x,
  p1,
  p2)
    return x(p1, p2)
  end
  oc.instances.show.Int = oc.std.ff.showInt
  oc.instances.show.Double = oc.std.ff.showDouble
  oc.instances.show.String = oc.std.ff.showString
  oc.instances.show.Char = oc.std.ff.showChar
  oc.instances.show.Bool = oc.std.ff.showBool
  oc.instances.eq.Int = oc.std.ff.eqInt
  oc.instances.eq.Double = oc.std.ff.eqDouble
  oc.instances.eq.String = oc.std.ff.eqString
  oc.instances.eq.Char = oc.std.ff.eqChar
  oc.instances.eq.Bool = oc.std.ff.eqBool
  oc.instances.mempty.String = ""
  oc.instances.append.String = oc.std.ff.appendString
  return {
    flip = flip,
    cnst = cnst,
    id = id,
    fst = fst,
    snd = snd,
    show = show,
    eq = eq,
    mempty = mempty,
    append = append,
    map = map,
    apply = apply,
    pure = pure,
    bind = bind,
    traverse = traverse
  }
end)()
oc.testing = (function()
  local check = function(b)
    return oc.std.ff._print((b and "Pass" or "FAIL!!!"))
  end
  local checkEq = function(_neq,
  x,
  y)
    return check(_neq(x, y))
  end
  return {
    check = check,
    checkEq = checkEq
  }
end)()
oc.test_numbers = {
  item1 = oc.testing.checkEq(oc.instances.eq.Bool, (1 == 2), false),
  item2 = oc.testing.checkEq(oc.instances.eq.Int, ((1 + 2) * 3), 9),
  item3 = oc.testing.checkEq(oc.instances.eq.Int, (1 + 2), 3),
  item4 = oc.testing.checkEq(oc.instances.eq.Int, (2 * 3), 6),
  item5 = oc.testing.checkEq(oc.instances.eq.Int, (9 - 5), 4),
  item6 = oc.testing.checkEq(oc.instances.eq.Int, oc.instances.div.Int(12, 4), 3),
  item7 = oc.testing.checkEq(oc.instances.eq.Double, (1.0 + 2.0), 3.0),
  item8 = oc.testing.checkEq(oc.instances.eq.Double, (2.0 * 3.0), 6.0),
  item9 = oc.testing.checkEq(oc.instances.eq.Double, (9.0 - 5.0), 4.0),
  item10 = oc.testing.checkEq(oc.instances.eq.Double, (12.0 / 4.0), 3.0)
}
