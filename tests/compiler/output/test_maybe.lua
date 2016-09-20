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
if oc.std == nil then
  oc.std = {}
end
oc.std.foldable = (function()
  oc.instances.foldl = {}
  local foldl = function(x,
  p1,
  p2,
  p3)
    return x(p1, p2, p3)
  end
  local any = function(_qfoldl,
  f,
  l)
    return _qfoldl(function(a,e)
      return (a or f(e))
    end, false, l)
  end
  local all = function(_aefoldl,
  f,
  l)
    return _aefoldl(function(a,e)
      return (a and f(e))
    end, true, l)
  end
  local _or = function(_amfoldl,l)
    return any(_amfoldl, oc.std.prelude.id, l)
  end
  local _and = function(_aufoldl,
  l)
    return all(_aufoldl, oc.std.prelude.id, l)
  end
  local sum = function(_azfoldl,
  _bmzero,
  _bmadd,
  l)
    return _azfoldl(_bmadd, oc.std.numbers.zero(_bmzero), l)
  end
  local product = function(_brfoldl,
  _ceone,
  _cemul,
  l)
    return _brfoldl(_cemul, oc.std.numbers.one(_ceone), l)
  end
  local contains = function(_cteq,
  _cufoldl,
  x,
  l)
    return any(_cufoldl, function(y)
      return _cteq(x, y)
    end, l)
  end
  local count = function(_czfoldl,
  l)
    return _czfoldl(function(a,e)
      return (a + 1)
    end, 0, l)
  end
  local foldMap = function(_dymempty,
  _dyappend,
  _dlfoldl,
  f,
  l)
    return _dlfoldl(function(a,e)
      return _dyappend(a, f(e))
    end, oc.std.prelude.mempty(_dymempty), l)
  end
  local concat = function(_ecfoldl,
  _ehmempty,
  _ehappend,
  l)
    return foldMap(_ehmempty, _ehappend, _ecfoldl, oc.std.prelude.id, l)
  end
  return {
    foldl = foldl,
    any = any,
    all = all,
    _or = _or,
    _and = _and,
    sum = sum,
    product = product,
    contains = contains,
    count = count,
    foldMap = foldMap,
    concat = concat
  }
end)()
if oc.std == nil then
  oc.std = {}
end
oc.std.maybes = (function()
  local none = {}
  oc.instances.eq.None = oc.std.ff.eqAny
  local noneMaybe = none
  local pureMaybe = function(x)
    return {
      just = x
    }
  end
  local maybe = function(_a1,
  _a2,
  _a3)
    if (function(x,f,_a)
      return oc.instances.eq.None(_a, none)
    end)(_a1, _a2, _a3) then
      return (function(x,f,_a)
        return x
      end)(_a1, _a2, _a3)
    else
      if (function(x,f,z)
        return z.just ~= nil
      end)(_a1, _a2, _a3) then
        return (function(x,f,z)
          return f(z.just)
        end)(_a1, _a2, _a3)
      else
        error("cases error")
      end
    end
  end
  local isNone = function(x)
    return maybe(true, oc.std.prelude.cnst(false), x)
  end
  local isJust = function(x)
    return maybe(false, oc.std.prelude.cnst(true), x)
  end
  local fromMaybe = function(x,m)
    return maybe(x, oc.std.prelude.id, m)
  end
  local mapMaybe = function(f,x)
    return maybe(noneMaybe, function(y)
      return {
        just = f(y)
      }
    end, x)
  end
  local bindMaybe = function(f,x)
    return maybe(none, f, x)
  end
  local applyMaybe = function(f,x)
    return maybe(none, function(y)
      return mapMaybe(y, x)
    end, f)
  end
  local showMaybe = function(_cvshow,
  x)
    return maybe("none", function(p1)
      return oc.std.prelude.show(_cvshow, p1)
    end, x)
  end
  local eqMaybe = function(_edeq,
  _a1,
  _a2)
    if (function(_b,_c)
      return (oc.instances.eq.None(_b, none) and oc.instances.eq.None(_c, none))
    end)(_a1, _a2) then
      return (function(_b,_c)
        return true
      end)(_a1, _a2)
    else
      if (function(_d,y)
        return oc.instances.eq.None(_d, none)
      end)(_a1, _a2) then
        return (function(_d,y)
          return false
        end)(_a1, _a2)
      else
        if (function(x,_e)
          return oc.instances.eq.None(_e, none)
        end)(_a1, _a2) then
          return (function(x,_e)
            return false
          end)(_a1, _a2)
        else
          if (function(x,y)
            return (x.just ~= nil and 
            y.just ~= nil)
          end)(_a1, _a2) then
            return (function(x,y)
              return _edeq(x.just, y.just)
            end)(_a1, _a2)
          else
            error("cases error")
          end
        end
      end
    end
  end
  local foldlMaybe = function(f,
  i,
  x)
    return maybe(i, function(y)
      return f(i, y)
    end, x)
  end
  oc.instances.show.Maybe = showMaybe
  oc.instances.eq.Maybe = eqMaybe
  oc.instances.foldl.Maybe = foldlMaybe
  oc.instances.pure.Maybe = pureMaybe
  oc.instances.map.Maybe = mapMaybe
  oc.instances.apply.Maybe = applyMaybe
  oc.instances.bind.Maybe = bindMaybe
  oc.instances.mempty.Maybe = none
  return {
    none = none,
    noneMaybe = noneMaybe,
    pureMaybe = pureMaybe,
    maybe = maybe,
    isNone = isNone,
    isJust = isJust,
    fromMaybe = fromMaybe,
    mapMaybe = mapMaybe,
    bindMaybe = bindMaybe,
    applyMaybe = applyMaybe,
    showMaybe = showMaybe,
    eqMaybe = eqMaybe,
    foldlMaybe = foldlMaybe
  }
end)()
oc.test_maybe = (function()
  local m = oc.instances.pure.Maybe(2)
  local m2 = oc.instances.pure.Maybe(3)
  local testNone = oc.std.maybes.none
  local maybeBindFunc = function(_a1)
    if (function(x)
      return oc.std.ff.eqInt(x, 1)
    end)(_a1) then
      return (function(x)
        return oc.std.maybes.none
      end)(_a1)
    else
      if (function(x)
        return oc.std.ff.eqInt(x, 2)
      end)(_a1) then
        return (function(x)
          return {
            just = 3
          }
        end)(_a1)
      else
        error("cases error")
      end
    end
  end
  return {
    m = m,
    m2 = m2,
    testNone = testNone,
    maybeBindFunc = maybeBindFunc,
    item1 = oc.testing.checkEq(oc.instances.eq.Bool, oc.std.maybes.isJust(m), true),
    item2 = oc.testing.checkEq(oc.instances.eq.Bool, oc.std.maybes.isNone(testNone), true),
    item3 = oc.testing.check((oc.std.maybes.maybe(1, oc.std.prelude.cnst(2), oc.std.maybes.none) == 1)),
    item4 = oc.testing.check((oc.std.maybes.maybe(1, function(x)
      return (x + 1)
    end, m2) == 4)),
    item5 = oc.testing.check((oc.std.maybes.fromMaybe(0, oc.std.maybes.none) == 0)),
    item6 = oc.testing.check((oc.std.maybes.fromMaybe(0, oc.std.maybes.pureMaybe(2)) == 2)),
    item7 = oc.testing.check((oc.std.maybes.fromMaybe(0, m) == 2)),
    item8 = oc.testing.check((oc.std.maybes.fromMaybe(0, oc.instances.map.Maybe(function(x)
      return (x + 1)
    end, m)) == 3)),
    item9 = oc.testing.check((oc.std.maybes.fromMaybe(0, oc.instances.bind.Maybe(maybeBindFunc, m)) == 3)),
    item10 = oc.testing.check((oc.instances.foldl.Maybe(function(a,
    e)
      return (a + e)
    end, 1, m) == 3)),
    item11 = oc.testing.check((oc.std.maybes.showMaybe(oc.instances.show.Int, testNone) == "none")),
    item12 = oc.testing.check(((function(p1)
      return oc.instances.show.Maybe(oc.instances.show.Int, p1)
    end)(m) == "2")),
    item13 = oc.testing.check((function(p1,
    p2)
      return oc.instances.eq.Maybe(oc.instances.eq.Int, p1, p2)
    end)(m, m))
  }
end)()
