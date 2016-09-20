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
