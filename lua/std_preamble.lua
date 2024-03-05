require("strict")
local int = {}
local num = require("num")
local str = require("str")
local bool = {}
local function default(x, y)
  if x == nil then
    return y
  else
    return x
  end
end
local iter = require("iter")
