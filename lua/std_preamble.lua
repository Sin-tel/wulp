require("strict")
local int = {}
local num = {}
local str = {}
local bool = {}
local function default(x, y)
  if x == nil then
    return y
  else
    return x
  end
end
local iter = require("iter")
