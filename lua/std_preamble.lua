require("strict")
local function default(x, y)
  if x == nil then
    return y
  else
    return x
  end
end
local iter = require("iter")
