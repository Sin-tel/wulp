require("strict")
math.randomseed(os.time())
local int = {}
local num = {}
local str = {}
local bool = {}
local iter = require("iter")
local function default(x, y) if x == nil then return y else return x end end

