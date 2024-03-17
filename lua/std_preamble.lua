local mt = {}
setmetatable(_G, mt)
mt.__newindex = function(t, n, v)
    error("script attempted to create global variable '" .. tostring(n) .. "'", 2)
end
math.randomseed(os.time())
local bit = require("bit")
local int = {}
local num = {}
local str = {}
local bool = {}
local array = {}
local iter = {}
local function wrap_iter(iter) return iter.next, iter end
local function default(x, y) if x == nil then return y else return x end end
local import_glob = function(t)
	for k, v in pairs(t) do
		if rawget(_G, k) ~= nil then
			print("warning: function " .. k .. " already exists in global scope")
		end
		rawset(_G, k, v)
	end
end
local MODULES = {}
