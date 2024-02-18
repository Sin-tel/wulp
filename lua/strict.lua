-- Error when trying to make a global
local mt = {}
setmetatable(_G, mt)
mt.__newindex = function(t, n, v)
    error("Script attempted to create global variable '" .. tostring(n) .. "'", 2)
end
