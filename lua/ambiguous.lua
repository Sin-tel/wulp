local f = function() end
local x = function() end
local y = function() end

-- ambiguous
f()
(x or y)()

-- ok
-- f();
-- (x or y)()

-- ok
-- f()(x or y)()
