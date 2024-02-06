-- this produces a runtime error
local fact = function(n)
	if n == 0 then
		return 1
	else
		return n * fact(n - 1)
	end
end

-- this is ok
local fact2
fact2 = function(n)
	if n == 0 then
		return 1
	else
		return n * fact2(n - 1)
	end
end

-- this is also ok
local function fact3(n)
	if n == 0 then
		return 1
	else
		return n * fact3(n - 1)
	end
end

return fact(6), fact2(6), fact3(6)
