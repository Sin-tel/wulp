require("strict")
local iter = require("iter")
local function fact(n)
	assert(n >= 0);
	if n ~= 0 then
		return n * fact(n - 1);
	else
		do
			return 1;
		end;
	end;
end;
local list = {[0]=2, 3, 4, 5, n=4};
for i in iter.wrap(iter.array(list)) do
	print(i, fact(i));
end;
