local iter = {}

local function array_next(self)
	local item = self.a[self.counter]
	if self.counter >= self.a.n then
		return nil
	end
	self.counter = self.counter + 1
	return item
end

function iter.array(a)
	return { a = a, counter = 0, ["next"] = array_next }
end


function iter.wrap(iter)
	return iter.next, iter
end

return iter
