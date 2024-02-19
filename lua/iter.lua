local iter = {}

local function range_next(self)
	if self.counter >= self.stop then
		return nil
	end
	self.counter = self.counter + 1
	return self.counter
end

function iter.range(stop)
	return { stop = stop, counter = 0, ["next"] = range_next }
end

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
