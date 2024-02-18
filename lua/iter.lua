local exports = {}

local function range_next()
	if self.counter >= self.stop then
		return nil
	end
	self.counter = self.counter + 1
	return self.counter
end

local function range(stop)
	return { stop = stop, counter = 0, ["next"] = range_next }
end

exports.range = range

local function array_next(self)
	local item = self.a[self.counter]
	if self.counter >= self.a.n then
		return nil
	end
	self.counter = self.counter + 1
	return item
end

local function array(a)
	return { a = a, counter = 0, ["next"] = array_next }
end

exports.array = array

local function wrap(iter)
	return iter.next, iter
end

exports.wrap = wrap

return exports
