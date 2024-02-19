local array = {}

function array.push(self, v)
	self[self.n] = v
	self.n = self.n + 1
end

function array.pop(self)
	self.n = self.n - 1
	local item = self[self.n]
	self[self.n] = nil
	return item
end

local function less_than(a, b)
	return a < b
end

function array.sort(self, f)
	-- basic in-place insertion sort
	-- ok for small tables (up to ~ 100)
	f = f or less_than

	for i = 1, self.n - 1 do
		local current = self[i]
		local h = i
		while h > 0 and f(current, self[h - 1]) do
			self[h] = self[h - 1]
			h = h - 1
		end
		self[h] = current
	end
end

return array
