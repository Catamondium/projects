Time = {hrs = 0, mins = 0}
function Time:new (o)
	o = o or {}
	setmetatable(o, self)
	self.__index = self
	self.__tostring = function(o) -- Python __repr__() equivlent
		return string.format("%02i:%02i", o.hrs, o.mins)
	end
	return o
end

function Time:add (elapse)
	local offset = self.hrs * 60 + self.mins
	local tot = offset + elapse
	return Time:new{
		hrs = math.floor(tot / 60),
		mins = tot % 60}
end

getNum = string.gmatch(arg[1], "%d+")
start = Time:new{
	hrs = getNum(),
	mins = getNum()}
elapse = tonumber(arg[2])

print(string.format("Start:\t%s\t%+d\nEnd:\t%s",
	start, elapse, start:add(elapse)))
