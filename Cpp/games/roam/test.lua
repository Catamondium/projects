
local i = 0
roam.register_on_tick(function ()
    file = io.open("test.log", "a")
    io.output(file)
    io.write(string.format("tick: %d\n", i))
    --for k,v in pairs(roam) do 
    --    io.write(string.format("%s: %s\n", k, v))
    --end
    io.close(file)
    i = i + 1
end)