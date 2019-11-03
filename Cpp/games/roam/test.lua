
roam.register_on_tick(function ()
    file = io.open("log", "a")
    io.output(file)

    local player = roam.player
    io.write(string.format("player: %s\n", player))

    player:setpos({x = player:getpos().x + 1})
    io.write(string.format("2player: %s\n", player))

    io.close(file)
end)