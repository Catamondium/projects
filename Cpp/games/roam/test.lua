
roam.register_on_tick(function ()
    file = io.open("log", "a")
    io.output(file)

    local player = roam.player
    io.write(string.format("P: %s\n", player))
    player.pos = {x = player.pos.x + 1}
    io.write(string.format("P2: %s\n", player))
    io.close(file)
end)