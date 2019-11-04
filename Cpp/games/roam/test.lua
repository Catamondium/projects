
roam.register_on_tick(function ()
    file = io.open("log", "a")
    io.output(file)

    local player = roam.player
    io.write(string.format("%d, %d\n", player.score, player.pos.x))
    io.close(file)
end)