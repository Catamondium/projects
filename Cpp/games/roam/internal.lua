-- Internal defs
-- initial 'roam' package

-- registries
roam = {
    on_tick_registry = {},
    on_move_registry = {},
}

-- Registrars
function roam.register_on_tick (callback)
    table.insert(roam.on_tick_registry, callback)
end

function roam.register_on_tick (callback)
    table.insert(roam.on_move_registry, callback)
end

-- Internal handlers
function _call_on_tick()
    for i,v in ipairs(roam.on_tick_registry) do
        v()
    end
end

function _call_on_move(ds)
    for i,v in ipairs(roam.on_move_registry) do
        v(ds)
    end
end