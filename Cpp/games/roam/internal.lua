-- Internal defs
-- initial 'roam' package
-- registries
roam = {
    -- we don't know all events
    -- we know it has a loop
    on_tick_registry = {},
}

function roam.register_on_tick (callback)
    table.insert(roam.on_tick_registry, callback)
end

-- Internal handler
function _call_on_tick()
    for i,v in ipairs(roam.on_tick_registry) do
        v()
    end
end