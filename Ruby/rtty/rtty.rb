#!/usr/bin/env ruby2.6

require 'drb'

path = "./sock"

def rtty(path)
    DRb.start_service

    success = system("gnome-terminal -- ./slave.rb #{path}")
    if !success
        raise "Failed to open slave"
    end

    while !File::exist?(path) # ensure conn
        sleep 0.2
    end

    return DRbObject.new_with_uri("drbunix:#{path}")
end

slave = rtty(path)
p slave.stdin.read(10)
slave.kill