#!/usr/bin/env ruby2.6

require 'drb'

path = "./sock"

DRb.start_service

success = system("gnome-terminal -- ./slave.rb #{path}")
if !success
    puts "ERROR"
end

while !File::exist?(path) # ensure conn
    sleep 0.2
end

slave = DRbObject.new_with_uri("drbunix:#{path}")

p slave.stdin.read(10)
slave.kill