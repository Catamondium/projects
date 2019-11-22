#!/usr/bin/env ruby2.6
=begin
Slave program for Remote tty control.

Default state is as a RAW/uncooked terminal until activated
for further duty
=end


require 'drb'
require 'delegate'
require 'io/console'

class Slave
    include DRb::DRbUndumped # Referential access only
    attr_accessor :stdin
    attr_accessor :stdout
    def initialize()
        @stdin = SimpleDelegator.new($stdin)
        @stdout = SimpleDelegator.new($stdout)
    end

    def kill()
        DRb.stop_service
    end
end

FRONT_OBJECT = Slave.new

path = ARGV[0]

DRb.start_service("drbunix:#{path}", FRONT_OBJECT)
DRb.thread.join