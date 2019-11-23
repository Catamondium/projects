#!/usr/bin/env ruby2.6

require 'drb'
require 'delegate'
require 'io/console'

class Slave
    include DRb::DRbUndumped # Referential access only
    attr_accessor :stdin
    attr_accessor :stdout
    attr_accessor :stderr
    def initialize()
        @stdin = SimpleDelegator.new($stdin)
        @stdout = SimpleDelegator.new($stdout)
        @stderr = SimpleDelegator.new($stderr)
    end

    def kill()
        DRb.stop_service
    end
end

FRONT_OBJECT = Slave.new

path = ARGV[0]

DRb.start_service("drbunix:#{path}", FRONT_OBJECT)
DRb.thread.join