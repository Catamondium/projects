#!/usr/bin/env ruby2.6

require 'drb'
require 'tmpdir'
require 'fileutils'
require 'forwardable'

class Rtty
    @@displays = 0
    attr_reader :path # slave sock
    attr_reader :id # display id

    attr_reader :slave # DRbObject Slave
    extend Forwardable
    def_delegators :@slave, :stdin, :stdout

    def initialize()
        @slave = nil
        DRb.start_service

        @id = @@displays
        @@displays += 1
        # Derive sock path like HMAC
        @path = "#{Dir.tmpdir}/rtty#{Process.id}_#{id}"
    end

    def activate()
        unless self.active?
            success = system("gnome-terminal -- ./slave.rb #{@path}")
            if !success
                raise "Failed to open slave"
            end

            while !File::exist?(@path) # ensure conn
                sleep 0.2
            end
            @slave = DRbObject.new_with_uri("drbunix:#{@path}")
        end
    end

    def deactivate()
        @slave&.kill
        @slave = nil
    end
    alias_method :kill, :deactivate

    def active=(stat)
        if stat
            self.activate
        else
            self.deactivate
        end
    end

    def active?()
        return !@slave.nil?
    end
    alias_method :active, :active?
end

tty = Rtty.new
tty2 = Rtty.new

tty2.active = true
tty.active = true
p tty.stdin.readline
p tty2.stdin.readline

tty.deactivate
tty2.deactivate