#!/usr/bin/env ruby2.6

require 'drb'
require 'tmpdir'
require 'fileutils'
require 'forwardable'
require 'stringio'

class Rtty
    @@displays = 0
    attr_reader :path # slave sock
    attr_reader :id # display id

    attr_reader :slave # DRbObject Slave
    extend Forwardable
    def_delegators :@slave, :stdin, :stdout, :stderr

    def initialize()
        @slave = nil
        @id = @@displays
        @@displays += 1
        # Derive sock path like HMAC
        @path = "#{Dir.tmpdir}/rtty#{Process.pid}_#{@id}"
    end

    def activate()
        unless self.active?
            DRb.start_service
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

    ## Call with stdio redirected through an rtty
    def self.redirect()
        tty = Rtty.new
        tty.activate

        $stdin = tty.stdin
        $stdout = tty.stdout
        $stderr = tty.stderr
        yield
        tty.deactivate
        $stdin = STDIN
        $stdout = STDOUT
        $stderr = STDERR
    end
end

str = StringIO.new
Rtty.redirect do
    puts "prompt"
    str.puts $stdin.readline
end
puts str.string.dump