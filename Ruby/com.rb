#!/usr/bin/ruby
require './cmd.rb'

class Shell < Cmd
    attr_reader :file
    attr_writer :file

    def initialize
        @intro = "crappy shell"
        @prompt = "!Sh> "
    end

    def do_me(arg)
        p self.methods.grep(/^do_/).map {|x| x.to_s[3..x.length]}
        nil
    end

    def do_reverse(arg)
        puts arg.reverse
    end

    def do_echo(arg)
        puts arg
    end

    def do_quit(arg)
        puts "Byeeeee"
        self.close
        return true
    end

    # -- record & playback, misc --
    def do_record(arg)
        @file = File.new(arg, 'a')
        nil
    end

    def do_playback(arg)
        self.close
        File::open(arg) do |f|
            @cmdqueue |= f.readlines
        end
        nil
    end

    def precmd(line)
        line&.downcase!
        begin
            if !@file.nil? and !line&.include? 'playback'
                file.write(line + "\n")
            end
        rescue
        end
        return line
    end

    def close
        if @file
            @file.close
            @file = nil
        end
    end
end

Shell.new.cmdloop