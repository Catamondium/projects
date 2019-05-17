require './cmd.rb'

class Shell < Cmd
    attr_reader :file
    attr_writer :file

    def initialize
        @intro = "crappy shell"
        @prompt = "!Sh> "
    end

    def do_me(arg)
        list =  self.methods.grep(/^do_/).map {|x| x.to_s}
        puts list
        puts list.class
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
        @file = IO::open(arg, 'a')
        nil
    end

    def do_playback(arg)
        self.close
        IO::open(arg, 'w') do |f|
            self.cmdqueue |= f.readlines
        end
        nil
    end

    def precmd(line)
        line.downcase!
        if !@file.nil? and !line.include? 'playback'
            file.write(line)
        end
        return line
    end

    def close
        if not @file.nil?
            @file.close
            @file = nil
        end
    end
end

Shell.new.cmdloop