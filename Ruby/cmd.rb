require "readline"
# translated from CPython

class Cmd
    def initialize
        @cmdqueue = []
        @lastcmd = nil
        @prompt = "(Com) "
    end

    # STUBS
    def precmd(line) line end
    def postcmd(stop, line) stop end
    def preloop; end
    def postloop; end
    # STUBS
    
    def cmdloop(intro: nil)
        self.preloop
        @cmdqueue ||= [] # nil guard
        if intro
            puts intro
        elsif @intro
            puts @intro
        end

        stop = nil
        while !stop
            if !@cmdqueue.empty?
                line = @cmdqueue.pop
            else
                Readline.completion_append_character = " "
                Readline.completion_proc = proc {|x| complete(x)}
                line = Readline.readline((@prompt or ''), true)
                line&.strip!
            end
            line = self.precmd line
            stop = self.onecmd line
            stop = self.postcmd(stop, line)
        end
        self.postloop
    end

    def parseline(line)
        if !line or line == ''
            return [nil, nil, line]
        elsif line[0] == '!'
            if self.respond_to? :do_shell
                line.sub!('!', "shell ")
            else
                return [nil, nil, line]
            end
        end
        cmd, arg = line.split(/\W/, 2).map {|x| x.strip}
        return [cmd, arg, line]
    end

    def onecmd(line)
        cmd, arg, line = self.parseline line
        @lastcmd = line
        return self.emptyline if !line
        if !cmd or cmd == ''
            return self.default(line)
        else
            command = "do_#{cmd}"
            if self.respond_to? command
                return self.send(command, arg)
            else
                return self.default(line)
            end
        end
    end

    def emptyline
        if @lastcmd
            return self.onecmd(@lastcmd)
        else
            puts
        end
    end

    def default(line)
        puts "*** Unknown syntax: #{line}"
    end

    private
    def complete(s)
        list = self.methods.grep(/^do_/).collect {|x| x.to_s[3...x.length]}
        return list.grep(/^#{Regexp.escape(s)}/).sort
    end
end