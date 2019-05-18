require "readline"
# translated from CPython

module LineOriented
    @cmdqueue = []
    @lastcmd = nil
    @prompt = "(Com) "
    @cmds = self.methods
        .grep(/^do_/)
        .collect {|x| x.to_s[3...x.length]}
    
    def cmdloop(intro: nil)
        preloop
        @cmdqueue ||= [] # nil guard
        if intro
            puts intro
        elsif @intro
            puts @intro
        end

        stop = nil
        while stop != :quit
            if !@cmdqueue.empty?
                line = @cmdqueue.pop
            else
                Readline.completion_append_character = " "
                Readline.completion_proc = proc {|x| complete(x)}
                line = Readline.readline((@prompt or ''), true)
                line&.strip!
            end
            line = precmd line
            stop = onecmd line
            stop = postcmd(stop, line)
        end
        postloop
    end
    
    def onecmd(line)
        cmd, arg, line = parseline line
        @lastcmd = line
        return emptyline if !line
        if !cmd or cmd == ''
            return default(line)
        else
            command = "do_#{cmd}"
            if self.respond_to? command
                return self.send(command, arg)
            else
                return default(line)
            end
        end
    end
    
    protected

    # STUBS
    def precmd(line) line end
    def postcmd(stop, line) stop end
    def preloop; end
    def postloop; end
    # STUBS

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
   

    def complete(s)
        return @cmds.grep(/^#{Regexp.escape(s)}/).sort
    end
end