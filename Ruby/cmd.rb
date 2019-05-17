require "readline"
# translated from CPython

IDENTCHARS = [*'a'...'z', *'0'...'9', '_'].join
class Cmd
    attr_reader :cmdqueue, :lastcmd, :prompt, :list
    attr_writer :cmdqueue, :lastcmd, :prompt
    def initialize
        @cmdqueue = []
        @lastcmd = nil
        @prompt = "(Com) "
    end
    
    def cmdloop(intro=nil)
        self.preloop
        @cmdqueue ||= []
        if intro
            puts intro
        elsif @intro
            puts @intro
        end

        stop = nil
        while !stop
            if !@cmdqueue.empty? # nil by default?
                line = @cmdqueue.pop
            else
                Readline.completion_append_character = " "
                Readline.completion_proc = proc {|x| GREP(x)}
                line = Readline.readline((@prompt or ''), true)
                line ||= 'eof'
                line.strip!
            end
            line = self.precmd line
            stop = self.onecmd line
            stop = self.postcmd(stop, line)
        end
        self.postloop
    end

    # stubs
    def precmd(line)
        return line
    end
    
    def postcmd(stop, line)
        return stop
    end

    def preloop
    end

    def postloop
    end
    # stubs

    def parseline(line)
        if line == ''
            return [nil, nil, line]
        elsif line[0] == '!'
            if self.respond_to? :do_shell
                line = "shell #{line.slice(1)}"
            else
                return [nil, nil, line]
            end
        end
        i, n = 0, line.length
        i +=1 while i < n and IDENTCHARS.include? line[i]
        cmd, arg = line[0...i], line[i...n].strip
        return [cmd, arg, line]
    end

    def onecmd(line)
        cmd, arg, line = self.parseline line
        self.lastcmd = line
        if !line
            return @emptyline
        end
        if cmd.nil?
            return self.default(line)
        elsif cmd == ''
            return self.default(line)
        else
            sym = "do_#{cmd}"
            if self.respond_to? sym
                return self.send(sym, arg)
            else
                return self.default(line)
            end
        end
    end

    def emptyline
        if @lastcmd
            return self.onecmd(@lastcmd)
        end
    end

    def default(line)
        puts "*** Unknown syntax: #{line}"
    end

    private
    def GREP(s)
        list = self.methods.grep(/^do_/).map {|x| x.to_s[3..x.length]}
        return list.grep(/^#{Regexp.escape(s)}/).sort
    end
end