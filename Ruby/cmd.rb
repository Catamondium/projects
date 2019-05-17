require "readline"
# translated from CPython

IDENTCHARS = [*'a'...'z', *'0'...'9', '_'].join
class Cmd
    attr_reader :cmdqueue, :lastcmd, :prompt, :completer
    attr_writer :cmdqueue, :lastcmd, :prompt
    def initialize
        @cmdqueue = []
        @lastcmd = nil
        @prompt = "(Com) "
        @completer = proc do |s|
            s.strip!
            list = self.methods.grep(/^do_/).map {|x| x.to_s}
            return list.sort.grep(/^do_#{Regexp.escape(s)}/).map {|x| x[2...x.length]}
        end
    end
    
    def cmdloop(intro=nil)
        self.preloop
        if intro
            puts intro
        end
        stop = nil
        while !stop
            if self.cmdqueue && !self.cmdqueue.empty?
                line = cmdqueue.pop
            else
                Readline.completion_append_character = " "
                Readline.completion_proc = self.completer # not working
                line = Readline.readline((self.prompt or ''), true)
                line ||= 'eof'
                line.strip!
            end
            line = self.precmd line
            stop = self.onecmd line
            stop = self.postcmd(stop, line)
        end
        self.postloop
    end

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
        i +=1 while i < n and (IDENTCHARS.include? line[i])
        cmd, arg = line[0...i], line[i...n].strip
        return [cmd, arg, line]
    end

    def onecmd(line)
        cmd, arg, line = self.parseline line
        self.lastcmd = line
        if !line
            return self.emptyline
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
        if self.lastcmd
            return self.onecmd(self.lastcmd)
        end
    end

    def default(line)
        puts "*** Unknown syntax: #{line}"
    end
end