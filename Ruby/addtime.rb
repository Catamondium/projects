#!/usr/bin/ruby

class Time
    attr_reader :hrs, :mins
    attr_writer :hrs, :mins
    def initialize(*args)
        if args.length == 1
            if args[0].is_a? String
                tmp = args[0].to_t
                @hrs = tmp.hrs
                @mins = tmp.mins
            else
                absmins = args[0]
                @hrs = (absmins / 60).floor
                @mins = absmins % 60
            end
        else
            @hrs =  args[0] or 0
            @mins = args[1] or 0
        end
    end

    def +(other)
        if other.is_a?(Integer)
            o = other
        elsif other.is_a?(Time)
            o = other.abs
        end
        # not defined <<= 0
        o ||= 0
        return Time.new(self.abs + o)
    end

    def to_s
        "%02d:%02d" % [self.hrs , self.mins]
    end

    def abs
        (60 * self.hrs) + self.mins
    end
end

class String
    def is_i?
        true if Integer(self) rescue false
    end

    def to_t
        if self.is_i?
            return Time.new(self.to_i)
        elsif m = (/(\d{1,2}):(\d{1,2})/.match self)
            return Time.new(m[1].to_i, m[2].to_i)
        end
    end
end

quiet, rest = ARGV.to_enum.partition {|x| x =~ /-/}

if rest.length < 2
    puts "Usage: #{$0} [-q] START ELAPSE"
    puts "\t-q: print elapsed time only"
    exit 1
end

start = rest[0].to_t or Time.new()
elapse = rest[1].to_t or Time.new()
if quiet.length > 0
    puts start + elapse
else
    puts "#{start} + #{elapse.abs} -> #{start + elapse}"
end