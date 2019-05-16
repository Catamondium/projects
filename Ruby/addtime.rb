#!/usr/bin/ruby

class Time
    attr_reader :hrs, :mins
    attr_writer :hrs, :mins
    def initialize(*args)
        case args.length <=> 1
        when 0
            if args[0].kind_of? String
                tmp = args[0].to_t
                @hrs = tmp.hrs
                @mins = tmp.mins
            else
                absmins = args[0]
                @hrs = (absmins / 60).floor
                @mins = absmins % 60
            end
        when 1
            @hrs = args[0]
            @mins = args[1]
        else
            @hrs = 0
            @mins = 0
        end
    end

    def +(other)
        if other.kind_of?(Numeric)
            o = other
        elsif other.kind_of?(Time)
            o = other.abs
        else
            throw "addition error, invalid argument: %s" % other.class
        end
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

quiet, rest = ARGV.to_enum.partition {|x| x =~ /-q/}

if rest.length < 2
    puts "Usage: #{$0} [-q] START ELAPSE"
    puts "\t-q: print elapsed time only"
    exit 1
end

start = rest[0].to_t
elapse = rest[1].to_t
if start.nil? or elapse.nil?
    puts "Invalid time"
    exit 1
end

if quiet.length > 0
    puts start + elapse
else
    puts "#{start} + #{elapse.abs} -> #{start + elapse}"
end