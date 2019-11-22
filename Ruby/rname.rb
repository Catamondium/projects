#!/usr/bin/ruby
require 'pathname'
require 'optparse'
require 'ostruct'

def re_sort(dir, parent, width)
    re = /^#{Regexp.escape(parent)}-\d{#{width}}/
    matched, unmatched = dir
    .partition {|f| f.basename.to_s =~ re}
    .map {|fs| fs.sort}
    return matched |= unmatched
end

def mv(parent, conf)
    path = Pathname.new(parent).expand_path
    files, dirs = path.each_child.partition {|f| f.file?}

        if files.length != 0
            width = files.length.to_s.length
            files = re_sort(files, path.basename.to_s, width)
            files.each_with_index do |f, i|
                n = "%s-%0*d%s" % [path.basename, width, i, f.extname]
                npath = path / n
                if npath == f
                    next
                end
                if conf.verbose
                    puts "#{f.basename.to_s} -> #{n}"
                end
                
                if not conf.dry
                    f.rename(npath)
                end
            end
        end

    if conf.recurse
        dirs.each {|d| mv(d, conf)}
    end
end

def approve(path)
    print "Rename inside #{path}? "
    char = STDIN.gets.chomp[0]&.downcase or 'y'
    return char == 'y'
end

def parse(args)
    options = OpenStruct.new
    options.dry = false
    options.verbose = false
    options.recurse = false
    options.force = false

    parser = OptionParser.new do |opts|
        opts.banner = "Usage: #{$0} [-vfdr] [DIRS]..."

        opts.separator ""
        opts.separator "Specific options:"

        opts.on("-v", "--[no-]verbose", "Print moves") do |v|
            options.verbose = v
        end
        opts.on("-f", "--[no-]force", "don't ask about argument DIRS") do |f|
            options.force = f
        end
        opts.on("-r", "--[no-]recurse", "rename recursively") do |r|
            options.recurse = r
        end
        opts.on("-d", "--[no-]dry", "Print moves") do |d|
            options.verbose |= d
            options.force &= !d
            options.dry = d
        end
    end

    parser.parse!(args)
    return options
end

conf = parse(ARGV)

ARGV.each do |a|
    if conf.force or approve(a)
        mv(a, conf)
    end
end
