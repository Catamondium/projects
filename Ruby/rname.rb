#!/usr/bin/ruby
require 'pathname'
Config = Struct.new(:dry, :verbose, :recurse, :force) do
    def self.parseopts(opts)
        dry = opts.include?('-d')     ? true : false
        verbose = opts.include?('-v') ? true : false
        recurse = opts.include?('-r') ? true : false
        force = opts.include?('-f') ? true : false

        if dry
            verbose = true
        end

        self.new(dry, verbose, recurse, force)
    end
end

def approve(path)
    print "Rename inside? #{path} "
    char = STDIN.gets.chomp[0]&.downcase or 'y'
    return char == 'y'
end

def re_sort(dir, parent, width)
    re = /^#{Regexp.escape(parent)}-\d{#{width}}/
    matched, unmatched = dir
        .partition {|f| f.basename.to_s =~ re}
        .map {|fs| fs.sort}
    return matched |= unmatched
end

def mv(parent, conf)
    path = Pathname.new(parent).expand_path
    files, dirs = path.each_child
        .partition {|f| f.file?}

    if files.length != 0
        width = Math::log10(files.length).ceil
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

opts, args = ARGV.partition {|x| x =~ /^-.*/}
conf = Config::parseopts(opts)
args.each do |a|
    if !conf.force and !approve(a)
        next
    end
    mv(a, conf)
end