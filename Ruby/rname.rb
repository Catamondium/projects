#!/usr/bin/ruby
require 'pathname'
Config = Struct.new(:dry, :verbose, :recurse)

class Pathname
    def resolve
        Pathname.new(self.to_s.sub('~', ENV['HOME'])).cleanpath
    end
end

def re_sort(dir, parent, width)
    re = /^#{Regexp.escape(parent)}-\d{#{width}}/
    matched, unmatched = dir
        .partition {|f| f.basename.to_s =~ re}
        .map {|fs| fs.sort}
    return matched |= unmatched
end

def mv(parent)
    path = Pathname.new(parent).resolve
    dirs, files = path.entries
        .reject {|f| f.basename.to_s == '.' or f.basename.to_s == '..'}
        .partition {|f| f.directory?}
    
    width = Math::log(10, files.length).ceil + 1
    files = re_sort(files, path.basename.to_s, width)
    files.each_with_index do |f, i|
        n = "%s-%0*d%s" % [path.basename, width, i, f.extname]
        ## TODO Renaming & flags
    end
end

mv("~/Downloads/DT")