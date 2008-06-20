#!/usr/bin/ruby

path = ARGV[0] || raise('give me path')
chooser = File.join(File.dirname(__FILE__), 'filechooser')

exec "(cd #{path}; find '!' -wholename '*.git/*' -a '!' -wholename '*.hg/*' -a '!' -wholename '*.svn/*' -type f -print0) | (cd #{File.dirname(__FILE__)}; #{chooser})"
