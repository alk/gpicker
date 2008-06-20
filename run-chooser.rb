#!/usr/bin/ruby

path = ARGV[0] || raise('give me path')
chooser = File.join(File.dirname(__FILE__), 'filechooser')

cmdline = "(cd #{path}; find '!' -wholename '*.git/*' -a '!' -wholename '*.hg/*' -a '!' -wholename '*.svn/*' -type f -print0) | (cd #{File.dirname(__FILE__)}; #{chooser})"

answer = `#{cmdline}`
answer = eval(answer)
if answer
  answer = File.join(path, answer)
end

puts answer.inspect
