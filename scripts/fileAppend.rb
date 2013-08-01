#!/usr/bin/env ruby

def files
  Dir.glob('*')
end

if ARGV.count > 1 || ARGV.count < 1
  puts "Incorrect parameters"
  exit
end

file_name = File.basename(__FILE__)
append_string = ARGV[0].strip

files.each do |file|
  if file == file_name
    next
  end

  base = File.basename(file, '.*')
  extension = File.extname(file)

  new_name = "#{ base } #{ append_string }#{ extension }"

  File.rename(file, new_name)
  puts "Renamed #{ file } => #{ new_name }"
end

