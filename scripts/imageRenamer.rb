#!/usr/bin/env ruby

require 'rubygems'
require 'colored'

$PREFIX = "Photo"
$REGEX = /.*Photo\s\d\s?.*/

accepted_types = ["jpg", "jpeg", "png"]

def stripFileName(name)
  while name =~ $REGEX
    text = name.match(/Photo\s\d\s?/).to_s
    name.slice!(text)
  end
  name
end

count = 1
Dir.glob('*').each do |file|
  extension = File.extname(file).gsub('.', '').downcase
  if !accepted_types.include? extension
    puts "Not renaming #{ file }".yellow
    next
  end

  base = File.basename(file, '.*')

  stripped = stripFileName(base.dup).strip

  new_name = "#{ $PREFIX } #{ count } #{ stripped }.#{ extension }"
  if new_name != file
    File.rename(file, new_name)
    puts "Renamed #{ file } => #{ new_name }"
  else
    puts "Not renaming #{ file }".yellow
  end

  count += 1
end

