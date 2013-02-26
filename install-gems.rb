#!/usr/bin/env ruby

$FILE = "default-gems"

contents = File.read $FILE
gems = contents.split(/\n/)

gems.each do |gem|
  print %x[gem install "#{ gem }"]
end

