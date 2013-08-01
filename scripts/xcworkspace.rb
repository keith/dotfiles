#!/usr/bin/env ruby

require 'rubygems'
require 'colored'

path_array = Array.new

Dir.foreach(".") { |x| path_array << x if x.include? "xcworkspace" }

if path_array.count == 1
  system %{ open "#{ path_array[0] }" }
  puts "Opening #{ path_array[0] }".green
elsif path_array.count > 1
  path_array.each { |x| puts "#{ path_array.index(x) + 1 }: #{ x }" }

  print("Enter a number to open: ")
  num = gets.to_i

  if num > path_array.count || num < 1
    puts "That doesn't seem reasonable"
  elsif num <= path_array.count
    num -= 1
    system %{ open "#{ path_array[num] }" }
    puts "Opening #{ path_array[num] }".green
  end
else
  puts "No xcworkspace file found in #{ Dir.pwd }".yellow
  puts "Looking for xcodeproj files..."
  Dir.foreach(".") { |x| path_array << x if x.include? "xcodeproj" }

  if path_array.count == 1
    system %{ open "#{path_array[0] }" }
    puts "Opening #{ path_array[0] }".green
  elsif path_array.count > 1
    path_array.each { |x| puts "#{ path_array.index(x) + 1 }: #{ x }" }

    print("Enter a number to open: ")
    num = gets.to_i

    if num > path_array.count || num < 1
      puts "That doesn't seem reasonable"
    elsif num <= path_array.count
      num -= 1
      system %{ open "#{ path_array[num] }" }
      puts "Opening #{ path_array[num] }".green
    end
  else
    puts "No xcworkspace or xcodeproj files in #{ Dir.pwd }".red
  end
end
