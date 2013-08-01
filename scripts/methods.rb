#!/usr/bin/env ruby

#
# This is a simple script for printing out the methods in your Objective-C .h or .m file.
# It reads the passed file and stores just the method names in a text file
#

# Default filename and type for the file output
$DEFAULT_FILENAME = "out_file"
$DEFAULT_FILE_EXTENSION = "txt"

# Dependiencies
require 'rubygems'

# This requires the 'colorize' gem. Install with '[sudo] gem install colorize'
require 'colored'

# This requires the 'launchy' gem. Install with '[sudo] gem install launchy'
require 'launchy'

def print_help
  puts "Usage: methods [filename]\nThis script will then create a file named #{ $DEFAULT_FILENAME } with a list of methods in the passed file".yellow
end

case ARGV.count
when 0
  puts "You must enter a filename to read. Use -help for instructions".red
when 1
  if ARGV[0] == "-h" or ARGV[0] == "-help"
    print_help
    exit
  end

  filename = ARGV[0]
  if File.exists?(filename)
    if File.readable?(filename)
      in_file = File.open(filename, 'r')

      methods = Array.new
      in_file.each { |line| methods << line if line.start_with?("-") or line.start_with?("+") }
      in_file.close()

      out_filename = "#{ $DEFAULT_FILENAME }.#{ $DEFAULT_FILE_EXTENSION }" 
      filename_count = 0
      while File.exists?(out_filename)
        filename_count += 1
        out_filename = "#{ $DEFAULT_FILENAME }#{ filename_count }.#{ $DEFAULT_FILE_EXTENSION }"
      end
      
      out_file = File.new(out_filename, "w")
      if File.exists?(out_file)
        methods.each { |line| out_file.write(line) }
        out_file.close()
        
        puts "Wrote #{ methods.count } methods/functions to #{ out_filename }".green
        Launchy.open(out_filename)
      else
        puts "Couldn't write to: #{ out_filename }".red
      end
    else
      puts "The file named: #{ filename } is unreadable by the current user".red
    end
  else
    puts "File named: #{ filename } doesn't exist".red
  end
else
  print_help
end
