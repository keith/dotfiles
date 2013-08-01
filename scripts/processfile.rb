#!/usr/bin/env ruby

# A simple script to read a text file of URLs, one on each line
# It then opens them in the default web browser

# WARNING: This script has currently only been tested on OS X. Unfortunately the cross platform solution of using Launchy to open URLs exits after the first URL. The current Linux and Windows solutions are untested, any suggestions/testing would be appreciated

# Requires some awesome gems
require 'rubygems'

# Requires the 'colorize' gem. Install with `[sudo] gem install colorize`
require 'colorize'

# Requires the 'launchy' gem. Install with `[sudo] gem install launchy`
require 'launchy'

# Requires the 'os' gem. Install with `[sudo] gem install os`
require 'os'

def printUsage
  puts "Usage #{ File.basename(__FILE__) } [-g] file_name".red
  exit
end

# Verify there is a valid number of arguments
if ARGV.count > 2 || ARGV.count == 0
  printUsage
end

def openURL(url)
  puts "Opening #{ url }".green
  # Check the version of the OS and attempt to use the Appropriate solution
  if OS.mac?
    system('open', url)
  elsif OS.linux
    system('xdg-open', url)
  elsif OS.windows?
    system('start', url)
  else
    Launchy.open(url)
    puts "Your current Ruby platform is unsupported. Please let me know how you're running ruby so I can fix this.".red
  end
end

# Get the passed file name
file_name = ARGV.shift

# Set a flag for googling to false
google_lines = false

# Check to see if the first argument was '-g'
if file_name.chomp == "-g"
  google_lines = true

  # Make sure there is another argument
  if ARGV.count == 0
    printUsage
  else
    # Get the second argument as the file name
    file_name = ARGV.shift
  end
end

# Make sure the file exists
if File.exists?(file_name)
  # Make sure the file is readable
  if File.readable?(file_name)
    
    # Open the file for reading
    in_file = File.open(file_name, 'r')

    if google_lines
      # To google each line prepend the line with the google search URL
      in_file.each { |line| openURL("https://www.google.com/search?q=#{ line.strip }")}
    else
      # Loop through each URL and call the open URL function with the trimmed URL
      in_file.each { |line| openURL(line.strip) }
    end
  else
    puts "The file #{ file_name } is not readable".red
    exit
  end
else
  puts "The file #{ file_name } does not exist".red
  exit
end


