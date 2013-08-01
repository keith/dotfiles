#!/usr/bin/env ruby

require 'rubygems'

# This requires the 'clipboard' gem. Install this gem with `[sudo] gem install clipboard` Github repo: https://github.com/janlelis/clipboard
require 'clipboard'

# Change the URL to your base cloudfront URL be sure to include the trailing /
service_url = "http://da61i03e1y72w.cloudfront.net/"

# Change this to whatever folderpath your files live in be sure to include the trailing /
remove_folder = "images/"

file_names = Array.new
Dir.foreach(".") { |x|	file_names << x if !x.start_with?(".") }

file_name_output = ""
file_names.each { |x| file_name_output += "#{ service_url }#{ remove_folder }#{ x }\n" }

Clipboard.copy file_name_output
puts "Copied URLs to clipboard"
