#!/usr/bin/env ruby

# 
# Purpose:
#   This script is for easily creating new websites with different boilerplates
#     to minimize the amount of wasted time downloading and renaming files
# 
# Installation:
#   1. Put this script somewhere permanent where you can call it from
#   2. In terminal run `chmod 755 newSite.rb` # Or whatever you name the script
#   3. Optionally add it to your terminal aliases with something like:
#     `alias ns="~/Dropbox/Scripts/newSite.rb"`
# 
# Info:
#   + This is only compatible with OS X and probably Linux
#   + This was tested with Ruby version 1.9.3p194 on OS X
#   + On OS X you'll probably need [Homebrew](http://mxcl.github.com/homebrew/)
#       to install these dependencies 
# 
# Dependencies:
#   + You must have `wget` installed. On OS X install with `brew install wget`
#   + You must have `git` installed. On OS X install with `brew install git`
#   + Launchy gem, install with `[sudo] gem install launchy`
#   + Colorize gem, install with `[sudo] gem install colorize`
# 
# Usage:
#   + In terminal navigate to the directory you'd like to create the new website in.
#       EX: `cd ~/Dropbox/Sites/`
#   + Run this script with the desired flags in the format:
#       `newSite.rb [switches] [folder name]
#       Run the script with the -help flag for switch descriptions
#   + If you chose to set up the alias earlier this may look like:
#       `ns -h -s myAwesomeSite`
# 
# Notes:
#   + You can combine flags however you'd like
#       EX: `-h -b -s` == `-hbs`
#   + You can easily customize this to add new sources,
#       look for the next block comment for more info
# 
# Possible Improvements:
#   + This would be better off using a gem to deal with git repos, downloads, and unzipping. Unfortunately it seems most of the gems for using git are abandoned.
#   + Add more possible repos, suggestions would be greatly appreciated
#   + Suggestions, I'd love to hear any great ideas you have!
#   

# The name for the site folder when no arguments are passed
default_folder_name = "newSite"

require 'rubygems'

# Require 'Pathname' class http://www.ruby-doc.org/stdlib-1.9.3/libdoc/pathname/rdoc/Pathname.html
require 'pathname'

# This requires the 'launchy' gem. Install with '[sudo] gem install launchy'
require 'launchy'

# This requires the 'colorize' gem. Install with '[sudo] gem install colorize'
require 'colorize'


# Class for repos. You must set the URL and switch fields
class Repo
  attr_accessor :url
  attr_accessor :is_zip
  attr_accessor :is_main
  attr_accessor :switch

  @@repos = Array.new

  def initialize
    @url = ""
    @switch = ""
    @is_zip = false
    @is_main = false
  end
  
  def url=(passedURL)
    @url = passedURL
    @@repos << self
  end

  def self.getRepos
    @@repos
  end
end


# Create each repo, make sure to add a switch and URL
# If you want it to be cloned into the base directory set the is_main flag
# If the source is a zip file set the is_zip flag
html5Boilerplate = Repo.new
html5Boilerplate.url = "git://github.com/h5bp/html5-boilerplate.git"
html5Boilerplate.switch = "h"
html5Boilerplate.is_main = true

bootstrap = Repo.new
bootstrap.url = "http://twitter.github.com/bootstrap/assets/bootstrap.zip"
bootstrap.is_zip = true
bootstrap.switch = "b"

skeleton = Repo.new
skeleton.url = "git://github.com/dhgamache/Skeleton.git"
skeleton.switch = "s"

foundation = Repo.new
foundation.url = "https://github.com/zurb/foundation/archive/v3.2.2.zip"
foundation.is_zip = true
foundation.switch = "f"

#
# Add custom repos below this set of comments.
# Use something like this:
# 
# reponame = Repo.new
# reponame.url = "url to git repo or zip file"
# reponame.switch = "x" # some single letter switch used to represent this repo
# reponame.is_zip = # set this to true if the file you're downloading is a zip archive
# reponame.is_main = # set to true if you always want that repo to expand in the root directory of your project, otherwise they will expand into their default folder
#

# Method for cloning the different repos. Unfortunately all the git related gems are deprecated or abandoned
def clone_repo(repo, intoBase)
  if repo.is_zip
    download_and_unzip(repo)
  else
    puts "Cloning into #{ repo.url }...".green

    # Get the repo at the address name it's folder the last part of the repo URL without anything after a '.'
    if intoBase or repo.is_main
      %x[git clone '#{ repo.url }' '.']
    else
      %x[git clone '#{ repo.url }']
    end
    puts "Cloned #{ repo.url }".green
  end
end

# Method for downloading and unzipping non-git repos
# Should change this to use some native ruby or gem method
def download_and_unzip(repo)
  puts "Downloading #{ repo.url }...".green
  fileName = Pathname.new(repo.url).basename

  %x[ wget '#{ repo.url }' ]
  %x[ unzip '#{ fileName }' ]
  %x[ rm '#{ fileName }']
  puts "Downloaded #{ fileName }".green
end

case ARGV.count
when 0
  puts "Invalid number of arguments use -help for instructions"
else
  # Print help
  if ARGV.include?("-help")
    puts "Usage: newSite [switches] [new folder name]\n-h use HTML5Boilerplate\n-b use Twitter bootstrap\n-s use the responsive Skeleton framework\n-nr don't rename hidden files"
  elsif
    # Initalize the folder name string and repos array
    repos_to_clone = Array.new
    new_folder_name = String.new 
    switches = String.new

    ARGV.delete_if do |arg|
      if arg.start_with?("-")
        switches = "#{ switches }#{ arg[1..-1] }"
        true
      end
    end

    Repo.getRepos.each do |repo|
      if switches.include? repo.switch
        switches = switches.gsub(repo.switch, "")
        repos_to_clone << repo
      end
    end
  
    # If the 'nr' switch is passed set the rename boolean to false
    rename = true
    if switches.include? "nr"
      switches = switches.gsub("nr", "")
      rename = false
    end

    switches.split(//).each { |switch| puts "No repo with the switch '#{ switch }' was not found".red }

    # Check what's left in the array, use it as the folder name or error
    case ARGV.count
    when 0
    when 1
      default_folder_name = ARGV[0]
    else
      puts "Invalid number of arguments use -help for the correct syntax".red
      exit
    end

    # Loop until the folder name isn't take
    new_folder_name = default_folder_name
    count = 0
    while File.directory?(new_folder_name)
      count += 1
      new_folder_name = "#{ default_folder_name }#{ count }"
    end

    puts "Creating Folder named #{ new_folder_name }...".green

    Dir.mkdir(new_folder_name)
    Dir.chdir(new_folder_name)

    case repos_to_clone.count
    when 1
      clone_repo(repos_to_clone[0], TRUE)
    else
      repos_to_clone.each do |repo|
        clone_repo(repo, FALSE)
      end
    end
    
    # If the '-nr' switch wasn't passed rename all hidden non directories to example.name so they don't go unoticed
    if rename
      puts "Renaming some files..."
      Dir.glob("**/.*") { |x|
        path = Pathname.new(x)
        if !path.directory? 
          puts "Renaming #{ x } to #{ path.dirname }/example#{ path.basename }".yellow
          File.rename("#{ x }", "#{ path.dirname }/example#{ path.basename }")
        end
      }
    end

    # Open folder in file browser
    puts "Opening folder..."
    Launchy.open('.')
    puts "Done.".green
  end
end

