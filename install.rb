#!/usr/bin/env ruby

# All the files that need to be linked
$FILES = ["aliases", "default-gems", "gitconfig", "gitignore_global", "hgrc", "slate", "tm_properties", "zshrc"]
$PATHS = { "default-gems" => ".rbenv" }
$NO_DOT = [ "default-gems" ]
$VIM_FILES = ["gvimrc", "vim", "vimrc"]

def newPath(filename)
  if !$NO_DOT.include? filename
    filename = ".#{ filename }"
  end

  path = ""
  if $PATHS[filename]
    path = "#{ $PATHS[filename] }/"
  end

  "#{ Dir.home }/#{ path }#{ filename }"
end

def buildCMD(filename)
  "ln -s #{ Dir.pwd }/#{ filename } #{ newPath(filename) }"
end

def error(filename)
  puts "#{ filename } doesn't exist in #{ Dir.pwd }"
  exit
end

def link(filename)
  error(filename) unless File.exists? filename
  if File.exists? newPath(filename)
    puts "#{ filename } already exists at #{ newPath(filename) }"
    return
  end

  print %x[#{ buildCMD(filename) }]
end

$FILES.each { |file| link(file) }

Dir.chdir("vim");
$VIM_FILES.each { |file| link(file) }

