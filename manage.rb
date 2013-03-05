#!/usr/bin/env ruby

# All the files that need to be linked
$FILES = ["aliases", "default-gems", "gemrc", "gitconfig", "gitignore_global", "hgrc", "slate", "tm_properties", "zshrc"]
$PATHS = { "default-gems" => ".rbenv" }
$NO_DOT = [ "default-gems" ]
$VIM_FILES = ["gvimrc", "vim", "vimrc"]

def newPath(filename)
  path = ""
  if $PATHS[filename]
    path = "#{ $PATHS[filename] }/"
  end
  
  if !$NO_DOT.include? filename
    filename = ".#{ filename }"
  end

  "#{ Dir.home }/#{ path }#{ filename }"
end

def buildCMD(filename)
  "ln -s #{ Dir.pwd }/#{ filename } #{ newPath(filename) }"
end

def buildRemoveCMD(filename)
  "rm #{ newPath(filename) }"
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

def remove(filename)
  file_path = newPath(filename)
  if file_path
    print %x[#{ buildRemoveCMD(filename) }]  
  else
    puts "#{ filename } doesn't exist in #{ Dir.home }"
  end
end

def die
  puts "Usage ./manage.rb {install|remove}"
  exit
end

def installLinks
  $FILES.each { |file| link(file) }

  Dir.chdir("vim");
  $VIM_FILES.each { |file| link(file) }
end

def removeLinks
  $FILES.each { |file| remove(file) }
  $VIM_FILES.each { |file| remove(file) }
end


if ARGV.count < 1 || ARGV.count > 1
  die
end

if ARGV.first.casecmp("install") == 0
  installLinks
  puts %x[vim +BundleInstall +qall]
elsif ARGV.first.casecmp("remove") == 0
  removeLinks
else
  die
end
