# Ruby 1.8.7 compatible
require 'rubygems'
require 'irb/completion'

# Interactive editor: use vim from within irb
begin
  require 'interactive_editor'
rescue LoadError => err
end

# Awesome print
begin
  require 'awesome_print'
  AwesomePrint.irb!
rescue LoadError => err
end

# Use >> as prompt
IRB.conf[:PROMPT_MODE] = :SIMPLE

IRB.conf[:AUTO_INDENT] = true

# Save more history
IRB.conf[:EVAL_HISTORY] = 1000
IRB.conf[:SAVE_HISTORY] = 1000
IRB.conf[:HISTORY_FILE] = File::expand_path("~/.irbhistory")
