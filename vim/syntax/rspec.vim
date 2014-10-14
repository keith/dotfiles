setlocal makeprg=rspec\ %

" Force Dispatch to run :Dispatch over :Make for better output
let b:dispatch=&makeprg
