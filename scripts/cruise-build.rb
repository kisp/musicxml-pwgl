def safe_system(command)
  puts command
  system command
  res = $?
  if not (res == 0) then
    puts "`" + command + "'"
    puts "returned " + res.to_s
    exit 1
  end
end

safe_system "git clean -f -x -d"
safe_system "sbcl --script scripts/cruise-build.lisp"
