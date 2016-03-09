#!/usr/bin/ruby -w

tests = [ 
	"public_NFA_closure", 
	"public_NFA_move", 
	"public_NFA_accept",
	"public_RE_to_str", 
	"public_RE_to_NFA", 
	"public_str_to_NFA" 
]

tests.each { |x|
	system("ocaml #{x}.ml > #{x}.log")
	if $? != 0
		puts "#{x} failed: run-time error"
	end
	system("echo ---------------------------------")
	system("echo Comparing output for #{x}")

	system("echo choose fc for Windows or diff for Apple or Unix in goTest.rb")
	# system("diff #{x}.log #{x}.out") # Apple/Unix
	# system("fc #{x}.log #{x}.out")   # Windows/DOS
}
