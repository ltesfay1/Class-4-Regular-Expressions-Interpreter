(* test NFA accept *)
#use "nfa.ml"

let test_accept m str =
    print_endline 
	("accept(" ^ str ^ ") = " ^ 
	(string_of_bool (Nfa.accept m str) ));;

let m = Nfa.make_nfa 0 [1] [(0, Some 'a', 1)];;
test_accept m "";;
test_accept m "a";;
test_accept m "b";;
test_accept m "ba";;

let m = Nfa.make_nfa 0 [2] [(0, Some 'a', 1); (0, Some 'b', 2)];;
test_accept m "";;
test_accept m "a";;
test_accept m "b";;
test_accept m "ba";;
