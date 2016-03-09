(* test RE to NFA *)
#use "nfa.ml"

let test_accept m str =
    print_endline 
        ("accept(" ^ str ^ ") = " ^ 
        (string_of_bool (Nfa.accept m str) ));;

let r = Nfa.Char('a');;
let m = Nfa.regexp_to_nfa r;;
test_accept m "";;
test_accept m "a";;
test_accept m "b";;
test_accept m "ba";;

let r = Nfa.Union(Nfa.Char('a'),Nfa.Char('b'));;
let m = Nfa.regexp_to_nfa r;;
test_accept m "";;
test_accept m "a";;
test_accept m "b";;
test_accept m "ba";;
