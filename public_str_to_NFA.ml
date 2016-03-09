(* test parsing string to RE, then to NFA *)
#use "nfa.ml"

let test_accept m str =
    print_endline 
        ("accept(" ^ str ^ ") = " ^ 
        (string_of_bool (Nfa.accept m str) ));;

let r = Nfa.string_to_regexp "ab";;
let m = Nfa.regexp_to_nfa r;;

test_accept m "a" ;;
test_accept m "b" ;;
test_accept m "ab" ;;
test_accept m "bb" ;;
