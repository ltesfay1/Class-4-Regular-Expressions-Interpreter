(* test RE to string *)
#use "nfa.ml"

let r = Nfa.Concat(Nfa.Char('a'), Nfa.Char('b'));;
print_endline (Nfa.regexp_to_string r) ;;

let r = Nfa.Union(Nfa.Char('c'), Nfa.Char('d'));;
print_endline (Nfa.regexp_to_string r) ;;

let r = Nfa.Star(Nfa.Char('e'));;
print_endline (Nfa.regexp_to_string r) ;;
