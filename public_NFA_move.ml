(* test NFA move *)

let string_of_int_list l =
    let rec string_of_int_elements l = match l with
          [] -> ""
        | (h::[]) -> string_of_int h
        | (h::t) -> string_of_int h ^ ";" ^ string_of_int_elements t
    in "[" ^ string_of_int_elements l ^ "]"
;;

#use "nfa.ml"

let m = Nfa.make_nfa 0 [1] [(0, Some 'a', 1)];;
print_endline ( string_of_int_list (Nfa.move m [0] 'a') ) ;;
print_endline ( string_of_int_list (Nfa.move m [1] 'a') ) ;;

let m = Nfa.make_nfa 0 [1] [(0, None, 1)];;
print_endline ( string_of_int_list (Nfa.move m [0] 'a') ) ;;
print_endline ( string_of_int_list (Nfa.move m [1] 'a') ) ;;

let m = Nfa.make_nfa 0 [2] [(0, Some 'a', 1); (0, Some 'b', 2)];;
print_endline ( string_of_int_list (Nfa.move m [0] 'a') ) ;;
print_endline ( string_of_int_list (Nfa.move m [1] 'a') ) ;;
print_endline ( string_of_int_list (Nfa.move m [2] 'a') ) ;;
print_endline ( string_of_int_list (Nfa.move m [0] 'b') ) ;;
print_endline ( string_of_int_list (Nfa.move m [1] 'b') ) ;;
print_endline ( string_of_int_list (Nfa.move m [2] 'b') ) ;;

let m = Nfa.make_nfa 0 [2] [(0, None, 1); (0, Some 'a', 2)];;
print_endline ( string_of_int_list (Nfa.move m [0] 'a') ) ;;
print_endline ( string_of_int_list (Nfa.move m [1] 'a') ) ;;
print_endline ( string_of_int_list (Nfa.move m [2] 'a') ) ;;
print_endline ( string_of_int_list (Nfa.move m [0] 'b') ) ;;
print_endline ( string_of_int_list (Nfa.move m [1] 'b') ) ;;
print_endline ( string_of_int_list (Nfa.move m [2] 'b') ) ;;

