(* test NFA e_closure *)

let string_of_int_list l =
    let rec string_of_int_elements l = match l with
          [] -> ""
        | (h::[]) -> string_of_int h
        | (h::t) -> string_of_int h ^ ";" ^ string_of_int_elements t
    in "[" ^ string_of_int_elements l ^ "]"
;;

#use "nfa.ml"

let m = Nfa.make_nfa 0 [1] [(0, Some 'a', 1)];;
print_endline ( string_of_int_list (Nfa.e_closure m [0]) ) ;;
print_endline ( string_of_int_list (Nfa.e_closure m [1]) ) ;;

let m = Nfa.make_nfa 0 [1] [(0, None, 1)];;
print_endline ( string_of_int_list (Nfa.e_closure m [0]) ) ;;
print_endline ( string_of_int_list (Nfa.e_closure m [1]) ) ;;

let m = Nfa.make_nfa 0 [2] [(0, Some 'a', 1); (0, Some 'b', 2)];;
print_endline ( string_of_int_list (Nfa.e_closure m [0]) ) ;;
print_endline ( string_of_int_list (Nfa.e_closure m [1]) ) ;;
print_endline ( string_of_int_list (Nfa.e_closure m [2]) ) ;;

let m = Nfa.make_nfa 0 [2] [(0, None, 1); (0, None, 2)];;
print_endline ( string_of_int_list (Nfa.e_closure m [0]) ) ;;
print_endline ( string_of_int_list (Nfa.e_closure m [1]) ) ;;
print_endline ( string_of_int_list (Nfa.e_closure m [2]) ) ;;

