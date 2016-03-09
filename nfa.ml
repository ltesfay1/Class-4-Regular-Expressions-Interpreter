(* CMSC 330 / Spring 2015 / Project 4 *)
(* Name: ?? *)

#load "str.cma"
(* ------------------------------------------------- *)
(* MODULE SIGNATURE *)
(* ------------------------------------------------- *)

module type NFA =
  sig
    (* You may NOT change this signature *)

    (* ------------------------------------------------- *)
    (* PART 1: NFA IMPLEMENTATION *)
    (* ------------------------------------------------- *)

    (* ------------------------------------------------- *)
    (* Abstract type for NFAs *)
    type nfa

    (* Type of an NFA transition.

       (s0, Some c, s1) represents a transition from state s0 to state s1
       on character c

       (s0, None, s1) represents an epsilon transition from s0 to s1
     *)
    type transition = int * char option * int 

    (* ------------------------------------------------- *)
    (* Returns a new NFA.  make_nfa s fs ts returns an NFA with start
       state s, final states fs, and transitions ts.
     *)
    val make_nfa : int -> int list -> transition list -> nfa

    (* ------------------------------------------------- *)
    (*  Calculates epsilon closure in an NFA.  

	e_closure m ss returns a list of states that m could 
	be in, starting from any state in ss and making 0 or 
	more epsilon transitions.

       There should be no duplicates in the output list of states.
     *)

    val e_closure : nfa -> int list -> int list

    (* ------------------------------------------------- *)
    (*  Calculates move in an NFA.  

	move m ss c returns a list of states that m could 
	be in, starting from any state in ss and making 1
	transition on c.

       There should be no duplicates in the output list of states.
     *)

    val move : nfa -> int list -> char -> int list

    (* ------------------------------------------------- *)
    (* Returns true if the NFA accepts the string, and false otherwise *)
    val accept : nfa -> string -> bool

    (* ------------------------------------------------- *)
    (* PART 2: REGULAR EXPRESSION IMPLEMENTATION *)
    (* ------------------------------------------------- *)

    (* ------------------------------------------------- *)
    type regexp =
	Empty_String
      | Char of char
      | Union of regexp * regexp
      | Concat of regexp * regexp
      | Star of regexp

    (* ------------------------------------------------- *)
    (* Given a regular expression, print it as a regular expression in 
       postfix notation (as in project 2).  Always print the first regexp 
       operand first, so output string will always be same for each regexp.
     *)
    val regexp_to_string : regexp -> string 

    (* ------------------------------------------------- *)
    (* Given a regular expression, return an nfa that accepts the same
       language as the regexp
     *)
    val regexp_to_nfa : regexp -> nfa

    (* ------------------------------------------------- *)
    (* PART 3: REGULAR EXPRESSION PARSER *)
    (* ------------------------------------------------- *)

    (* ------------------------------------------------- *)
    (* Given a regular expression as string, parses it and returns the
       equivalent regular expression represented as the type regexp.    
     *)
    val string_to_regexp : string -> regexp

    (* ------------------------------------------------- *)
    (* Given a regular expression as string, parses it and returns 
       the equivalent nfa 
     *)
    val string_to_nfa: string -> nfa

    (* ------------------------------------------------- *)
    (* Throw IllegalExpression expression when regular
       expression syntax is illegal
     *)
    exception IllegalExpression of string

end

(* ------------------------------------------------- *)
(* MODULE IMPLEMENTATION *)
(* ------------------------------------------------- *)

    (* Make all your code changes past this point *)
    (* You may add/delete/reorder code as you wish 
       (but note that it still must match the signature above) *)

module NfaImpl =
struct
   let rec fold f a l = match l with
       [] -> a
         | (h::t) -> fold f (f a h) t
   ;;
let rec map f l = match l with
    [] -> []
  | (h::t) -> (f h)::(map f t)
;;
   type transition = int * char option * int;;

type nfa =  Constructor of int*int list*transition list;;

let make_nfa ss fs ts = Constructor (ss,fs,ts);;

let rec contain x l = match l with
[] -> false
| h::t -> if x = h then true else (contain x t)
;;


let rec aux element translist = 
   fold (fun acc (x,y,z) -> 
 if x = element && y = None && contain z acc = false then (z::acc) else acc) [element] translist
;;


let compare a b = if a > b then 1 else if a < b then -1 else 0;;
let rec uniq s = match s with
[] -> []
| h1::h2::t1 when h1 = h2 -> h1::uniq t1
| h3::t2 -> h3::uniq t2
;;

let rec e_closure m ss = match m with
Constructor (x,y,z) -> let b = uniq (List.sort compare (fold (fun a h -> (uniq (List.sort compare ((aux h z)@a)))) [] ss)) in 
if b = ss then ss else uniq (List.sort compare (e_closure m b))
;;

let move m ss c = match m with
 Constructor (x1,y1,z1) ->  uniq (List.sort compare (fold (fun a h -> (uniq (List.sort compare ((
       fold (fun acc (x2,y2,z2) -> match y2 with
       None -> acc
| Some c1 -> if x2 = h && c1 = c && contain z2 acc = false then z2::acc else acc) [] z1)@a)))) [] ss));;


let str_to_list str1 =
  let rec char_list idx l = 
    if idx < 0 then l else char_list (idx - 1)(str1.[idx]::l) in 
      char_list (String.length str1 - 1) []
    ;;

let compare2 end_list l = 
  fold (fun a h -> a || (List.exists (fun x -> x = h) l)) false end_list;;


let aux2 m c curr_list = 
e_closure m (move m (e_closure m curr_list) c)
;;

let accept m s = 
let str_list = str_to_list s in 
match m with
Constructor (x1,y1,z1) ->  
compare2 (match str_list with
[] -> e_closure m [x1]
| _ -> fold (fun a c -> aux2 m c a) [x1] str_list) y1;;


type regexp =
	  Empty_String
	| Char of char
	| Union of regexp * regexp
	| Concat of regexp * regexp
	| Star of regexp
let rec regexp_to_string2 r = match r with
Empty_String -> "E "
| Char x -> (String.make 1 x) ^ " "
| Union (x1,x2) -> (regexp_to_string2 x1) ^ (regexp_to_string2 x2) ^ "| "
| Concat (y1,y2) -> (regexp_to_string2 y1) ^ (regexp_to_string2 y2) ^ ". "
| Star z -> (regexp_to_string2 z) ^ "* "
;;
let regexp_to_string r = let str = regexp_to_string2 r in String.sub str 0 ((String.length str)-1)
;;
let next = 
  let count = ref 0 in 
    function () -> 
      let temp = !count in
      count := (!count) + 1;
      temp;;

let make_end_trans l element = fold (fun a h -> [(h,None,element)]@a) [] l
;; 


let rec regexp_to_nfa r = match r with
Empty_String -> let a = next () in Constructor (a,[a],[])
| Char x -> let b = next () in let c = next () in Constructor (b,[c],[(b,Some x,c)])
| Union (x1, x2) -> (let l = regexp_to_nfa x1 in 
  match l with
    Constructor (y1,y2,y3) -> let k = regexp_to_nfa x2 in 
      match k with 
        Constructor (z1,z2,z3) -> let d = next () in let e = next () in 
          (Constructor (d,[e],[(d,None,y1);(d,None,z1)]@y3@z3@(make_end_trans y2 e)@(make_end_trans z2 e))))
| Concat (x3,x4) -> (let m = regexp_to_nfa x3 in let n = regexp_to_nfa x4 in 
  match m with
    Constructor (y4,y5,y6) -> 
      match n with 
        Constructor (z4,z5,z6) -> Constructor (y4,z5,(make_end_trans y5 z4)@y6@z6))
| Star z -> let p = regexp_to_nfa z in let q = next () in let r = next () in
    match p with 
      Constructor (x5,x6,x7) -> Constructor (q,[r],[(q,None,x5);(q,None,r);(r,None,q)]@x7@(make_end_trans x6 r))
;;


exception IllegalExpression of string

(************************************************************************)
(* PARSER. You shouldn't have to change anything below this point *)
(************************************************************************)

(* Scanner code provided to turn string into a list of tokens *)

type token =
   Tok_Char of char
 | Tok_Epsilon
 | Tok_Union
 | Tok_Star
 | Tok_LParen
 | Tok_RParen
 | Tok_END

let re_var = Str.regexp "[a-z]"
let re_epsilon = Str.regexp "E"
let re_union = Str.regexp "|"
let re_star = Str.regexp "*"
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"

let tokenize str =
 let rec tok pos s =
   if pos >= String.length s then
     [Tok_END]
   else begin
     if (Str.string_match re_var s pos) then
       let token = Str.matched_string s in
       (Tok_Char token.[0])::(tok (pos+1) s)
	 else if (Str.string_match re_epsilon s pos) then
       Tok_Epsilon::(tok (pos+1) s)
	 else if (Str.string_match re_union s pos) then
       Tok_Union::(tok (pos+1) s)
	 else if (Str.string_match re_star s pos) then
       Tok_Star::(tok (pos+1) s)
     else if (Str.string_match re_lparen s pos) then
       Tok_LParen::(tok (pos+1) s)
     else if (Str.string_match re_rparen s pos) then
       Tok_RParen::(tok (pos+1) s)
     else
       raise (IllegalExpression "tokenize")
   end
 in
 tok 0 str

(* 
  A regular expression parser. It parses strings matching the 
  context free grammar below.

   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen 

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let lookahead tok_list = match tok_list with
	[] -> raise (IllegalExpression "lookahead")
	| (h::t) -> (h,t)

let rec parse_S l = 
	let (a1,l1) = parse_A l in
	let (t,n) = lookahead l1 in
	match t with 
		Tok_Union -> (
		let (a2,l2) = (parse_S n) in
		(Union (a1,a2),l2) 
		)
		| _ -> (a1,l1)

and parse_A l =
	let (a1,l1) = parse_B l in
	let (t,n) = lookahead l1 in
	match t with
	Tok_Char c ->
		let (a2,l2) = (parse_A l1) in (Concat (a1,a2),l2) 
	| Tok_Epsilon ->
		let (a2,l2) = (parse_A l1) in (Concat (a1,a2),l2) 
	| Tok_LParen -> 
		let (a2,l2) = (parse_A l1) in (Concat (a1,a2),l2) 
	| _ -> (a1,l1)

and parse_B l =
	let (a1,l1) = parse_C l in
	let (t,n) = lookahead l1 in
	match t with
	Tok_Star -> (Star a1,n) 
	| _ -> (a1,l1)

and parse_C l =
	let (t,n) = lookahead l in
	match t with
   	  Tok_Char c -> (Char c, n)
	| Tok_Epsilon -> (Empty_String, n)
	| Tok_LParen -> 
		let (a1,l1) = parse_S n in
		let (t2,n2) = lookahead l1 in
		if (t2 = Tok_RParen) then
			(a1,n2)
		else
			raise (IllegalExpression "parse_C 1")
	| _ -> raise (IllegalExpression "parse_C 2")

let string_to_regexp str = 
	let tok_list = tokenize str in
	let (a,t) = (parse_S tok_list) in
	match t with
	[Tok_END] -> a
	| _ -> raise (IllegalExpression "string_to_regexp")

let string_to_nfa s = regexp_to_nfa (string_to_regexp s)

end

module Nfa : NFA = NfaImpl;;
