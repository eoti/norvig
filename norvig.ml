open Printf
open List
open Int64

let _s = sprintf and _sl = String.length and _ll  = List.length
(********* Tokenize a string s containing a lisp-like expression. *************)

type lex = {index: int; line: int; offset: int}
type token = {sym: string; dbg: lex}
type a = TSTR of int | TSYM of int | TCMT | TWS 

let tokenize s =
	let n = _sl s in
	let str a b = String.sub s a (b - a + 1) in
	let rec f i state dbg r =
		if i >= n then r else
		let j = i + 1 and k = i - 1 and esc = (i != 0) && s.[i-1] = '\\' 
		and ndbg = if s.[i] = '\n' then 
			{ index = i; line = dbg.line + 1; offset = 0 } else 
			{ index = i; line = dbg.line; offset = dbg.offset + 1 }
		in 
		let tk i j = { sym = str i j; dbg = dbg } in
		match s.[i] with
		| ' ' | '\n' | '\t' | '\r' ->
				(match state with 
				 | TSYM x -> f j TWS ndbg ((tk x k)::r)
				 | TCMT   -> if s.[i] = '\n' then f j TWS ndbg r else f j state ndbg r
				 | _     -> f j TWS ndbg r)
		|'"' -> (match state with
                 | TSTR x -> if esc then f j state ndbg r else f j TWS ndbg ((tk x i) :: r)
		         | TSYM x -> f j (TSTR i) ndbg ( (tk x k) :: r)
				 | TWS    -> f j (TSTR i) ndbg r
				 | _     -> f j state ndbg r)
		|';' -> (match state with
				 | TSYM x -> f j TWS ndbg ((tk x k) :: r)
				 | _     -> f j TCMT ndbg r)
		| '(' | ')' | '\'' ->
				(match state with
				 | TSYM x -> f j TWS ndbg ((tk i i) :: ((tk x k) :: r))
				 | TWS    -> f j TWS ndbg ((tk i i) :: r)
				 | _     -> f j state ndbg r)
		| _  -> (match state with
				 | TWS    -> f j (TSYM i) ndbg r
				 | _     -> f j state ndbg r) 
	in
	rev (f 0 TWS { index = 0; line = 0; offset = 0 } [])
	
(************************** Parser ********************************************)

type lisp_expr =
	| Nil
	| Int of int64
	| Symbol of symbol
	| Cons of lisp_expr * lisp_expr

and lisp_code =
	| Quote of lisp_expr
	| Var of int
	| Application of lisp_code * (lisp_code list)
	| Closure of closure
	| Global of symbol * lisp_code

and symbol = 
	{ name: string; expr: lisp_expr; sdbg: lex option }
	
and closure =
	{ code: lisp_code; env: symbol list; args: symbol list }	
	
let rec parse lst = 
	let match_paren s1 s2 dbg lst =
		let rec f c r = function
			| [] 		->	failwith (_s "Unmatched %s on line %d" s1 dbg.line)
			| h :: t	-> 
				if h.sym = s2 then
					if c = 0 then (rev r, t) else f (c - 1) (h :: r) t
				else
					if h.sym = s1 then f (c + 1) (h :: r) t else f c (h :: r) t
		in f 0 [] lst	
	and classify s =
		try Int (of_string s.sym) 
		with _ -> Symbol { name = s.sym; expr = Nil; sdbg = Some s.dbg }
	in
	let rec cons = function
	| [] -> Nil
	| h :: t -> Cons (h, cons t) in
	match lst with
	| [] 		-> []
	| h :: t	-> 	if h.sym = "(" then
						let r2, t2 = match_paren "(" ")" h.dbg t in
						(cons (parse r2)) :: (parse t2)
					else
						(classify h) :: (parse t)						

(********************** Pretty print ******************************************)
let cata f l = String.concat " " (map f l)

let rec string_expr = function
	| Nil ->  "()"
	| Int i -> to_string i
	| Symbol { name = n } -> n
	| Cons (c,d) -> _s "(%s)" (string_cons(Cons (c, d))) 

and string_code  = function
	| Quote e		-> _s "'%s" (string_expr e)
	| Var i			-> _s "(#var %d)" i
	| Application (c, a) 	-> _s "(#application %s %s)" (string_code c) (cata string_code a)
	| Closure c 	-> _s "(#build_closure %s [e: %s][a: %s])" (string_code c.code) (cata (fun x -> x.name) c.env) (cata (fun x -> x.name) c.args)
	| Global (s, c)	-> _s "(#global_set %s %s)" s.name (string_code c)

and string_cons = function
	| Cons(car, Cons(cadr, cddr)) -> _s "%s %s" (string_expr car) (string_cons(Cons(cadr, cddr)))
	| Cons(car, Nil) -> string_expr car
	| Cons(car, cdr) -> _s "%s . %s" (string_expr car) (string_expr cdr)
	| _ -> failwith "Error in string_cons"

(************************************ Compiler ********************************)
let global_env = ref []

let retrieve f l =
	if exists f l then Some (find f l) else None

let global_set name v =
	let symbol = 
	(match (retrieve (fun n -> n.name = name) !global_env) with
	| Some x -> { name = x.name; expr = v; sdbg = x.sdbg }
	| None   -> { name = name ; expr = v; sdbg = None })
    in global_env := symbol :: !global_env ;
    symbol
				
let rec uncons = function
	| Nil 				-> []
	| Cons(car, cdr) 	-> car :: uncons cdr
	| x 				-> failwith ("Cannot uncons " ^ (string_expr x))
	
let unsym = 
	map (fun x -> 
			match x with 
			Symbol s 	-> s 
			| _ 		-> failwith ("Cannot unsym " ^ (string_expr x)))	

let rec unpair = function
	| Cons(Symbol s, Cons(e, Nil))				-> (Cons(Symbol s, Nil), Cons(e, Nil))
	| Cons(Cons(Symbol s, Cons(e, Nil)), Nil)	-> (Cons(Symbol s, Nil), Cons(e, Nil))
	| Cons(Cons(Symbol s, Cons(e, Nil)), cdr)	-> let (a, b) = unpair cdr in (Cons(Symbol s, a), Cons(e, b))
	| x -> failwith ("Cannot unpair " ^ (string_expr x))
   
let transform_let args expr =
	let (s, e) = unpair args in
	Cons(Cons(Symbol { name = "lambda"; expr = Nil; sdbg = None }, Cons(s, Cons(expr, Nil))), e) 

let rec compile p e = function
	| Symbol s -> env_lookup s p e
	| Cons(Symbol { name = "let" }, Cons(args, Cons(expr, Nil))) -> compile p e (transform_let args expr)
	| Cons(Symbol { name = "lambda" }, Cons(args, Cons(body, Nil))) -> let a = unsym (uncons args) in Closure (unfold { code = compile p (e @ a) body; env = e; args = a })
	| Cons(Symbol { name = "define" }, Cons(Symbol { name = name }, Cons(v, Nil) )) ->	Global (global_set name v, compile p e v)
	| Cons(c, Nil) -> compile p e c
	| Cons(func, args) -> (let a = uncons args in 
						   Application (compile a e func, map (compile p e) a))
	| x 	-> Quote x
	
and env_lookup s p e = 
	let rec f i = function
	| []		-> Quote (Symbol s)
	| h :: t	-> if h.name = s.name then Var i else f (i + 1) t 
	in f 0 e 

and unfold c = 
	let rec func i preplst outargs = function
		| [] -> (i, preplst, outargs)
		| Application(f, a) :: t -> 
			let (j, l, o) = func (i) preplst [] a in
			func (j+1) (l @ [Application(f, o)]) (outargs @ [Var j]) t
		| h::t -> func i preplst (outargs @ [h]) t 	
	in
	match c.code with
	| Application (f, a) -> let (i, l, o) = func (length (c.env @ c.args)) [] [] a in 
		if l = [] then
		{ code = Application (f, o); env = c.env; args = c.args}
		else
		{ code = Application (Quote Nil, l @ [Application (f, o)]); env = c.env; args = c.args}
	| _ ->  c	
	
let compile = compile [] []	
	
(************************************ Assemble ********************************)

let rec assemble = function
	| Quote (Nil) 		-> "0"
	| Quote (Int i) 	-> to_string i
	| Quote (Symbol s) 	-> s.name
	| Var i				-> _s "[rsp + %d]" ((i + 1)*8)
	| Application (Quote (Symbol s), a) -> if (ismacro s.name) then callmacro s a else "***" 
	| Application (f, a)-> _s "%s %s" (assemble f) (cata assemble a)
	| Global (s, Quote (Nil)) -> _s "%s = %s" s.name (assemble (Quote (Nil)))
	| Global (s, Quote (Int i)) -> _s "%s = %s" s.name (assemble (Quote (Int i)))
	| Global (s, Quote (Symbol n)) -> _s "%s = %s" s.name (assemble (Quote (Symbol n)))
	| Global (s, Application (f, a)) -> _s "%s:\n%s" s.name (assemble (Application (f, a)))
	| Global (s, Closure c) -> _s "%s:\n%s" s.name (assemble (Closure c))
	| Closure { code = Quote (Int i); env = e; args = a } -> _s "\t%s\n\tret %s" (aout (assemble (Quote (Int i)))) (close e a 0)
	| Closure { code = Quote (Symbol s); env = e; args = a } -> _s "\t%s\n\tret %s" (aout (assemble (Quote (Symbol s)))) (close e a 0)
	| Closure { code = c; env = e; args = a } -> _s "\t%s\n\tret %s" (assemble c) (close e a 0)
	| x					-> "Cannot assemble " ^ (string_code x)
	
and aout s = _s "mov rax, %s" s	

and close e a l =
	let n =  of_int ((l + length (e@a)) * 8) in
	assemble (Quote (Int n))	

and ismacro s =  
	s != "" && s.[0] = '.' && s = String.uppercase s
	
and callmacro s a =
	_s "%s %s" s.name (String.concat ", " (map assemble a))

(*************************** Test *********************************************)
let read_file f =
	let ic = open_in_bin f in
	let len = in_channel_length ic in
	let s = String.make len ' ' in
	let _ = really_input ic s 0 len in s

let _ =
	if (Array.length Sys.argv) < 3 then
		printf "Usage: %s <input-file> <output-file>\n" Sys.argv.(0)
	else			
		let s = read_file Sys.argv.(1) in
		let t = tokenize s in
		let p = parse t in
		let c = map compile p in
		let a = map assemble c in
		let oc = open_out_bin Sys.argv.(2) in
		output_string oc ".include \"stdlib.inc\"\n\n";
		iter2 (fun x y -> output_string oc (sprintf "/* %s */\n%s\n\n" (string_code x) y)) c a



let _ = printf "%b\n" (ismacro ".plus")
