open Printf
open List
open Int64

let _s = sprintf and _sl = String.length

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
	| Local of int
	| Var of int
	| Application of lisp_code * (lisp_code list)
	| Closure of closure
	| Function of closure
	| Global of symbol * lisp_code

and symbol = 
	{ name: string; expr: lisp_expr; sdbg: lex option }
	
and closure =
	{ code: lisp_code; env: symbol list; args: symbol list; local: int }	
	
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
	| Local i		-> _s "(#local %d)" i
	| Var i			-> _s "(#var %d)" i
	| Application (c, a) 	-> _s "(#application %s %s)" (string_code c) (cata string_code a)
	| Function c 	-> _s "(#function %s [e: %s][a: %s][l: %d])" (string_code c.code) (cata (fun x -> x.name) c.env) (cata (fun x -> x.name) c.args) c.local
	| Closure c 	-> _s "(#closure %s [e: %s][a: %s][l: %d])" (string_code c.code) (cata (fun x -> x.name) c.env) (cata (fun x -> x.name) c.args) c.local
	| Global (s, c)	-> _s "(#global_set %s %s)" s.name (string_code c)

and string_cons = function
	| Cons(car, Cons(cadr, cddr)) -> _s "%s %s" (string_expr car) (string_cons(Cons(cadr, cddr)))
	| Cons(car, Nil) -> string_expr car
	| Cons(car, cdr) -> _s "%s . %s" (string_expr car) (string_expr cdr)
	| _ -> failwith "Error in string_cons"

(************************************ Compiler ********************************)
let global_env = ref []

let global_code = ref []

let mksym n = Symbol { name = n; expr = Nil; sdbg = None }	

let rec global_set name v =
	let symbol = 
	(match (retrieve (fun n -> n.name = name) !global_env) with
	| Some x -> { name = x.name; expr = v; sdbg = x.sdbg }
	| None   -> { name = name ; expr = v; sdbg = None })
    in global_env := symbol :: !global_env ;
    symbol

and retrieve f l =
	if exists f l then Some (find f l) else None
	
let rec pcompile p e = function
	| Symbol s -> env_lookup s p e
	| Cons(Symbol { name = "let" }, Cons(args, Cons(expr, Nil))) -> pcompile p e (transform_let args expr)
	| Cons(Symbol { name = "lambda" }, Cons(args, Cons(body, Nil))) -> let a = unsym (uncons args) in unfold { code = pcompile p (e @ a) body; env = e; args = a; local = 0 }
	| Cons(Symbol { name = "define" }, Cons(Symbol { name = name }, Cons(v, Nil) )) ->	Global (global_set name v, pcompile p e v)
	| Cons(Symbol { name = "if" }, Cons(c, Cons(t, Cons(f, Nil)))) -> pcompile p e (transform_if c t f)
	| Cons(c, Nil) -> pcompile p e c
	| Cons(func, args) -> (let a = uncons args in Application (pcompile a e func, map (pcompile p e) a))
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
			let (j, l, o) = func i preplst [] a in
			func (j+1) (l @ [Application(defunc f, map defunc o)]) (outargs @ [Local j]) t
		| h::t -> func i preplst (outargs @ [h]) t 	
	in
	match c.code with
	| Application (f, a) -> let (i, l, o) = func 0 [] [] (rev a) in 
		if l = [] then
		Function { code = Application (defunc f, map defunc o); env = c.env; args = c.args; local = 0 }
		else
		Function { code = Application (Quote Nil, l @ [Application (defunc f, map defunc o)]); env = c.env; args = c.args; local = length l }
	| Quote _ | Local _ | Var _ -> Function c	
	| _ ->  Closure c	

and defunc = function
	| Function c -> 
		let i = Random.int 1000 in
		let n = { name = _s "L%d" (i + length !global_code); expr = Nil; sdbg = None } in
		global_code := (Global (n, Function c)) :: !global_code;
		(Quote (Symbol n))
	| x -> x

and mklambda e =
	Cons(mksym "lambda", Cons(Nil, Cons(e, Nil))) 	

and transform_let args expr =
	let (s, e) = unpair args in
	Cons(Cons(mksym "lambda", Cons(s, Cons(expr, Nil))), e) 

and transform_if c t f =
	Cons(mksym "?", Cons(c, Cons(mklambda t, Cons(mklambda f, Nil))))
	
and unsym = 
	map (fun x -> 
			match x with 
			Symbol s 	-> s 
			| _ 		-> failwith ("Cannot unsym " ^ (string_expr x)))	

and unpair = function
	| Cons(Symbol s, Cons(e, Nil))				-> (Cons(Symbol s, Nil), Cons(e, Nil))
	| Cons(Cons(Symbol s, Cons(e, Nil)), Nil)	-> (Cons(Symbol s, Nil), Cons(e, Nil))
	| Cons(Cons(Symbol s, Cons(e, Nil)), cdr)	-> let (a, b) = unpair cdr in (Cons(Symbol s, a), Cons(e, b))
	| x -> failwith ("Cannot unpair " ^ (string_expr x))

and uncons = function
	| Nil 				-> []
	| Cons(car, cdr) 	-> car :: uncons cdr
	| x 				-> failwith ("Cannot uncons " ^ (string_expr x))

  
	
(************************************ Assemble ********************************)
let macros = ["+", ".add."; "-", ".sub."; "?", ".if."]

let unix = Sys.os_type = "Unix"

let mapi f l = Array.to_list (Array.mapi f (Array.of_list l))

let rec assemble = function
	| Quote (Nil) 		-> ""
	| Quote (Int i) 	-> to_string i
	| Quote (Symbol s) 	-> s.name
	| Var i				-> mkvar i
	| Local i			-> mklocal i
	| Application (Quote (Symbol s), a) -> callbyname s.name a
	| Function { code = Quote x }	-> indent [(result (Quote x)); "ret"]
	| Function { code = Var i   }	-> indent [(result (Var i)); "ret"]
	| Function { code = Local i   }	-> indent [(result (Local i)); "ret"]
	| Function { code = Application (Quote Nil, a) }	-> indent ((statements a) @ ["ret"])
	| Function { code = Application (f, a) }	-> indent [assemble (Application (f, a)); "ret"]
	| Closure  { code = Function f; env = e; args = a } -> indent [build_closure "1f" e a; "ret"; label "1" (Function f)]
	| Closure  { code = Closure c; env = e; args = a } -> indent [build_closure "1f" e a; "ret"; label "1" (Closure c)]
	| Global (s, Quote x) -> _s "%s = %s" s.name (assemble (Quote x))
	| Global (s, x) -> label s.name x
	| x					-> "Cannot assemble " ^ (string_code x)
	
and mkvar = function
	| 0	-> if unix then "rdi" else "rcx"
	| 1	-> if unix then "rsi" else "rdx"
	| 2	-> if unix then "rdx" else "r8"
	| 3	-> if unix then "rcx" else "r9"
	| 4	-> if unix then "r8"  else "[rbp+16]"
	| 5	-> if unix then "r9"  else "[rbp+24]"
	| i	->  _s "[rbp+%d]" (if unix then (8*(i-4)) else (8*(i-2)))
	
and mklocal i = _s "[rbp-%d]" ((i + 1)*8)

and callbyname s a =
	try 
		let n = assoc s macros in
		_s "%s %s" n (String.concat ", " (map assemble a))
	with Not_found -> 	
		_s "cinvoke %s %s" s (String.concat ", " (map assemble a))
		
and statements a = 
	match a with
	| h :: t -> fold_left (fun a x -> a @ ["push rax"] @ [assemble x]) [assemble h] t 
	| _		 ->  map assemble a		

and build_closure i e a =
	let l = mapi (fun i x -> Var i) (e@a) in
	_s "build_closure %s %s" i (String.concat ", " (map assemble l))

and enter i =
	_s "enter %d, 0" (i*8)
	
and result s =
	_s "mov rax, %s" (assemble s)	
	
and label n c =
	_s "%s:\n\t%s" n (assemble c)	

and indent l =
	(String.concat "\n\t" l)

let compile t = 
	ignore(global_code := []); 
	let c = pcompile [] [] t in
	String.concat "\n" (map assemble (c :: !global_code))
	
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
		let _ = Random.self_init () in
		let s = read_file Sys.argv.(1) in
		let t = tokenize s in
		let p = parse t in
		let c = map compile p in
		let oc = open_out_bin Sys.argv.(2) in
		output_string oc ".include \"stdlib.inc\"\n\n"; 
		iter (fun x -> output_string oc (_s "%s\n\n" x)) c
