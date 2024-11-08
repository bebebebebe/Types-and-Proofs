(* Church numerals, logic, arithmetic in lambda calculus *)

open Lambda_implement

(* Identity *)
let id = Abs ("x", Var "x")

(* Booleans *)
let btrue = Abs ("x", Abs ("y", Var "x"))
let bfalse = Abs ("x", Abs ("y", Var "y"))

(* Not, And, Or defined in terms of Booleans *)
let negation = Abs ("x", App (App (Var "x", bfalse), btrue))

(* λx.(λy.(x ((y ((λx.(λy.x)) (λx.(λy.y)))) (λx.(λy.y)))))) *)
let conjunction =
  Abs ("x", Abs ("y",                         (* λx.(λy. *)
    App (Var "x",                             (* (x *)
      App (
        App (Var "y", App (btrue, bfalse)),   (* (y ((λx.(λy.x)) (λx.(λy.y))) *)
        bfalse                                (* (λx.(λy.y) *)
      )
    )
  ))

let disjunction =
  Abs ("x", Abs ("y",
    App (Var "x",
      App (
        btrue,                                
        App (Var "y", App (btrue, bfalse))
      )
    )
  ))

(* Helper functions: abstraction, application over lists *)

(* test case: (abss ["x"; "y"; "z"] t = Abs ("x", Abs ("y", Abs ("z", t))))
*)

exception Invalid_argument

let rec abss lst t =
  match lst with
  | [] -> raise Invalid_argument
  | [x] -> Abs (x, t)
  | x :: tail -> Abs (x, abss tail t)

(* test case: (apps [t; u; v; w] = App (App (App (t, u), v), w)) *)

let apps lst =
  match lst with
  | [] | [_] -> raise Invalid_argument
  | t :: u :: tail -> (let rec aux acc = function
    | [] -> acc
    | x :: tail -> aux (App (acc, x) ) tail
    in  
    aux (App (t, u)) tail)

(* apps above, defined using List.rev instead of auxilary function *)
let rec apps_rev lst =
  match List.rev lst with
  | [] | [_] -> raise Invalid_argument
  | [u; t] -> App (t, u)
  | t :: tail -> App (apps_rev (List.rev tail), t)

(* Conditional: if b then x else y *)

let bif = abss ["b"; "x"; "y"] (apps [Var "b"; Var "x"; Var "y"])

(* Natural Numbers (Church Numerals) *)

let nat n =
  let rec aux f x = function 
    | 0 -> x
    | n -> App (f, aux f x (n - 1))
  in
  abss ["f"; "x"] (aux (Var "f") (Var "x") n)

(* Successor *)

(* (λn.(λf.(λx.(((f n) f) x)))) *)
let succ =
  abss ["n"; "f"; "x"] (apps [Var "f"; apps [Var "n"; Var "f"; Var "x"]])


(* Arithmetic *)
let add = abss ["m"; "n"] (apps [Var "m"; succ; Var "n"])

let mul = abss ["m"; "n"] (apps [Var "m"; App (add, Var "n"); nat 0])

let iszero = abss ["n"; "x"; "y"] (apps [Var "n"; Abs ("z", Var "y"); Var "x"])


(* Pairs *)
let pair = abss ["x"; "y"; "b"] (apps [bif; Var "b"; Var "x"; Var "y"])

let fst = Abs ("p", App (Var "p", btrue))

let snd = Abs ("p", App (Var "p", bfalse))

(* Finobacci *)

(* naive *)
let rec fib_naive n =
  if n = 0 then 0 else 
    if n = 1 then 1 else
      fib_naive (n - 1) + fib_naive (n - 2)

(* iterative *)
let fib_fun (b, a) = (a, a + b)

let rec iter n f x =
  if n = 0 then x
  else f (iter (n - 1) f x)

let fib n = Stdlib.fst (iter n fib_fun (0, 1))

(* predecessor *)
let pred_fun = Abs ("p", apps [pair; App (snd, Var "p"); App(succ, App (snd, Var "p"))])

let pred = Abs ("n", apps [fst; apps [Var "n"; pred_fun; apps [pair; nat 0; nat 0]]])


