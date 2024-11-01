(* Calculating with the Lambda Calculus *)

type var = string

type t =
  | Var of var
  | App of t * t
  | Abs of var * t

let rec to_string : t -> string = function
  | Var x -> x
  | App (t, u) -> "(" ^ to_string t ^ " " ^ to_string u ^ ")"
  | Abs (x, t) -> "(" ^"λ" ^ x ^ "." ^ to_string t ^ ")"

(* Identity *)
let id = Abs ("x", Var "x")

(* Booleans *)
let btrue = Abs ("x", Abs ("y", Var "x"))
let bfalse = Abs ("x", Abs ("y", Var "y"))

(* Not, And, Or defined in terms of Booleans *)
(* (λx.(x ((λx.(λy.y)) (λx.(λy.x))))) *)
let negation = Abs ("x", App (Var "x", App (bfalse, btrue)))

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

let rec apss lst =
  match List.rev lst with
  | [] | [_] -> raise Invalid_argument
  | [u; t] -> App (t, u)
  | t :: tail -> App (apss (List.rev tail), t)

(* Rewrite above with aux function instead of List.rev *)
let apss lst =
  match lst with
  | [] | [_] -> raise Invalid_argument
  | [t; u] -> App (t, u)
  | t :: u :: tail -> (let rec aux acc = function
    | [] -> acc
    | x :: tail -> aux (App (acc, x) ) tail
    in  
    aux (App (t, u)) tail)

(* Conditional: if b then x else y *)

let bif = abss ["b"; "x"; "y"] (apss [Var "b"; Var "x"; Var "y"])

(* Natural Numbers (Church Numerals) *)

let nat n =
  let rec aux f x = function 
    | 0 -> x
    | n -> App (f, aux f x (n - 1))
  in
  abss ["f"; "x"] (aux (Var "f") (Var "x") n)
