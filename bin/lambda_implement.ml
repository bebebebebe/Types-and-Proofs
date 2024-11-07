(* implementing Lambda Calculus *)

type var = string

type t =
  | Var of var
  | App of t * t
  | Abs of var * t

let rec to_string : t -> string = function
  | Var x -> x
  | App (t, u) -> "(" ^ to_string t ^ " " ^ to_string u ^ ")"
  | Abs (x, t) -> "(" ^"λ" ^ x ^ "." ^ to_string t ^ ")"

let rec has_fv x term = match term with
  | Var x' -> x = x'
  | App (t, u) -> has_fv x t || has_fv x u
  | Abs (y, t) -> if y = x then false else has_fv x t

let fresh =
  let x = ref 0 in
  fun () -> incr x;
            "x" ^ string_of_int !x

(* TODO: below from pairing session. refactor? *)
let rec sub x u = function
  | Var x' -> if x = x' then u else Var x'
  | App (t, z) -> App (sub x u t, sub x u z)
  | Abs (y, t) ->
     if y = x then
       (
         let f = fresh () in
         let t' = sub y (Var f) t in
         Abs (f, t')
       )
     else if has_fv y u then
       (
         let f = fresh () in
         let t' = sub y (Var f) t in
         Abs (f, sub x u t')
       )
     else
       Abs (y, sub x u t)


let rec red = function
  | Var _ -> None
  | Abs (x, t) ->
     (
       match red t with
       | Some t' -> Some (Abs (x, t'))
       | None -> None
     )
  | App (t, u) ->
     match t with
     | Abs (x, t') -> Some (sub x u t')
     | _ ->
        (
          match red t with
          | Some t' -> Some (App (t', u))
          | None ->
             (
               match red u with
               | Some u' -> Some (App (t, u'))
               | None -> None
             )
        )

let rec normalize t =
  match red t with
  | None -> t
  | Some t' -> normalize t'

let reduction t =
  let s = ref 0 in
  let rec aux t' =
    match red t' with
    | None ->
       print_endline (string_of_int !s ^ " reduction steps")
    | Some t'' ->
       incr s;
       print_endline (to_string t'');
       aux t''
  in
  aux t

let eq t u =
  normalize t = normalize u

  let rec alpha t u =
    match (t, u) with
    | (Var x, Var y) -> x = y
    | (Abs (x, t'), Abs (y, u')) ->
       let f = fresh () in
       sub x (Var f) t' = sub y (Var f) t'
    | (App (t', u'), App (t'', u'')) -> alpha t' t'' && alpha u' u''
    | _ -> false
  
  let eq_mod t u =
    alpha (normalize t) (normalize u)