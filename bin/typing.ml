type prog =
  | Bool of bool
  | Int of int
  | Add of prog * prog
  | Lt of prog * prog
  | If of prog * prog * prog

type typ =
  | TBool
  | TInt

exception Type_error
let rec infer = function
  | Bool _ -> TBool
  | Int _ -> TInt
  | Add (m, n) -> begin match (infer m, infer n) with
    | (TInt, TInt) -> TInt
    | _ -> raise Type_error
  end
  | Lt (m, n) -> begin match (infer m, infer n) with
    | (TInt, TInt) -> TBool
    | _ -> raise Type_error
  end
  | If (p, q, r) -> begin match (infer p, infer q, infer r) with
    | (TBool, TBool, TBool) -> TBool
    | (TBool, TInt, TInt) -> TInt
    | _ -> raise Type_error
  end

let typable p =
  try
    let _ = infer p in true
  with
    | Type_error -> false

(** Perform one reduction step. *)
let rec red : prog -> prog option = function
| Bool _ | Int _ -> None
| Add (Int n1 , Int n2) -> Some (Int (n1 + n2))
| Add (p1 , p2) ->
(
match red p1 with
| Some p1' -> Some (Add (p1' , p2))
| None ->
  match red p2 with
  | Some p2' -> Some (Add (p1 , p2'))
  | None -> None
)
| Lt (Int n1 , Int n2) -> Some (Bool (n1 < n2))
| Lt (p1 , p2) ->
(
match red p1 with
| Some p1' -> Some (Lt (p1' , p2))
| None ->
match red p2 with
| Some p2' -> Some (Lt (p1 , p2'))
| None -> None
)
| If (Bool true , p1 , p2) -> Some p1
| If (Bool false , p1 , p2) -> Some p2
| If (p , p1 , p2) ->
match red p with
| Some p' -> Some (If (p' , p1 , p2))
| None -> None

(* Normalize: reduce as much as possible *)
let rec normalize p =
  match red p with
  | None -> p
  | Some p' -> normalize p'
