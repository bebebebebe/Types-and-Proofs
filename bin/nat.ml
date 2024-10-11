(* Define types for natural numbers, and basic operations on them *)

(* natural numbers *)
type nat =
| Zero
| Suc of nat

(* addition *)
let rec add m n =
  match m with
  | Zero -> n
  | Suc m -> Suc (add m n)

(* is even *)
let rec even n =
  match n with
  | Zero -> true
  | Suc Zero -> false
  | Suc(Suc m) -> even m

(* predecessor *)
let rec pred n =
  match n with
  | Zero -> None
  | Suc m -> Some m

(* half, rounded down *)
let rec half n =
  match n with
  | Zero -> Zero
  | Suc(Zero) -> Zero
  | Suc(Suc(x)) -> Suc(half(x))

  (* half if even, None if odd *)
  let half_opt n =
    let rec aux n count =
      match n with
      | Zero -> Some count          (* Return the count when n is zero *)
      | Suc Zero -> None            (* Return None when n is one (odd) *)
      | Suc (Suc m) -> aux m (Suc count)  (* Subtract 2 and increment count *)
    in
    aux n Zero