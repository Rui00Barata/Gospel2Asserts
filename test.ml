let fact = fun num ->
  let rec helper : int -> int -> int = fun n acc ->
    if n > 0
    then helper (n-1) (acc * n)
    else acc
  in
  helper num 1

let f n0 = 
  let n = ref n0 in
  let r = ref 1 in
  while (!n <> 0) do
    (*@ 
      invariant n0 >= !n >= 0 
      variant !n
     *)
    r := !r * !n;
    n := !n - 1
  done;
  !r
(*@ 
    r = f x 
      requires x >= 1
      requires x > 0
      requires not false

      ensures r = fact n0
*)


let f_spec n0 = 
  assert (n0 >= 1);
  assert (n0 > 0);
  assert (not false);
  let r = f n0 in
  assert (r = fact n0)


let f x y =
    let q = ref 0 in
    let r = ref 0 in
    let a = ref 1 in
    while (x >= !a) do
      (*@ invariant 0 <= !r < y *)
      (if (!r = !y-1) then (
        incr q;
        r := 0)
      else
        incr r);
      incr a
    done;
    !q

