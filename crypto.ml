open Printf
open Big_int

let is_prime n =
  let rec iter a n =
    if n mod a = 0 then false
    else if a = n - 1 then true
    else iter (a + 1) n
  in
  if n < 2 then false
  else if n = 2 then true
  else iter 2 n
;;

let pow n p =
  let rec iter a n p =
    if p = 0 then a
    else iter (n*a) n (p-1)
  in
  iter 1 n p
;;

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)
;;

let print_list_int a =
  List.iter (printf "%d :: ") a;
  print_endline "[]"
;;

let euclid m n =
  let rec find_k a b =
    if b = 0 then [] else (a / b) :: find_k b (a mod b)
  in
  let rec calc_list (a, b, c, d) l =
    let calc k (a, b, c, d) = (c, d, a-k*c, b-k*d) in
    match l with
      []      -> (a, b)
    | k :: ks -> calc_list (calc k (a, b, c, d)) ks
  in
  calc_list (1, 0, 0, 1) (find_k m n)
;;

let gen_rsa p q =
  let n = p * q in
  let phi = (p - 1) * (q - 1) in
  let e =
    let rec find_e e = if phi mod e = 0 then find_e (e+1) else e in
    find_e 1
  in
  let mx, d = euclid phi e in
  assert (d * e mod phi = 1);
  assert (d * e + mx * phi = 1);
  (e, d, n)
;;

let encrypt_rsa (e, n) x =
  int_of_big_int (mod_big_int (power_int_positive_int x e) (big_int_of_int n))
;;
let decrypt_rsa (d, n) x =
  int_of_big_int (mod_big_int (power_int_positive_int x d) (big_int_of_int n))
;;

let encrypt_char (e, n) c =
  let ascii_code = int_of_char c in
  let encr_code = encrypt_rsa (e, n) ascii_code in
  char_of_int encr_code
;;

let decrypt_char (d, n) c =
  let ascii_code = int_of_char c in
  let decr_code = decrypt_rsa (d, n) ascii_code in
  char_of_int decr_code
;;

let () = if Sys.argv.(0) = "./crypto" then
  for i = 1 to 100 do
    if is_prime i then printf "%d\n" i
  done
;;

let () = if Sys.argv.(0) = "./crypto" then
  let e, d, n = gen_rsa 15 17 in

  let chara = 'a' in
  let en_chara = encrypt_char (e, n) chara in
  let de_chara = decrypt_char (d, n) en_chara in
  printf "e: %d d: %d n: %d ~ %c %c %c\n" e d n chara en_chara de_chara;

  let letter = int_of_char 'a' in
  let en_letter = encrypt_rsa (e, n) letter in
  let de_letter = decrypt_rsa (d, n) en_letter in
  printf "e: %d d: %d n: %d ~ %d %d %d\n" e d n letter en_letter de_letter;
;;
