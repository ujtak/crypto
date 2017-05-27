open Unix

let tcp_server ~host ~port ~cipher =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let addr = ADDR_INET (inet_addr_of_string host, port) in
  bind sock addr;
  listen sock 5;
  let sock0, addr0 = accept sock in
  let buf = Bytes.of_string (String.map cipher "HELLO") in
  let written_bytes = write sock0 buf 0 (Bytes.length buf) in
  Printf.printf "bytes: %d\n" written_bytes;
  close sock0;
  close sock;
;;

let () =
  let rsa_encrypt = Crypto.encrypt_char (3, 255) in
  tcp_server ~host:"127.0.0.1" ~port:12345 ~cipher:rsa_encrypt
;;
