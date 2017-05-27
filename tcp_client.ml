open Unix

let tcp_client ~host ~port ~cipher =
  let sock0 = socket PF_INET SOCK_STREAM 0 in
  let addr0 = ADDR_INET (inet_addr_of_string host, port) in
  connect sock0 addr0;
  let buf = Bytes.create 32 in
  let read_bytes = read sock0 buf 0 (Bytes.length buf) in
  let word = Bytes.to_string buf in
  Printf.printf "bytes: %d, buf: %s, decr_buf: %s\n"
    read_bytes word (String.map cipher word);
  close sock0
;;

let () =
  let rsa_decrypt = Crypto.decrypt_char (75, 255) in
  tcp_client ~host:"127.0.0.1" ~port:12345 ~cipher:rsa_decrypt
;;
