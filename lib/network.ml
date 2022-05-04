open Unix ;;

type client = {
  sock: file_descr;
  input: in_channel;
  output: out_channel;
} ;;

let create_client ip port =
  let s = socket PF_INET SOCK_STREAM 0 in
  connect s (ADDR_INET(inet_addr_of_string ip, port));
  { sock = s; input = in_channel_of_descr s; output = out_channel_of_descr s }
;;

let send_string c str =
  output_string c.output (str ^ "\n");
  flush c.output
;;

let recv_string c =
  input_line c.input
;;

(*let send_string sock str =
  let buf = Bytes.of_string (str ^ "\n") in
  send sock buf 0 (Bytes.length buf) []
;;

let recv_string =
  let buf = Bytes.create 512 in
  let _recv_string sock =
    ignore (recv sock buf 0 (Bytes.length buf) []);
    Bytes.to_string buf
  in _recv_string
;;

let create_server port f =
  let s = socket PF_INET SOCK_STREAM 0 in
  bind s (ADDR_INET(inet_addr_any, port));
  listen s 100;
  let (cs, _) = accept s in
  f cs;
  close cs;
  close s
;;*)