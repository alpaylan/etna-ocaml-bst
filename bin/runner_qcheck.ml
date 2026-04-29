(* etna2 contract: emit a single-line JSON object on stdout and always
   exit 0. status is "passed" | "failed" | "aborted" — etna-cli's
   driver matches against those strings; "Finished"/"Failed" don't
   round-trip through the visualize layer. *)
let print_result (r : BST.Spec_qcheck.test_result) =
  let status = if r.passed then "passed" else "failed" in
  let elapsed_us = int_of_float (r.elapsed_s *. 1_000_000.) in
  Printf.printf
    "{\"status\":\"%s\",\"tests\":%d,\"discards\":%d,\"time\":\"%dus\"}\n"
    status r.generated r.discards elapsed_us;
  flush stdout

let run
    ~prop:
      (make_test :
        BST.Impl.t QCheck2.Gen.t -> int -> unit -> BST.Spec_qcheck.test_result)
    ~(gen : BST.Impl.t QCheck2.Gen.t)
    ~(seed: int) : unit =
  match (try Ok (make_test gen seed ()) with e -> Error e) with
  | Ok r -> print_result r
  | Error e ->
      let msg = Printexc.to_string e |> String.escaped in
      Printf.printf
        "{\"status\":\"aborted\",\"tests\":0,\"discards\":0,\"time\":\"0us\",\"error\":\"%s\"}\n"
        msg;
      flush stdout
