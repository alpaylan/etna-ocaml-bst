open BST.Spec_crowbar

(* etna2 contract: emit a single-line JSON object on stdout and exit 0.
   The dummy test runs once after the real property exhausts (or
   fails); we print JSON, flush, and `exit 0` to bypass Crowbar's
   non-zero-on-test-failure exit path that would otherwise mark the
   trial as `aborted`. *)
let run prop gen =
  prop gen;
  Crowbar.add_test ~name:"_etna_emit" [ Crowbar.bool ] (fun _ ->
      let status =
        if !end_time <> None then "failed" else "passed"
      in
      let elapsed_us =
        match (!start_time, !end_time) with
        | Some s, Some e -> int_of_float ((e -. s) *. 1_000_000.)
        | Some s, None ->
            int_of_float ((Unix.gettimeofday () -. s) *. 1_000_000.)
        | _ -> 0
      in
      Printf.printf
        "{\"status\":\"%s\",\"tests\":%d,\"discards\":%d,\"time\":\"%dus\"}\n"
        status !generated !discards elapsed_us;
      flush stdout;
      exit 0)
