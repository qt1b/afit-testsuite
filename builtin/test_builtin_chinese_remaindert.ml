(** Test suites for builtin builtin ml file using alcotest. *)

open Alcotest
open Test_tools
open Chinese_remaindert

let sprintf = Printf.sprintf

let crt_image_tests () =
  let cases = [((5,[2;3]), [1;2])]
    and do_check ((x,l), expected) =
        check 
          (list int)
          (sprintf "remainder image: %i %s" x (string_of_intlist l))  expected (crt_image x l)
      in
    List.iter do_check cases


let crt_solver_tests () =
  let cases = [((5,[2;3],[1;2]), 1)] (* not sure about these tests, I think this is what this should be  *)
    and do_check ((m,l,y), expected) =
        check 
          (int)
          (sprintf "solver: m=%i l=%s y=%s" m (string_of_intlist l) (string_of_intlist y))  expected (crt_solver m l y)
      in
    List.iter do_check cases


(****************************************************************************)
(****************************************************************************)

let chinese_remaindert_set =
  [("Chinese_remaindert image function", `Quick, crt_image_tests);
   ("Chinese_remaindert solver", `Quick, crt_solver_tests)]
