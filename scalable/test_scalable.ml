(** Test suites for builtin basic_arithmetic ml file using alcotest. *)

open Alcotest
open Test_tools
open Scalable

let sprintf = Printf.sprintf

let add_n_tests () =
    let cases =
        [([0;0;0;0;0;0;1], [0;0;1;1]), [0;0;1;1;0;0;1];
         ([0;0;1;0;0;1], [0;0;0;1;1]), [0;0;1;1;1;1];
         ([1;0;1;0;0;1], [1;0;0;1;1]), [0;1;1;1;1;1]]
    and do_check ((a, b), expected) =
        check
            (list int)
            (sprintf "gcd_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (add_n a b)
    in
    List.iter do_check cases
    
    
let scalable_set =
    [("Addition of natural bitLists", `Quick, add_n_tests)(*;
     ("Bezout on bitarrays function", `Quick, bezout_b_tests*)]
