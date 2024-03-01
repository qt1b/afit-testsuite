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
            (sprintf "add_n: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (add_n a b)
    in
    List.iter do_check cases

let mult_b_tests () = 
    let cases = 
      [([0;1],[0; 0; 1; 1; 0; 1]), [0;0;1;1;0;1];
       ([0;0;1],[0; 0; 1; 1; 0; 1]), [0;0;0;1;1;0;1];
       ([0;1;1],[0; 0; 1; 1; 0; 1]), [0;0;1;0;0;0;0;1]]
    and do_check ((a, b), expected) =
        check
            (list int)
            (sprintf "mult_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (mult_b a b)
    in
    List.iter do_check cases

let comp_b_tests () = 
    let cases = 
      [(([0;1],[0; 0; 1; 1; 0; 1]), -1);
       (([],[0; 1]), -1;);
       (([0;1;1],[0;1;1]), 0);
       (([1;1;0],[1;1;1]),1);
       (([0;0;0;0;1],[1;1;1;1;1;1;1;1]),1)]
    and do_check ((a, b), expected) =
        check
            (int)
            (sprintf "compare_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (compare_b a b)
    in
    List.iter do_check cases

let to_int_tests () = 
    let cases = 
      [[1;1], -1;
       [],0;
       [0;1;1],3;
       [1;0;1;0;1;0;1],-42;
       [0;0;0;0;0;0;0;0;0;1],256]
    and do_check (a, expected) =
        check
            (int)
            (sprintf "to_int: %s" (string_of_intlist a))
            expected
            (to_int a)
    in
    List.iter do_check cases

(*let rmv_zer_tests () = 
    let cases = 
      [[1;1;0;0;0;0;0;0], [1;1]]
    and do_check (a, expected) =
        check
            (list int)
            (sprintf "list to reduce: %s" (string_of_intlist a))
            expected
            (rmv_zeroes a)
    in
    List.iter do_check cases*)

let diff_n_tests () =
  let cases =
    [
      ([], []), [];
      ([1;1], []), [1;1];
      ([1;1], [1;1]), [];
      ([1;1;1;1;1;0;1], [0;1;1;0;1;1]), [1;0;0;1;0;1];
      ([1;1;0;1;0;1;1], [1;1;0;1;1;1]), [0;0;0;0;1;1];
      ([1;1;0;0;1;0;1], [0;1;1;1;0;1]), [1;0;1;0;0;1];
      ([1;1;0;1;1;0;1], [0;1;0;0;1;1]), [1;0;0;1;0;1];
      ([0;1;1;0;1;0;1], [1;1;0;1;0;1]), [1;1;0;1;0;1];
    ]
  and do_check ((a, b), expected) =
    check
      (list int)
      (sprintf "diff_n: %s and %s" (string_of_intlist a) (string_of_intlist b))
      expected
      (diff_n a b)
  in
  List.iter do_check cases

let diff_b_tests () = 
  let cases =
    [
      ([], []), [];
      ([1;1], []), [1;1];
      ([1;1], [1;1]), [];
      ([1;1;1], [0;1;1]), [1;0;1;1];
      ([0;1;0;1;1], [1;1;0;1]), [0;0;1;0;0;1];
      ([0;0;1;0;0;1],[0;0;1;1;0;1]), [1; 0; 0; 1]
    ]
  and do_check ((a, b), expected) =
    check
      (list int)
      (sprintf "diff_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
      expected
      (diff_b a b)
  in
  List.iter do_check cases  


let quot_b_tests () =
  let cases =
    [
      ([0;0;0;1], [0;1]), [0;0;0;1];
      ([1;1], [0;0;1]), [1;1];
      ([0;0;1;1], [0;0;1]), [0;1;1];
      ([1;1;1;1;1],[0;0;1]),[1;0;0;0;1]
    ]
  and do_check ((a, b), expected) =
    check
      (list int)
      (sprintf "quot_b: %s and %s, expected res = %s" (string_of_intlist a) (string_of_intlist b) (string_of_intlist expected))
      expected
      (quot_b a b)
  in
  List.iter do_check cases  

let mod_b_tests () =
  let cases =
    [
      ([0;0;0;1], [0;1;1]), [0;1];
      ([1;1], [0;1;1;0;1]), [0;0;1;0;1];
      ([1;1], [0;0;1]), [0;1];
      ([1;1;1;1;1],[0;0;1]),[0;1]

    ]
  and do_check ((a, b), expected) =
    check
      (list int)
      (sprintf "mod_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
      expected
      (mod_b a b)
  in
  List.iter do_check cases  


let scalable_set =
    [("Addition of natural bitLists", `Quick, add_n_tests);
     ("Multiplication of bitArrarys", `Quick, mult_b_tests);
     ("Comparaison of bitArrarys",`Quick, comp_b_tests);
     ("Binary List to int",`Quick, to_int_tests);
     (*"Remove Trailing zeroes",`Quick, rmv_zer_tests); why can't i get the test working ?*)
     ("Difference with naturals bitarrays",`Quick, diff_n_tests);
    ("Difference with bitarrays",`Quick, diff_b_tests);
    ("Quotient of bitarrays",`Quick, quot_b_tests);
    ("Modulo of bitarrays",`Quick, mod_b_tests)
    ]
