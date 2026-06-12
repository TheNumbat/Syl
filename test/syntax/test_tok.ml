open! Core
open! Syl

let go = Common.tokenize

let%expect_test _ =
  go "if then else ;;; fn -+*/&&||! let = () true and fun @ false -> : hello h_eL_O_o";
  [%expect
    {|
    (token If)
    (token Then)
    (token Else)
    (token Double_semicolon)
    (token Semicolon)
    (token Fn)
    (token (Op Minus))
    (token (Op Plus))
    (token (Op Star))
    (token (Op Slash))
    (token (Op And))
    (token (Op Or))
    (token (Op Not))
    (token Let)
    (token Asn)
    (token Unit)
    (token (Bool true))
    (token And)
    (token Fun)
    (token At)
    (token (Bool false))
    (token (Op Arrow))
    (token Colon)
    (token (Ident hello))
    (token (Ident h_eL_O_o))
    (token Eof)
    |}]
;;

let%expect_test _ =
  go "()((=))(->in)=()))(=)(hello:)";
  [%expect
    {|
    (token Unit)
    (token Lparen)
    (token Lparen)
    (token Asn)
    (token Rparen)
    (token Rparen)
    (token Lparen)
    (token (Op Arrow))
    (token In)
    (token Rparen)
    (token Asn)
    (token Unit)
    (token Rparen)
    (token Rparen)
    (token Lparen)
    (token Asn)
    (token Rparen)
    (token Lparen)
    (token (Ident hello))
    (token Colon)
    (token Rparen)
    (token Eof)
    |}]
;;

let%expect_test "caret" =
  go "hello ^";
  [%expect
    {|
    (token (Ident hello))
    (token (Op Caret))
    (token Eof)
    |}]
;;

let%expect_test "error line and column" =
  go ~print:true "hello\nworld\n-!";
  [%expect
    {|
    ((token (Ident hello)) (loc ((line 1) (column 0))))
    ((token (Ident world)) (loc ((line 2) (column 0))))
    ((token (Op Minus)) (loc ((line 3) (column 0))))
    ((token Eof) (loc ((line 3) (column 1))))
    |}]
;;

let%expect_test "location after each token" =
  go
    ~print:true
    {|if then
    else
    fn
    let @
    =
    ()  true fun
    false         hello    and    h_eL_O_o|};
  [%expect
    {|
    ((token If) (loc ((line 1) (column 0))))
    ((token Then) (loc ((line 1) (column 3))))
    ((token Else) (loc ((line 2) (column 4))))
    ((token Fn) (loc ((line 3) (column 4))))
    ((token Let) (loc ((line 4) (column 4))))
    ((token At) (loc ((line 4) (column 8))))
    ((token Asn) (loc ((line 5) (column 4))))
    ((token Unit) (loc ((line 6) (column 4))))
    ((token (Bool true)) (loc ((line 6) (column 8))))
    ((token Fun) (loc ((line 6) (column 13))))
    ((token (Bool false)) (loc ((line 7) (column 4))))
    ((token (Ident hello)) (loc ((line 7) (column 18))))
    ((token And) (loc ((line 7) (column 27))))
    ((token (Ident h_eL_O_o)) (loc ((line 7) (column 34))))
    ((token Eof) (loc ((line 7) (column 42))))
    |}]
;;

let%expect_test "parens" =
  go "(true)";
  [%expect
    {|
    (token Lparen)
    (token (Bool true))
    (token Rparen)
    (token Eof)
    |}]
;;
