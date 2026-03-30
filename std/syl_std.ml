(* TODO make this an actual library once we have imports *)
let source =
  {|
builtin unit = syl_unit_t;;
builtin bool = syl_bool_t;;
builtin int = syl_int_t;;
builtin type = syl_type_t;;

builtin ( + ) = syl_int_add;;
builtin ( - ) = syl_int_sub;;
builtin ( * ) = syl_int_mul;;
builtin ( / ) = syl_int_div;;
builtin ( % ) = syl_int_mod;;
builtin ( ~- ) = syl_int_neg;;
builtin ( == ) = syl_int_eq;;
builtin ( != ) = syl_int_neq;;
builtin ( < ) = syl_int_lt;;
builtin ( <= ) = syl_int_lte;;
builtin ( > ) = syl_int_gt;;
builtin ( >= ) = syl_int_gte;;
builtin ( && ) = syl_bool_and;;
builtin ( || ) = syl_bool_or;;
builtin ( ! ) = syl_bool_not;;

external print_unit : unit -> unit = syl_std_print_unit;;
external print_bool : bool -> unit = syl_std_print_bool;;
external print_int : int -> unit = syl_std_print_int;;
|}
;;

let runtime =
  {|
syl_unit syl_std_print_unit() {
  printf("()\n");
}
syl_unit syl_std_print_bool(syl_bool b) {
  printf("%s\n", b ? "true" : "false");
}
syl_unit syl_std_print_int(syl_int i) {
  printf("%ld\n", i);
}

//SYL_STD_END
|}
;;
