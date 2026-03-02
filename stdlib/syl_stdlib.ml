(* TODO make this an actual library once we have imports *)
let source =
  {|
builtin unit = syl_unit_t;;
builtin bool = syl_bool_t;;
builtin int = syl_int_t;;
builtin type = syl_type_t;;

external print_unit : unit -> unit = sylstd_print_unit;;
external print_bool : bool -> unit = sylstd_print_bool;;
external print_int : int -> unit = sylstd_print_int;;
|}
;;

let runtime =
  {|
syl_unit sylstd_print_unit() {
  printf("()\n");
}
syl_unit sylstd_print_bool(syl_bool b) {
  printf("%s\n", b ? "true" : "false");
}
syl_unit sylstd_print_int(syl_int i) {
  printf("%ld\n", i);
}

//SYL_STDLIB_END
|}
;;
