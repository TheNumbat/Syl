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
#include <stdlib.h>

static syl_unit syl_assert(syl_bool cond) {
  if(!cond) abort();
}
static syl_int syl_int_add(syl_tuple<syl_int,syl_int> x) {
  return x.first + x.rest.first;
}
static syl_int syl_int_sub(syl_tuple<syl_int,syl_int> x) {
  return x.first - x.rest.first;
}
static syl_int syl_int_mul(syl_tuple<syl_int,syl_int> x) {
  return x.first * x.rest.first;
}
static syl_int syl_int_div(syl_tuple<syl_int,syl_int> x) {
  return x.first / x.rest.first;
}
static syl_int syl_int_mod(syl_tuple<syl_int,syl_int> x) {
  syl_int mod = x.first % x.rest.first;
  syl_int abs = x.rest.first < 0 ? -x.rest.first : x.rest.first;
  if (mod < 0) mod += abs;
  return mod;
}
static syl_int syl_int_neg(syl_int x) {
  return -x;
}
static syl_bool syl_int_eq(syl_tuple<syl_int,syl_int> x) {
  return x.first == x.rest.first;
}
static syl_bool syl_int_neq(syl_tuple<syl_int,syl_int> x) {
  return x.first != x.rest.first;
}
static syl_bool syl_int_lt(syl_tuple<syl_int,syl_int> x) {
  return x.first < x.rest.first;
}
static syl_bool syl_int_lte(syl_tuple<syl_int,syl_int> x) {
  return x.first <= x.rest.first;
}
static syl_bool syl_int_gt(syl_tuple<syl_int,syl_int> x) {
  return x.first > x.rest.first;
}
static syl_bool syl_int_gte(syl_tuple<syl_int,syl_int> x) {
  return x.first >= x.rest.first;
}
static syl_bool syl_bool_and(syl_tuple<syl_bool,syl_bool> x) {
  return x.first && x.rest.first;
}
static syl_bool syl_bool_or(syl_tuple<syl_bool,syl_bool> x) {
  return x.first || x.rest.first;
}
static syl_bool syl_bool_not(syl_bool x) {
  return !x;
}

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
