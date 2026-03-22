open! Core
open Modes
module Literal = Cst.Literal

module Expr = struct
  type fun_ =
    { var : Ident.t
    ; arg : Ident.t
    ; erased : Erasure.t
    ; arg_mode : Modes.Maybe.t
    ; arg_ty : t
    ; ret_mode : Modes.Maybe.t
    ; ret_ty : t
    ; body : t
    ; loc : Lex.Location.t
    }

  and t =
    | If of
        { cond : t
        ; then_ : t
        ; else_ : t
        ; static : Staticity.t
        ; loc : Lex.Location.t
        }
    | Let of
        { var : Ident.t
        ; bind : t
        ; rest : t
        ; loc : Lex.Location.t
        }
    | Fun of
        { funs : fun_ Nonempty_list.t
        ; rest : t
        ; loc : Lex.Location.t
        }
    | Lambda of
        { arg : Ident.t
        ; erased : Erasure.t
        ; arg_mode : Modes.Maybe.t
        ; arg_ty : t
        ; body : t
        ; loc : Lex.Location.t
        }
    | Apply of
        { fn : t
        ; arg : t
        ; loc : Lex.Location.t
        }
    | Var of
        { id : Ident.t
        ; loc : Lex.Location.t
        }
    | Literal of
        { value : Literal.t
        ; loc : Lex.Location.t
        }
    | Unop of
        { op : Ident.Unop.t
        ; arg : t
        ; loc : Lex.Location.t
        }
    | Binop of
        { op : Ident.Binop.t
        ; lhs : t
        ; rhs : t
        ; loc : Lex.Location.t
        }
    | Nop of
        { op : Ident.Nop.t
        ; elts : t list
        ; loc : Lex.Location.t
        }
    | Arrow of
        { arg : t
        ; arg_id : Ident.t
        ; arg_mode : Modes.Maybe.t
        ; ret : t
        ; ret_mode : Modes.Maybe.t
        ; loc : Lex.Location.t
        }
    | Assert of
        { cond : t
        ; static : Staticity.t
        ; loc : Lex.Location.t
        }
    | Unreachable of { loc : Lex.Location.t }
    | Type_annotation of
        { expr : t
        ; ty : t
        ; loc : Lex.Location.t
        }
    | Mode_annotation of
        { expr : t
        ; mode : Modes.Maybe.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  let rec free_vars (expr : t) : Ident.Set.t =
    match expr with
    | Assert { cond; _ } -> free_vars cond
    | Arrow { arg; ret; _ } -> Set.union (free_vars arg) (free_vars ret)
    | Var { id; _ } -> Ident.Set.singleton id
    | Mode_annotation { expr; _ } -> free_vars expr
    | Type_annotation { expr; ty; _ } -> Set.union (free_vars expr) (free_vars ty)
    | Nop { elts; _ } -> Ident.Set.union_list (List.map elts ~f:free_vars)
    | Unreachable _ | Literal _ -> Ident.Set.empty
    | Unop { arg; _ } -> free_vars arg
    | Binop { lhs; rhs; _ } -> Set.union (free_vars lhs) (free_vars rhs)
    | If { cond; then_; else_; _ } ->
      Ident.Set.union_list [ free_vars cond; free_vars then_; free_vars else_ ]
    | Let { var; bind; rest; _ } -> Set.union (free_vars bind) (Set.remove (free_vars rest) var)
    | Apply { fn; arg; _ } -> Set.union (free_vars fn) (free_vars arg)
    | Lambda { arg; arg_ty; body; _ } ->
      let fv_body = Set.remove (free_vars body) arg in
      Set.union fv_body (free_vars arg_ty)
    | Fun { funs; rest; _ } ->
      let bound_ids =
        Nonempty_list.map funs ~f:(fun f -> f.var) |> Nonempty_list.to_list |> Ident.Set.of_list
      in
      let fvs_in_funs =
        Nonempty_list.fold funs ~init:Ident.Set.empty ~f:(fun acc f ->
          let fv_arg_ty = free_vars f.arg_ty in
          let fv_ret_ty =
            let fv = free_vars f.ret_ty in
            if Modes.is_static (Modes.annotate (Modes.default ()) f.arg_mode)
            then Set.remove fv f.arg
            else fv
          in
          let fv_body = Set.remove (free_vars f.body) f.arg in
          Ident.Set.union_list [ acc; fv_arg_ty; fv_ret_ty; fv_body ])
      in
      Set.diff (Set.union fvs_in_funs (free_vars rest)) bound_ids
  ;;

  let loc = function
    | If { loc; _ }
    | Fun { loc; _ }
    | Let { loc; _ }
    | Lambda { loc; _ }
    | Apply { loc; _ }
    | Var { loc; _ }
    | Literal { loc; _ }
    | Unop { loc; _ }
    | Binop { loc; _ }
    | Arrow { loc; _ }
    | Assert { loc; _ }
    | Unreachable { loc; _ }
    | Type_annotation { loc; _ }
    | Mode_annotation { loc; _ }
    | Nop { loc; _ } -> loc
  ;;
end

module Top_level = struct
  type t =
    | Let of
        { var : Ident.t
        ; bind : Expr.t
        ; loc : Lex.Location.t
        }
    | Fun of
        { funs : Expr.fun_ Nonempty_list.t
        ; loc : Lex.Location.t
        }
    | External of
        { var : Ident.t
        ; ty : Expr.t
        ; symbol : string
        ; loc : Lex.Location.t
        }
    | Builtin of
        { var : Ident.t
        ; name : string
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  let loc = function
    | Fun { loc; _ } | Let { loc; _ } | External { loc; _ } | Builtin { loc; _ } -> loc
  ;;
end

module Program = struct
  type t = Top_level.t list [@@deriving sexp]
end
