open! Core
open Modes

module Literal = struct
  type t =
    | Unit
    | Bool of bool
    | Int of int64
  [@@deriving sexp]

  let print () = function
    | Unit -> "()"
    | Bool b -> sprintf "%b" b
    | Int i -> sprintf "%Ld" i
  ;;
end

module With_loc = struct
  type 'a t =
    { node : 'a
    ; loc : Lex.Location.t
    ; before : Lex.Comment.t list
    ; after : Lex.Comment.t list
    }
  [@@deriving sexp]

  let create ?(before = []) ?(after = []) ~loc node = { node; loc; before; after }
end

module Expr = struct
  type arg =
    { var : Ident.Raw.t
    ; mode : Modes.Maybe.t
    ; ty : t
    ; loc : Lex.Location.t
    }

  and fun_ =
    { var : Ident.Raw.t
    ; erased : Erasure.t
    ; args : arg Nonempty_list.t
    ; ret_mode : Modes.Maybe.t
    ; ret_ty : t
    ; body : t
    ; loc : Lex.Location.t
    }

  and node =
    | If of
        { cond : t
        ; then_ : t
        ; else_ : t
        ; static : Staticity.t
        }
    | Let of
        { var : Ident.Raw.t
        ; erased : Erasure.t
        ; args : arg list
        ; bind : t
        ; rest : t
        }
    | Fun of
        { funs : fun_ Nonempty_list.t
        ; rest : t
        }
    | Lambda of
        { erased : Erasure.t
        ; args : arg Nonempty_list.t
        ; body : t
        }
    | Apply of
        { fn : t
        ; arg : t
        }
    | Paren of { expr : t }
    | Var of { id : Ident.Raw.t }
    | Literal of { value : Literal.t }
    | Unop of
        { op : Ident.Unop.t
        ; arg : t
        }
    | Binop of
        { op : Ident.Binop.t
        ; lhs : t
        ; rhs : t
        }
    | Nop of
        { op : Ident.Nop.t
        ; elts : t list
        }
    | Arrow of
        { arg : t
        ; arg_id : Ident.Raw.t Option.t
        ; arg_mode : Modes.Maybe.t
        ; ret : t
        ; ret_mode : Modes.Maybe.t
        }
    | Assert of
        { cond : t
        ; static : Staticity.t
        }
    | Unreachable
    | Type_annotation of
        { expr : t
        ; ty : t
        }
    | Mode_annotation of
        { expr : t
        ; mode : Modes.Maybe.t
        }

  and t = node With_loc.t [@@deriving sexp]

  let loc (t : t) = t.loc

  let strip_comments =
    List.map ~f:(fun (c : Lex.Comment.t) -> { c with loc = Lex.Location.empty })
  ;;

  let rec strip (e : t) : t =
    { node = strip_node e.node
    ; loc = Lex.Location.empty
    ; before = strip_comments e.before
    ; after = strip_comments e.after
    }

  and strip_node = function
    | If { cond; then_; else_; static } ->
      If { cond = strip cond; then_ = strip then_; else_ = strip else_; static }
    | Let { var; erased; args; bind; rest } ->
      Let { var; erased; args = List.map args ~f:strip_arg; bind = strip bind; rest = strip rest }
    | Fun { funs; rest } -> Fun { funs = Nonempty_list.map funs ~f:strip_fun; rest = strip rest }
    | Lambda { erased; args; body } ->
      Lambda { erased; args = Nonempty_list.map args ~f:strip_arg; body = strip body }
    | Apply { fn; arg } -> Apply { fn = strip fn; arg = strip arg }
    | Paren { expr } -> Paren { expr = strip expr }
    | (Var _ | Literal _ | Unreachable) as n -> n
    | Unop { op; arg } -> Unop { op; arg = strip arg }
    | Binop { op; lhs; rhs } -> Binop { op; lhs = strip lhs; rhs = strip rhs }
    | Nop { op; elts } -> Nop { op; elts = List.map elts ~f:strip }
    | Arrow { arg; arg_id; arg_mode; ret; ret_mode } ->
      Arrow { arg = strip arg; arg_id; arg_mode; ret = strip ret; ret_mode }
    | Assert { cond; static } -> Assert { cond = strip cond; static }
    | Type_annotation { expr; ty } -> Type_annotation { expr = strip expr; ty = strip ty }
    | Mode_annotation { expr; mode } -> Mode_annotation { expr = strip expr; mode }

  and strip_arg (a : arg) : arg = { a with ty = strip a.ty; loc = Lex.Location.empty }

  and strip_fun (f : fun_) : fun_ =
    { f with
      args = Nonempty_list.map f.args ~f:strip_arg
    ; ret_ty = strip f.ret_ty
    ; body = strip f.body
    ; loc = Lex.Location.empty
    }
  ;;
end

module Top_level = struct
  type node =
    | Let of
        { var : Ident.Raw.t
        ; erased : Erasure.t
        ; args : Expr.arg list
        ; bind : Expr.t
        }
    | Fun of { funs : Expr.fun_ Nonempty_list.t }
    | External of
        { var : Ident.Raw.t
        ; ty : Expr.t
        ; symbol : string
        }
    | Builtin of
        { var : Ident.Raw.t
        ; name : string
        }
  [@@deriving sexp]

  type t = node With_loc.t [@@deriving sexp]

  let loc (t : t) = t.loc

  let strip (tl : t) : t =
    { node =
        (match tl.node with
         | Let { var; erased; args; bind } ->
           Let { var; erased; args = List.map args ~f:Expr.strip_arg; bind = Expr.strip bind }
         | Fun { funs } -> Fun { funs = Nonempty_list.map funs ~f:Expr.strip_fun }
         | External { var; ty; symbol } -> External { var; ty = Expr.strip ty; symbol }
         | Builtin _ as n -> n)
    ; loc = Lex.Location.empty
    ; before = Expr.strip_comments tl.before
    ; after = Expr.strip_comments tl.after
    }
  ;;
end

module Program = struct
  type t =
    { items : Top_level.t list
    ; after : Lex.Comment.t list
    }
  [@@deriving sexp]

  let strip (p : t) : t =
    { items = List.map p.items ~f:Top_level.strip
    ; after = Expr.strip_comments p.after
    }
  ;;
end
