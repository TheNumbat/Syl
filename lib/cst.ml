open! Core

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
  type arg_node =
    { var : Ident.Raw.t
    ; mode : Modes.Maybe.t
    ; ty : t
    ; after_open : Lex.Comment.t list
    ; after_mode : Lex.Comment.t list
    ; after_var : Lex.Comment.t list
    }

  and pattern_node = Var of { id : Ident.Raw.t }

  and fun_node =
    { var : Ident.Raw.t
    ; erased : Modes.Erasure.t
    ; arg : arg
    ; ret_mode : Modes.Maybe.t
    ; ret_ty : t
    ; body : t
    ; after_erased : Lex.Comment.t list
    ; after_arg : Lex.Comment.t list
    }

  and node =
    | If of
        { cond : t
        ; then_ : t
        ; else_ : t
        ; static : Modes.Staticity.t
        ; before_static : Lex.Comment.t list
        }
    | Match of
        { cond : t
        ; arms : (pattern * t) Nonempty_list.t
        ; static : Modes.Staticity.t
        ; before_static : Lex.Comment.t list
        }
    | Let of
        { var : Ident.Raw.t
        ; erased : Modes.Erasure.t
        ; bind : t
        ; rest : t
        ; before_erased : Lex.Comment.t list
        ; after_erased : Lex.Comment.t list
        ; after_var : Lex.Comment.t list
        }
    | Fun of
        { funs : fun_ Nonempty_list.t
        ; rest : t
        }
    | Lambda of
        { erased : Modes.Erasure.t
        ; arg : arg
        ; body : t
        ; before_erased : Lex.Comment.t list
        ; after_arg : Lex.Comment.t list
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
    | Arrow of
        { arg : t
        ; arg_id : Ident.Raw.t Option.t
        ; arg_mode : Modes.Maybe.t
        ; ret : t
        ; ret_mode : Modes.Maybe.t
        }
    | Tuple of { elts : t list }
    | Make_tuple of { elts : t list }
    | Assert of
        { cond : t
        ; static : Modes.Staticity.t
        ; before_static : Lex.Comment.t list
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
  and arg = arg_node With_loc.t [@@deriving sexp]
  and pattern = pattern_node With_loc.t [@@deriving sexp]
  and fun_ = fun_node With_loc.t [@@deriving sexp]

  let loc (t : t) = t.loc
  let strip_comments = List.map ~f:(fun (c : Lex.Comment.t) -> { c with loc = Lex.Location.empty })

  let rec strip (t : t) : t =
    { node = strip_node t.node
    ; loc = Lex.Location.empty
    ; before = strip_comments t.before
    ; after = strip_comments t.after
    }

  and strip_node = function
    | If { cond; then_; else_; static; before_static } ->
      If
        { cond = strip cond
        ; then_ = strip then_
        ; else_ = strip else_
        ; static
        ; before_static = strip_comments before_static
        }
    | Match { cond; arms; static; before_static } ->
      Match
        { cond = strip cond
        ; arms = Nonempty_list.map arms ~f:(fun (p, e) -> strip_pattern p, strip e)
        ; static
        ; before_static = strip_comments before_static
        }
    | Let { var; erased; bind; rest; before_erased; after_erased; after_var } ->
      Let
        { var
        ; erased
        ; bind = strip bind
        ; rest = strip rest
        ; before_erased = strip_comments before_erased
        ; after_erased = strip_comments after_erased
        ; after_var = strip_comments after_var
        }
    | Fun { funs; rest } -> Fun { funs = Nonempty_list.map funs ~f:strip_fun; rest = strip rest }
    | Lambda { erased; arg; body; before_erased; after_arg } ->
      Lambda
        { erased
        ; arg = strip_arg arg
        ; body = strip body
        ; before_erased = strip_comments before_erased
        ; after_arg = strip_comments after_arg
        }
    | Apply { fn; arg } -> Apply { fn = strip fn; arg = strip arg }
    | Paren { expr } -> Paren { expr = strip expr }
    | (Var _ | Literal _ | Unreachable) as n -> n
    | Unop { op; arg } -> Unop { op; arg = strip arg }
    | Binop { op; lhs; rhs } -> Binop { op; lhs = strip lhs; rhs = strip rhs }
    | Make_tuple { elts } -> Make_tuple { elts = List.map elts ~f:strip }
    | Arrow { arg; arg_id; arg_mode; ret; ret_mode } ->
      Arrow { arg = strip arg; arg_id; arg_mode; ret = strip ret; ret_mode }
    | Tuple { elts } -> Tuple { elts = List.map elts ~f:strip }
    | Assert { cond; static; before_static } ->
      Assert { cond = strip cond; static; before_static = strip_comments before_static }
    | Type_annotation { expr; ty } -> Type_annotation { expr = strip expr; ty = strip ty }
    | Mode_annotation { expr; mode } -> Mode_annotation { expr = strip expr; mode }

  and strip_pattern (p : pattern) : pattern =
    { node = p.node
    ; loc = Lex.Location.empty
    ; before = strip_comments p.before
    ; after = strip_comments p.after
    }

  and strip_arg (a : arg) : arg =
    { node =
        { a.node with
          ty = strip a.node.ty
        ; after_open = strip_comments a.node.after_open
        ; after_mode = strip_comments a.node.after_mode
        ; after_var = strip_comments a.node.after_var
        }
    ; loc = Lex.Location.empty
    ; before = strip_comments a.before
    ; after = strip_comments a.after
    }

  and strip_fun (f : fun_) : fun_ =
    { node =
        { f.node with
          arg = strip_arg f.node.arg
        ; ret_ty = strip f.node.ret_ty
        ; body = strip f.node.body
        ; after_erased = strip_comments f.node.after_erased
        ; after_arg = strip_comments f.node.after_arg
        }
    ; loc = Lex.Location.empty
    ; before = strip_comments f.before
    ; after = strip_comments f.after
    }
  ;;
end

module Top_level = struct
  type node =
    | Let of
        { var : Ident.Raw.t
        ; erased : Modes.Erasure.t
        ; bind : Expr.t
        ; before_erased : Lex.Comment.t list
        ; after_erased : Lex.Comment.t list
        ; after_var : Lex.Comment.t list
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
         | Let { var; erased; bind; before_erased; after_erased; after_var } ->
           Let
             { var
             ; erased
             ; bind = Expr.strip bind
             ; before_erased = Expr.strip_comments before_erased
             ; after_erased = Expr.strip_comments after_erased
             ; after_var = Expr.strip_comments after_var
             }
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
    { items = List.map p.items ~f:Top_level.strip; after = Expr.strip_comments p.after }
  ;;
end
