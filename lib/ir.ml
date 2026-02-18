open! Core

type ty =
  | Unit
  | Bool
  | Int
  | Arrow of
      { arg_ty : ty
      ; ret_ty : ty
      }
  | Pack
[@@deriving sexp]

and closure =
  { arg : Ident.t
  ; ty : ty
  ; body : expr
  }
[@@deriving sexp]

and mono =
  { arg : Tst.Value.Concrete.t
  ; ty : ty
  ; body : expr
  }
[@@deriving sexp]

and pack = mono Vec.t [@@deriving sexp]

and value =
  | Unit
  | Bool of bool
  | Int of int64
  | Closure of closure
  | Pack of pack
  | External of
      { symbol : string
      ; ty : ty
      }
[@@deriving sexp]

and fun_ =
  | Mono of
      { var : Ident.t
      ; arg : Ident.t
      ; body : expr
      ; ty : ty
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Pack of
      { var : Ident.t
      ; pack : pack
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
[@@deriving sexp]

and expr =
  | Literal of
      { value : value
      ; ty : ty
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Fun of
      { funs : fun_ Nonempty_list.t
      ; rest : expr
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Lambda of
      { arg : Ident.t
      ; ty : ty
      ; body : expr
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Apply of
      { fn : expr
      ; arg : expr
      ; ty : ty
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Let of
      { var : Ident.t
      ; bind : expr
      ; rest : expr
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Unop of
      { op : Cst.Unop.t
      ; arg : expr
      ; ty : ty
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Binop of
      { op : Cst.Binop.t
      ; lhs : expr
      ; rhs : expr
      ; ty : ty
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | If of
      { cond : expr
      ; then_ : expr
      ; else_ : expr
      ; ty : ty
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Var of
      { id : Ident.t
      ; ty : ty
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Pack of
      { pack : pack
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Symbol of
      { id : Ident.t
      ; arg : Tst.Value.Concrete.t
      ; ty : ty
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
[@@deriving sexp]

module Ty = struct
  type t = ty =
    | Unit
    | Bool
    | Int
    | Arrow of
        { arg_ty : t
        ; ret_ty : t
        }
    | Pack
  [@@deriving sexp]
end

module Closure = struct
  type t = closure =
    { arg : Ident.t
    ; ty : ty
    ; body : expr
    }
  [@@deriving sexp]
end

module Value = struct
  type t = value =
    | Unit
    | Bool of bool
    | Int of int64
    | Closure of closure
    | Pack of pack
    | External of
        { symbol : string
        ; ty : ty
        }
  [@@deriving sexp]
end

module Pack = struct
  module Mono = struct
    type t = mono =
      { arg : Tst.Value.Concrete.t
      ; ty : ty
      ; body : expr
      }
    [@@deriving sexp]
  end

  type t = mono Vec.t [@@deriving sexp]
end

module Expr = struct
  type nonrec fun_ = fun_ =
    | Mono of
        { var : Ident.t
        ; arg : Ident.t
        ; body : expr
        ; ty : ty
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Pack of
        { var : Ident.t
        ; pack : pack
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
  [@@deriving sexp]

  type t = expr =
    | Literal of
        { value : value
        ; ty : ty
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Fun of
        { funs : fun_ Nonempty_list.t
        ; rest : expr
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Lambda of
        { arg : Ident.t
        ; ty : ty
        ; body : expr
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Apply of
        { fn : expr
        ; arg : expr
        ; ty : ty
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Let of
        { var : Ident.t
        ; bind : expr
        ; rest : expr
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Unop of
        { op : Cst.Unop.t
        ; arg : expr
        ; ty : ty
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Binop of
        { op : Cst.Binop.t
        ; lhs : expr
        ; rhs : expr
        ; ty : ty
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | If of
        { cond : expr
        ; then_ : expr
        ; else_ : expr
        ; ty : ty
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Var of
        { id : Ident.t
        ; ty : ty
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Pack of
        { pack : pack
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Symbol of
        { id : Ident.t
        ; arg : Tst.Value.Concrete.t
        ; ty : ty
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
  [@@deriving sexp]
end

module Top_level = struct
  type t =
    | Let of
        { var : Ident.t
        ; bind : Expr.t
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Fun of
        { funs : Expr.fun_ Nonempty_list.t
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | External of
        { var : Ident.t
        ; symbol : string
        ; ty : Ty.t
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
  [@@deriving sexp]
end

module Program = struct
  type t = Top_level.t list [@@deriving sexp]
end
