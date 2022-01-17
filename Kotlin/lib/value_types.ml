open Ast

type value =
  | Primitive of primitive_value
  | AnonymousFunction of function_t
  | Object of object_t
  | Unitialized of int option
      (** Данное значение присваивается переменной, которая была объявлена без инициализации (например, [var x: Int]). При этом, если это не просто переменная, а поле объекта, тогда Unitialized будет содержать identity_code объекта, в котором поле было объявлено *)

and record_content =
  | Variable of variable_t
  | Function of function_t
  | Class of class_t

and record_t =
  { name : string
  ; modifiers : modifier list
  ; content : record_content
  }

and variable_t =
  { var_typename : typename
  ; mutable_status : bool
  ; mutable value : value
  }

and function_t =
  { identity_code : int
  ; fun_typename : typename
  ; clojure : (record_t list[@opaque])
  ; enclosing_object : (object_t option[@opaque])
  ; arguments : (string * typename) list
  ; statement : statement list
  }

and object_t =
  { identity_code : int
  ; super : object_t option
  ; obj_class : class_t
  ; fields : record_t list
  ; methods : record_t list
  }

and class_t =
  { classname : string
  ; constructor_args : (string * typename) list
  ; clojure : (record_t list[@opaque])
  ; super_constructor : (class_t * expression) option
  ; field_initializers : var_initializer list
  ; method_initializers : fun_initializer list
  ; init_statements : statement list
  }
[@@deriving show { with_path = false }]
