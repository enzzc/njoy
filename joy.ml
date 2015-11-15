(* joy.ml *)

type 'a pass =
    | Ok of 'a
    | TypeError of string
    | ValueError of string
    | StackError

let (>>=) mv mf = match mv with
    | Ok x -> mf x
    | TypeError msg -> TypeError msg
    | ValueError msg -> ValueError msg
    | StackError -> StackError

let (>=>) f g = fun x -> f x >>= g

let return x = Ok x

let rec sequence = function
    | [] -> return []
    | m::ms ->
        m >>= fun x ->              (* x <- m *)
        sequence ms >>= fun xs ->   (* xs <- sequence ms *)
        return (x::xs)              (* return x::xs *)

let mapM f xs = sequence (List.map f xs)

type joy =
    | Symbol of string
    | Int of int
    | Char of char
    | Float of float
    | Set of int list
    | Str of string
    | Bool of bool
    | Prog of joy list

let joy_pop = function
    | [] -> StackError
    | m::stack -> return stack

let joy_swap = function
    | [] -> StackError
    | [_] -> StackError
    | x::y::stack -> return (y::x::stack)

let joy_add = function
    | [] -> StackError
    | [_] -> StackError
    | x::y::stack ->
        match x, y, stack with
        | Int n, Int m, _ -> return (Int (n + m) :: stack)
        | Float x, Float y, _ -> return (Float (x +. y) :: stack)
        | Float x, Int n, _ -> return (Float (x +. float_of_int n) :: stack)
        | Int n, Float x, _ -> return (Float (float_of_int n +. x) :: stack)
        | _, _, _ -> TypeError "+ expects two numbers"

let joy_pow = function
    | [] -> StackError
    | [_] -> StackError
    | b::a::stack ->
        match b, a, stack with
        | Float x, Float y, _ -> return (Float (y ** x) :: stack)
        | Float x, Int n, _ -> return (Float (float_of_int n ** x) :: stack)
        | Int n, Float x, _ -> return (Float (x ** float_of_int n) :: stack)
        | _, _, _ -> TypeError "pow expects two numbers"

let joy_succ = function
    | [] -> StackError
    | x::stack ->
        match x, stack with
        | Int n, _ -> return (Int (n+1) :: stack)
        | Char c, _ -> return (Char (Char.chr (Char.code c + 1)) :: stack)
        | _, _ -> TypeError "succ expects an integer or a char"

let joy_push x = fun stack -> return (x :: stack)

let meaning xs =
    let table = function
        | Symbol "+" -> joy_add
        | Symbol "pow" -> joy_pow
        | Symbol "id" -> (fun state -> return state)
        | Symbol "pop" -> joy_pop
        | Symbol "swap" -> joy_swap
        | Symbol "succ" -> joy_succ
        (*| Symbol "map" -> joy_map*)
        | x -> joy_push x
    in List.fold_right (>=>) (List.map table xs) return

let joy_map = function
    | [] -> StackError
    | [_] -> StackError
    | x::y::stack ->
        match x, y, stack with
        | Prog p, Prog ls, _ ->
            mapM (fun x -> return [x] >>= meaning p) ls >>= fun x ->
            return ( List.map (List.hd) x ) >>= fun y ->
            return ( Prog y ) >>= fun z ->
            return ( z :: stack )
        | _, _, _ -> TypeError "map expects a list and a program"
