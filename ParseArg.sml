structure ParseArg : PARSE_ARG = struct
  exception Exit of string list
  datatype t = Nullary of string * (unit -> unit)
             | Unary of string * (string -> unit)
             | Multi of string * (string list -> unit)
  fun isFlag x = (String.sub(x,0) = #"-") handle _ => false
  fun read_args xs =
      let fun loop nil a = (rev a, nil) 
            | loop (x::xs) a =
              if isFlag x then (rev a, x::xs)
              else loop xs (x::a)
      in loop xs nil
      end
  fun getNullary x (Nullary(y,f)::ts) = 
      if x = y then SOME f
      else getNullary x ts
    | getNullary x (t::ts) = getNullary x ts
    | getNullary x nil = NONE
  fun getUnary x (Unary(y,f)::ts) = 
      if x = y then SOME f
      else getUnary x ts
    | getUnary x (t::ts) = getUnary x ts
    | getUnary x nil = NONE
  fun getMulti x (Multi(y,f)::ts) = 
      if x = y then SOME f
      else getMulti x ts
    | getMulti x (t::ts) = getMulti x ts
    | getMulti x nil = NONE
                      
  fun err s = (print (s ^ "\n"); raise Fail "ParseArg error")
  fun unknown x = err ("Unknown argument '" ^ x ^ "'")
  fun run ts =
      let 
        fun loop nil = []
          | loop (x::xs) =
            if isFlag x then
              let val (args, xs) = read_args xs
                  val () = 
                      case args of
                        nil => 
                        (case getNullary x ts of
                           SOME f => f()
                         | NONE => unknown x)
                      | [a] => 
                        (case getUnary x ts of
                           SOME f => f a
                         | NONE => 
                           (case getMulti x ts of
                              SOME f => f [a]
                            | NONE => 
                              (case getNullary x ts of
                                 SOME f => (f(); raise Exit(a::xs))
                               | NONE => unknown x)))
                      | a::aa =>
                        (case getMulti x ts of
                           SOME f => f args
                         | NONE => 
                           (case getUnary x ts of
                              SOME f => (f a; raise Exit(aa@xs))
                            | NONE =>
                              (case getNullary x ts of
                                 SOME f => (f(); raise Exit(args@xs))
                               | NONE => unknown x)))
              in loop xs
              end
            else raise Exit(x::xs)
      in loop (CommandLine.arguments()) handle Exit ys => ys
      end
end
