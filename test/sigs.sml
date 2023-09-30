
(** A signature *)

signature A = sig
  val a : int
end

(**

[a] an integer.

*)

val hi = 45 (* a top-level binding *)

structure K : A =
  struct val a = 6 end

(** Another signature *)

signature B = sig
  val b : int
end

(**

[b] another integer.

*)

structure J : A =
  K

structure S : B =
  struct val b = 34 end
