(* 1. ESPECIFICACIÓN (Lo que el mundo exterior ve) *)
(* Según Watt, esto define la API que el usuario debe conocer  *)
module type CONTADOR_SPEC = sig
  type t
  val crear : int -> t
  val incrementar : t -> t
  val leer_valor : t -> int
end

(* 2. IMPLEMENTACIÓN (Cómo funciona por dentro) *)
(* Al usar ": CONTADOR_SPEC", ocultamos los detalles internos *)
module Contador : CONTADOR_SPEC = struct
  type t = int

  (* Esta función es PRIVADA (Encapsulada). *)
  (* No se puede usar fuera de este módulo porque no está en la firma. *)
  let validar n = n >= 0

  let crear inicial = 
    if validar inicial then inicial else 0

  let incrementar c = c + 1

  let leer_valor c = c
end

(* 3. PRUEBA DE USO *)
let mi_contador = Contador.crear 5
let nuevo_valor = Contador.incrementar mi_contador
let () = Printf.printf "El valor del contador es: %d\n" (Contador.leer_valor nuevo_valor)