(** 
  Ejercicio 6.1.2 – Abstracción y Encapsulación en OCaml

  Este ejercicio consiste en diseñar un módulo que oculte su implementación
  interna mediante el uso de una firma (module type). 

  El objetivo es aplicar el principio de encapsulación descrito por Watt:
  el usuario solo debe conocer la especificación (la interfaz pública),
  mientras que los detalles internos deben permanecer ocultos.

  En este caso se implementa un contador no negativo como tipo abstracto.
*)

(** Firma del módulo: define lo que el usuario puede ver y usar *)
module type CONTADOR = sig

  (** Tipo abstracto del contador *)
  type t

  (** [crear n] crea un contador con valor inicial [n].
      Si [n] es negativo, se crea en 0. *)
  val crear : int -> t

  (** [incrementar c] devuelve un nuevo contador
      incrementado en 1. *)
  val incrementar : t -> t

  (** [leer c] devuelve el valor actual del contador. *)
  val leer : t -> int

end

(** Implementación del módulo Contador *)
module Contador : CONTADOR = struct

  (** Representación interna (oculta) *)
  type t = int

  let crear n =
    if n < 0 then 0 else n

  let incrementar c =
    c + 1

  let leer c =
    c

end

(** Ejemplo de uso *)
let () =
  let c = Contador.crear 5 in
  let c2 = Contador.incrementar c in
  Printf.printf "Valor actual del contador: %d\n" (Contador.leer c2)