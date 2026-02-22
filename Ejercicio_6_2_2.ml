
(* Ejercicio: Tipo Abstracto Money              *)
(* Moneda: Pesos Colombianos (COP)              *)


(* Defino la interfaz *)
module type MONEY = sig
  type t   (* tipo abstracto *)

  val crear : int -> int -> t
  val sumar : t -> t -> t
  val restar : t -> t -> t
  val multiplicar : t -> int -> t
  val a_cadena : t -> string
end

(* Implementación *)
module Money : MONEY = struct

  (* Decidí guardar el dinero en centavos
     para evitar errores de precisión con float *)
  type t = int  (* total en centavos *)

  (* Crear una cantidad de dinero *)
  let crear pesos centavos =
    (pesos * 100) + centavos

  (* Sumar dos cantidades *)
  let sumar dinero1 dinero2 =
    dinero1 + dinero2

  (* Restar dos cantidades *)
  let restar dinero1 dinero2 =
    dinero1 - dinero2

  (* Multiplicar por un entero *)
  let multiplicar m n =
    m * n

  (* Mostrar en formato pesos colombianos *)
  let a_cadena m =
    let pesos = m / 100 in
    let centavos = abs (m mod 100) in
    Printf.sprintf "$%d.%02d COP" pesos centavos
end

(* Pruebas que hice para verificar *)
let () =
  let dinero1 = Money.crear 15000 50 in
  let dinero2 = Money.crear 3200 75 in

  let suma = Money.sumar dinero1 dinero2 in
  let resta = Money.restar dinero1 dinero2 in
  let triple = Money.multiplicar dinero1 3 in

  print_endline ("Dinero 1 = " ^ Money.a_cadena dinero1);
  print_endline ("Dinero 2 = " ^ Money.a_cadena dinero2);
  print_endline ("Suma = " ^ Money.a_cadena suma);
  print_endline ("Resta = " ^ Money.a_cadena resta);
  print_endline ("Triple del dinero = " ^ Money.a_cadena triple);