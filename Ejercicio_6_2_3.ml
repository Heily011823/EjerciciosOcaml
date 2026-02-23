
(* Ejercicio 6.2.3: Tipo Abstracto Date *)
(* Representa fechas del calendario *)
(* Formato ISO: yyyy-mm-dd *)
(*                         *)
(* Operaciones incluidas:  *)
(* - Crear una fecha válida *)
(* - Comparar fechas con la prueba de igualdad y anterior a *)
(* - Sumar y restar días *)
(* - Convertir a formato ISO *)


(* Definición de la interfaz *)
module type DATE = sig
  type t (* tipo abstracto *)

  val crear : int -> int -> int -> t
  val igual : t -> t -> bool
  val anterior : t -> t -> bool
  val devolver_si_no : t -> t -> string   
  val sumar_dias : t -> int -> t
  val restar_dias : t -> int -> t
  val a_iso : t -> string
end


(* Implementación *)
module Date : DATE = struct

  (* Representación interna de la fecha *)
  type t = { dia:int; mes:int; anio:int }

  (* Determina si un año es bisiesto *)
  let es_bisiesto a =
    (a mod 4 = 0 && a mod 100 <> 0) || (a mod 400 = 0)

  (* Devuelve cuántos días tiene un mes *)
  let dias_en_mes m a =
    match m with
    | 1|3|5|7|8|10|12 -> 31
    | 4|6|9|11 -> 30
    | 2 -> if es_bisiesto a then 29 else 28
    | _ -> invalid_arg "Mes invalido"

  (* Constructor: crea una fecha válida *)
  let crear d m a =
    if m < 1 || m > 12 then invalid_arg "Mes invalido";
    if d < 1 || d > dias_en_mes m a then invalid_arg "Dia invalido";
    { dia=d; mes=m; anio=a }

  (* Compara si dos fechas son iguales *)
  let igual fecha1 fecha2 =
    fecha1 = fecha2

  (* Determina si una fecha es anterior a otra *)
  let anterior fecha1 fecha2 =
    if fecha1.anio <> fecha2.anio then fecha1.anio < fecha2.anio
    else if fecha1.mes <> fecha2.mes then fecha1.mes < fecha2.mes
    else fecha1.dia < fecha2.dia

  (* Devuelve "Si" o "No" dependiendo si es anterior *)
  let devolver_si_no fecha1 fecha2 =
    if anterior fecha1 fecha2 then "Si"
    else "No"

  (* Suma los días *)
  let sumar_dias f n =
    { f with dia = f.dia + n }

  (* Resta los días *)
  let restar_dias f n =
    { f with dia = f.dia - n }

  (* Convierte la fecha al formato ISO yyyy-mm-dd *)
  let a_iso f =
    Printf.sprintf "%04d-%02d-%02d" f.anio f.mes f.dia

end


(* Pruebas para verificar que si funciona *)

let () =
  let fecha1 = Date.crear 23 2 2026 in
  let fecha2 = Date.crear 25 2 2026 in

  print_endline ("Fecha 1: " ^ Date.a_iso fecha1);
  print_endline ("Fecha 2: " ^ Date.a_iso fecha2);

  (* Igualdad *)
  print_endline ("¿La fecha 1 es igual a fecha 2? " ^
    (if Date.igual fecha1 fecha2 then "Si" else "No"));

  (* Comparación *)
  print_endline ("¿La fecha 1 es anterior a la fecha 2? " ^
    Date.devolver_si_no fecha1 fecha2);

  (* Sumar los días *)
  let fecha3 = Date.sumar_dias fecha1 2 in
  print_endline ("Fecha 1 + 2 días: " ^ Date.a_iso fecha3);

  (* Restar los días *)
  let fecha4 = Date.restar_dias fecha2 2 in
  print_endline ("Fecha 2 - 2 días: " ^ Date.a_iso fecha4);
