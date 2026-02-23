(* Ejercicio 6.2.2 *)
(* Tipo Abstracto Complex *)
(* Aquí se define un tipo abstracto para representar números complejos *)

(* Operaciones incluidas:
   - Crear un número complejo
   - Sumar dos números complejos
   - Restar dos números complejos
   - Multiplicar dos números complejos
   - Calcular la magnitud
   - Convertir a cadena
*)

(* Definición de la interfaz *)
module type COMPLEJO = sig
  type t   (* tipo abstracto *)

  val crear : float -> float -> t
  val sumar : t -> t -> t
  val restar : t -> t -> t
  val multiplicar : t -> t -> t
  val magnitud : t -> float
  val convertir_a_texto : t -> string
end

(* Implementación *)
module Complex : COMPLEJO = struct

  (* Representación interna: Se guarda como parte real y parte imaginaria *)
  type t = { real: float; imaginaria: float }

  (* Crear un número complejo *)
  let crear r i =
    { real = r; imaginaria = i }

  (* Suma de números complejos *)
  let sumar complejo1 complejo2 =
    {
      real = complejo1.real +. complejo2.real;
      imaginaria = complejo1.imaginaria +. complejo2.imaginaria
    }

  (* Resta de números complejos *)
  let restar complejo1 complejo2 =
    {
      real = complejo1.real -. complejo2.real;
      imaginaria = complejo1.imaginaria -. complejo2.imaginaria
    }

  (* Multiplicación de números complejos *)
  let multiplicar complejo1 complejo2 =
    {
      real = (complejo1.real *. complejo2.real) -. (complejo1.imaginaria *. complejo2.imaginaria);
      imaginaria = (complejo1.real *. complejo2.imaginaria) +. (complejo1.imaginaria *. complejo2.real)
    }

  (* Magnitud del número complejo *)
  let magnitud complejo =
    sqrt ((complejo.real *. complejo.real) +. (complejo.imaginaria *. complejo.imaginaria))

  (* Convertir a formato a + bi *)
  let convertir_a_texto complejo =
    Printf.sprintf "%.2f + %.2fi" complejo.real complejo.imaginaria

end

(* Pruebas para verificar que funciona *)
let () =
  let complejo1 = Complex.crear 3.0 4.0 in
  let complejo2 = Complex.crear 1.0 2.0 in

  (* Suma *)
  let suma = Complex.sumar complejo1 complejo2 in

  (* Resta *)
  let resta = Complex.restar complejo1 complejo2 in

  (* Multiplicación *)
  let multiplicacion = Complex.multiplicar complejo1 complejo2 in
  let magnitud_complejo1 = Complex.magnitud complejo1 in

  print_endline ("Número complejo 1 = " ^ Complex.convertir_a_texto complejo1);
  print_endline ("Número complejo 2 = " ^ Complex.convertir_a_texto complejo2);
  print_endline ("Suma = " ^ Complex.convertir_a_texto suma);
  print_endline ("Resta = " ^ Complex.convertir_a_texto resta);
  print_endline ("Multiplicación = " ^ Complex.convertir_a_texto multiplicacion);
  print_endline ("Magnitud del número complejo 1 = " ^ string_of_float magnitud_complejo1);