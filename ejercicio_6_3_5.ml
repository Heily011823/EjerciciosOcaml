(** 
  Ejercicio: Modelado de interfaces en OCaml con estudiantes.

  Se definen dos interfaces:
  - [comparable] para comparar estudiantes por promedio.
  - [printable] para mostrar información del estudiante.
*)

(** 
  Interfaz [comparable].

  Define el contrato de comparación.
*)
class type comparable =
object
  method compare : 'a -> int
end

(** 
  Interfaz [printable].

  Define el contrato para convertir un objeto a texto.
*)
class type printable =
object
  method to_string : string
end

(** 
  Clase [estudiante].

  Representa un estudiante con nombre y promedio.

  Implementa:
  - [printable]
  - [comparable]

  La comparación se realiza según el promedio.
*)
class estudiante (nombre : string) (promedio : float) =
object (self)

  (** Devuelve el nombre del estudiante *)
  method get_nombre = nombre

  (** Devuelve el promedio del estudiante *)
  method get_promedio = promedio

  (** 
    Compara dos estudiantes según su promedio.

    Devuelve:
    - negativo si el actual tiene menor promedio
    - 0 si son iguales
    - positivo si el actual tiene mayor promedio
  *)
  method compare (other : estudiante) =
    if promedio < other#get_promedio then -1
    else if promedio > other#get_promedio then 1
    else 0

  (** 
    Devuelve la representación textual del estudiante.
  *)
  method to_string =
    nombre ^ " - Promedio: " ^ string_of_float promedio
end


(** 
  Programa principal.

  Se crean dos estudiantes y se comparan sus promedios.
*)
let () =
  let e1 = new estudiante "Ana" 4.5 in
  let e2 = new estudiante "Carlos" 3.8 in

  print_endline (e1#to_string);
  print_endline (e2#to_string);

  print_endline ("Resultado comparación: " ^string_of_int (e1#compare e2))
              