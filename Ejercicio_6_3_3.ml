(** 
  Ejemplo básico de herencia y polimorfismo en OCaml.

  Se define una clase abstracta [vehiculo] y dos clases concretas
  [carro] y [motocicleta] que heredan de ella.
*)

(**
  Clase virtual [vehiculo]

  Representa un vehículo genérico identificado por una marca.
  Es virtual porque declara el método [encender] sin implementación,
  obligando a las clases hijas a definirlo.
*)
class virtual vehiculo (marca : string) =
object
  (** Marca del vehículo *)
  val marca : string = marca

  (**
    Devuelve la marca del vehículo.
    @return string con la marca
  *)
  method obtener_marca : string = marca

  (**
    Método abstracto que debe ser implementado
    por las clases derivadas.
  *)
  method virtual encender : unit
end


(**
  Clase [carro]

  Representa un carro.
  Hereda de [vehiculo] e implementa el método [encender].
*)
class carro (marca : string) =
object
  inherit vehiculo marca

  (**
    Imprime un mensaje indicando que el carro se está encendiendo.
  *)
  method encender : unit =
    print_endline (marca ^ " se está encendiendo como carro")
end


(**
  Clase [motocicleta]

  Representa una motocicleta.
  Hereda de [vehiculo] e implementa el método [encender].
*)
class motocicleta (marca : string) =
object
  inherit vehiculo marca

  (**
    Imprime un mensaje indicando que la motocicleta se está encendiendo.
  *)
  method encender : unit =
    print_endline (marca ^ " se está encendiendo como motocicleta")
end


(**
  Función principal.

  Crea instancias de [carro] y [motocicleta]
  y ejecuta el método [encender] en cada una,
  demostrando polimorfismo dinámico.
*)
let () =
  let v1 = new carro "Toyota" in
  let v2 = new motocicleta "Yamaha" in

  v1#encender;
  v2#encender