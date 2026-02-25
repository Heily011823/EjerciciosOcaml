(** 
  6.3.3 Clase [text]

  Representa un texto que puede:
  - Cargarse desde un archivo
  - Guardarse en un archivo
  - Insertar texto en una posición
  - Eliminar parte del texto
  - Mostrar el contenido
*)

(**
  Clase [text]

  Modela un documento simple almacenado como una cadena.
*)
class text () =
object (self)

  (** Contenido del texto *)
  val mutable content : string = ""

  (**
    Carga el contenido desde un archivo.
    @param filename Nombre del archivo a leer
  *)
  method load (filename : string) : unit =
    let channel = open_in filename in
    let length = in_channel_length channel in
    content <- really_input_string channel length;
    close_in channel

  (**
    Guarda el contenido en un archivo.
    @param filename Nombre del archivo a escribir
  *)
  method save (filename : string) : unit =
    let channel = open_out filename in
    output_string channel content;
    close_out channel

  (**
    Inserta texto en una posición específica.
    @param position Posición donde insertar
    @param new_text Texto a insertar
  *)
  method insert (position : int) (new_text : string) : unit =
    if position >= 0 && position <= String.length content then
      let before = String.sub content 0 position in
      let after = String.sub content position (String.length content - position) in
      content <- before ^ new_text ^ after

  (**
    Elimina texto entre dos posiciones.
    @param start_pos Posición inicial
    @param end_pos Posición final
  *)
  method delete (start_pos : int) (end_pos : int) : unit =
    if start_pos >= 0 && end_pos <= String.length content && start_pos < end_pos then
      let before = String.sub content 0 start_pos in
      let after = String.sub content end_pos (String.length content - end_pos) in
      content <- before ^ after

  (**
    Muestra el contenido actual del texto.
  *)
  method display : unit =
    print_endline content

end
(**
  Prueba del ejercicio 6.3.3
*)
let () =
  let t = new text () in

  (* Insertamos texto *)
  t#insert 0 "Hola mundo";
  t#display;

  (* Insertamos más texto *)
  t#insert 4 " hermoso";
  t#display;

  (* Eliminamos parte del texto *)
  t#delete 4 12;
  t#display