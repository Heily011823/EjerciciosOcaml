(** 
  6.3.5 Jerarquía de cuentas bancarias

  Clase base abstracta [account].
  Representa una cuenta bancaria genérica.
*)

class virtual account (name : string) (address : string) (balance : float) =
object (self)

  (** Nombre del titular *)
  val mutable name : string = name

  (** Dirección del titular *)
  val mutable address : string = address

  (** Saldo actual *)
  val mutable balance : float = balance

  (**
    Deposita dinero en la cuenta.
    @param amount Cantidad a depositar
  *)
  method deposit (amount : float) : unit =
    balance <- balance +. amount

  (**
    Retira dinero si hay saldo suficiente.
    @param amount Cantidad a retirar
  *)
  method withdraw (amount : float) : unit =
    if amount <= balance then
      balance <- balance -. amount

  (**
    Devuelve el saldo actual.
  *)
  method get_balance : float = balance

  (**
    Cambia la dirección del titular.
  *)
  method change_address (new_address : string) : unit =
    address <- new_address

  (**
    Método abstracto que cada cuenta debe implementar.
  *)
  method virtual print_statement : unit

end

(**
  Clase [basic_account]

  Cuenta básica que solo usa el comportamiento normal.
*)
class basic_account name address balance =
object
  inherit account name address balance

  method print_statement : unit =
    Printf.printf "Basic Account\nSaldo: %.2f\n" balance
end
(**
  Clase [savings_account]

  Cuenta de ahorros que genera interés.
*)
class savings_account name address balance interest_rate =
object
  inherit account name address balance

  val interest_rate : float = interest_rate

  (**
    Aplica interés al saldo.
  *)
  method pay_interest : unit =
    balance <- balance +. (balance *. interest_rate)

  method print_statement : unit =
    Printf.printf "Savings Account\nSaldo: %.2f\n" balance
end
(**
  Clase [current_account]

  Cuenta corriente que permite sobregiro.
*)
class current_account name address balance overdraft_limit =
object
  inherit account name address balance

  val overdraft_limit : float = overdraft_limit

  (**
    Permite retirar incluso si el saldo queda negativo
    hasta un límite permitido.
  *)
  method withdraw (amount : float) : unit =
    if balance -. amount >= -.overdraft_limit then
      balance <- balance -. amount

  method print_statement : unit =
    Printf.printf "Current Account\nSaldo: %.2f\n" balance
end

(**
  Prueba del ejercicio 6.3.5

  En esta sección se crean instancias de las diferentes
  clases de cuentas bancarias para demostrar el uso
  de herencia y polimorfismo.

  Se realizan las siguientes acciones:

  - Se crea una cuenta básica y se deposita dinero.
  - Se crea una cuenta de ahorros y se aplica interés.
  - Se crea una cuenta corriente y se realiza un retiro
    que utiliza el sobregiro permitido.

  Finalmente, se imprime el estado de cada cuenta
  usando el método [print_statement].
*)
let () =
  (**
    Se crea una cuenta básica con saldo inicial de 1000.0
  *)
  let cuenta1 = new basic_account "Ana" "Calle 1" 1000.0 in

  (**
    Se crea una cuenta de ahorros con interés del 5%
  *)
  let cuenta2 = new savings_account "Luis" "Calle 2" 2000.0 0.05 in

  (**
    Se crea una cuenta corriente con límite de sobregiro de 300.0
  *)
  let cuenta3 = new current_account "Maria" "Calle 3" 500.0 300.0 in

  (**
    Operaciones realizadas sobre las cuentas
  *)
  cuenta1#deposit 200.0;        (* Nuevo saldo: 1200.0 *)
  cuenta2#pay_interest;         (* Aplica interés al saldo *)
  cuenta3#withdraw 700.0;       (* Usa sobregiro permitido *)

  (**
    Se imprime el estado final de cada cuenta
  *)
  cuenta1#print_statement;
  cuenta2#print_statement;
  cuenta3#print_statement