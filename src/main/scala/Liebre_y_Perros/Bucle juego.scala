package Liebre_y_Perros
import Liebre_y_Perros._

def bucleJuego(tablero: TableroJuego, estado: Estado): Jugador =
  pintarTablero(estado) // paso 1

  val movimientos: Set[Posicion] = estado.turno match { // paso 2
    case liebre => MovimientoLiebre(tablero, estado)
    case sabuesos => MovimientoSabueso(tablero, estado)
  }

mostrarMovimientos(movimientos) // paso 3

print("Elige un movimiento (número): ") // paso 4
val eleccion = scala.io.StdIn.readInt()
val listaMovimientos = movimientos.toList
val movimientoElegido = listaMovimientos(eleccion - 1)

val nuevoEstado = Estado.turno match { // paso 5
  case liebre => Estado.copy (Liebre = movimientoElegido)
  case sabuesos => 
    val (origen, destino) = movimientoElegido
    Estado.copy (Sabuesos = Estado.sabuesos - origen + destino)
}

val finalPartida = 