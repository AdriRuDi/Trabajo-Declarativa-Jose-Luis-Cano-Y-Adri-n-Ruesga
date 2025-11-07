package Liebre_y_Perros
import Liebre_y_Perros._

def bucleJuego(tablero: TableroJuego, estado: Estado): Jugador =
  pintarTablero(estado) // paso 1

  val movimientos: Set[Posicion] = estado.turno match { // paso 2
    case Jugador.Liebre => MovimientoLiebre.movimientosPosibles(tablero, estado)
    case Jugador.Sabuesos => MovimientoSabueso.movimientosPosibles(tablero, estado)
  }

  movimientos.toList.zipWithIndex.foreach { // paso 3
    case (mov, idx) => 
    println(s"${idx + 1}. $mov")
  }

print("Elige un movimiento (número): ") // paso 4
val eleccion = scala.io.StdIn.readInt()
val listaMovimientos = movimientos.toList
val movimientoElegido = listaMovimientos(eleccion - 1)

val nuevoEstado = estado.turno match { // paso 5
  case Jugador.Liebre => estado.copy (liebre = movimientoElegido, turno = Jugador.Sabuesos)
  case Jugador.Sabuesos => 
    val (origen, destino) = movimientoElegido
    estado.copy (sabuesos = estado.sabuesos - origen + destino, turno = Jugador.Liebre)}

esFinPartida(estado,tablero) match {
  case Some(ganador) => 
    pintarTablero(estado)
    println(s"\nEl ganador es: $ganador")
    ganador
  case None => bucleJuego(tablero, nuevoEstado)  
}


