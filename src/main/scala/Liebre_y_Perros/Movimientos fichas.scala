package Liebre_y_Perros
import Liebre_y_Perros._

sealed trait MovimientoFicha:
  /** Devuelve todas las posiciones a las que puede moverse la ficha */
  def movimientosPosibles(tab: TableroJuego, est: Estado): Set[Posicion]

case object MovimientoLiebre extends MovimientoFicha:
  override def movimientosPosibles(tab: TableroJuego, est: Estado): Set[Posicion] =
    // Obtiene las posiciones accesibles desde la posición actual de la liebre
    val posibles = tab.movimientosDesde(est.liebre)
    // Elimina las posiciones ocupadas por los sabuesos
    posibles.diff(est.ocupadas)

case object MovimientoSabueso extends MovimientoFicha:
  override def movimientosPosibles(tab: TableroJuego, est: Estado): Set[Posicion] =
    // Calcula todos los movimientos posibles de los tres sabuesos
    val movimientos = for
      sabueso <- est.sabuesos
      destino <- tab.movimientosDesde(sabueso)
      // El sabueso solo puede avanzar o mantenerse en la misma columna (no retroceder)
      if destino.x >= sabueso.x
      // No puede moverse a una posición ya ocupada
      if !est.ocupadas.contains(destino)
    yield destino
    movimientos

  def movimientosPosiblesPorSabueso(tab: TableroJuego, est: Estado): Set[(Posicion, Posicion)] =
    // Devuelve los pares (origen, destino) posibles para cada sabueso
    val pares = for
      sabueso <- est.sabuesos
      destino <- tab.movimientosDesde(sabueso)
      // Solo puede avanzar o quedarse en la misma columna
      if destino.x >= sabueso.x
      // No puede moverse a una casilla ocupada
      if !est.ocupadas.contains(destino)
    yield (sabueso, destino)
    pares

def mostrarMovimientos(movimientos: Set[Posicion]): Unit = {
  println("\nMovimientos posibles:")
  movimientos.toList.zipWithIndex.foreach { case (mov, idx) =>
    println(s"${idx + 1}. $mov")
  }
}
