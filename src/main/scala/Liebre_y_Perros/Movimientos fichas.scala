package Liebre_y_Perros
import Liebre_y_Perros._

sealed trait MovimientoFicha:
  /** Devuelve todas las posiciones a las que puede moverse la ficha */
  def movimientosPosibles(tab: TableroJuego, est: Estado): Set[Posicion]

case object MovimientoLiebre extends MovimientoFicha{
  override def movimientosPosibles(tab: TableroJuego, est: Estado): Set[Posicion] =
    // Obtiene las posiciones accesibles desde la posición actual de la liebre
    val posibles = tab.movimientosDesde(est.liebre)
    // Elimina las posiciones ocupadas por los sabuesos
    posibles.diff(est.ocupadas)

   override def evaluarMovimiento(tablero: TableroJuego, estado: Estado, destino: Posicion): (Int, Int) =
     //primero vamos a calcular el primer valor
     val rebasados = estado.sabuesos.count(sab => estado.liebre.x < sab.x) // numero de sabuesos rebasados
     val metricaDistanciaMeta = 4 - destino.manhattan(TableroJuego.posicionMetaLiebre) // metrica de liebre hasta la meta
     val sumaDistancias = estado.sabuesos.map(sab => destino.manhattan(sab)).sum // suma de la distancia entre la liebre y todos los sabuesos
     if (estado.sabuesos.exists(sab => estado.liebre.x >= sab.x)){ val resultado = (rebasados,sumaDistancias)}
     else{val resultado = (metricaDistanciaMeta,sumaDistancias)}
}

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
