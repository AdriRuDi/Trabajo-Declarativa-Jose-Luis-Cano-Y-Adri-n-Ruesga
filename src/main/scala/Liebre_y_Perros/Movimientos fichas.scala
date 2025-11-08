package Liebre_y_Perros

sealed trait MovimientoFicha:
  def movimientosPosibles(tab: TableroJuego, est: Estado): Set[Posicion]

case object MovimientoLiebre extends MovimientoFicha:
  def movimientosPosibles(tab: TableroJuego, est: Estado): Set[Posicion] =
    tab.movimientosDesde(est.liebre).diff(est.sabuesos)

  def evaluarMovimiento(tab: TableroJuego, est: Estado, destino: Posicion): (Int, Int) =
    val rebasados = est.sabuesos.count(s => destino.x < s.x)
    val distanciaMeta = TableroClasicoLyS.posicionMetaLiebre.manhattan(destino)
    val sumaDistancias = est.sabuesos.map(s => destino.manhattan(s)).sum
    if (rebasados == 0) (rebasados, sumaDistancias)
    else (-distanciaMeta, sumaDistancias)

case object MovimientoSabueso extends MovimientoFicha:
  def movimientosPosibles(tab: TableroJuego, est: Estado): Set[Posicion] =
    for
      s <- est.sabuesos
      d <- tab.movimientosDesde(s)
      if d.x >= s.x && !est.ocupadas.contains(d)
    yield d

  def movimientosPosiblesPorSabueso(tab: TableroJuego, est: Estado): Set[(Posicion, Posicion)] =
    for
      s <- est.sabuesos
      d <- tab.movimientosDesde(s)
      if d.x >= s.x && !est.ocupadas.contains(d)
    yield (s, d)

  def evaluarMovimiento(tab: TableroJuego, est: Estado, origen: Posicion, destino: Posicion): (Int, Int, Int) =
    val nuevasSabuesos = est.sabuesos - origen + destino
    val movimientosAntes = MovimientoLiebre.movimientosPosibles(tab, est)
    val nuevoEstado = est.copy(sabuesos = nuevasSabuesos)
    val movimientosDespues = MovimientoLiebre.movimientosPosibles(tab, nuevoEstado)
    val bloqueos = movimientosAntes.size - movimientosDespues.size
    val distanciaMinima = nuevasSabuesos.map(_.manhattan(est.liebre)).min
    val distanciaCentro = math.abs(destino.x)
    (bloqueos, -distanciaMinima, -distanciaCentro)
