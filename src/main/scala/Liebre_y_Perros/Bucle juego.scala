package Liebre_y_Perros
import Liebre_y_Perros._

def bucleJuego(tablero: TableroJuego, estado: Estado, modoIA: Set[Jugador]): Jugador =
  tablero.pintarTablero(estado)

  val movimientoElegido = if modoIA.contains(estado.turno) then
    estado.turno match {
      case Jugador.Liebre =>
        val movimientos = MovimientoLiebre.movimientosPosibles(tablero, estado).toSeq
        val evaluaciones = movimientos.map { destino =>
          val heur = MovimientoLiebre.evaluarMovimiento(tablero, estado, destino)
          println(s"Liebre → $destino → heurística: $heur")
          (destino, heur)
        }
        evaluaciones.maxBy { case (_, heur) => heur }._1

      case Jugador.Sabuesos =>
        val movimientos = MovimientoSabueso.movimientosPosiblesPorSabueso(tablero, estado).toSeq
        val evaluaciones = movimientos.map { case (origen, destino) =>
          val heur = MovimientoSabueso.evaluarMovimiento(tablero, estado, origen, destino)
          println(s"Sabueso de $origen a $destino → heurística: $heur")
          ((origen, destino), heur)
        }
        evaluaciones.maxBy { case (_, heur) => heur }(
          Ordering.Tuple3(Ordering.Int, Ordering.Int, Ordering.Int)
        )._1
    }
  else
    estado.turno match {
      case Jugador.Liebre =>
        val movimientos = MovimientoLiebre.movimientosPosibles(tablero, estado).toSeq
        def pedirMovimiento(): Posicion =
          println("Movimientos posibles para la liebre:")
          movimientos.zipWithIndex.foreach { case (m, i) => println(s"${i + 1}. $m") }
          val entrada = scala.io.StdIn.readLine("Elige movimiento (número): ")
          if entrada.forall(_.isDigit) then
            val opcion = entrada.toInt
            if opcion >= 1 && opcion <= movimientos.size then movimientos(opcion - 1)
            else
              println("Opción fuera de rango. Intenta de nuevo.\n")
              pedirMovimiento()
          else
            println("Entrada no válida. Intenta de nuevo.\n")
            pedirMovimiento()
        pedirMovimiento()

      case Jugador.Sabuesos =>
        val movimientos = MovimientoSabueso.movimientosPosiblesPorSabueso(tablero, estado).toSeq
        def pedirMovimiento(): (Posicion, Posicion) =
          println("Movimientos posibles para los sabuesos:")
          movimientos.zipWithIndex.foreach { case ((origen, destino), i) =>
            println(s"${i + 1}. De $origen a $destino")
          }
          val entrada = scala.io.StdIn.readLine("Elige movimiento (número): ")
          if entrada.forall(_.isDigit) then
            val opcion = entrada.toInt
            if opcion >= 1 && opcion <= movimientos.size then movimientos(opcion - 1)
            else
              println("Opción fuera de rango. Intenta de nuevo.\n")
              pedirMovimiento()
          else
            println("Entrada no válida. Intenta de nuevo.\n")
            pedirMovimiento()
        pedirMovimiento()
    }

  val nuevoEstado = estado.turno match {
    case Jugador.Liebre =>
      estado.copy(liebre = movimientoElegido.asInstanceOf[Posicion], turno = Jugador.Sabuesos)

    case Jugador.Sabuesos =>
      val (origen, destino) = movimientoElegido.asInstanceOf[(Posicion, Posicion)]
      estado.copy(sabuesos = estado.sabuesos - origen + destino, turno = Jugador.Liebre)
  }

  tablero.esFinPartida(nuevoEstado) match {
    case Some(ganador) =>
      tablero.pintarTablero(nuevoEstado)
      println(s"🏁 ¡Ganador: $ganador!")
      ganador

    case None =>
      bucleJuego(tablero, nuevoEstado, modoIA)
  }
