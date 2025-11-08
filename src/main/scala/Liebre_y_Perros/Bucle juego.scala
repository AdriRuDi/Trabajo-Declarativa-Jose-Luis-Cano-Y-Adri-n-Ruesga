package Liebre_y_Perros

def bucleJuego(tablero: TableroJuego, estado: Estado, modoIA: Set[Jugador]): Jugador =
  tablero.pintarTablero(estado)

  val movimientoElegido = if modoIA.contains(estado.turno) then
    estado.turno match {
      case Jugador.Liebre =>
        val movimientos = MovimientoLiebre.movimientosPosibles(tablero, estado)
        val evaluaciones = movimientos.toSeq.map { destino =>
          val heur = MovimientoLiebre.evaluarMovimiento(tablero, estado, destino)
          println(s"Liebre → $destino → heurística: $heur")
          (destino, heur)
        }
        evaluaciones.maxBy { case (_, heur) => heur }._1

      case Jugador.Sabuesos =>
        val movimientos = MovimientoSabueso.movimientosPosiblesPorSabueso(tablero, estado)
        val evaluaciones = movimientos.toSeq.map { case (origen, destino) =>
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
        MovimientoLiebre.movimientosPosibles(tablero, estado).head

      case Jugador.Sabuesos =>
        MovimientoSabueso.movimientosPosiblesPorSabueso(tablero, estado).head
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
      println(s"¡Ganador: $ganador!")
      ganador

    case None =>
      bucleJuego(tablero, nuevoEstado, modoIA)
  }

@main def partidaInicial(): Unit =
  val turno = sortearTurno()
  val estado = Estado.inicial(TableroClasicoLyS, turno)
  val modoIA = Set(Jugador.Sabuesos) // Puedes cambiar esto a Set(Jugador.Liebre) o ambos

  println(s"Empieza el turno de: $turno")
  val ganador = bucleJuego(TableroClasicoLyS, estado, modoIA)
  println(s"Ganador: $ganador")

