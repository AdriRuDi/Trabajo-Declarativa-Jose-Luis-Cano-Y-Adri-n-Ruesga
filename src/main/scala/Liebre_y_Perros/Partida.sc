

def sortearTurno(): Jugador =
  val sorteo = scala.util.Random.nextInt(2)
  if (sorteo == 0) Jugador.Liebre
  else Jugador.Sabuesos

case class Estado(
                   liebre: Posicion,
                   sabuesos: Set[Posicion],
                   turno: Jugador
                 )
def ocupadas: Set[Posicion] =
  sabuesos + liebre

// --- Pintado ---
private def pintarNodo(p: Posicion, estado: Estado): String =
  val RESET = "\u001B[0m"
  val ROJO = "\u001B[31m"
  val AZUL = "\u001B[34m"
  val BLANCO = "\u001B[37m"

  if (estado.liebre == p) s"${ROJO}L${RESET}"
  else if (estado.sabuesos.contains(p)) s"${AZUL}S${RESET}"
  else s"${BLANCO}o${RESET}"

override def pintarTablero(estado: Estado): Unit =
  val s = pintarNodo(_, estado)
  println(s"         ${s(I1A)}-----${s(MA)}-----${s(D1A)}")
  println("       ╱ | \\ | / | \\")
  println(s"             ${s(I2M)}---${s(I1M)}-----${s(MM)}-----${s(D1M)}---${s(D2M)}")
  println("       \\ | / | \\ | /")
  println(s"         ${s(I1B)}-----${s(MB)}-----${s(D1B)}")