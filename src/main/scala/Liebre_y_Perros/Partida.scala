package Liebre_y_Perros
import Liebre_y_Perros._

/** Sortea aleatoriamente quién empieza la partida */
def sortearTurno(): Jugador =
  if scala.util.Random.nextInt(2) == 0 then Jugador.Liebre else Jugador.Sabuesos

/** Estado del juego con turno incluido */
case class Estado(
                   liebre: Posicion,
                   sabuesos: Set[Posicion],
                   turno: Jugador
                 ):
  def ocupadas: Set[Posicion] = sabuesos + liebre

/** Companion object para crear el estado inicial */
object Estado:
  def inicial(tablero: TableroJuego, turnoInicial: Jugador): Estado =
    Estado(
      liebre = tablero.posicionInicialLiebre,
      sabuesos = tablero.posicionesInicialesSabuesos,
      turno = turnoInicial
    )

