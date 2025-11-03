package Liebre_y_Perros

// --- Enumeraciones ---
enum Fila(val y: Int):
  case A extends Fila(1)
  case M extends Fila(0)
  case B extends Fila(-1)

enum Columna(val x: Int):
  case I2 extends Columna(-2)
  case I1 extends Columna(-1)
  case M  extends Columna(0)
  case D1 extends Columna(1)
  case D2 extends Columna(2)

// --- Posición ---
case class Posicion(col: Columna, fila: Fila):
  def x: Int = col.x
  def y: Int = fila.y

// --- Jugadores ---
enum Jugador:
  case Liebre, Sabuesos

// --- Trait general del tablero ---
trait TableroJuego:
  def movimientosDesde(p: Posicion): Set[Posicion]
  def posicionInicialLiebre: Posicion
  def posicionesInicialesSabuesos: Set[Posicion]
  def posicionMetaLiebre: Posicion
  def pintarTablero(estado: Estado): Unit
  def esFinPartida(estado: Estado): Option[Jugador]

// --- Implementación del tablero clásico Liebre y Sabuesos ---
object TableroClasicoLyS extends TableroJuego:

  // --- Nodos (posiciones) ---
  val I1A = Posicion(Columna.I1, Fila.A)
  val MA  = Posicion(Columna.M,  Fila.A)
  val D1A = Posicion(Columna.D1, Fila.A)

  val I2M = Posicion(Columna.I2, Fila.M)
  val I1M = Posicion(Columna.I1, Fila.M)
  val MM  = Posicion(Columna.M,  Fila.M)
  val D1M = Posicion(Columna.D1, Fila.M)
  val D2M = Posicion(Columna.D2, Fila.M)

  val I1B = Posicion(Columna.I1, Fila.B)
  val MB  = Posicion(Columna.M,  Fila.B)
  val D1B = Posicion(Columna.D1, Fila.B)

  // --- Grafo de adyacencias ---
  val adyacencias: Map[Posicion, Set[Posicion]] = Map(
    I1A -> Set(MA, I1M, I2M, MM),
    MA  -> Set(I1A, D1A, MM),
    D1A -> Set(MA, D1M, D2M),

    I2M -> Set(I1A, I1M, I1B),
    I1M -> Set(I1A, I2M, MM, I1B),
    MM  -> Set(MA, I1M, D1M, MB, D1A, I1A, D1B, I1B),
    D1M -> Set(D1A, MM, D2M, D1B),
    D2M -> Set(D1A, D1M, D1B),

    I1B -> Set(I1M, MB, I2M, MM),
    MB  -> Set(MM, I1B, D1B),
    D1B -> Set(D1M, MB, D2M)
  )

  // --- Posiciones iniciales ---
  val posicionInicialLiebre: Posicion = D2M
  val posicionesInicialesSabuesos: Set[Posicion] = Set(I1A, I2M, I1B)
  val posicionMetaLiebre: Posicion = I2M

  // --- Movimientos posibles ---
  def movimientosDesde(p: Posicion): Set[Posicion] =
    adyacencias.getOrElse(p, Set.empty)
1
  // --- Pintado (con colores y forma hexagonal) ---
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
    println(s"    ${s(I2M)}---${s(I1M)}-----${s(MM)}-----${s(D1M)}---${s(D2M)}")
    println("       \\ | / | \\ | /")
    println(s"         ${s(I1B)}-----${s(MB)}-----${s(D1B)}")

  // --- Fin de partida (pendiente de implementar) ---
  def esFinPartida(estado: Estado): Option[Jugador] = None
