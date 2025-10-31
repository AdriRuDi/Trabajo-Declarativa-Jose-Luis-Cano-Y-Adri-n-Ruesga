// enumerar filas y columnas
enum Fila(val y: Int):
  case A extends Fila(1)
  case B extends Fila(0)
  case C extends Fila(-1)

enum Columna(val x: Int):
  case I2 extends Columna(-2)
  case I1 extends Columna(-1)
  case M extends Columna(0)
  case D1 extends Columna(1)
  case D2 extends Columna(2)

// Posición
case class Posicion (col: Columna, fila: Fila):
  def x: Int = col.x
  def y: Int = fila.y

// Jugador
enum Jugador:
  case Liebre, Sabuesos

// Estado de la partida
case class Estado (
                    liebre: Posicion,
                    sabuesos: Set[Posicion])

// Trait TableroJuego
trait TableroJuego:
  // Devuelve las posiciones accesibles desde una posición
  def movimientosDesde(p: Posicion): Set[Posicion]

  // Posiciones iniciales de la liebre y sabuesos
  def posicionInicialLiebre: Posicion
  def posicionesInicialesSabuesos: Set[Posicion]
  def posicionesMetaLiebre: Set[Posicion]

  // Pinta el tablero para un estado dado
  def pintarTablero(estado: Estado): Unit

  // Comprueba si ha terminado la partida
  def esFinPartida(estado: Estado): Option[Jugador]

// Tablero clásico Liebre y Sabuesos
object TableroClasicoLyS extends TableroJuego:

  // Definir nodos (posiciones)

  val A_I1 = Posicion(Columna.I1, Fila.A)
  val A_M  = Posicion(Columna.M,  Fila.A)
  val A_D1 = Posicion(Columna.D1, Fila.A)


  val M_I2 = Posicion(Columna.I2, Fila.B)
  val M_I1 = Posicion(Columna.I1, Fila.B)
  val M_M  = Posicion(Columna.M,  Fila.B)
  val M_D1 = Posicion(Columna.D1, Fila.B)
  val M_D2 = Posicion(Columna.D2, Fila.B)


  val B_I1 = Posicion(Columna.I1, Fila.C)
  val B_M  = Posicion(Columna.M,  Fila.C)
  val B_D1 = Posicion(Columna.D1, Fila.C)


  // Grafo de adyacencias
  val adyacencias: Map[Posicion, Set[Posicion]] = Map(

    A_I1 -> Set( A_M, M_I1, M_I2, M_M),
    A_M  -> Set(A_I1, A_D1, M_M),
    A_D1 -> Set(A_M, M_D1, M_D2),


    M_I2 -> Set(A_I1, M_I1,B_I1),
    M_I1 -> Set(A_I1, M_I2, M_M, B_I1),
    M_M  -> Set(A_M, M_I1, M_D1, B_M, A_D1, A_I1, B_D1, B_I1),
    M_D1 -> Set(A_D1, M_M, M_D2, B_D1),
    M_D2 -> Set(A_D1, M_D1, B_D1),


    B_I1 -> Set(M_I1, B_M, M_I2, M_M),
    B_M  -> Set(M_M, B_I1, B_D1),
    B_D1 -> Set(M_D1, B_M, M_D2)
  )

  // Implementación de métodos
  def movimientosDesde(p: Posicion): Set[Posicion] = adyacencias.getOrElse(p, Set())

  val posicionInicialLiebre: Posicion = M_D2
  val posicionesInicialesSabuesos: Set[Posicion] = Set(A_I1, M_I2, B_I1)
  val posicionesMetaLiebre: Set[Posicion] = M_I2

  def pintarTablero(estado: Estado): Unit =
    println("Pintar tablero aún no implementado")

  def esFinPartida(estado: Estado): Option[Jugador] =
    None // Lógica de fin de partida pendiente