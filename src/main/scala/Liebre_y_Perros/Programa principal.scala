package Liebre_y_Perros
import Liebre_y_Perros._

@main def programaPrincipal(): Unit =
  println("Bienvenido al juego: Liebre y Sabuesos")
  println("=============================================")

  def elegirModoIA(): Set[Jugador] =
    println("¿Quién quieres que juegue con IA?")
    println("1. Solo sabuesos")
    println("2. Solo liebre")
    println("3. Ambos")
    println("4. Ninguno (juego manual)")
    scala.io.StdIn.readLine("Opción (1-4): ") match
      case "1" => Set(Jugador.Sabuesos)
      case "2" => Set(Jugador.Liebre)
      case "3" => Set(Jugador.Liebre, Jugador.Sabuesos)
      case "4" => Set.empty
      case _ =>
        println("Opción no válida. Se usará IA para sabuesos por defecto.")
        Set(Jugador.Sabuesos)

  def jugarPartida(): Unit =
    val turno = sortearTurno()
    val estado = Estado.inicial(TableroClasicoLyS, turno)
    val modoIA = elegirModoIA()

    println(s"\n Turno inicial: $turno")
    val ganador = bucleJuego(TableroClasicoLyS, estado, modoIA)
    println(s"\n ¡Partida terminada! Ganador: $ganador\n")

  def buclePrincipal(): Unit =
    jugarPartida()
    println("¿Quieres jugar otra partida? (s/n)")
    scala.io.StdIn.readLine().toLowerCase match
      case "s" => buclePrincipal()
      case _ =>
        println("Gracias por jugar. ¡Hasta la próxima!")

  buclePrincipal()
