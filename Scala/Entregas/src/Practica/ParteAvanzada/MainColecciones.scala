package Practica.ParteAvanzada

import scala.collection.immutable.Vector

object MainColecciones {

  /** Crea un tablero en forma de Vector con las medidas adecuadas.
   *
   * @return una instancia del vector tablero
   */
  def crearTablero(): Vector[Int] = {
    val random = scala.util.Random // Utilizamos la libreria correspondiente para usar valores aleatorios
    List.tabulate(7 * 9)(_ => random.nextInt(6) + 1).toVector // Creamos un Vector de 7x9 con valores aleatorios entre 1 y 6 simulando asi el color del diamante
  }

  /** Funcion para imprimir el tablero de 7x9.
   *
   * @param vect        es el Vector a imprimir en forma de matriz
   * @param longFila es la longitud dde la fila de la matriz
   * @param cont     un contador para saber cuando hacer el salto de linea
   * @return una instancia del vector tablero
   */
  def imprimir(vect: Vector[Int], longFila: Int, cont: Int): Unit = (vect, longFila, cont) match {
    case (IndexedSeq(), _, _) => println
    case (vect, t, n) if t == n => {
      println(vect.head)
      imprimir(vect.tail, t, 1)
    }
    case (vect, t, n) => {
      print(vect.head)
      print(" ")
      imprimir(vect.tail, t, n + 1)
    }
  }

  /** Funcion para comprobar, posicion a posicion y fila a fila (de forma horizontal) si en el tablero hay combinaciones válidas.
   * Existen tres casos principales:
   * 1 - No quedan posiciones que mirar
   * 2 - La posicion en la que nos encontramos es un final de linea
   * 3 - La posicion en la que nos encontramos NO es un final de linea
   *
   * @param tablero      es el tablero del juego
   * @param repeticiones es una tupla donde se almacena la informacion de una repeticion de diamantes como: (tipoDiamante,nVecesRepetido,posicionComienzoRep)
   * @param posicion     es un contador para saber en que posición de la matriz nos encontramos
   * @param tableroAct   es el tablero actualizado (se actualiza fila a fila, es decir, cada vez que terminamos de comprobar una fila)
   * @param listaFila    es un vector; cada fila de la matriz se guarda aqui para ser actualizada con ceros si hay repeticiones
   * @param estadoAnt    es el valor de la posicion anterior a la actual (es decir, el valor anterior de la recursion de la matriz)
   * @return el vector tablero actualizada (cambiando, si las hubiere, las repeticiones obtenidas por 0s)
   */
  def comprobarTablero(tablero: Vector[Int], repeticiones: (Int, Int, Int), posicion: Int, tableroAct: Vector[Int], vectorFila: Vector[Int], estadoAnt: Int): Vector[Int] = (tablero, repeticiones, posicion, tableroAct, vectorFila, estadoAnt) match { //pos = 1

    // **************** CASO 1 - FIN DE TABLERO ****************
    // Cuando ya no queden elementos del tablero que examinar, devolvemos finalmente el tablero actualizado
    case (IndexedSeq(), _, 63, tableroA, _, _) => tableroA

    //  **************** CASO 2 - FINAL DE LINEA ****************

    // Si es final de linea y ademas esa posicion final de la fila entra dentro de una repeticion
    // Comprobamos que nos encontramos en el final de fila (pos % 7 == 6), que el diamante de la posicion (head) sea equivalente al tipo de diamante que podemos eliminar (rep._1)
    // y que ademas, y para evitar errores, sea una cadena de diamantes consecutivos de este tipo (mediante el valor del estado anterior, estadoA == head)
    case (tab, rep, pos, tableroA, vectorF, estadoA) if (pos % 7 == 6 && rep._1 == tab.head && rep._2 >= 2 && estadoA == tab.head) => {
      comprobarTablero(tab.tail, (0, 0, 0), pos + 1, tableroA ++ actualizarVector(vectorF ++ Vector(tab.head), (rep._1, rep._2 + 1, rep._3), 0), Vector(), 0)
    }
    // Si es final de linea y es necesario actualizar
    // Comprobamos nuevamente que nos encontramos en el final de la fila y ademas existen minimo 3 diamantes (una repeticion) que cambiar para actualizar (rep._2 >= 3)
    case (tab, rep, pos, tableroA, vectorF, estadoA) if (pos % 7 == 6 && rep._2 >= 3) => {
      comprobarTablero(tab.tail, (0, 0, 0), pos + 1, tableroA ++ actualizarVector(vectorF ++ Vector(tab.head), rep, 0), Vector(), 0)
    }
    // Si es final de linea y no es necesario actualizar
    // Comprobamos nuevamente que nos encontramos en el final de la fila y ademas no existe ninguna repeticion, es decir, no hay un minimo de 3 diamantes del mismo tipo consecutivos
    case (tab, rep, pos, tableroA, vectorF, estadoA) if (pos % 7 == 6 && rep._2 < 3) => {
      comprobarTablero(tab.tail, (0, 0, 0), pos + 1, tableroA ++ (vectorF ++ Vector(tab.head)), Vector(), 0)
    }

    // **************** CASO 3 - NO ES FINAL DE LINEA ****************

    // Si no nos encontramos en el final de fila
    case (tab, rep, pos, tableroA, vectorF, estadoA) if (pos % 7 != 6) => {
      // Si el diamante actual es repeticion
      // Comprobamos, para ello, que el tipo de diamante actual (head) es igual al que tenemos almacenado como posible repeticion (rep._1) y ademas es consecutivo (estadoA)
      if (tab.head == rep._1 && tab.head == estadoA) {
        comprobarTablero(tab.tail, (rep._1, rep._2 + 1, rep._3), pos + 1, tableroA, vectorF ++ Vector(tab.head), tab.head)
      }
      // Si el diamante actual no es repeticion
      else {
        // Si ya existe una repeticion previa (3 diamantes del mismo tipo consecutivos), NO modifico nada
        if (rep._2 >= 3) { //Pero ya hay una de 3 o mas anteriores
          comprobarTablero(tab.tail, rep, pos + 1, tableroA, vectorF ++ Vector(tab.head), tab.head)
        }
        else { //Si no existe una repeticion previa, entonces este diamante actual puede comenzar una posible repeticion futura (reinicio el valor de la tupla "repeticiones")
          comprobarTablero(tab.tail, (tab.head, 1, pos % 7), pos + 1, tableroA, vectorF ++ Vector(tab.head), tab.head)
        }
      }
    }
  }

  /** Funcion que dada un vector y las repeticiones que hay en el, cambia por cero la combinacion.
   *
   * @param fila         es e vector de la fila a actualizar
   * @param repeticiones tupla donde se obtiene la informacion de la repeticion de la fila (diamante,nVeces,pos)
   * @param contador     un contador para saber cuando hay que empezar el cambio a 0
   * @return un vector tablero con las repeticiones cambiadas a ceros
   */
  def actualizarVector(fila: Vector[Int], repeticiones: (Int, Int, Int), contador: Int): Vector[Int] = (fila, repeticiones, contador) match {
    // Si ha llegado al final de la fila, retorna vector vacio
    case (IndexedSeq(), _, 7) => Vector()
    // Si todavia no ha llegado a la posicion de la fila donde comienza la repeticion, no modifica el valor (es decir, head)
    case (fil, r, con) if (con < r._3) => Vector(fil.head) ++ actualizarVector(fil.tail, r, con + 1)
    // Si se encuentra en la posicion exacta donde comienza la repeticion, establezco el primer 0
    case (fil, r, con) if (con == r._3) => Vector(0) ++ actualizarVector(fil.tail, (r._1, r._2 - 1, r._3), con + 1)
    // Si se encuentra en una posicion posterior al comienzo de la repeticion y ademas quedan 0s por poner, pongo 0
    case (fil, r, con) if (con > r._3 && r._2 > 0) => Vector(0) ++ actualizarVector(fil.tail, (r._1, r._2 - 1, r._3), con + 1)
    // Si se encuentra en una posicion posterior al comienzo de la repeticion PERO no quedan 0s por poner, no modifica el valor (es decir, head)
    case (fil, r, con) if (con > r._3 && r._2 <= 0) => Vector(fil.head) ++ actualizarVector(fil.tail, r, con + 1)
  }

  /** * Busca dos valores del vector tablero dadas dos posiciones.
   *
   * @param tablero un tablero cualquiera
   * @param pos1    primera posicion de la que se quiere obtener el valor
   * @param pos2    segunda posicion de la que se quiere obtener el valor
   * @return una tupla con los valores encontrados en cada posicion
   */
  def buscarValor(tablero: Vector[Int], pos1: Int, pos2: Int): (Int, Int) = {
    (tablero(pos1), tablero(pos2))
  }

  /** Calcula la posicion dadas unas coordenadas.
   *
   * @param coordenadas tupla con las coordenadas de una posicion en la matriz (x,y)
   * @return el valor de la posicion que se corresponde en el vector
   */
  def calcularPos(coordenadas: (Int, Int)): Int = {
    (coordenadas._1) * 7 + coordenadas._2
  }

  /** * Cambia dos valores de posicion entre ellos.
   *
   * @param tablero es el vector del tablero donde se buscan los valores
   * @param pos1    posicion del primer valor a cambiar
   * @param pos2    posicion del segundo valor a cambiar
   * @param valores tupla con los valores de intercambio
   * @return el vector del tablero con los valores cambiados de sitio
   */
  def cambio(tablero: Vector[Int], pos1: Int, pos2: Int, valores: (Int, Int)): Vector[Int] = {
    tablero.updated(pos1, valores._2).updated(pos2, valores._1)
  }

  /** Mueve los ceros hacia arriba del tablero dejando caer los demas diamantes.
   *
   * @param tablero  es el vector del tablero donde se mueven los ceros
   * @param posicion contador para saber en que posicion del tablero nos encontramos para hacer el cambio
   * @return el vector del tablero con los valores cambiados de sitio
   */
  def moverCeros(tablero: Vector[Int], posicion: Int): Vector[Int] = (tablero, posicion) match {
    case (t, p) if (p <= 6) => t.updated(p, scala.util.Random.nextInt(6) + 1) //Si el cero se encuentra en la primera fila
    //Si el 0 tiene valores por encima de el, entonces lo intercambiamos con la funcion "cambio" por el valor inmediatamente por encima de el
    case (t, p) if (p > 6) => cambio(t, p, p - 7, buscarValor(t, p, p - 7))
  }

  /** Comprueba la existencia de 0 en el tablero
   *
   * @param tablero el tablero con posibles ceros en el
   * @return el indice del primer cero del tablero si existiese, si no, -1
   */
  def hayCeros(tablero: Vector[Int]): Int = {
    tablero.indexOf(0)
  }

  /** Simula la caida de los diamantes cuando el inmediatamente superior es destruido o ha caido.
   *
   * @param tablero tablero que puede o no contener huecos vacios (representados por 0)
   * @return un tablero sin huecos vacios
   */

  def caer(tablero: Vector[Int]): Vector[Int] = (tablero) match {
    // Si no hay ceros (es decir, "hayCeros" ha retornado -1), entonces devuelvo el tablero sin modificarlo
    case (t) if (hayCeros(t) == -1) => t
    // Si hay ceros, entonces hacemos uso de "moverCeros" para simular esa caida (recordamos que cambia el 0 por su valor superior)
    case (t) if (hayCeros(t) != -1) => caer(moverCeros(t, hayCeros(t)))
  }

  /** Pide por teclado la casilla sobre la que se quiere operar y la dirección a la que se le quiere desplazar
   *
   * @return una tupla de cuatro elementos, siendo los dos primeros las coordenadas (x,y) de la casilla sobre la que
   *         se quiere operar y los dos ultimos las coordenadas (x,y) a la que se quiere hacer llegar el diamante de la
   *         casilla sobre la que se quiere operar.
   */

  def pedirCoordenadas(): (Int, Int, Int, Int) = {
    println("Intoduzca la posicion X1: ")
    val x1 = scala.io.StdIn.readInt()
    println("Intoduzca la posicion Y1: ")
    val y1 = scala.io.StdIn.readInt()
    println("Intoduzca la direccion del diamante (arriba: 1, derecha: 2, abajo: 3, izquierda: 4 ): ")
    val dir = scala.io.StdIn.readInt()
    dir match {
      case 1 => (x1, y1, x1 - 1, y1) //Arriba
      case 2 => (x1, y1, x1, y1 + 1) //Derecha
      case 3 => (x1, y1, x1 + 1, y1) //Abajo
      case 4 => (x1, y1, x1, y1 - 1) //Izquierda
    }

  }

  /** Realiza llamadas a los metodos comprobarTablero y caer hasta que no es necesario que lo hagan más porque
   * no hay más ceros en el tablero.
   *
   * @param tablero un tablero cualquiera de dimensiones 7x9
   * @return el tablero tras la simulación del proceso del juego necesaria
   */

  def actualizarTablero(tablero: Vector[Int]): Vector[Int] = (tablero) match { //El tablero no tiene ceros, lo devuelve cuando este ya no necesite mas actualizaciones
    case (t) => {
      if (hayCeros(t) == -1) { //Si no hay mas ceros, devuelve el tablero
        t
      } else {
        actualizarTablero(comprobarTablero(caer(t), (0, 0, 0), 0, Vector(), Vector(), 0)) //Llamamos a actualizar tras comprobar que al caer los ceros no haya mas combinaciones
      }
    }
  }

  /** Intercambia los turnos de juego, del estado 0 que representa la actualizacion del tablero a 1 que es el turno
   * del jugador.
   *
   * @param tablero    tablero con el que se juega actualmente
   * @param estado     representa el estado de juego
   * @param puntuacion puntuacion asignada por cada turno ganado +1
   */

  def turnos(tablero: Vector[Int], estado: Int, puntuacion: Int): Unit = (tablero, estado, puntuacion) match {
    case (t, 0, p) => { //Caso de mirar repeticiones
      printf("---------- Tablero Actualizandose ----------\n\n")
      imprimir(t, 7, 1) // el tablero tiene ceros
      turnos(actualizarTablero(t), 1, p + 1) //Vamos al turno del jugador tras la actualizacion
    }
    case (t, 1, p) => { //Turno del jugador
      printf("---------- Tablero Jugador ----------\n\n")
      imprimir(t, 7, 1)
      printf(f"---------- Puntuacion: $p ----------\n\n")
      println("¿Tienes la posibilidad de combinar? Si es asi, introduzca S, si no, introduzca N: ")
      val jugar = scala.io.StdIn.readChar().toUpper
      if (jugar.equals('N')) {
        printf("---------- FIN DE LA PARTIDA  ----------\n\n")
        printf(f"---------- PUNTUACION CONSEGUIDA => $p  ----------\n\n")
      } else {
        val coordenadas = pedirCoordenadas()
        turnos(comprobarTablero(cambio(t, calcularPos((coordenadas._1, coordenadas._2)), calcularPos((coordenadas._3, coordenadas._4)),
          buscarValor(t, calcularPos((coordenadas._1, coordenadas._2)),
            calcularPos((coordenadas._3, coordenadas._4)))), (0, 0, 0), 0, Vector(), Vector(), 0), 0, p) //Llamada para ahorrar memoria en este formato
      }
    }
  }

  /** Inicia el juego, imprime las reglas, imprime el tablero y comienza el ciclo para comprobar si se han
   * eliminado diamantes y la caida de los diamantes superiores si fuera necesario.
   *
   * @param tableroIni tablero aleatorio sin procesar.
   */
  def jugar(tableroIni: Vector[Int]): Unit = {
    printf("---------- Normas: ----------\n\n")
    printf("1. El rango de las coordenadas es desde (0,0) a (6,8).\n")
    printf("2. El objetivo del juego es juntar tres diamantes del mismo color Ej: 6 6 6.\n")
    printf("3. Al principio de cada turno se preguntará si quiere seguir jugando.\n")
    printf("4. El jugador tiene una puntuación.\n")
    printf("5. La puntuacion aumenta cada ronda, hasta que no queden mas movimientos para puntuar, si es asi su puntuación final es la mostrada.\n")
    printf("---------- Tablero Inicial ----------\n\n")
    imprimir(tableroIni, 7, 1)
    printf("\n¡Comienza el juego!\n\n")
    turnos(comprobarTablero(tableroIni, (0, 0, 0), 0, Vector(), Vector(), 0), 0, 0) //Llamamos a turnos con el tablero con ceros ya
  }

  def main(args: Array[String]): Unit = {
    val tableroInicial: (Vector[Int]) = crearTablero()
    jugar(tableroInicial)
  }

}