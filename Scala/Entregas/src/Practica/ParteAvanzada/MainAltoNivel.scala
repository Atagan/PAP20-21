package Practica.ParteAvanzada
object Main {

  /** Funcion que crea un tablero en forma de lista con las medidas adecuadas.
   *
   * @return una instancia de la lista tablero
   */
  def crearTablero(): List[Int] = {
    val random = scala.util.Random // Utilizamos la libreria correspondiente para usar valores aleatorios
    List.tabulate(7 * 9)(_ => random.nextInt(6) + 1) // Creamos una lista de 7x9 con valores aleatorios entre 1 y 6 simulando asi el color del diamante
  }

  /** Funcion para imprimir el tablero de 7x9.
   *
   * @param l        es la lista a imprimir en forma de matriz
   * @param longFila es la longitud de la fila de la matriz
   * @param cont     es un contador para saber cuando hacer el salto de linea
   * @return una instancia de la lista tablero
   */
  def imprimir(l:List[Int], longFila: Int, cont: Int):Unit = (l,longFila,cont) match {
    case (Nil,_,_) => println
    case (head::tail,t,n) if t == n => {
      println(head)
      imprimir(tail, t, 1)
    }
    case (head::tail,t,n) => {
      print(f"$head ")
      imprimir(tail, t, n+1)
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
   * @param listaFila    es una lista; cada fila de la matriz se guarda aqui para ser actualizada con ceros si hay repeticiones
   * @param estadoAnt    es el valor de la posicion anterior a la actual (es decir, el valor anterior de la recursion de la matriz)
   * @return la lista tablero actualizada (cambiando, si las hubiere, las repeticiones obtenidas por 0s)
   */
  def comprobarTablero(tablero: List[Int], repeticiones: (Int,Int,Int), posicion: Int, tableroAct: List[Int], listaFila : List[Int], estadoAnt: Int): List[Int] = (tablero,repeticiones,posicion,tableroAct,listaFila,estadoAnt) match { //pos = 1

    // **************** CASO 1 - FIN DE TABLERO ****************
    // Cuando ya no queden elementos del tablero que examinar, devolvemos finalmente el tablero actualizado
    case (Nil,_,63,tableroA,_,_) => tableroA


    //  **************** CASO 2 - FINAL DE LINEA ****************

    // Si es final de linea y ademas esa posicion final de la fila entra dentro de una repeticion
    // Comprobamos que nos encontramos en el final de fila (pos % 7 == 6), que el diamante de la posicion (head) sea equivalente al tipo de diamante que podemos eliminar (rep._1)
    // y que ademas, y para evitar errores, sea una cadena de diamantes consecutivos de este tipo (mediante el valor del estado anterior, estadoA == head)
    case (head::tail, rep, pos, tableroA, listaF, estadoA) if (pos % 7 == 6 && rep._1 == head && rep._2 >= 2 && estadoA == head) => {
      comprobarTablero(tail, (0,0,0), pos + 1, tableroA:::actualizarLista(listaF:::List(head), (rep._1, rep._2 + 1, rep._3), 0), List(), 0)
    }

    // Si es final de linea y es necesario actualizar
    // Comprobamos nuevamente que nos encontramos en el final de la fila y ademas existen minimo 3 diamantes (una repeticion) que cambiar para actualizar (rep._2 >= 3)
    case (head::tail, rep, pos, tableroA, listaF, estadoA) if (pos % 7 == 6 && rep._2 >= 3) => {
      comprobarTablero(tail, (0,0,0), pos + 1, tableroA:::actualizarLista(listaF:::List(head), rep, 0), List(), 0)
    }

    // Si es final de linea y no es necesario actualizar
    // Comprobamos nuevamente que nos encontramos en el final de la fila y ademas no existe ninguna repeticion, es decir, no hay un minimo de 3 diamantes del mismo tipo consecutivos
    case (head::tail, rep, pos, tableroA, listaF, estadoA) if (pos % 7 == 6 && rep._2 < 3) => {
      comprobarTablero(tail, (0,0,0), pos + 1, tableroA:::(listaF:::List(head)), List(), 0)
    }

    // **************** CASO 3 - NO ES FINAL DE LINEA ****************

    // Si no nos encontramos en el final de fila
    case(head::tail, rep, pos, tableroA, listaF, estadoA) if (pos % 7 != 6)=> {
      // Si el diamante actual es repeticion
      // Comprobamos, para ello, que el tipo de diamante actual (head) es igual al que tenemos almacenado como posible repeticion (rep._1) y ademas es consecutivo (estadoA)
      if(head == rep._1 && head == estadoA){
        comprobarTablero(tail, (rep._1, rep._2 + 1, rep._3), pos + 1, tableroA, listaF:::List(head), head)
      }
      // Si no es repeticion
      else {
        // Si ya existe una repeticion previa (3 diamantes del mismo tipo consecutivos), NO modifico nada
        if (rep._2 >= 3){
          comprobarTablero(tail, rep, pos + 1, tableroA, listaF:::List(head), head)
        }
        else { //Si no existe una repeticion previa, entonces este diamante actual puede comenzar una posible repeticion
          comprobarTablero(tail, (head, 1, pos % 7), pos + 1, tableroA, listaF:::List(head), head)
        }
      }
    }
  }

  /** Funcion que dada una lista y las repeticiones que hay en ella, cambia por 0 la combinacion.
   *
   * @param fila         es la lista de la fila a actualizar
   * @param repeticiones tupla donde se obtiene la informacion de la repeticion de la fila (tipoDiamante,nVecesRepetido,posicionComienzoRep)
   * @param contador     es un contador para saber cuando hay que empezar el cambio a 0
   * @return una lista fila con las repeticiones cambiadas a ceros
   */
  def actualizarLista(fila: List[Int], repeticiones: (Int,Int,Int), contador : Int): List[Int] = (fila,repeticiones,contador) match {
    case (Nil,_,7) => Nil
    // Si no hemos llegado a la posicion de la lista donde comienza la repeticion
    case (head::tail,r,con) if (con < r._3)              => head::actualizarLista(tail, r, con + 1)

    // Si hemos llegado a la posicion exacta donde empieza la repeticion
    case (head::tail,r,con) if (con == r._3)             => 0::actualizarLista(tail, (r._1, r._2 - 1, r._3), con + 1)

    // Si ya hemos sobrepasado la posicion donde empieza la repeticion y quedan 0s por poner
    case (head::tail,r,con) if (con > r._3 && r._2 > 0)  => 0::actualizarLista(tail,(r._1, r._2 - 1, r._3), con + 1)

    // Si ya hemos sobrepasado la posicion donde empieza la repeticion y NO quedan 0s por poner
    case (head::tail,r,con) if (con > r._3 && r._2 <= 0) => head::actualizarLista(tail, r, con + 1)
  }

  /*** Funcion que busca dos valores de la lista tablero dadas dos posiciones.
   *
   * @param tablero un tablero cualquiera
   * @param pos1 primera posicion de la que se quiere obtener el valor
   * @param pos2 segunda posicion de la que se quiere obtener el valor
   * @return
   */
  def buscarValor(tablero : List[Int], pos1 : Int, pos2 : Int): (Int,Int) =  {
    (tablero(pos1),tablero(pos2))
  }

  /** Funcion que calcula la posicion dadas unas coordenadas.
   *
   *  @param cordenadas tupla con las coordenadas de una posicion en la matriz (x,y)
   *  @return el valor de la posicion que se corresponde en la lista
   */
  def calcularPos(coordenadas : (Int,Int)): Int ={
    (coordenadas._1) * 7 + coordenadas._2
  }

  /*** Funcion que cambia dos valores de posicion entre ellos.
   *
   * @param tablero es la lista del tablero donde se buscan los valores
   * @param pos1 posicion del primer valor a cambiar
   * @param pos2 posicion del segundo valor a cambiar
   * @param valores tupla con los valores de intercambio
   * @return la lista del tablero con los valores cambiados de sitio
   */
  def cambio(tablero : List[Int], pos1 : Int, pos2 : Int, valores : (Int,Int)): List[Int] =  {
    tablero.updated(pos1, valores._2).updated(pos2, valores._1)
  }

  /** Funcion que mueve los ceros hacia arriba del tablero dejando caer los demas diamantes.
   *
   *  @param tablero es la lista del tablero donde se mueven los ceros
   *  @param posicion contador para saber en que posicion del tablero nos encontramos para hacer el cambio
   *  @return la lista del tablero con los valores cambiados de sitio
   */
  def moverCeros(tablero : List[Int], posicion : Int): List[Int] = (tablero,posicion) match {
    case (t,p) if (p <= 6) => t.updated(p, scala.util.Random.nextInt(6) + 1)  //Si el cero se encuentra en la primera fila
    case (t,p) if (p > 6)  => cambio(t, p, p - 7, buscarValor(t, p, p - 7))
  }

  /** Funcion que comprueba la existencia de 0 en el tablero
   *
   * @param tablero el tablero con posibles ceros en el
   * @return el indice del primer cero del tablero si existiese, si no, -1
   */
  def hayCeros(tablero : List [Int]): Int = {
    tablero.indexOf(0)
  }

  /** Funcion que simula la caida de los diamantes cuando el inmediatamente superior es destruido o ha caido.
   *
   * @param tablero tablero que puede o no contener huecos vacios (representados por 0)
   * @return un tablero sin huecos vacios
   */

  def caer(tablero : List[Int]):List[Int] = (tablero) match{ //Limpia de ceros el tablero
    case(t) if (hayCeros(t) == -1) => t
    case(t) if (hayCeros(t) != -1) => caer(moverCeros(t, hayCeros(t)))
  }

  /** Funcion que pide por teclado la casilla sobre la que se quiere operar y la dirección a la que se le quiere desplazar
   *
   * @return una tupla de cuatro elementos, siendo los dos primeros las coordenadas (x,y) de la casilla sobre la que
   *         se quiere operar y los dos ultimos las coordenadas (x,y) a la que se quiere hacer llegar el diamante de la
   *         casilla sobre la que se quiere operar.
   */

  def pedirCoordenadas() : (Int,Int,Int,Int) = {
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

  /** Funcion que realiza llamadas a los metodos comprobarTablero y caer hasta que no es necesario que lo hagan más porque
   *  no hay más ceros en el tablero.
   *
   * @param tablero un tablero cualquiera de dimensiones 7x9
   * @return el tablero tras la simulación del proceso del juego necesaria
   */

  def actualizarTablero(tablero : List[Int]): List[Int] = (tablero) match{ //El tablero no tiene ceros, lo devuelve cuando este ya no necesite mas actualizaciones
    case(t) => {
      if (hayCeros(t) == -1){ //Si no hay mas ceros, devuelve el tablero
        t
      }else{
        actualizarTablero(comprobarTablero(caer(t), (0,0,0), 0, List(), List(), 0)) //Llamamos a actualizar tras comprobar que al caer los ceros no haya mas combinaciones
      }
    }
  }

  /** Funcion que intercambia los turnos de juego, del estado 0 que representa la actualizacion del tablero a 1 que es el turno
   *  del jugador.
   *
   * @param tablero tablero con el que se juega actualmente
   * @param estado representa el estado de juego
   * @param puntuacion puntuacion asignada por cada turno ganado +1
   */

  def turnos(tablero : List[Int], estado : Int, puntuacion : Int): Unit = (tablero, estado, puntuacion) match {
    // Si el estado del turno es 0 (es decir, hay que actualizarlo por posibles repeticiones), lo imprimo una vez actualizado
    case (t,0,p) => { //Caso de mirar repeticiones
      printf("---------- Tablero Actualizandose ----------\n\n")
      imprimir(t, 7, 1) // el tablero tiene ceros
      turnos(actualizarTablero(t), 1, p + 1) //Vamos al turno del jugador tras la actualizacion (es decir, estado = 1) sumando 1 a la puntuacion
    }

    // Si el estado del turno es 1 (es decir, es el turno del jugador)
    case(t,1,p) => { //Turno del jugador
      printf("---------- Tablero Jugador ----------\n\n")
      imprimir(t, 7, 1)
      printf(f"---------- Puntuacion: $p ----------\n\n")
      println("¿Tienes la posibilidad de combinar? Si es asi, introduzca S, si no, introduzca N: ") // Se pregunta al usuario si desea continuar
      val jugar = scala.io.StdIn.readChar().toUpper
      if(jugar.equals('N')){
        printf("---------- FIN DE LA PARTIDA  ----------\n\n")
        printf(f"---------- PUNTUACION CONSEGUIDA => $p  ----------\n\n")
      }else{
        val coordenadas = pedirCoordenadas()

        // Comprobamos las repeticiones en el tablero resultante de hacer un cambio entre la posicion indicada por el usuario y aquella acorde a la direccion hacia la que
        // desea cambiar (arriba, abajo, izquierda, derecha)
        // coordenadas._1 y coordenadas._2 -> X,Y de la posicion inicial
        // coordenadas._3 y coordenadas._4 -> X,Y de la posicion final una vez aplicada la direccion

        turnos(comprobarTablero(cambio(t,calcularPos((coordenadas._1, coordenadas._2)), calcularPos((coordenadas._3, coordenadas._4)),
          buscarValor(t, calcularPos((coordenadas._1, coordenadas._2)),
            calcularPos((coordenadas._3, coordenadas._4)))), (0,0,0), 0, List(), List(), 0), 0, p) //Llamada para ahorrar memoria en este formato
      }
    }
  }

  /** Funcion que inicia el juego, imprime las reglas, imprime el tablero y comienza el ciclo para comprobar si se han
   *  eliminado diamantes y la caida de los diamantes superiores si fuera necesario.
   *
   * @param tableroIni tablero aleatorio sin procesar.
   */
  def jugar(tableroIni : List[Int]): Unit = {
    printf("---------- Normas: ----------\n\n")
    printf("1. El rango de las coordenadas es desde (0,0) a (6,8).\n")
    printf("2. El objetivo del juego es juntar tres diamantes del mismo color Ej: 6 6 6.\n")
    printf("3. Al principio de cada turno se preguntará si quiere seguir jugando.\n")
    printf("4. El jugador tiene una puntuación.\n")
    printf("5. La puntuacion aumenta cada ronda, hasta que no queden mas movimientos para puntuar, si es asi su puntuación final es la mostrada.\n")
    printf("---------- Tablero Inicial ----------\n\n")
    imprimir(tableroIni, 7, 1)
    printf("\n¡Comienza el juego!\n\n")
    turnos(comprobarTablero(tableroIni, (0,0,0), 0, List(), List(), 0), 0, 0) //Llamamos a turnos con el tablero con ceros ya
  }

  def main (args:Array[String]): Unit =
  {
    val tableroInicial: List[Int] = crearTablero()
    jugar(tableroInicial)
  }

}