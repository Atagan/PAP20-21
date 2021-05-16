package ParteObligatoria

import scala.annotation.tailrec

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

  @tailrec
  def imprimir(l:List[Int], longFila: Int, cont: Int):Unit = (l,longFila,cont) match {
    case (Nil,_,_) => println
    case (head::tail, t, n) if t == n => {
      println(head)
      imprimir(tail, t, 1)
    }
    case (head::tail, t, n) => {
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
    case (_,_,64,tableroA,_,_) => tableroA

    //  **************** CASO 2 - FINAL DE LINEA ****************

    // Si es final de linea y ademas esa posicion final de la fila entra dentro de una repeticion
    // Comprobamos que nos encontramos en el final de fila (pos % 7 == 6), que el diamante de la posicion (head) sea equivalente al tipo de diamante que podemos eliminar (rep._1)
    // y que ademas, y para evitar errores, sea una cadena de diamantes consecutivos de este tipo (mediante el valor del estado anterior, estadoA == head)
    case (head::tail, rep, pos, tableroA, listaF, estadoA) if (pos % 7 == 0 && rep._1 == head && rep._2 >= 2 && estadoA == head) => {
      comprobarTablero(tail, (0,0,0), pos + 1, tableroA:::actualizarLista(listaF:::List(head), (rep._1,rep._2 + 1, rep._3),1), List(), 0)
    }
    // Si es final de linea y es necesario actualizar
    // Comprobamos nuevamente que nos encontramos en el final de la fila y ademas existen minimo 3 diamantes (una repeticion) que cambiar para actualizar (rep._2 >= 3)
    case (head::tail, rep, pos, tableroA, listaF, estadoA) if (pos % 7 == 0 && rep._2 >= 3) => {
      comprobarTablero(tail, (0,0,0), pos + 1, tableroA:::actualizarLista(listaF:::List(head),rep,1), List(), 0)
    }
    // Si es final de linea y no es necesario actualizar
    // Comprobamos nuevamente que nos encontramos en el final de la fila y ademas no existe ninguna repeticion, es decir, no hay un minimo de 3 diamantes del mismo tipo consecutivos
    case (head::tail, rep, pos, tableroA, listaF, estadoA) if (pos % 7 == 0 && rep._2 < 3) => {
      comprobarTablero(tail, (0,0,0), pos + 1, tableroA:::(listaF:::List(head)), List(), 0)
    }

    // **************** CASO 3 - NO ES FINAL DE LINEA ****************

    // Si no nos encontramos en el final de fila
    case(head::tail, rep, pos, tableroA, listaF, estadoA) if (pos % 7 != 0)=> {
      // Si el diamante actual es repeticion
      // Comprobamos, para ello, que el tipo de diamante actual (head) es igual al que tenemos almacenado como posible repeticion (rep._1) y ademas es consecutivo (estadoA)
      if(head == rep._1 && head == estadoA){
        // println("Llega a la linea 44 con pos = ",pos,rep)
        comprobarTablero(tail, (rep._1, rep._2 + 1, rep._3), pos + 1, tableroA, listaF:::List(head), head)
      }
      // Si el diamante actual no es repeticion
      else {
        // Si ya existe una repeticion previa (3 diamantes del mismo tipo consecutivos), NO modifico nada
        if (rep._2 >= 3){ //Pero ya hay una de 3 o mas anteriores
          comprobarTablero(tail, rep, pos + 1, tableroA, listaF:::List(head), head)
        }
        else { //Si no existe una repeticion previa, entonces este diamante actual puede comenzar una posible repeticion futura (reinicio el valor de la tupla "repeticiones")
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

  def actualizarLista(fila: List[Int], repeticiones: (Int, Int, Int), contador: Int): List[Int] = (fila, repeticiones, contador) match {
    // Si ha llegado al final de la fila, retorna lista vacia
    case (Nil, _, _) => Nil
    // Si todavia no ha llegado a la posicion de la fila donde comienza la repeticion, no modifica el valor (es decir, head)
    case (head :: tail, r, con) if (con < r._3) => head :: actualizarLista(tail, r, con + 1)
    // Si se encuentra en la posicion exacta donde comienza la repeticion, establezco el primer 0
    case (head :: tail, r, con) if (con == r._3) => 0 :: actualizarLista(tail, (r._1, r._2 - 1, r._3), con + 1)
    // Si se encuentra en una posicion posterior al comienzo de la repeticion y ademas quedan 0s por poner, pongo 0
    case (head :: tail, r, con) if (con > r._3 && r._2 > 0) => 0 :: actualizarLista(tail, (r._1, r._2 - 1, r._3), con + 1)
    // Si se encuentra en una posicion posterior al comienzo de la repeticion PERO no quedan 0s por poner, no modifica el valor (es decir, head)
    case (head :: tail, r, con) if (con > r._3 && r._2 <= 0) => head :: actualizarLista(tail, r, con + 1)
  }

  /** Funcion que busca dos valores de la lista tablero dadas dos posiciones
   * Su utilidad reside principalmente, por ejemplo, para el cambio de dos diamantes deseadas por el usuario (3 2 -> 2 3)
   * Esto lo hacemos asi y no como una unica posicion para una mayor eficiencia y ahorro de recursos
   *
   * @param tablero  es la lista del tablero donde buscar
   * @param contador es un contador para saber cuando hay que obtener ese valor a buscar
   * @param pos1     es la posicion del primer valor a buscar
   * @param pos2     es la posicion del segundo valor a buscar
   * @param output   es una tupla donde almacenamos finalmente los dos valores encontrados (valor1, valor2)
   * @return una tupla con los valores encontrados en cada posicion
   */

  def buscarValor(tablero: List[Int], contador: Int, pos1: Int, pos2: Int, output: (Int, Int)): (Int, Int) = (tablero, contador, pos1, pos2, output) match {
    // Si no quedan posiciones donde buscar, devuelve la tupla con los dos valores encontrados
    case (Nil, _, _, _, out) => out
    // Si nos encontramos en la posicion donde esta el primer valor que queremos encontrar, lo metemos en la tupla (en out._1)
    case (head :: tail, cont, p1, p2, out) if (cont == pos1) => buscarValor(tail, cont + 1, p1, p2, (head, out._2))
    // Si nos encontramos en la posicion donde esta el segundo valor que queremos encontrar, lo metemos en la tupla (en out._2)
    case (head :: tail, cont, p1, p2, out) if (cont == pos2) => buscarValor(tail, cont + 1, p1, p2, (out._1, head))
    // Si no nos encontramos en la posicion donde estemos buscando alguno de los valores, sigo buscando
    case (head :: tail, cont, p1, p2, out) if (cont != pos1 && cont != pos2) => buscarValor(tail, cont + 1, p1, p2, out)
  }

  /** Funcion que calcula la posicion dadas unas coordenadas (X,Y).
   *
   * @param coordenadas es una tupla con las coordenadas de una posicion en la matriz (X,Y)
   * @return el valor de la posicion que se corresponde en el tablero
   */

  def calcularPos(coordenadas : (Int,Int)): Int ={
    (coordenadas._1 - 1) * 7 + coordenadas._2
  }

  /** Funcion que cambia dos valores de posicion entre ellos.
   *
   * @param tablero es la lista del tablero donde se cambian los valores
   * @param contador es un contador para saber en que posicion de la lista nos encontramos para hacer el cambio
   * @param pos1     es la posicion del primer valor a cambiar
   * @param pos2     es la posicion del segundo valor a cambiar
   * @param valores  es una tupla con los valores de intercambio (valor1, valor2)
   * @return la lista del tablero con los valores cambiados de sitio
   */

  def cambio(tablero : List[Int], contador : Int, pos1 : Int, pos2 : Int, valores : (Int,Int)): List[Int] = (tablero,contador,pos1,pos2,valores) match {
    // Si no quedan cambios por realizar, se devuelve una lista vacia
    case(Nil,_,_,_,_) => Nil
    // Si hemos encontrado el primer valor a cambiar, lo modifico por el segundo
    case(head::tail,cont,p1,p2,valores) if (cont == p1)               => valores._2::cambio(tail, cont + 1, p1, p2, valores)
    // Si hemos encontrado el segundo valor a cambiar, lo modifico por el primero
    case(head::tail,cont,p1,p2,valores) if (cont == p2)               => valores._1::cambio(tail, cont + 1, p1, p2, valores)
    // Si no estamos en ninguna de las posiciones deseadas, seguimos buscando
    case(head::tail,cont,p1,p2,valores) if (cont != p1 && cont != p2) => head::cambio(tail, cont + 1, p1, p2, valores)
  }

  /** Funcion que mueve los ceros hacia arriba del tablero dejando caer los demas diamantes.
   *
   * @param tablero es la lista del tablero donde se mueven los ceros
   * @param posicion es un contador para saber en que posicion del tablero nos encontramos para hacer el cambio
   * @return la lista del tablero con los 0s trasladados
   */

  def moverCeros(tablero : List[Int], posicion : Int): List[Int] = (tablero,posicion) match {
    // Si el 0 se encuentra en la primera fila, entonces le damos un valor aleatorio de entre 1 y 6
    case (t,p) if (p <= 7) => cambio(t, 1, p, -1, (0, scala.util.Random.nextInt(6) + 1))  //Si el cero se encuentra en la primera fila
    // Si el 0 tiene valores por encima de el, entonces lo intercambiamos con la funcion "cambio" por el valor inmediatamente por encima de el
    case (t,p) if (p > 7)  => cambio(t, 1, p, p - 7, buscarValor(t, 1, p, p-7, (0,0)))
  }

  /** Funcion que comprueba la existencia de 0 en el tablero y el indice del primero obtenido, en caso de haberlo
   *
   * @param tablero es un tablero cualquiera
   * @param contador es la posición del tablero por la que se ha desplazado de momento
   * @return el indice del primer cero del tablero si existiese, si no, -1
   */
  def hayCeros(tablero: List[Int], contador: Int): Int = (tablero, contador) match {
    // Si se ha llegado al final de la lista y no lo ha encontrado, retornamos -1
    case (Nil, _) => -1
    // Si la posicion que estamos mirando corresponde a un 0, devolvemos su posicion
    case (head :: tail, c) if (head == 0) => c
    // Si la posicion que estamos mirando NO corresponde a un 0, sigo buscando
    case (head :: tail, c) if (head != 0) => hayCeros(tail, c + 1)
  }

  /** Funcion que simula la caida de los diamantes cuando el inmediatamente inferior es destruido o ha caido.
   * Su ejecucion no es inmediata para todos los casos, es decir, lo hace de uno en uno.
   *
   * @param tablero es un tablero que puede o no contener huecos vacios (representados por 0)
   * @return un tablero sin huecos vacios
   */
  def caer(tablero: List[Int]): List[Int] = (tablero) match { //Limpia de ceros el tablero
    // Si no hay ceros (es decir, "hayCeros" ha retornado -1), entonces devuelvo el tablero sin modificarlo
    case (t) if (hayCeros(t, 1) == -1) => t
    // Si hay ceros, entonces hacemos uso de "moverCeros" para simular esa caida (recordamos que cambia el 0 por su valor superior)
    case (t) if (hayCeros(t, 1) != -1) => caer(moverCeros(t, hayCeros(t, 1)))
  }

  /** Funcion que intercambia los turnos de juego, del estado 0 que representa la actualizacion del tablero a 1 que es el turno del jugador.
   *
   * @param tablero es el tablero con el que se juega actualmente
   * @estado representa el estado de juego (0: actualizacion ,1: jugador)
   * @puntuacion puntuacion asignada por cada turno ganado +1
   */

  def turnos(tablero : List[Int], estado : Int, puntuacion : Int): Unit = (tablero, estado, puntuacion) match {
    // Si el estado del turno es 0 (es decir, hay que actualizarlo por posibles repeticiones), lo imprimo una vez actualizado
    case (t,0,p) => { //Caso de mirar repeticiones
      printf("---------- Tablero Actualizandose ----------\n\n")
      imprimir(t, 7, 1) // el tablero tiene ceros
      turnos(actualizarTablero(t), 1, p + 1)  //Vamos al turno del jugador tras la actualizacion (es decir, estado = 1) sumando 1 a la puntuacion
    }
    // Si el estado del turno es 1 (es decir, es el turno del jugador)
    case(t,1,p) => { //Turno del jugador
      printf("---------- Tablero Jugador ----------\n\n")
      imprimir(t, 7, 1)
      printf(f"---------- Puntuacion: $p ----------\n\n")
      println("¿Tienes la posibilidad de combinar? Si es asi, introduzca S, si no, introduzca N: ")  // Se pregunta al usuario si desea continuar

      val jugar = scala.io.StdIn.readChar().toUpper
      // Si no desea continuar, entonces se acaba la partida y se muestra la puntuacion obtenida
      if(jugar.equals('N')){
        printf("---------- FIN DE LA PARTIDA  ----------\n\n")
        printf(f"---------- PUNTUACION CONSEGUIDA => $p  ----------\n\n")
      }  // Si desea continuar, entonces se le exige unas coordenadas (X,Y) a partir de las cuales se buscan posibles repeticiones (comprobarTablero) a partir del cambio de
      // posiciones que el usuario desee hacer (cambio)
      else{
        val coordenadas = pedirCoordenadas()

        // Comprobamos las repeticiones en el tablero resultante de hacer un cambio entre la posicion indicada por el usuario y aquella acorde a la direccion hacia la que
        // desea cambiar (arriba, abajo, izquierda, derecha)
        // coordenadas._1 y coordenadas._2 -> X,Y de la posicion inicial
        // coordenadas._3 y coordenadas._4 -> X,Y de la posicion final una vez aplicada la direccion
        turnos(
          comprobarTablero(
            cambio(t, 1, calcularPos((coordenadas._1, coordenadas._2)), calcularPos((coordenadas._3, coordenadas._4)),
              buscarValor(t, 1, calcularPos((coordenadas._1, coordenadas._2)), calcularPos((coordenadas._3, coordenadas._4)), (0, 0))),
            (0, 0, 0), 1, List(), List(), 0),
          0, p)
      }
    }
  }

  /** Funcion que pide por teclado la casilla sobre la que se quiere operar y la dirección a la que se le quiere desplazar
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

  /** Funcion que realiza llamadas a los metodos comprobarTablero y caer hasta que no es necesario que lo hagan más porque
   * no hay más ceros en el tablero.
   *
   * @param tablero un tablero cualquiera de dimensiones 7x9
   * @return el tablero tras la simulación del proceso del juego necesaria
   */

  def actualizarTablero(tablero : List[Int]): List[Int] = (tablero) match{ //El tablero no tiene ceros, lo devuelve cuando este ya no necesite mas actualizaciones
    case(t) => {
      if (hayCeros(t, 1) == -1){ //Si no hay mas ceros, devuelve el tablero
        t
      }else{
        actualizarTablero(comprobarTablero(caer(t), (0,0,0), 1, List(), List(), 0)) //Llamamos a actualizar tras comprobar que al caer los ceros no haya mas combinaciones
      }
    }
  }

  /** Funcion que inicia el juego, imprime las reglas, imprime el tablero y comienza el ciclo para comprobar si se han
   * eliminado diamantes y la caida de los diamantes superiores si fuera necesario.
   *
   * @param tableroIni tablero aleatorio sin procesar.
   */
  def jugar(tableroIni : List[Int]): Unit ={
    printf("---------- Normas: ----------\n\n")
    printf("1. El rango de las coordenadas es desde (1,1) a (7,9).\n")
    printf("2. El objetivo del juego es juntar tres diamantes del mismo color Ej: 6 6 6.\n")
    printf("3. Al principio de cada turno se preguntará si quiere seguir jugando.\n")
    printf("4. El jugador tiene una puntuación.\n")
    printf("5. La puntuacion aumenta cada ronda, hasta que no queden mas movimientos para puntuar, si es asi su puntuación final es la mostrada.\n")
    printf("---------- Tablero Inicial ----------\n\n")
    imprimir(tableroIni, 7, 1)
    printf("\n¡Comienza el juego!\n\n")
    turnos(comprobarTablero(tableroIni, (0,0,0), 1, List(), List(), 0), 0, 0) //Llamamos a turnos con el tablero con ceros ya
  }

  def main (args:Array[String]): Unit =
  {
    val tableroInicial: List[Int] = crearTablero()
    jugar(tableroInicial)
  }
}