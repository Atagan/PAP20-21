object Ejercicio9 {
  
  def main (args:Array[String]):Unit = {
    println("Inserte el la cantidad desesada de elementos a eliminar de la lista: ")
    val nElementos = scala.io.StdIn.readInt()
    val lista = (1 to 50).toList
    println("Pulse 1 si quiere eliminar los N primeros elementos, 2 si quiere los N ultimos elementos:")
    val opcion = scala.io.StdIn.readInt()
    if(opcion==1){
      println("La lista: \n"+ lista + "\n con las "+ nElementos + " primeras posiciones eliminadas es: \n" + deja(nElementos,lista))
    }else{
      println("La lista: \n"+ lista + "\n con las "+ nElementos + " ultimas eliminadas posiciones es: \n" + toma(nElementos,lista))
    }
    
  }
  
  def toma(n : Int , l : List[Int]): List[Int]= n match{
    case 0 => l.head::Nil
    case n => l.head::toma(n-1,l.tail)
    
  }
  
  def deja(n : Int , l : List[Int]): List[Int]= n match {
    case 0 => l.tail
    case n => deja(n-1,l.tail)
  }
}