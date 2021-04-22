package Ejercicios


object Ejercicio9 {
  
  def main (args:Array[String]):Unit = {
    println("Inserte el numero desesado de elementos a borrar o extraer de la lista: ")
    val nElementos = readInt() 
    val lista = (1 to 25).toList 
    println("Inserte 1 si quiere eliminar los N primeros elementos, 2 si quiere los N elementos:")
    val opcion = readInt()
    if(opcion==1){
      println("La lista: "+ lista + " con las "+ nElementos + " primeras posiciones eliminadas es: " + deja(nElementos,lista))
    }else{
      println("La lista: "+ lista + " con las "+ nElementos + " primeras posiciones es: " + toma(nElementos,lista))
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