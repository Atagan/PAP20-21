

package Ejercicios

object Ejercicio1 {
  
  def main (args:Array[String]):Unit = {
    println("Inserte el numero desesado. ")
    val n = readInt()
    val lista = (0 to n).toList 
    println("La lista es: " + lista);
    
  }
  
}