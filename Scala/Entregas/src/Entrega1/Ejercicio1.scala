object Ejercicio1 {
  
  def main (args:Array[String]):Unit = {
    println("Inserte el numero superior. ")
    val n = scala.io.StdIn.readInt()
    val lista = (0 to n).toList 
    println("La lista generada es: " + lista);
    
  }
  
}