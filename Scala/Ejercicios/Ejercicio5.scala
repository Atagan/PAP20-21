

package Ejercicios

object Ejercicio5 {
  
  def main (args:Array[String]):Unit = {
    val random = scala.util.Random 
    val n1 = random.nextInt(5)
    val n2 = random.nextInt(20)
    val lista = (n1 to n2).toList
    
    println("La suma de los rangos de esta lista: " +lista)
    println("Es: "+ sumaCuadrados(lista))
  }
  
  def sumaCuadrados(lista:List[Int]): Int = lista match{
    case Nil => 0
    case head::tail => head * head + sumaCuadrados(tail)  
  }
  
}