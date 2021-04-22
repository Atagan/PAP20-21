

package Ejercicios

object Ejercicio2 {
  
  def main (args:Array[String]):Unit = {
    //Creamos una lista con un numero random del 0 al 100
    val random = scala.util.Random 
    val n = random.nextInt(50)
    val lista = (0 to n).toList 
    //Sumamos los valores de la lista
    println("La lista es: " + lista)
    println("Y su suma es: " + sumaValores(lista))
  }
  
  def sumaValores(lista:List[Int]): Int= lista match {
    case Nil => -1
    case head::Nil => head
    case head::tail => head + sumaValores(tail)
   
  }
  
}