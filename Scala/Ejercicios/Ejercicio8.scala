package Ejercicios

object Ejercicio8 {
  def main (args:Array[String]):Unit = {
    println("Inserte un numero: ")
    val n = readInt()
    val cifras = cuantasC(n)
    println("El numero de cifras que tiene el numero : "+ n +" es "+ cifras)
  }
  
  def cuantasC (numero:Int) : Int = numero match{
    case n if (n <= 0) => 0
    case n => 1 + cuantasC (n/10)
    
  }

}