object Ejercicio8 {
  def main (args:Array[String]):Unit = {
    println("Introduzca un numero: ")
    val n = scala.io.StdIn.readInt()
    val cifras = cuantasC(n)
    println("El numero "+n +" tiene: "+cifras +"cifras ")
  }
  
  def cuantasC (numero:Int) : Int = numero match{
    case n if (n <= 0) => 0
    case n => 1 + cuantasC (n/10)
    
  }

}