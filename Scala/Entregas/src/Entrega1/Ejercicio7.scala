object Ejercicio7 {
  
  def main (args:Array[String]):Unit = {
    val random = scala.util.Random 
    val n2 = random.nextInt(200)
    val suma = sumf1n(0,n2)
    println(f"La suma de los valores entre 0 y $n2 y que acaban en 2 o 3 es: $suma")
  }
  
  def sumf1n (a:Int, b:Int) : Int = {  
    if(a == b) 0
    else {
      a % 10 match{
        case 2 | 3 => a + sumf1n(a+1,b)
        case _ => sumf1n(a+1,b)       
        }
    } 
  }
}
