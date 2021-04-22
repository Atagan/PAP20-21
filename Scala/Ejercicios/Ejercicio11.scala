package Ejercicios

object Ejercicio11 {
  def main (args:Array[String]):Unit = {
    val random = scala.util.Random 
    val lista = List.tabulate(16)(_ => random.nextInt(10))
    val tam = 4
    imprimir(lista,tam,1)
  }
  
  def imprimir(l:List[Int], tam: Int, n: Int):Unit = (l,tam,n) match {
    case (Nil,_,_) => println
    case (head::tail,t,n) if t == n => {
      println(head)
      imprimir(tail,t,1)
    }
    case (head::tail,t,n) => {
      print(f"$head ")
      imprimir(tail,t,n+1)
    }
  }
}
