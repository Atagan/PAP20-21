object Ejercicio12 {
  def main (args:Array[String]):Unit = {
    val random = scala.util.Random 
    val lista = List.tabulate(64)(_ => random.nextInt(10))
    val tam = 8
    println("Imprimimos la matriz M: ")
    imprimir(lista,tam,1)
    println("¿De qué fila quiere leer? (indexado en 0): ")
    val fila = scala.io.StdIn.readInt()
    println("¿De qué columna quiere leer? (indexado en 0) : ")
    val columna = scala.io.StdIn.readInt()
    val elem = encontrarElemento(lista,fila,columna,tam)
    println(f"El elemento de la posicion ($fila,$columna) es : $elem")
    println("¿Que fila quieres ver? (indexado en 0): ")
    val fil = scala.io.StdIn.readInt()
    println("La fila es: " + encontrarFila(lista,fil,tam-1,tam))
    println("¿Que columna quieres ver? (indexado en 0): ")
    val col = scala.io.StdIn.readInt()
    println("La columna es: " + encontrarColumna(lista, col, 0))
    
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
   
  def encontrarElemento(l:List[Int], fila:Int, columna:Int, tam:Int): Int = (l,fila,columna) match{
    case(Nil,_,_) => -1
    case(head::tail,0,0) => head
    case(head::tail,f,0) => encontrarElemento(tail,f-1,tam-1,tam)
    case(head::tail,f,c) => encontrarElemento(tail,f,c-1,tam)
  }
  
  def encontrarFila(l:List[Int], fila:Int, columna:Int, tam:Int): List[Int] = (l,fila,columna) match{
    case(Nil,_,_) => Nil
    case(head::tail,0,0) => head::Nil
    case(head::tail,0,c) => head :: encontrarFila(tail, 0, c-1 ,tam)
    case(head::tail,f,0) => encontrarFila(tail, f-1, tam-1, tam)
    case(head::tail,f,c) => encontrarFila(tail, f, c-1 ,tam)
  }
  
 def encontrarColumna(l:List[Int], columna:Int, prof:Int): List[Int] = (columna,prof) match{
   case (c, 64) => Nil
   case (c, p) if(p % 8 == c) => l.head::encontrarColumna(l.tail, c, p+1)
   case (c, p) if(p % 8 != c) => encontrarColumna(l.tail, c, p+1)
 }
   
}