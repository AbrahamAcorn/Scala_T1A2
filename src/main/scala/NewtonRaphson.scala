object NewtonRaphson {

  def raiz_cuadrada(n: Int): Double = {

    def f (x: Double): Double = Math.pow(x, 2) - n

    def dx (x: Double): Double = 2 * x

    def paro (x:Double): Boolean = Math.abs(x * x - n) / n < 0.001

    def mejora (x: Double): Double = x - f(x) / dx(x)

    def calcula (x:Double): Double = if (paro(x)) x else calcula(mejora(x))

    calcula(1.0) // Punto de partida, valor inicial
  }
  def main(args: Array[String]): Unit = {
    println(raiz_cuadrada(25))
  }
}
