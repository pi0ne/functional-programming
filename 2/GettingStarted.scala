object MyModule{
  
  def abs(n: Int): Int =
    if(n < 0) -n
    else n
  
  private def formatAbs(x: Int) = {
    msg.format(x, abs(x))
  }
  
  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
}