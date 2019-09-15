object MyModule{
  
  // 純粋関数
  def abs(n: Int): Int =
    if(n < 0) -n
    else n
  
  // 純粋関数
  // 複数ステートメントのため中括弧が必要. ステートメントは改行もしくはセミコロンで区切られる
  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }
  
  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
}