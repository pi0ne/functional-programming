object MyModule{
  
  // 純粋関数
  def abs(n: Int): Int =
    if(n < 0) -n
    else n
  
  // 末尾再帰による階乗計算
  // 自己再帰は末尾再帰である場合、whileループと同じ種類のコードとしてコンパイルされる
  def factorial(n: Int): Int = {
    // 再帰ヘルパー関数. goまたはloopで命名するのが慣例となっている
    // acc...Accumulation(累積)
    def go(n: Int, acc: Int): Int = 
      if(n <= 0) acc
      else go(n-1, n*acc)             // 末尾再帰
  }
  
  // 純粋関数
  // 複数ステートメントのため中括弧が必要. ステートメントは改行もしくはセミコロンで区切られる
  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }
  
  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
  }
  
  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
    println(formatFactorial(7))
}