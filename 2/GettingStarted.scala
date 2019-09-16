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
      
    go(n ,1)
  }
  
  // 高階関数. 引数に関数の方が存在するため
  // 下のformatメソッドを一般化するために、この関数内で関数の結果を取得している
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }
  
  // 多相関数 (polymorphic function)
  // 配列内の要素を検索し、最初の添え字を返却する
  // 検索の判定をpとして渡す
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int =
      if(n >= as.length) -1
      else if(p(as(n))) n
      else loop(n+1)
      
    loop(0)
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
  
  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
  }
  
}