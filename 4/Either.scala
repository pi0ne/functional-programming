sealed trait Either[+E, +A]
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either{
  // 平均を計算する
  def mean(xs: IndexedSeq[Double]) =
    if(xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)
      
  // 詳細なエラー情報が欲しい場合は、NoneではなくEitherで例外を返すことができる
  def safeDiv(x: Int, y: Int) =
    try Right(x / y)
    catch{ case e: Exception => Left(e) }
  
  // Optionでの値の変換も、Eitherにより実装可能
  def Try[A](a: => A): Either[Either, A] =
    try Right(a)
    catch{ case e: Exception => Left(e) }
}

// 保険料計算をするクラス
trait Insurance{
  
  // 年齢、速度違反番号から保険料を計算する式. フォームでバリエーションされていない前提だと、2つの引数には文字列が入る恐れがある
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double
  
  // 入力値をパースし、成功した場合にinsuranceRateQuoteへ値を渡す
  def parseInsuranceRateQuote(
    age: String,
    numberOfSpeedingTickets: String): Either[Exception, Double] =
    for{
      a <- Try{ age.toInt }
      tickets <- Try{ numberOfSpeedingTickets.toInt }
    } yield insuranceRateQuote(a, tickets)
}