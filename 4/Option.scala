// 例外を使わないエラー処理

// エラーの値を定めて返却する構造には以下の問題がある
// 呼び出し元でエラー処理の記述を忘れてもコンパイルエラーにならず、エラーが隠れて伝播する可能性がある
// 呼び出し元にボイラープレートが生じる

// 戻り値がOption型であることにより、エラー判定を先送りにできる
// lookupByName("Joe").map(_.department) lookupByNameで社員を取得し、部署を取得する場合を考える. lookupByNameがOption型を返すなら、Joeがいない場合のエラー処理を先送りにすることが可能

sealed trait Option[+A]{
  def map[B](f: A => B): Option[B]                  // OptionがNoneでない場合、fを適用しB型を返す.
  def flatMap[B](f: A => Option[B]): Option[B]      // OptionがNoneでない場合、fを適用しB型を返す. この時、fはflatMap内のエラー判定とは別に、f自体が失敗する可能性がある
  def getOrElse[B >: A](default: => B): B           // B >: A はパラメータB型はA型のスーパークラスでなければならないことを意味する
  def orElse[B >: A](ob: => Option[B]): Option[B]   // obを評価しない
  def filter(f: A => Boolean): Option[A]            // fの条件を満たさない場合、SomeをNoneに変換する
}
// case class Some[+A](get: A) extends Option[A]
// case object None extends Option[Nothing]

object Option {
  
  // リストの平均を計算する
  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)
  
  // Option型の2つの値を結合する総称関数
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap(aa => b map (bb => f(aa, bb)))  // aはNoneでない場合にf(aa,bb)を返すが、この中でbの評価を行っており、aはaとは別の値によって失敗する可能性がある
  
  // map2をfor内包記法(for-comprehension)で書き直すと以下の様になる
  // この呼び出しは、flatMapとmapの呼び出しとして自動的に展開される
  def _map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for{
      aa <- a
      bb <- b
    } yield f(aa, bb)
}

// 保険料計算をするクラス
trait Insurance{
  
  // 年齢、速度違反番号から保険料を計算する式. フォームでバリエーションされていない前提だと、2つの引数には文字列が入る恐れがある
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double
  
  // 入力値をパースし、成功した場合にinsuranceRateQuoteへ値を渡す
  def parseInsuranceRateQuote(
    age: String,
    numberOfSpeedingTickets: String
  ): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    Option.map2(optAge, optTickets)(insuranceRateQuote)
  }
  
  // => A は非正格な引数. 遅延引数とも呼ばれる. Aの評価中に発生する例外を全てキャッチし、Noneに変換することが可能
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }   // エラーに関する情報を削除している
}

object OptionTest{
}