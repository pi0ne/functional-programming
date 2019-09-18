// 例外を使わないエラー処理

// エラーの値を定めて返却する構造には以下の問題がある
// 呼び出し元でエラー処理の記述を忘れてもコンパイルエラーにならず、エラーが隠れて伝播する可能性がある
// 呼び出し元にボイラープレートが生じる

sealed trait Option[+A]{
  def map[B](f: A => B): Option[B]                  // OptionがNoneでない場合、fを適用しB型を返す
  def flatMap[B](f: A => Option[B]): Option[B]      // OptionがNoneでない場合、fを適用しB型を返す. この時、fはflatMap内のエラー判定とは別に、f自体が失敗する可能性がある
  def getOrElse[B >: A](default: => B): B           // B >: A はパラメータB型はA型のスーパークラスでなければならないことを意味する
  def orElse[B >: A](ob: => Option[B]): Option[B]   // obを評価しない
  def filter(f: A => Boolean): Option[A]            // fの条件を満たさない場合、SomeをNoneに変換する
}
// case class Some[+A](get: A) extends Option[A]
// case object None extends Option[Nothing]

object OptionTest{
  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)
}