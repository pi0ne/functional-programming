// [+A]について
// +は変位アノテーションで、AがListの共変パラメータであることを意味する
// 一般的にXがYの部分型であるならば、List[X]もList[Y]の部分方であると扱うということ
// より具体的には、DogがAnimalの部分方なら、List[Dog]はList[Animal]の部分型であり、List[Animal]型の変数にList[Dog]型のインスタンスを格納可能であることを意味する

sealed trait List[+A]                                         // A型のListデータ型. これは単方向リストになる
case object Nil extends List[Nothing]                         // 空のリストを表すデータコンストラクタ
case class Cons[+A](head: A, tail: List[A]) extends List[A]   // Constructの略. tailもList[A]型. tailに渡されるリストが空でない場合、別のConsとしてオブジェクトが生成され、空の場合、Nilが生成される

object List{
  
  // パターンマッチによる加算
  // 渡されたList[Int]型がConsなら加算していき、末尾のNilになった時に0を返す
  def sum(ints: List[Int]): Int = ints match{
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  
  // 掛け算
  // 変数パターン_は任意の式とマッチする. 変数パターンはxなどでも良いが、ケースの結果において値を無視する場合は_を使用する
  def product(ds: List[Double]): Double = ds match{
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  
  // 可変長の引数からList[A]を生成する
  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  
  // a1の末尾にa2を接続する. List(1,2,3) List(4,5)ならば、Cons(1,Cons(2,Cons(3,Cons(4,Cons(5)))))となる
  // Listはイミュータブルであるという前提であれば、Listのインスタンスをコピーする必要はない(元のインスタンスを変更することが無いため)
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match{
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  
  // 先頭から見ていき、fを満たす要素より後ろの要素を返す
  // def dropWhile[A](l: List[A], f: A => Boolean): List[A]  ... 変更前
  // カリー化することにより、第一引数を指定することで第二引数の型を推定できるようになっている
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => as
    }
}

object ListTest{
  def main(args: Array[String]): Unit ={
    val xs: List[Int] = List(1, 2, 3, 4, 5)
    val ex1 = List.dropWhile(xs)(x => x < 4)
    println(ex1)    // Cons(4,Cons(5,Nil))
  }
}