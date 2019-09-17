// 単純化したList型

// [+A]について
// +は変位アノテーションで、AがListの共変パラメータであることを意味する
// 一般的にXがYの部分型であるならば、List[X]もList[Y]の部分方であると扱うということ
// より具体的には、DogがAnimalの部分方なら、List[Dog]はList[Animal]の部分型であり、List[Animal]型の変数にList[Dog]型のインスタンスを格納可能であることを意味する

// Listは代数的データ型である
// 代数的データ型とは1つ以上のデータコンストラクタによって定義されるデータ型であり、コンストラクタは0個以上の引数を受け取る
// 代数的データ型は、それを使って他のデータ構造の定義を行うことができる. 例としてListを使って二分木を実装できるのがそれである

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
  
  // sumとproductを一般化した関数
  // asに計算対象のList, zにasがNilだった場合に返す値, fに演算の内容を渡す
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z // zはasがNilの場合に返す値. 加算の場合は0, 掛け算の場合は1を返す
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)
  
  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // (x, y) => x * y はこのように簡易表記することが可能. 無名関数のアンダースコア(FP in Scala p50参照)
  
}

object ListTest{
  def main(args: Array[String]): Unit ={
    val xs: List[Int] = List(1, 2, 3, 4, 5)
    val ex1 = List.dropWhile(xs)(x => x < 4)
    println(ex1)    // Cons(4,Cons(5,Nil))
    
    val ex2 = List.sum2(xs)
    println(ex2)
    
    val ys: List[Double] = List(1.0, 2.0, 3.0)
    val ex3 = List.product2(ys)
    println(ex3)
  }
}