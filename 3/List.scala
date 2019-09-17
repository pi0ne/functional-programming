package fpinscala.datastructures

// [+A]について
// +は変位アノテーションで、AがListの共変パラメータであることを意味する
// 一般的にXがYの部分型であるならば、List[X]もList[Y]の部分方であると扱うということ
// より具体的には、DogがAnimalの部分方なら、List[Dog]はList[Animal]の部分型であり、List[Animal]型の変数にList[Dog]型のインスタンスを格納可能であることを意味する

sealed trait List[+A]                                         // A型のListデータ型
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
  def product(ds: List[Double]): Double = ds match{
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  
  // 可変長の引数からList[A]を生成する
  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  
  // def main(args: Array[String]):Unit ={
  //   val ex1: List[Double] = Nil
  //   val ex2: List[Int] = Cons(1, Nil)
  //   val ex3: List[String] = Cons("a", Cons("b", Nil))
  // }
}