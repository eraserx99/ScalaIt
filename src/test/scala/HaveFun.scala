import org.scalatest.FunSuite

abstract class Expr

case class Var(name: String) extends Expr

case class Number(num: Double) extends Expr

case class UnOp(operator: String, arg: Expr) extends Expr

case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

class HaveFun extends FunSuite {

  def fun: PartialFunction[Any, String] = {
    case 0 => "zero"
    case true => "true"
    case "hello" => "you said 'hello'"
    case Nil => "an empty List"
    case _ => "Not acceptable!"
  }

  def append[T](xs: List[T], ys: List[T]): List[T] =
    xs match {
      case List() => ys
      case x :: xs1 => x :: append(xs1, ys)
    }

  def echoWhatYouGaveMe(x: Any): String = x match {
    // constant patterns
    case 0 => "zero"
    case true => "true"
    case "hello" => "you said 'hello'"
    case Nil => "an empty List"

    // sequence patterns
    case List(0, _, _) => "a three-element list with 0 as the first element"
    case List(1, _*) => "a list beginning with 1, having any number of elements"
    case Vector(1, _*) => "a vector starting with 1, having any number of elements"

    // tuples
    case (a, b) => s"got $a and $b"
    case (a, b, c) => s"got $a, $b, and $c"

    // constructor patterns
    case Var(_) => "Var expr"
    case UnOp(_, _) => "UnOp expr"

    // typed patterns
    case s: String => s"you gave me this string: $s"
    case i: Int => s"thanks for the int: $i"
    case f: Float => s"thanks for the float: $f"
    case a: Array[Int] => s"an array of int: ${a.mkString(",")}"
    case as: Array[String] => s"an array of strings: ${as.mkString(",")}"
    case list: List[_] => s"thanks for the List: $list"
    case m: Map[_, _] => m.toString

    // the default wildcard pattern
    case _ => "Unknown"
  }

  test("traits, with Incrementing with Filtering") {
    val q1 = (new BasicIntQueue with Incrementing with Filtering)
    q1.put(-1)
    q1.put(0)
    q1.put(1)

    assert(q1.get() == 1)
  }

  test("traits, with Incrementing and Filtering") {
    val q1 = (new BasicIntQueue with Filtering with Incrementing)
    q1.put(-1)
    q1.put(0)
    q1.put(1)

    assert(q1.get() == 0)
  }

  test("match") {
    val op: Expr = UnOp("-", UnOp("-", Var("x")))
    val res: Expr = op match {
      case UnOp("-", UnOp("-", e)) => e
      case BinOp("+", e, Number(0)) => e
      case BinOp("*", e, Number(1)) => e
      case _ => null
    }

    assert(res == Var("x"))
  }

  test("match, variable binding") {
    val res: Expr = UnOp("abs", UnOp("abs", Var("x"))) match {
      case UnOp("abs", e@UnOp("abs", Var("x"))) => e
      case _ => null
    }

    assert(res == UnOp("abs", Var("x")))
  }

  test("List, patterns") {
    val l = List(1, 2, 3, 4, 5, 6)
    val t = l match {
      case e@List(_, _, _) => e
      case _ :: _ :: rest => rest
    }

    assert(t == List(3, 4, 5, 6))
  }

  test("Have some fun with PartialFunction!") {
    val v = fun
    val l = v(List())

    assert(l == "an empty List")
  }

  test("append, parametric function") {
    val l = append(List(1,2), List(3,4))

    assert(l == List(1, 2, 3, 4))
  }
}
