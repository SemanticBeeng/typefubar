package fubar

import fubar.MyTypeClass.Aux

import scala.annotation.tailrec

trait MyTypeClass[T] {
  type In
  type Out

  def doMagic(foo: T, param1: In): Out
}

object MyTypeClass {

  //aux pattern
  type Aux[T0, In0, Out0] = MyTypeClass[T0] {type In = In0; type Out = Out0}

  // convenience constructor so I don't need to use implicitly
  def apply[T](implicit evidence: MyTypeClass[T]): Aux[T, evidence.In, evidence.Out] = evidence



  // known type class members
  implicit val greeterToTypeclass: Aux[Greeter, String, String] = new MyTypeClass[Greeter] {
    type In = String
    type Out = String

    override def doMagic(foo: Greeter, param1: In): Out = foo.greeting(param1)
  }

  implicit val imAnAlienToTypeclass: Aux[ImAnAlien.type, Int, String] = new MyTypeClass[ImAnAlien.type] {
    type In = Int
    type Out = String

    override def doMagic(foo: ImAnAlien.type, param1: In): Out = foo.say(param1)
  }

}

case class Greeter(name: String) {

  def greeting(greeting: String): String = {
    s"$greeting, $name"
  }
}

case object ImAnAlien {

  @tailrec
  def sayThis(thing: String, accumlator: String, numTimes: Int): String = numTimes match {
    //base case
    case 0 => accumlator
    //recurse
    case _ => sayThis(thing, accumlator + " " + thing, (numTimes - 1))

  }

  def say(num: Int): String = {
    sayThis("I'm an Alien", "", num)
  }
}

object SomeApi {
  def voodoo[T, In0, Out0](foo: T, sayThis: In0)(implicit ev: MyTypeClass.Aux[T, In0, Out0]): Out0 = {
    val tc = MyTypeClass[T]
    tc.doMagic(foo, sayThis)
    ev.doMagic(foo, sayThis)
  }

  def hoodoo[T, In0, Out0](foo: T, sayThis: In0)(implicit ev: MyTypeClass.Aux[T, In0, Out0]): Out0 = {
    voodoo(foo, sayThis);
  }
}

case class Yoodoo[T, In0, Out0](foo: T, sayThis: In0)(implicit ev: MyTypeClass.Aux[T, In0, Out0]) {


  import SomeApi._

  def doVoodoo(): Out0 = {
    voodoo(foo, sayThis);
//    ev.doMagic(foo, sayThis)
  }
}

class Fakir[T,In0,Out0](foo: T, sayThis: In0)(implicit ev: MyTypeClass.Aux[T,In0,Out0]) {

  import SomeApi._

  def doVoodoo(): Out0 = {
    voodoo(foo, sayThis);
//    ev.doMagic(foo, sayThis);
  }
}

object Fakir {
  def apply[T, In0, Out0](foo: T, sayThis: In0)(implicit ev: Aux[T, In0, Out0]) = new Fakir(foo,sayThis)
}





object Main extends App {

  //bring typeclass instance into scope
  import MyTypeClass._
  import SomeApi._

  println("Greeting:- ")
  println(voodoo(Greeter("karl"), "wassup"))

  println("\nAliens:- ")
  println(voodoo(ImAnAlien, 3))


}

object Main1 extends App {

  //bring typeclass instance into scope
  import MyTypeClass._
  import SomeApi._

  println("Greeting hoodoo:- ")
  println(hoodoo(Greeter("hoodo karl"), "wassup"))

  println("\nAliens hoodoo :- ")
  println(voodoo(ImAnAlien, 3))

}

object Main2 extends App {

  //bring typeclass instance into scope
  import MyTypeClass._
  import SomeApi._

  val yoodoo = Yoodoo(Greeter("YOODOO karl"), "Howzithangin")
  println("Greeting yoodoo:- ")
  println(yoodoo.doVoodoo())


}

object Main3 extends App {

  //bring typeclass instance into scope
  import MyTypeClass._
  import SomeApi._

  val fakir = Fakir(Greeter("YOODOO karl"), "Howzithangin")
  println("Greeting yoodoo:- ")
  println(fakir.doVoodoo())


}

/* // bah wont compile
case class Charleton[T](foo: T)(implicit ev: MyTypeClass[T])(implicit sayThis: ev.type.In ) {
  import SomeApi._
  //  implicit def tcToAux[T](ev: MyTypeClass[T]): Aux[T, ev.In, ev.Out] = ev

  //  val tc = MyTypeClass[T]
  val tc = implicitly[MyTypeClass[T]]

  def doVoodoo() = {
    //  voodoo(foo, sayThis);
    tc.doMagic(foo, sayThis)
  }
}

object Main4 extends App {

  //bring typeclass instance into scope
  import MyTypeClass._
  import SomeApi._

  val charleton = Charleton(Greeter("YOODOO karl"), "Howzithangin")
  println("Greeting yoodoo:- ")
  println(fakir.doVoodoo())


}
 */

