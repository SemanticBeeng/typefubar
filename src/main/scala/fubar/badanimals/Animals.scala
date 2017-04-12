package fubar.badanimals

/**
  * Created by robertk on 23/03/17.
  *
  */

trait Food {
  override def toString(): String
}

object grass extends Food {
  override def toString: String = "grass"
}

object dogfood extends Food {
  override def toString: String = "dogfood"
}

abstract class BadAnimal  {
  def eat(fodder: Food): Unit = {
    println(s" ${this.getClass} Eating my food: "+ fodder)
  }
}

object badfeeder {
  def feed(animal: BadAnimal, food: Food): Unit = {
    animal.eat(food)
  }
}

class Cow extends BadAnimal with Food

object BadAnimal1 extends App {

  val Daisy = new Cow
  //Acceptable
  badfeeder.feed(Daisy, grass)

  //naughty
  badfeeder.feed(Daisy, dogfood)

  //totally unacceptable. Canabalism
  badfeeder.feed(Daisy, Daisy)
}
