package fubar.goodanimals

trait Food {
  override def toString(): String
}

object grass extends Food {
  override def toString: String = "grass"
}

object dogfood extends Food {
  override def toString: String = "dogfood"
}

abstract class GoodAnimal {
  type SuitableFood <: Food
  def eat(food: SuitableFood): Unit = {
      println(s" ${this.getClass} Eating my food: "+ food)
  }
}

class Cow extends GoodAnimal with Food {
  type SuitableFood = grass.type
}

object goodfeeder {
  def feed(animal: GoodAnimal, food: GoodAnimal#SuitableFood): Unit = {
//    OOPS cant do this it says
//     expected animal.Suitablefood, actual:GoodAnimal#SuitableFood
//
//    animal.eat(food)
  }

  // OOPs cant resolve symbol animal
  // can re-refer to a param in same parameter list
  // type system works left to right
  /*def feed1(animal: GoodAnimal, food: animal.SuitableFood): Unit = {
        animal.eat(food)
  }*/

  // solution is add a second parameter list now type is known at call site
  def feed2(animal: GoodAnimal)( food: animal.SuitableFood): Unit = {
    animal.eat(food)
  }
}

object GoodAnimal extends App {

  val Daisy = new Cow
  //Acceptable
  goodfeeder.feed2(Daisy)(grass)

  //naughty
  // wont compile says: expected Cow:SuitableFood, actual dogfood.type
  //goodfeeder.feed2(Daisy)(dogfood)

  //totally unacceptable. Canabalism
  // wont compile says: expected Cow:SuitableFood, actual GoodAnimal.Daisy.type
  // goodfeeder.feed2(Daisy)(Daisy)
}


