object minimal {
  
  def main(args: Array[String]): Unit = {
    val a: X = new VC_FINAL$X()
    val b: a.VC_T$Y = a.VC_NEW$Y()
  }
}

/*class X {

  class Y <: {
    val bla : Y = null
    val foo = "Bar"
  }

}*/

abstract class X {
  type VC_T$Y >: Null <: VC_TRAIT$Y

  trait VC_TRAIT$Y { self: VC_T$Y =>

  }
  
  def VC_NEW$Y() : VC_T$Y
}

class VC_FINAL$X extends X {
  type VC_T$Y = VC_TRAIT$Y
  
  class VC_FIX$Y extends VC_TRAIT$Y {
  }
  
  def VC_NEW$Y(): VC_T$Y = {
    new VC_FIX$Y()
  }
}