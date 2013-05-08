package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class FoldableTest extends Spec {
  "maximum" ! prop {
    (xs: List[Int]) =>
      val F = Foldable[List]
	  if (xs.isEmpty)
		(F maximum xs) must be_===(None: Option[Int])
	  else
	    (F maximum xs) must be_===(Some(xs.max): Option[Int])
  }
  "maximumOf" ! prop {
    (xs: List[Int]) =>
      val F = Foldable[List]
	  val f: Int => Double = 1D + _
	  if (xs.isEmpty)
		(F.maximumOf(xs)(f)) must be_===(None: Option[Double])
	  else
	    (F.maximumOf(xs)(f)) must be_===(Some( (xs.iterator map f).max ): Option[Double])
  }
  "maximumBy" ! prop {
    (xs: List[Int]) =>
      val F = Foldable[List]
	  val f: Int => String = _.toString
	  if (xs.isEmpty)
		(F.maximumBy(xs)(f)) must be_===(None: Option[Int])
	  else
	    (F.maximumBy(xs)(f)) must be_===(Some( (xs zip (xs map f)).maxBy(_._2)._1 ): Option[Int])
  }
  "minimum" ! prop {
    (xs: List[Int]) =>
      val F = Foldable[List]
	  if (xs.isEmpty)
		(F minimum xs) must be_===(None: Option[Int])
	  else
	    (F minimum xs) must be_===(Some(xs.min): Option[Int])
  }
  "minimumOf" ! prop {
    (xs: List[Int]) =>
      val F = Foldable[List]
	  val f: Int => Double = 1D + _
	  if (xs.isEmpty)
		(F.minimumOf(xs)(f)) must be_===(None: Option[Double])
	  else
	    (F.minimumOf(xs)(f)) must be_===(Some((xs.iterator map f).min): Option[Double])
  }
  "minimumBy" ! prop {
    (xs: List[Int]) =>
      val F = Foldable[List]
	  val f: Int => String = _.toString
	  if (xs.isEmpty)
		(F.minimumBy(xs)(f)) must be_===(None: Option[Int])
	  else
	    (F.minimumBy(xs)(f)) must be_===(Some( (xs zip (xs map f)).minBy(_._2)._1 ): Option[Int])
  }
  
  "foldLeft1Opt" ! prop {
      (xs: List[Int]) =>
      val F = Foldable1[NonEmptyList]
      val gt = (i : Int, j : Int) => if (i > j ) i else j
      import syntax.foldable._
      import syntax.std.list._
      xs.toNel match {
        case None      => (xs foldLeft1Opt gt) must be_===(None: Option[Int])
        case Some(nel) => (xs foldLeft1Opt gt) must be_===(Some(F.foldLeft1(nel)(gt)): Option[Int])
      }

  }
  "foldRight1Opt" ! prop {
      (xxs: Stream[Int]) =>
      val F = Foldable[Stream]
      val gt: (Int, => Int) => Int = (i, j) => if (i > 0) i else j
      import syntax.foldable._
      import syntax.std.stream._
      xxs match {
        case Stream.Empty => (xxs foldRight1Opt gt) must be_===(None: Option[Int])
        case _            => (xxs foldRight1Opt gt) must be_===(Some(F.foldRight(xxs.init, xxs.last)(gt)): Option[Int])
      }
  }
  
  "foldr1Opt" ! prop {
      (xxs: Stream[Int]) =>
      val F = Foldable[Stream]
      val gt = (i: Int, j: Int) => if (i > j ) i else j
      import syntax.foldable._
      xxs match {
        case Stream.Empty => (xxs foldr1Opt (i => (j => gt(i, j)))) must be_===(None: Option[Int])
        case _            => (xxs foldr1Opt (i => (j => gt(i, j)))) must be_===(Some(F.foldr(xxs.init, xxs.last)((i => (j => gt(i, j))))): Option[Int])
      }
  }
  "foldl1Opt" ! prop {
      (xs: List[Int]) =>
      val F = Foldable1[NonEmptyList]
      val gt = (i: Int) => (j: Int) => if (i > j ) i else j
      import syntax.foldable._
      import syntax.std.list._
      xs.toNel match {
        case None      => (xs foldl1Opt gt) must be_===(None: Option[Int])
        case Some(nel) => (xs foldl1Opt gt) must be_===(Some(F.foldl1(nel)(gt)): Option[Int])
      }

  }
  
}
