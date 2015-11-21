package ar.edu.tadp.dragonball

import scala.util.Try

object Utils {
  
  implicit class OptionList[T](list: List[T]) {
    def maxByOptionable[U: Ordering](f: T => U) = Try(list.maxBy(f)).toOption
  }
}