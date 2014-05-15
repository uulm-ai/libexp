package shapeless

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 8/28/13
 */
package object backports {
  /** Dependent nullary function type. */
    trait DepFn0 {
      type Out
      def apply(): Out
    }

    /** Dependent unary function type. */
    trait DepFn1[T] {
      type Out
      def apply(t: T): Out
    }

    /** Dependent binary function type. */
    trait DepFn2[T, U] {
      type Out
      def apply(t: T, u: U): Out
    }
}
