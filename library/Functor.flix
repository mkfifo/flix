/**
 * A constructor class for types that support the map operation.
 */
class Functor[M[A]] {

    /**
     * Applies the function `f` to the value inside the container `x`.
     */
    def map[B](f: A -> B, x: M[A]): M[B]

}

//
// ## Options.
//

/**
 * Implementation of `Functor[M[A]]` for `Opt[A]`.
 */
impl Functor[Opt[A]] {
    def map[B](f: A -> B, x: Opt[A]): Opt[B] = match x with {
        case None => None
        case Some(v) => Some(f(v))
    }
}

//
// ## Lists.
//

/**
 * Implementation of `Functor[M[A]]` for `List[A]`.
 */
impl Functor[List[A]] {
    def map[B](f: A -> B, x: List[A]): List[B] = match x with {
        case Nil => Nil
        case y :: rs => f(y) :: map(f, rs)
    }
}

