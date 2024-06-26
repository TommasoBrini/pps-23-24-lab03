package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    /*
    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil() 
    */
    
    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = flatMap(l)(a => Cons(mapper(a), Nil()))
       
    /*
    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()
    */

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = 
      flatMap(l1)(k => pred(k) match
        case true => Cons(k, Nil())
        case _ => Nil()
      ) 

    // Lab 03
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(a,b), Cons(c,d)) => Cons((a, c), zip[A,B](b,d))
      case _ => Nil()
    

    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case Cons(h,t) if n > 0 => Cons(h, take[A](t)(n-1))
      case _ => Nil()
    
    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = l1 match
      case(Cons(a,b)) => Cons(a, concat(b, l2))
      case _ => l2
  
    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Cons(a,b) => concat(mapper(a),flatMap(b)(mapper))
      case _ => Nil()

    def min(l: Sequence[Int]): Optional[Int] = l match
      case Nil() => Optional.Empty()
      case Cons(a, Nil()) => Optional.Just(a)
      case Cons(a, b) => min(b) match
        case Optional.Just(x) => (a < x) match
          case true => Optional.Just(a) 
          case _ => Optional.Just(x) 
        case Optional.Empty() => Optional.Empty()
      
    
    
@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 60

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
