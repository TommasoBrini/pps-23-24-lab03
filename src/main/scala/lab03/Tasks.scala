package lab03

import lab03.Sequences.Sequence.flatMap
import u03.Optionals.*

// TASK 1 (list) svolto da solo
object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0
    
    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = 
      flatMap(l)(a => Cons(mapper(a), Nil()))
   
    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = 
      flatMap(l1)(k => pred(k) match
        case true => Cons(k, Nil())
        case _ => Nil()
      ) 

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
      case Cons(h, Nil()) => Optional.Just(h)
      case Cons(h, t) => min(t) match
        case Optional.Just(x) => (h < x) match
          case true => Optional.Just(h) 
          case _ => Optional.Just(x) 
        case Optional.Empty() => Optional.Empty()


// TASK 2 (more on list) svolto da solo
import Sequences.* 
import Sequences.Sequence.*

enum Person:
  case Student(name: String, year: Int)
  case Teacher(name: String, course: String)
  
object Person:
  def getCourses(persons: Sequence[Person]) : Sequence[String] = 
    flatMap(persons)(t => t match
      case Teacher(_, course) => Cons(course, Nil())
      case _ => Nil()
    )

// for show the step-by-step develop in the same file, create new version of sequence called MoreSequence
object MoreSequences:
  
  enum MoreSequence[E]:
    case Cons(head: E, tail: MoreSequence[E])
    case Nil()

  object MoreSequence:

    extension (l: MoreSequence[Int])
      def sum: Int = l match
      case Cons(h, t) => h + t.sum
      case _          => 0

      def min: Optional[Int] = l match
        case Nil() => Optional.Empty()
        case Cons(h, Nil()) => Optional.Just(h)
        case Cons(h, t) => t.min match
          case Optional.Just(x) => (h < x) match
            case true => Optional.Just(h) 
            case _ => Optional.Just(x) 
          case _ => Optional.Empty()
    
    extension [A](l: MoreSequence[A])
      def map[B](mapper: A => B): MoreSequence[B] = 
        l.flatMap(a => Cons(mapper(a), Nil()))
   
      def filter(pred: A => Boolean): MoreSequence[A] = 
        l.flatMap(k => pred(k) match
          case true => Cons(k, Nil())
          case _ => Nil()
        ) 

      def zip[B](l1: MoreSequence[B]): MoreSequence[(A, B)] = (l, l1) match
        case (Cons(a,b), Cons(c,d)) => Cons((a, c), b.zip(d))
        case _ => Nil()
    
      def take(n: Int): MoreSequence[A] = l match
        case Cons(h,t) if n > 0 => Cons(h, t.take(n-1))
        case _ => Nil()
    
      def concat(l1: MoreSequence[A]): MoreSequence[A] = l match
        case Cons(a,b) => Cons(a, b.concat(l1))
        case _ => l1
  
      def flatMap[B](mapper: A => MoreSequence[B]): MoreSequence[B] = l match
        case Cons(a,b) => mapper(a).concat(b.flatMap(mapper))
        case _ => Nil()

      def foldLeft[B](default: B)(f: (B, A) => B): B = l match
        case Cons(x, y) => y.foldLeft(f(default, x))(f) 
        case _ => default
    
      