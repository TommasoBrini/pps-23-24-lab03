import u03.Optionals.Optional.*
import org.junit.*
import org.junit.Assert.*

// Test of TASK1 (lists)
class TestTask1:

    import lab03.Sequences.* 
    import Sequence.*

    val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

    @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

    @Test def testFilter() =
        assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
        assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))
  
    @Test def testTake() =
        assertEquals(Cons(10, Cons(20, Nil())), take(l)(2))
        assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(l)(3))
        assertEquals(Nil(), take(l)(0))
        assertEquals(Nil(), take(Nil())(2))
  
    @Test def testZip() = 
        val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
        assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(l, l2))
        assertEquals(Nil(), zip(l, Nil()))
        assertEquals(Nil(), zip(Nil(), l2))
        assertEquals(Nil(), zip(Nil(), Nil()))

    @Test def testConcat() =
        val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
        assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(l, l2))
        assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))
  
    @Test def testFlatMap() =
        assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
        assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

    @Test def testMin() =
        assertEquals(Just(10), min(l))
        assertEquals(Just(1), min(Cons(1, Nil())))
        assertEquals(Empty(), min(Nil()))


class TestTask2:
    import lab03.Person.*
    import lab03.Sequences.* 
    import Sequence.*

    @Test def testCourses() = 
        val s = Student("Mario" , 1)
        val t = Teacher("Viroli", "PPS")
        val t2 = Teacher("Maltoni", "ml")
        assertEquals(Nil(), getCourses(Nil()))
        assertEquals(Nil(), getCourses(Cons(s, Nil())))
        assertEquals(Cons("PPS", Nil()), getCourses(Cons(s, Cons(t, Nil()))))

    import lab03.MoreSequences.*
    import MoreSequence.* 

    val l: MoreSequence[Int] = MoreSequence.Cons(10, MoreSequence.Cons(20, MoreSequence.Cons(30, MoreSequence.Nil())))

    @Test def testMap() =
        assertEquals(MoreSequence.Cons(11, MoreSequence.Cons(21, MoreSequence.Cons(31, MoreSequence.Nil()))), l.map(_ + 1))
        assertEquals(MoreSequence.Cons("10", MoreSequence.Cons("20", MoreSequence.Cons("30", MoreSequence.Nil()))), l.map(_ + ""))

    @Test def testFilter() =
        assertEquals(MoreSequence.Cons(20, MoreSequence.Cons(30, MoreSequence.Nil())), l.filter(_ >= 20))
        assertEquals(MoreSequence.Cons(10, MoreSequence.Cons(30, MoreSequence.Nil())), l.filter(_ != 20))
  
    @Test def testTake() =
        assertEquals(MoreSequence.Cons(10, MoreSequence.Cons(20, MoreSequence.Nil())), l.take(2))
        assertEquals(MoreSequence.Cons(10, MoreSequence.Cons(20, MoreSequence.Cons(30, MoreSequence.Nil()))), l.take(3))
        assertEquals(MoreSequence.Nil(), l.take(0))
        assertEquals(MoreSequence.Nil(), MoreSequence.Nil().take(2))
  
    @Test def testZip() = 
        val l2: MoreSequence[String] = MoreSequence.Cons("10", MoreSequence.Cons("20", MoreSequence.Cons("30", MoreSequence.Nil())))
        assertEquals(MoreSequence.Cons((10, "10"), MoreSequence.Cons((20, "20"), MoreSequence.Cons((30, "30"), MoreSequence.Nil()))), l.zip(l2))
        assertEquals(MoreSequence.Nil(), l.zip(MoreSequence.Nil()))
        assertEquals(MoreSequence.Nil(), MoreSequence.Nil().zip(l))
        assertEquals(MoreSequence.Nil(), MoreSequence.Nil().zip(MoreSequence.Nil()))

    @Test def testConcat() =
        val l2: MoreSequence[Int] = MoreSequence.Cons(40, MoreSequence.Cons(50, MoreSequence.Nil()))
        assertEquals(MoreSequence.Cons(10, MoreSequence.Cons(20, MoreSequence.Cons(30, MoreSequence.Cons(40, MoreSequence.Cons(50, MoreSequence.Nil()))))), l.concat(l2))
        assertEquals(MoreSequence.Cons(40, MoreSequence.Cons(50, MoreSequence.Nil())), MoreSequence.Nil().concat(l2))
  
    @Test def testFlatMap() =
        assertEquals(MoreSequence.Cons(11, MoreSequence.Cons(21, MoreSequence.Cons(31, MoreSequence.Nil()))), l.flatMap(v => MoreSequence.Cons(v + 1, MoreSequence.Nil())))
        assertEquals(MoreSequence.Nil(), MoreSequence.Nil().flatMap(v => MoreSequence.Cons(v, MoreSequence.Nil())))

    @Test def testMin() =
        assertEquals(Just(10), l.min)
        assertEquals(Just(1), MoreSequence.Cons(1, MoreSequence.Nil()).min)
        assertEquals(Empty(), MoreSequence.Nil().min)

    @Test def testFoldLeft() =
        val lst = MoreSequence.Cons(3, MoreSequence.Cons(7, MoreSequence.Cons(1, MoreSequence.Cons(5, MoreSequence.Nil()))))
        assertEquals(-16, lst.foldLeft(0)(_ - _))
        assertEquals(0, lst.foldLeft(0)(_ * _))


class TestTask3:
    import lab03.Sequences.* 
    import Sequence.*
    import lab03.Streams.* 
    import Stream.* 

    @Test def testIterate(): Unit = 
        val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
        assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Nil())))), toList(Stream.take(str1)(4)))

    @Test def testMap(): Unit =
        val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
        val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
        assertEquals(Cons(1, Cons(2, Cons(3, Cons(4, Nil())))), toList(Stream.take(str2)(4)))

    @Test def testFilter(): Unit =
        val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
        val str2 = Stream.filter(str1)(x => x % 2 == 1) // {1,3,5,7,..}
        assertEquals(Cons(1, Cons(3, Cons(5, Cons(7, Nil())))), toList(Stream.take(str2)(4)))

    @Test def takeWhile(): Unit = 
        val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
        val str2 = Stream.takeWhile(str1)(_ < 5) // {0,1,2,3,4}
        assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(str2))

    @Test def testFill(): Unit =
        assertEquals(Cons("a", Cons("a", Cons("a", Nil()))), Stream.toList(Stream.fill(3)("a")))
        assertEquals(Cons(1, Cons(1, Cons(1, Cons(1, Nil())))), Stream.toList(Stream.fill(4)(1)))

    @Test def testPell(): Unit =
        val pell: Stream[Int] = Stream.pell()
        assertEquals(Cons(0, Cons(1, Cons(2, Cons(5, Cons(12, Nil()))))), Stream.toList(Stream.take(pell)(5)))