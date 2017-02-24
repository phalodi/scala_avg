import scala.collection.mutable
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

case class Input(t: Long, v: Long)

object ScalaAvg extends App {

  val inputPriorityQueue = new mutable.PriorityQueue[Input]()(customerOrdering)

  def customerOrdering = {
    new Ordering[Input] {
      def compare(c1: Input, c2: Input) = {
        c2.v.compareTo(c1.v)
      }
    }
  }

  val inputList = mutable.ListBuffer[Input]()
  println("Enter number of Inputs")
  val noOfInputs = StdIn.readInt()

  println("Enter time Inputs with time and values with space example 0 3")

  for (i <- 1 to noOfInputs) {
    val ln = StdIn.readLine()
    val customer = Try(Input(ln.split(" ")(0).toInt, ln.split(" ")(1).toInt))
    customer match {
      case Success(a) => inputList += a
      case Failure(_) => println("Input is invalid it will not consider for avg result")
    }
  }

  var totalTime = 0L
  var value = 0L

  while (inputList.nonEmpty || inputPriorityQueue.nonEmpty) {
    while (inputList.nonEmpty && (inputPriorityQueue.isEmpty || inputList(0).t <= value)) {
      val c = inputList.head
      inputList -= c
      inputPriorityQueue.enqueue(c)
      value = Math.max(value, c.t)
    }

    val c = inputPriorityQueue.dequeue()
    value += c.v
    totalTime += value - c.t
  }

  val avgTime = totalTime / noOfInputs
  println("Min Avg time is: " + avgTime)
}
