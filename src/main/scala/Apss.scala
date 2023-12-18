import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math.sqrt

object Apss {

  def jaccardSim(r: Set[String], s: Set[String]): Double = {
    val intersect = r.intersect(s)
    val union = r.union(s)
    intersect.size.toDouble / union.size
  }

  def readTxtFile(filename: String): List[List[String]] = {
    val fileObject = Source.fromFile(filename)
    fileObject.getLines()
      .map(line => line.split("\\s+").map(_.replaceAll("[.,;]", "")).toList)
      .toList
  }

  def cardinality(vector: List[Double]): Double = {
    sqrt(vector.map(x => x * x).sum)
  }

  def dotProduct(listA: List[Double], listB: List[Double]): Double = {
    listA.zip(listB).map { case (a, b) => a * b }.sum
  }

  def cosineSim(listA: List[Double], listB: List[Double]): Double = {
    dotProduct(listA, listB) / (cardinality(listA) * cardinality(listB))
  }

  def makeDocumentVectors(ds: List[List[String]]): List[List[Double]] = {
    val charSet = ds.flatten.toSet
    ds.map(vector => charSet.map(char => vector.count(_ == char).toDouble).toList)
  }

  def normalizeDocVectors(docVectors: List[List[Double]]): List[List[Double]] = {
    docVectors.map(vector => vector.map(_ / cardinality(vector)))
  }


  def sparseRepresentation(documentVectors: List[List[Double]]): List[List[(Int, Double)]] = {
    documentVectors.map(vector => vector.zipWithIndex.filter { case (value, _) => value > 0 }.map { case (value, index) => (index, value) })
  }


  def checkResults(res: List[(Int, Int, Double)], documentVectors: List[List[Int]]): List[(Int, Int, Boolean)] = {
    res.map { case (id1, id2, sim) =>
      val vect1 = documentVectors(id1)
      val vect2 = documentVectors(id2)
      val isCorrect = math.round(sim * 10000) == math.round(cosineSim(vect1.map(_.toDouble), vect2.map(_.toDouble)) * 10000)
      (id1, id2, isCorrect)
    }
  }

  def getDimensionality(V: List[List[(Int, Double)]]): Int = {
    val flatList = V.flatten
    flatList.map { case (dimension, _) => dimension }.max + 1
  }

  def findMatches(xID: Int, x: List[(Int, Double)], I: Map[Int, List[(Int, Double)]], t: Double): List[(Int, Int, Double)] = {
    val A = collection.mutable.Map[Int, Double]()
    val M = ArrayBuffer[(Int, Int, Double)]()

    for ((i, xi) <- x) {
      for ((yID, yi) <- I.getOrElse(i, List())) {
        A.update(yID, A.getOrElse(yID, 0.0) + xi * yi)
      }
    }

    for ((yID, value) <- A) {
      if (value > t) {
        M += ((xID, yID, value))
      }
    }

    M.toList
  }

  def allPairs0(V: List[List[(Int, Double)]], t: Double): List[(Int, Int, Double)] = {
    val m = getDimensionality(V)
    println(s"dimension m of vectors in V: $m")
    var O = List[(Int, Int, Double)]()
    var I = Map[Int, List[(Int, Double)]]().withDefaultValue(List())

    for ((x, xID) <- V.zipWithIndex) {
      println(s"\nvector number $xID:\n$x")
      val matches = findMatches(xID, x, I, t)
      println(s"found matches: $matches")
      O = O ::: matches
      for ((i, xi) <- x) {
        I += (i -> (I(i) ::: List((xID, xi))))
      }
      println(s"updated I:\n${I.filter { case (_, v) => v.nonEmpty }}")
    }

    O
  }

  def main(args: Array[String]): Unit = {
    val mini = readTxtFile("src/main/data/mini_data_set.txt")
    val docVectors = makeDocumentVectors(mini)
    val normDocVectors = normalizeDocVectors(docVectors)
    val sparseVectors = sparseRepresentation(normDocVectors)
    println(sparseVectors)

    val result = allPairs0(sparseVectors, 0.5)
    println(s"\n\nresult: ${result.length} matching pairs:\n$result")

    val docVectorsAsInt: List[List[Int]] = docVectors.map(_.map(_.toInt))
    val checkedResult = checkResults(result, docVectorsAsInt)
    println(s"\nchecking pairs:\n$checkedResult")
  }
}
