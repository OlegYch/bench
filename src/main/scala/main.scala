import java.io.File
import scala.io.Source

@main
def main(): Unit = {
  val files   = new File("javabench").listFiles().filter(_.getName.endsWith("csv"))
  case class Bench(name: String, jdk: Int, file: File)
  val benches = files
    .map { case f =>
      val name = ".*bench-(.*)-openjdk(\\d+).csv".r
      f.getName match {
        case name(name, jdk) => Bench(name, jdk.toInt, f)
      }
    }
  case class Results(bench: Bench, results: Vector[Long]) {
    case object Exists {
      lazy val median          = results.sorted.apply(results.size / 2).toDouble
      lazy val average         = results.sum.toDouble / results.size
      lazy val withoutOutliers = Results(bench, results.filter(r => r >= median * 0.2))
    }
    val exists = Option(Exists).filter(_ => results.nonEmpty)
  }

  val r = benches.toVector.map { b =>
    val result = ".*,(\\d+),.*,.*".r
    val r      = Source.fromFile(b.file).getLines().collect { case result(r) =>
      r.toLong
    }
    Results(b, r.toVector)
  }
//  r.foreach(println)
  r.groupBy(_.bench.name).foreach { case (name, rs) =>
    println(
      name -> rs
        .filter(_.exists.nonEmpty)
        .flatMap(_.exists.map(_.withoutOutliers))
        .sortBy(_.exists.map(_.average))
        .map(r => r.bench.jdk -> r.exists.map(e => (e.median, e.average)))
    )
  }
}
