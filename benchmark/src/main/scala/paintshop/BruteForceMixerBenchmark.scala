package paintshop

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(1)
@Warmup(iterations = 15)
@Measurement(iterations = 15)
@State(Scope.Benchmark)
class BruteForceMixerBenchmark {

  @Param(scala.Array("1", "2", "3", "4", "5", "10", "15", "20"))
  var colors: Int = _

  var feasibleMix: List[PaintSelection] = _
  var unfeasibleMix: List[PaintSelection] = _

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    feasibleMix = List.tabulate(50)(_ => {
      val paints = (1 to colors).map(c => Paint(Color(c), if (c % 2 == 0) Gloss else Matte)).toSet
      PaintSelection(paints)
    })

    unfeasibleMix =
      PaintSelection(Set(Paint(Color(colors + 1), Gloss), Paint(Color(colors + 2), Gloss))) ::
      PaintSelection(Set(Paint(Color(colors + 1), Gloss), Paint(Color(colors + 2), Matte))) ::
      PaintSelection(Set(Paint(Color(colors + 1), Matte), Paint(Color(colors + 2), Gloss))) ::
      PaintSelection(Set(Paint(Color(colors + 1), Matte), Paint(Color(colors + 2), Matte))) ::
      List.tabulate(50)(_ => PaintSelection((1 to colors - 2).map(c => Paint(Color(c), Matte)).toSet))
  }

  @Benchmark
  def feasible(bh: Blackhole): Unit = {
    bh.consume(BruteForceMixer.mix(feasibleMix))
  }

  @Benchmark
  def unfeasible(bh: Blackhole): Unit = {
    bh.consume(BruteForceMixer.mix(unfeasibleMix))
  }
}
