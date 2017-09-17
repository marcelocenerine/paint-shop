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
class MixerBenchmark {

  @Param(scala.Array("1", "2", "3", "4", "5", "10", "15", "20"))
  var colors: Int = _

  var selections: List[PaintSelection] = _
  var palette: Palette = _

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    palette = Palette((1 to colors).map(Color).toSet, Sheen.all)

    selections = List.tabulate(50)(_ => {
      val paints = (1 to colors).map(c => Paint(Color(c), if (c % 2 == 0) Gloss else Matte)).toSet
      PaintSelection(paints)
    })
  }

  @Benchmark
  def feasible(bh: Blackhole): Unit = {
    bh.consume(Mixer.mix(selections, palette))
  }

  @Benchmark
  def unfeasible(bh: Blackhole): Unit = {
    val unfeasibleSelection =
      PaintSelection(Set(Paint(Color(1), Gloss))) :: PaintSelection(Set(Paint(Color(1), Matte))) :: selections

    bh.consume(Mixer.mix(unfeasibleSelection, palette))
  }
}
