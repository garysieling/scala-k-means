class Point(dx: Double, dy: Double) {
  val x: Double = dx
  val y: Double = dy

  override def toString(): String = {
    "(" + x + ", " + y + ")"
  }

  def dist(p: Point): Double = {
    return x * p.x + y * p.y
  }
}

object kmeans extends App {
  val k: Int = 2

  // Correct answers to centers are (10, 20) and (25, 5)
  val points: List[Point] = List(
    new Point(10.8348626966492, 18.7800980127523),
    new Point(10.259545802461, 23.4515683763173),
    new Point(11.7396623802245, 17.7026240456956),
    new Point(12.4277617893575, 19.4887691804508),
    new Point(10.1057940183815, 18.7332929859685),
    new Point(11.0118378554584, 20.9773232834654),
    new Point(7.03171204763376, 19.1985058633283),
    new Point(6.56491029696013, 21.5098251711267),
    new Point(10.7751248849735, 22.1517666115673),
    new Point(8.90149362263775, 19.6314465074203),
    new Point(11.931275122466, 18.0462702532436),
    new Point(11.7265904596619, 16.9636039793709),
    new Point(11.7493214262468, 17.8517235677469),
    new Point(12.4353462881232, 19.6310467981989),
    new Point(13.0838514816799, 20.3398794353494),
    new Point(7.7875624720831, 20.1569764307574),
    new Point(11.9096128931784, 21.1855674228972),
    new Point(8.87507602702847, 21.4823134390704),
    new Point(7.91362116378194, 21.325928219919),
    new Point(26.4748241341303, 9.25128245838802),
    new Point(26.2100410238973, 5.06220487544192),
    new Point(28.1587146197594, 3.70625885635717),
    new Point(26.8942422516129, 5.02646862012427),
    new Point(23.7770902983858, 7.19445492687232),
    new Point(23.6587920739353, 3.35476798095758),
    new Point(23.7722765903534, 3.74873642284525),
    new Point(23.9177161897547, 8.1377950229489),
    new Point(22.4668345067162, 8.9705504626857),
    new Point(24.5349708443852, 5.00561881333415),
    new Point(24.3793349065557, 4.59761596097384),
    new Point(27.0339042858296, 4.4151109960116),
    new Point(21.8031183153743, 5.69297814349064),
    new Point(22.636600400773, 2.46561420928429),
    new Point(25.1439536911272, 3.58469981317611),
    new Point(21.4930923464916, 3.28999356823389),
    new Point(23.5359486724204, 4.07290025106778),
    new Point(22.5447925324242, 2.99485404382734),
    new Point(25.4645673159779, 7.54703465191098)).sortBy(
      p => (p.x + " " + p.y).hashCode())

  def clusterMean(points: List[Point]): Point = {
    val cumulative = points.reduceLeft((a: Point, b: Point) => new Point(a.x + b.x, a.y + b.y))

    return new Point(cumulative.x / points.length, cumulative.y / points.length)
  }

  def render(points: Map[Int, List[Point]]) {
    for (clusterNumber <- points.keys.toSeq.sorted) {
      System.out.println("  Cluster " + clusterNumber)

      val meanPoint = clusterMean(points(clusterNumber))
      System.out.println("  Mean: " + meanPoint)

      for (j <- 0 to points(clusterNumber).length - 1) {
        System.out.println("    " + points(clusterNumber)(j) + ")")
      }

      System.out.println("")
    }
  }

  val clusters =
    points.zipWithIndex.groupBy(
      x => x._2 % k) transform (
        (i: Int, p: List[(Point, Int)]) => for (x <- p) yield x._1)

  System.out.println("Initial State: ")
  render(clusters)

  def iterate(clusters: Map[Int, List[Point]]): Map[Int, List[Point]] = {
    val unzippedClusters =
      (clusters: Iterator[(Point, Int)]) => clusters.map(cluster => cluster._1)

    // find cluster means
    val means =
      (clusters: Map[Int, List[Point]]) =>
        for (clusterIndex <- clusters.keys)
          yield clusterMean(clusters(clusterIndex))

    // find the closest index
    def closest(p: Point, means: Iterable[Point]): Int = {
      val distances = for (center <- means) yield p.dist(center)
      return distances.zipWithIndex.min._2
    }

    // assignment step
    val newClusters =
      points.groupBy(
        (p: Point) => closest(p, means(clusters)))

    render(newClusters)

    return newClusters
  }

  var clusterToTest = clusters
  for (i <- 0 to 5) {
    System.out.println("Iteration: " + i)
    clusterToTest = iterate(clusterToTest)
  }
}
