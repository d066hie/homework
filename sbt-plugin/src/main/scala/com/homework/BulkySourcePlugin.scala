package com.homework

import sbt._
import Keys._

object BulkySourcePlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    val bulkyThresholdInLines = settingKey[Int]("Minimal LOC number to be reported (default: 100)")
    val bulkySources = taskKey[Seq[(Int, File)]]("Returns list of sources with LOC number greater than the threshold")
  }

  import autoImport._

  override lazy val projectSettings: Seq[Def.Setting[_]] =
    (bulkyThresholdInLines := 100) ++
      Seq(Compile, Test)
        .flatMap(config => inConfig(config)(bulkySources := getBulkySources(sources.value, bulkyThresholdInLines.value)))

  private def getBulkySources(files: Seq[File], threshold: Int): Seq[(Int, File)] = {
    files
      .map(file => (sbt.IO.readLines(file).size, file))
      .filter { case (count, name) => count >= threshold }
      .sortBy { case (count, _) => count }(Ordering[Int].reverse)
  }
}
