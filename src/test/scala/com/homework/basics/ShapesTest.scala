package com.homework.basics

import org.scalactic.Tolerance.convertNumericToPlusOrMinusWrapper
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ShapesTest extends AnyFlatSpec {
  "Point" should "provide a method to calculate distance between two points" in {
    val p1 = Shapes.Point(1, 1)
    val p2 = Shapes.Point(4, 5)
    p1 distance p2 shouldEqual 5.0 +- 0.01
  }

  "Origin" should "provide unique instance of Origin object" in {
    val origin = Shapes.Origin
    origin.volume shouldEqual 0
    origin.surfaceArea shouldEqual 0
  }

  "Point3D" should "provide a method to calculate distance between two points" in {
    val p1 = Shapes.Point3D(1, 1, 0)
    val p2 = Shapes.Point3D(2, 1, 2)
    p1 distance p2 shouldEqual 2.24 +- 0.01
  }

  "Circle" should "provide methods for working with circular objects" in {
    val circle = Shapes.Circle(0, 0, 4)
    circle.area shouldEqual 50.26 +- 0.1
  }

  "Sphere" should "provide methods for working with spherical objects" in {
    val sphere = Shapes.Sphere(0, 0, 0, 5)
    sphere.surfaceArea shouldEqual 314.15 +- 0.1
    sphere.volume shouldEqual 392.69 +- 0.1
  }

  "Square" should "provide methods for square objects" in {
    val square = Shapes.Square(0, 0, 5)
    square.area shouldEqual 25
  }

  "Rectangle" should "provide methods for rectangle objects" in {
    val rectangle = Shapes.Rectangle(0, 0, 5, 10)
    rectangle.area shouldEqual 50
  }

  "Cuboid" should "provide methods for cuboid objects" in {
    val cuboid = Shapes.Cuboid(0, 0, 0, 2, 3, 4)
    cuboid.volume shouldEqual 24
    cuboid.surfaceArea shouldEqual 52
  }

  "Cube" should "provide methods for cubical objects" in {
    val cube = Shapes.Cube(0, 0, 0, 5)
    cube.volume shouldEqual 125
    cube.surfaceArea shouldEqual 150
  }

  "Triangle" should "provide methods for triangular objects" in {
    val triangle = Shapes.Triangle(0, 0, 0, 4, 4, 0)
    triangle.area shouldEqual 8.0 +- 0.1
  }

  "Triangle3D" should "provide methods for triangular objects in 3D" in {
    val triangle3D = Shapes.Triangle3D(0, 0, 0, 4, 0, 0, 0, 0, 4)
    triangle3D.surfaceArea shouldEqual 8.0 +- 0.1
    triangle3D.volume shouldEqual 0
  }
}
