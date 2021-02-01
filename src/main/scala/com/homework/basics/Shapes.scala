package com.homework.basics

import scala.math.{hypot, pow, sqrt}

object Shapes {
  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Shape extends Located with Bounded {
    def area: Double
  }

  type Point2D = Point

  final case class Point(x: Double, y: Double) extends Shape {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def area: Double = 0

    def distance(p: Point): Double = {
      hypot(p.x - x, p.y - y)
    }
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = centerX - radius
    override def maxX: Double = centerX + radius
    override def minY: Double = centerY - radius
    override def maxY: Double = centerY + radius
    override def area: Double = Math.PI * pow(radius, 2)
  }

  abstract class AbstractRectangle(x: Double, y: Double, width: Double, height: Double) extends Shape {
    override def minX: Double = x
    override def maxX: Double = x + width
    override def minY: Double = y
    override def maxY: Double = y + height
    override def area: Double = width * height
  }

  case class Rectangle(x: Double, y: Double, width: Double, height: Double) extends AbstractRectangle(x, y, width, height)
  case class Square(x: Double, y: Double, size: Double) extends AbstractRectangle(x, y, size, size)

  final case class Triangle(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double) extends Shape {
    override def minX: Double = List(x1, x2, x3).min
    override def maxX: Double = List(x1, x2, x3).max
    override def minY: Double = List(y1, y2, y3).min
    override def maxY: Double = List(y1, y2, y3).max

    override def x: Double = x1
    override def y: Double = y1

    override def area: Double = {
      val d1 = Point(x1, y1) distance Point(x2, y2)
      val d2 = Point(x1, y1) distance Point(x3, y3)
      val d3 = Point(x2, y2) distance Point(x3, y3)
      val p = (d1 + d2 + d3) / 2

      sqrt(p * (p - d1) * (p - d2) * (p - d3))
    }
  }

  type Bounded2D = Bounded
  type Shape2D = Shape
  type Located2D = Located

  sealed trait Located3D extends Located2D {
    def z: Double
  }

  sealed trait Bounded3D extends Bounded2D {
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Shape3D extends Located3D with Bounded3D {
    def volume: Double
    def surfaceArea: Double
  }

  final case object Origin extends Located3D with Bounded3D with Shape3D {
    override def minX: Double = 0
    override def maxX: Double = 0
    override def minY: Double = 0
    override def maxY: Double = 0
    override def minZ: Double = 0
    override def maxZ: Double = 0

    override def x: Double = 0
    override def y: Double = 0
    override def z: Double = 0

    override def volume: Double = 0
    override def surfaceArea: Double = 0
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z

    override def volume: Double = 0
    override def surfaceArea: Double = 0

    def distance(p: Point3D): Double = {
      Math.sqrt(List(x - p.x, y - p.y, z - p.z).map(pow(_, 2)).sum)
    }
  }

  final case class Sphere(x: Double, y: Double, z: Double, radius: Double) extends Shape3D {
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def minZ: Double = z - radius
    override def maxZ: Double = z + radius

    override def volume: Double = (4 / 3) * Math.PI * pow(radius, 3)
    override def surfaceArea: Double = 4 * Math.PI * pow(radius, 2)
  }

  abstract class AbstractCuboid(x: Double, y: Double, z: Double, width: Double, height: Double, depth: Double) extends Shape3D {
    override def minX: Double = x
    override def maxX: Double = x + width
    override def minY: Double = y
    override def maxY: Double = y + height
    override def minZ: Double = z
    override def maxZ: Double = z + depth

    override def volume: Double = width * height * depth
    override def surfaceArea: Double = 2 * (width * height + width * depth + depth * height)
  }

  sealed case class Cuboid(x: Double, y: Double, z: Double, width: Double, height: Double, depth: Double) extends AbstractCuboid(x, y, z, width, height, depth)
  sealed case class Cube(x: Double, y: Double, z: Double, size: Double) extends AbstractCuboid(x, y, z, size, size, size)

  final case class Triangle3D(x1: Double, y1: Double, z1: Double, x2: Double, y2: Double, z2: Double, x3: Double, y3: Double, z3: Double) extends Shape3D {
    override def minX: Double = List(x1, x2, x3).min
    override def maxX: Double = List(x1, x2, x3).max
    override def minY: Double = List(y1, y2, y3).min
    override def maxY: Double = List(y1, y2, y3).max
    override def minZ: Double = List(z1, z2, z3).min
    override def maxZ: Double = List(z1, z2, z3).max

    override def x: Double = x1
    override def y: Double = y1
    override def z: Double = z1

    override def volume: Double = 0
    override def surfaceArea: Double = {
      val d1 = Point3D(x1, y1, z1) distance Point3D(x2, y2, z2)
      val d2 = Point3D(x1, y1, z1) distance Point3D(x3, y3, z3)
      val d3 = Point3D(x3, y3, z3) distance Point3D(x2, y2, z2)
      val p = (d1 + d2 + d3) / 2

      sqrt(p * (p - d1) * (p - d2) * (p - d3))
    }
  }
}
