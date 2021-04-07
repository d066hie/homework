package com.homework.effects

import com.homework.effects.EffectsHomework1._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.funsuite.AnyFunSuite
import java.util.concurrent.atomic.AtomicInteger

class EffectsHomework1Spec extends AnyFunSuite {
  test("IO map") {
    var state = false
    val io = IO(1)
    val chain = io.map(value => {
      state = true
      value + 1
    })

    assert(!state)
    assert(chain.unsafeRunSync() == 2)
    assert(state)
  }

  test("IO flatMap") {
    var state = false
    val io = IO(1)
    val chain = io.flatMap(value => {
      state = true
      IO(value + 1)
    })

    assert(!state)
    assert(chain.unsafeRunSync() == 2)
    assert(state)
  }

  test("IO *>") {
    var state = new AtomicInteger(0)

    val start = IO({
      state.incrementAndGet()
    })
    val chain = IO({
      state.incrementAndGet()
      true
    })

    assert(0 == state.get())
    val result = start *> chain
    assert(result.unsafeRunSync())
    assert(2 == state.get())
  }

  test("IO as") {
    val newValue = 2
    assert(newValue == IO(1).as(newValue).unsafeRunSync())
  }

  test("IO void") {
    assert(IO.unit == IO(2).void)
  }

  test("IO attempt") {
    val value = 1
    val error = new java.lang.Exception()

    assert(IO(value).attempt.unsafeRunSync() == Right(value))
    assert(IO(throw error).attempt.unsafeRunSync() == Left(error))
  }

  test("IO option") {
    val value = 1
    val error = new java.lang.Exception()

    assert(IO(value).option.unsafeRunSync() == Some(value))
    assert(IO(throw error).option.unsafeRunSync() == None)
  }

  test("IO handleErrorWith") {
    val value = 1
    val default = 2
    val error = new java.lang.Exception()

    assert(IO(value).handleErrorWith(_ => IO(default)).unsafeRunSync() == 1)
    assert(IO(throw error).handleErrorWith(_ => IO(default)).unsafeRunSync() == default)
  }

  test("IO redeem") {
    val value = 1
    val default = 2
    val error = new java.lang.Exception()

    assert(IO(value).redeem(_ => default, (x: Int) => x + 2).unsafeRunSync() == 3)
    assert(IO(throw error).redeem(_ => default, (x: Int) => x + 2).unsafeRunSync() == 2)
  }

  test("IO redeemWith") {
    val value = 1
    val default = 2
    val error = new java.lang.Exception()

    assert(IO(value).redeemWith(_ => IO(default), (x: Int) => IO(x + 2)).unsafeRunSync() == 3)
    assert(IO(throw error).redeemWith(_ => IO(default), (x: Int) => IO(x + 2)).unsafeRunSync() == 2)
  }
}
