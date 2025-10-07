package unitTests

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import units._

class nodeTester extends AnyFlatSpec with ChiselScalatestTester {

  "node" should "initialize" in {
    test(new node(16, 8)) { dut =>

      // PROGRESS CLOCK
      dut.clock.step(1)

      // PEEK AND VERIFY OUTPUTS
      // Empty node, all zeros
      dut.ioBack.CMD.expect(0.U)        // IDLE
      dut.ioBack.keyOut.expect(0.U)
      dut.ioBack.valueOut.expect(0.U)

      dut.ioFront.keyOut.expect(0.U)
      dut.ioFront.valueOut.expect(0.U)
    }
  }

  "node" should "push" in {
    test(new node(16, 8)) { dut =>

      // SET DATA INPUTS
      dut.ioFront.CMD.poke(1.U)       // 1 = PUSH
      dut.ioFront.keyIn.poke(100.U)
      dut.ioFront.valueIn.poke(200.U)

      dut.ioBack.keyIn.poke(0.U)      // Empty back
      dut.ioBack.valueIn.poke(0.U)

      // PROGRESS CLOCK
      dut.clock.step(1)

      // PEEK AND VERIFY OUTPUTS
      // Node was empty, stores the value
      dut.ioBack.CMD.expect(0.U)        // IDLE - no propagation
      dut.ioBack.keyOut.expect(0.U)
      dut.ioBack.valueOut.expect(0.U)

      dut.ioFront.keyOut.expect(0.U)    // No POP
      dut.ioFront.valueOut.expect(0.U)
    }
  }

  "node" should "pop" in {
    test(new node(16, 8)) { dut =>

      // First, PUSH a value so node has data
      dut.ioFront.CMD.poke(1.U)
      dut.ioFront.keyIn.poke(50.U)
      dut.ioFront.valueIn.poke(150.U)
      dut.ioBack.keyIn.poke(0.U)
      dut.ioBack.valueIn.poke(0.U)
      dut.clock.step(1)

      // Now POP it
      dut.ioFront.CMD.poke(2.U)       // 2 = POP
      dut.ioFront.keyIn.poke(0.U)
      dut.ioFront.valueIn.poke(0.U)

      dut.ioBack.keyIn.poke(0.U)      // No data from back
      dut.ioBack.valueIn.poke(0.U)

      // PROGRESS CLOCK
      dut.clock.step(1)

      // PEEK AND VERIFY OUTPUTS
      // Stored value (50, 150) outputs to front
      dut.ioFront.keyOut.expect(50.U)
      dut.ioFront.valueOut.expect(150.U)

      // POP command propagates to back
      dut.ioBack.CMD.expect(2.U)        // POP propagates
      dut.ioBack.keyOut.expect(0.U)
      dut.ioBack.valueOut.expect(0.U)
    }
  }

  "node" should "push and pop with frontKey smaller" in {
    test(new node(16, 8)) { dut =>

      // First, PUSH a value
      dut.ioFront.CMD.poke(1.U)
      dut.ioFront.keyIn.poke(50.U)
      dut.ioFront.valueIn.poke(150.U)
      dut.ioBack.keyIn.poke(0.U)
      dut.ioBack.valueIn.poke(0.U)
      dut.clock.step(1)

      // PUSH AND POP: frontKeyIn < backKeyIn
      dut.ioFront.CMD.poke(3.U)       // 3 = PUSH AND POP
      dut.ioFront.keyIn.poke(30.U)    // Smaller (higher priority) - should stay
      dut.ioFront.valueIn.poke(130.U)

      dut.ioBack.keyIn.poke(100.U)    // Larger (lower priority) - not needed
      dut.ioBack.valueIn.poke(199.U)

      // PROGRESS CLOCK
      dut.clock.step(1)

      // PEEK AND VERIFY OUTPUTS
      // POP: Output stored value (50) to front
      dut.ioFront.keyOut.expect(50.U)
      dut.ioFront.valueOut.expect(150.U)

      // frontKeyIn (30) < backKeyIn (100)
      // Keep 30, send IDLE to back (don't need the 100)
      dut.ioBack.CMD.expect(0.U)        // IDLE
      dut.ioBack.keyOut.expect(0.U)
      dut.ioBack.valueOut.expect(0.U)
    }
  }

  "node" should "push and pop with backKey smaller" in {
    test(new node(16, 8)) { dut =>

      // First, PUSH a value
      dut.ioFront.CMD.poke(1.U)
      dut.ioFront.keyIn.poke(50.U)
      dut.ioFront.valueIn.poke(150.U)
      dut.ioBack.keyIn.poke(0.U)
      dut.ioBack.valueIn.poke(0.U)
      dut.clock.step(1)

      // PUSH AND POP: backKeyIn < frontKeyIn
      dut.ioFront.CMD.poke(3.U)       // 3 = PUSH AND POP
      dut.ioFront.keyIn.poke(100.U)   // Larger (lower priority) - push back
      dut.ioFront.valueIn.poke(200.U)

      dut.ioBack.keyIn.poke(30.U)     // Smaller (higher priority) - keep
      dut.ioBack.valueIn.poke(130.U)

      // PROGRESS CLOCK
      dut.clock.step(1)

      // PEEK AND VERIFY OUTPUTS
      // POP: Output stored value (50) to front
      dut.ioFront.keyOut.expect(50.U)
      dut.ioFront.valueOut.expect(150.U)

      // backKeyIn (30) < frontKeyIn (100)
      // Keep 30, forward 100 to back with PUSH/POP
      dut.ioBack.CMD.expect(3.U)        // PUSH/POP propagates
      dut.ioBack.keyOut.expect(100.U)   // Forward the larger key
      dut.ioBack.valueOut.expect(200.U)
    }
  }

  "node" should "idle" in {
    test(new node(16, 8)) { dut =>

      // SET DATA INPUTS
      dut.ioFront.CMD.poke(0.U)       // 0 = IDLE
      dut.ioFront.keyIn.poke(100.U)   // Ignored
      dut.ioFront.valueIn.poke(200.U)

      dut.ioBack.keyIn.poke(99.U)     // Ignored
      dut.ioBack.valueIn.poke(199.U)

      // PROGRESS CLOCK
      dut.clock.step(1)

      // PEEK AND VERIFY OUTPUTS
      dut.ioBack.CMD.expect(0.U)        // IDLE propagates
      dut.ioBack.keyOut.expect(0.U)
      dut.ioBack.valueOut.expect(0.U)

      dut.ioFront.keyOut.expect(0.U)
      dut.ioFront.valueOut.expect(0.U)
    }
  }

}