package units

import chisel3._
import chisel3.util._

// Command enumeration for clarity
object CMD {
  val IDLE     = 0.U(2.W)
  val PUSH     = 1.U(2.W)
  val POP      = 2.U(2.W)
  val PUSH_POP = 3.U(2.W)
}

class node (dataWidth: Int, keyWidth: Int) extends Module {

  val ioFront = IO(new Bundle {
    val keyOut   = Output(UInt(keyWidth.W))
    val keyIn    = Input(UInt(keyWidth.W))
    val CMD      = Input(UInt(2.W))
    val valueOut = Output(UInt(dataWidth.W))
    val valueIn  = Input(UInt(dataWidth.W))
  })

  val ioBack = IO(new Bundle {
    val keyOut   = Output(UInt(keyWidth.W))
    val keyIn    = Input(UInt(keyWidth.W))
    val CMD      = Output(UInt(2.W))
    val valueOut = Output(UInt(dataWidth.W))
    val valueIn  = Input(UInt(dataWidth.W))
  })

  // Registers to store key-value pair
  val storedKey   = RegInit(0.U(keyWidth.W))
  val storedValue = RegInit(0.U(dataWidth.W))

  // Helper to check if node is empty (key == 0 means empty)
  val isEmpty = storedKey === 0.U

  // Default outputs (idle state)
  ioFront.keyOut   := 0.U
  ioFront.valueOut := 0.U
  ioBack.CMD       := CMD.IDLE
  ioBack.keyOut    := 0.U
  ioBack.valueOut  := 0.U

  // Command processing
  switch(ioFront.CMD) {

    is(CMD.IDLE) {
      // Do nothing, maintain current state
      ioBack.CMD := CMD.IDLE
    }

    is(CMD.PUSH) {
      when(isEmpty) {
        // Node is empty, store the incoming key/value
        storedKey   := ioFront.keyIn
        storedValue := ioFront.valueIn
        ioBack.CMD  := CMD.IDLE
      }.otherwise {
        // Node is full, compare and determine what to keep
        when(ioFront.keyIn =/= 0.U && ioFront.keyIn < storedKey) {
          // Incoming key is smaller (higher priority)
          // Push current stored value backward, store new value
          ioBack.CMD      := CMD.PUSH
          ioBack.keyOut   := storedKey
          ioBack.valueOut := storedValue
          storedKey       := ioFront.keyIn
          storedValue     := ioFront.valueIn
        }.elsewhen(ioFront.keyIn =/= 0.U) {
          // Incoming key is larger (lower priority)
          // Keep current value, forward incoming value backward
          ioBack.CMD      := CMD.PUSH
          ioBack.keyOut   := ioFront.keyIn
          ioBack.valueOut := ioFront.valueIn
        }.otherwise {
          // keyIn is 0 (invalid), ignore the push
          ioBack.CMD := CMD.IDLE
        }
      }
    }

    is(CMD.POP) {
      // Output stored value to front
      ioFront.keyOut   := storedKey
      ioFront.valueOut := storedValue

      // Pull from back to refill
      storedKey   := ioBack.keyIn
      storedValue := ioBack.valueIn

      // Propagate POP command backward
      ioBack.CMD := CMD.POP
    }

    is(CMD.PUSH_POP) {
      // POP: Output stored value to front
      ioFront.keyOut   := storedKey
      ioFront.valueOut := storedValue

      // PUSH: Compare frontKeyIn vs backKeyIn
      // Keep smaller key (higher priority), forward larger key
      when(ioFront.keyIn =/= 0.U && (ioBack.keyIn === 0.U || ioFront.keyIn < ioBack.keyIn)) {
        // frontKeyIn is smaller or back is empty
        // Keep frontKeyIn, send IDLE (don't need backKeyIn)
        storedKey   := ioFront.keyIn
        storedValue := ioFront.valueIn
        ioBack.CMD  := CMD.IDLE
      }.elsewhen(ioBack.keyIn =/= 0.U) {
        // backKeyIn is smaller (or frontKeyIn is 0)
        // Keep backKeyIn, forward frontKeyIn backward
        storedKey   := ioBack.keyIn
        storedValue := ioBack.valueIn

        when(ioFront.keyIn =/= 0.U) {
          // Forward frontKeyIn with PUSH/POP
          ioBack.CMD      := CMD.PUSH_POP
          ioBack.keyOut   := ioFront.keyIn
          ioBack.valueOut := ioFront.valueIn
        }.otherwise {
          // frontKeyIn is 0, just propagate POP
          ioBack.CMD := CMD.POP
        }
      }.otherwise {
        // Both keys are 0, node becomes empty
        storedKey   := 0.U
        storedValue := 0.U
        ioBack.CMD  := CMD.IDLE
      }
    }
  }
}

object nodeMain extends App {
  println("Generating the node hardware")
  emitVerilog(new node(16, 8), Array("--target-dir", "generated"))
}