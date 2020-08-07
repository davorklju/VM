package vm

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Using

class Decoder() {
  import java.lang.{ Integer => JInt }
  var dataBytesRead = 0

  def hex(str: String): Byte =
    JInt.parseInt(str,16).toByte

  def loadVFile(vm: VM, sourceFile: String): Unit = {
    Using( Source.fromFile(sourceFile) ){ source =>
      val iter = source.getLines()
      val dataSize = iter.next().toInt
      vm.pc = dataSize
      iter.foreach{ line =>
        for( i <- 0 until line.length/2 ){
          val byte = hex( line.substring(i*2,(i+1)*2) )
          vm.mem(dataBytesRead) = byte
          dataBytesRead += 1
        }
      }
    }
    vm.regs(VM.sp) = dataBytesRead
    vm.regs(VM.fp) = dataBytesRead
  }

  def nextByte(vm: VM): Byte = {
    val byte = vm.mem(vm.pc)
    vm.pc += 1
    byte
  }

  def nextLit(vm: VM, size: Size): Literal = {
    var out: Long = 0
    for( i <- size.range ){
      val byte = (nextByte(vm) & 0xFF).toLong
      out = out | (byte << 8*i)
    }
    NumLit( size.cast(out) )
  }

  def nextJumpType(vm: VM): JumpType = {
    nextByte(vm) match {
      case 0 => Jeq
      case 1 => Jne
      case 2 => Jlt
      case 3 => Jgt
      case 4 => Jle
      case 5 => Jge
      case 6 => Jmp
      case x => throw new Error(s"Unexpected jump type: $x at pc=${vm.pc}")
    }
  }

  def arithType(arithT: Int): ArithType = arithT match {
    case 0 => Add
    case 1 => Sub
    case 2 => Mul
    case 3 => Div
    case 4 => Mod
    case 5 => LS
    case 6 => RS
    case 7 => And
    case 8 => Or
    case 9 => XOr
    case 10 => Not
    case 11 => Neg
    case _ => throw new Error(s"Unknown arithType: $arithT at $dataBytesRead")
  }

  def nextSysCall(vm: VM) = nextByte(vm) match {
    case 0 => Print
    case 1 => Read
    case x => throw new Error(s"Unkown sysCall $x at pc = ${vm.pc}")
  }

  def decode(vm: VM): Instruction = {
    val b0 = nextByte(vm)
    val instruction = (b0 >> 4) & 0xF
    val size  = (b0 >> 2) & 3 match {
      case 0 => I8
      case 1 => I16
      case 2 => I32
      case 3 => I64
      case x => throw new Error(s"Unknown size: $x at pc=${vm.pc}")
    }
    val isReg = (b0 & 1) == 0

    instruction match {
      case 0 => Noop
      case 1 => Halt
      case 2 => Ret

      case 3 =>
        val b1 = nextByte(vm)
        val dstReg = b1 & 0xF
        Pop(size, dstReg)

      case 4 =>
        if( isReg ){
          val b1 = nextByte(vm)
          val srcReg = (b1 >> 4) & 0xF
          Push(size,srcReg)
        }
        else {
          val srcLit = nextLit(vm,size)
          PushLit(size,srcLit)
        }

      case 5 =>
        val address = nextLit(vm,I32)
        Call(address)

      case 6 =>
        val sysCall = nextSysCall(vm)
        Trap(size,sysCall)

      case 7 =>
        val b1 = nextByte(vm)
        val lhs = (b1 >> 4) & 0xF
        if( isReg ){
          val rhs = b1 & 0xF
          Cmp(size,lhs,rhs)
        }
        else {
          val rhs = nextLit(vm,size)
          CmpLit(size,lhs,rhs)
        }

      case 8 =>
        val b1 = nextByte(vm)
        val dstReg = (b1 >> 4) & 0xF
        if( isReg ){
          val srcReg = b1 & 0xF
          Move(size,dstReg,srcReg)
        }
        else {
          val srcLit = nextLit(vm,size)
          MoveLit(size,dstReg,srcLit)
        }

      case 9 =>
        val jumpT = nextJumpType(vm)
        val address = nextLit(vm,size)
        Jump(size,jumpT,address)

      case 10 =>
        val b1 = nextByte(vm)
        val arithT = arithType((b1 >> 4) & 0xF)
        val dstReg = b1 & 0x0F
        if( isReg ){
          val srcReg = nextByte(vm)
          Arith(size,arithT,dstReg,srcReg)
        }
        else {
          val srcLit = nextLit(vm,size)
          ArithLit(size, arithT, dstReg, srcLit)
        }

      case 11 =>
        val b1 = nextByte(vm)
        val dstReg = (b1 >> 4) & 0xF
        val srcReg = b1 & 0x0F
        if( isReg ){
          val offsetReg = nextByte(vm).toInt
          Load(size,dstReg,srcReg,offsetReg)
        }
        else{
          val offsetLit = nextLit(vm,size)
          LoadLit(size,dstReg,srcReg,offsetLit)
        }

      case 12 =>
        val b1 = nextByte(vm)
        val dstReg = (b1 >> 4) & 0xF
        val srcReg = b1 & 0x0F
        if( isReg ){
          val offsetReg = nextByte(vm).toInt
          Store(size,dstReg,srcReg,offsetReg)
        }
        else{
          val offsetLit = nextLit(vm,size)
          StoreLit(size,dstReg,srcReg,offsetLit)
        }
    }
  }
}

object Decode {
  def main(args: Array[String]): Unit = {
    val sourceFile = "./VM/asm/sample.v"
    val vm = new VM(100)

    val decoder = new Decoder()
    val evaluator = new Eval(Map.empty)
    decoder.loadVFile(vm,sourceFile)

    var instruction: Instruction = Noop
    while( instruction != Halt ){
      instruction = decoder.decode(vm)
      evaluator.stepVM(vm,instruction)
    }

    println("---------REG-----------")
    for( i <- vm.regs ){
      print(s"$i, ")
    }
    println()

    println("---------MEM-----------")
    val numCells = 20
    var line = new ArrayBuffer[String]
    vm.mem.foreach{ byte =>
      line += byte.formatted("%02X ")
      if(line.length % numCells == 0){
        line.foreach{ print }
        println()
        line.clear()
      }
    }
  }
}