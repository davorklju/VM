package vm

import java.io.PrintWriter
import java.io.File

import scala.util.Using

class Encoder(val sourceFile: String) {

  def writeToFile(code: List[Instruction], data: List[Data]): Unit = {
    Using(new PrintWriter(new File(sourceFile))){ writer =>
      val initSP = data.map{_.data.length}.sum
      writer.println(initSP)

      for( datum <- data ){
//        writer.print( "%02X" format size2Byte(datum.size) )
        for( byte <- datum.data ){
          writer.print("%02X" format byte)
        }
        writer.println()
      }

      for( instruction <- code ){
        for( byte <- encode(instruction) ){
          writer.print("%02X" format byte)
        }
        writer.println()
      }

    }
  }

  def size2Byte(size: Size): Byte = size match {
    case I8  => 0
    case I16 => 1
    case I32 => 2
    case I64 => 3
  }

  def writeBytes(size: Size,value: Long, idxOffset: Int, bytes: Array[Byte]): Unit = {
    val offset = 8*(8 - size.bytes)
    val sev = value << offset >> offset
    for( i <- size.range ){
      val byte = (sev >> 8*i) & 0xFF
      bytes(idxOffset + i) = byte.toByte
    }
  }

  def jump2Byte(jumpT: JumpType): Byte = jumpT match {
    case Jeq => 0
    case Jne => 1
    case Jlt => 2
    case Jgt => 3
    case Jle => 4
    case Jge => 5
    case Jmp => 6
  }

  def arith2Byte(arithT: ArithType): Byte = arithT match {
    case Add =>  0
    case Sub =>  1
    case Mul =>  2
    case Div =>  3
    case Mod =>  4
    case LS =>   5
    case RS =>   6
    case And =>  7
    case Or =>   8
    case XOr =>  9
    case Not => 10
    case Neg => 11
  }

  def encode(instruction: Instruction): Array[Byte] = {
    val bytes = new Array[Byte](instruction.bytes)
    instruction match {
      case Noop => bytes(0) = 0
      case Halt => bytes(0) = 1 << 4
      case Ret  => bytes(0) = 2 << 4

      case Pop(size, dstReg) =>
        bytes(0) = ((3 << 4) | (size2Byte(size) << 2)).toByte
        bytes(1) = dstReg.toByte

      case Push(size, srcReg) =>
        bytes(0) = ((4 << 4) | (size2Byte(size) << 2) | 0).toByte
        bytes(1) = (srcReg << 4).toByte

      case PushLit(size, NumLit(srcLit)) =>
        bytes(0) = ((4 << 4) | size2Byte(size) << 2 | 1).toByte
        writeBytes(size,srcLit,1,bytes)

      case Call(NumLit(address)) =>
        bytes(0) = (5 << 4).toByte
        writeBytes(I32,address,1,bytes)

      case Trap(size,sysCall) =>
        val s = sysCall match {
          case Print => 0
          case Read => 1
        }
        bytes(0) = ((6 << 4) | (size2Byte(size) << 2) ).toByte
        bytes(1) = s.toByte

      case Cmp(size, lhsReg, rhsReg) =>
        bytes(0) = ((7 << 4) | (size2Byte(size) << 2) | 0).toByte
        bytes(1) = ((lhsReg << 4) | rhsReg).toByte

      case CmpLit(size, lhsReg, NumLit(rhsLit)) =>
        bytes(0) = ((7 << 4) | (size2Byte(size) << 2) | 1).toByte
        bytes(1) = (lhsReg << 4).toByte
        writeBytes(size,rhsLit,2,bytes)

      case Move(size, dstReg, srcReg) =>
        bytes(0) = ((8 << 4) | (size2Byte(size) << 2) | 0).toByte
        bytes(1) = ((dstReg << 4) | srcReg).toByte

      case MoveLit(size, dstReg, NumLit(srcLit)) =>
        bytes(0) = ((8 << 4) | (size2Byte(size) << 2) | 1).toByte
        bytes(1) = (dstReg << 4).toByte
        writeBytes(size,srcLit,2,bytes)

      case Jump(size, jumpT, NumLit(address)) =>
        bytes(0) = ((9 << 4) | (size2Byte(size) << 2)).toByte
        bytes(1) = jump2Byte(jumpT)
        writeBytes(size,address,2,bytes)

      case Arith(size, arithT, dstReg, srcReg) =>
        bytes(0) = ((10 << 4) | (size2Byte(size) << 2) | 0).toByte
        bytes(1) = ((arith2Byte(arithT) << 4) | dstReg).toByte
        bytes(2) = srcReg.toByte

      case ArithLit(size, arithT, dstReg, NumLit(srcLit)) =>
        bytes(0) = ((10 << 4) | (size2Byte(size) << 2) | 1).toByte
        bytes(1) = ((arith2Byte(arithT) << 4) | dstReg).toByte
        writeBytes(size,srcLit,2,bytes)

      case Load(size, dstReg, srcReg, offsetReg) =>
        bytes(0) = ((11 << 4) | (size2Byte(size) << 2) | 0).toByte
        bytes(1) = ((dstReg << 4) | srcReg).toByte
        bytes(2) = offsetReg.toByte

      case LoadLit(size, dstReg, srcReg, NumLit(offsetLit)) =>
        bytes(0) = ((11 << 4) | (size2Byte(size) << 2) | 1).toByte
        bytes(1) = ((dstReg << 4) | srcReg).toByte
        writeBytes(size,offsetLit,2,bytes)

      case Store(size, dstReg, srcReg, offsetReg) =>
        bytes(0) = ((12 << 4) | (size2Byte(size) << 2) | 0).toByte
        bytes(1) = ((dstReg << 4) | srcReg).toByte
        bytes(2) = offsetReg.toByte

      case StoreLit(size, dstReg, srcReg, NumLit(offsetLit)) =>
        bytes(0) = ((12 << 4) | (size2Byte(size) << 2) | 1).toByte
        bytes(1) = ((dstReg << 4) | srcReg).toByte
        writeBytes(size,offsetLit,2,bytes)

      case other =>
        throw new Error(s"Could not encode: $other")
    }
    bytes
  }

}

object Encoder {
  def main(args: Array[String]): Unit = {
    val sourceFile = "./VM/asm/sample.asm"
    val destFile = "./VM/asm/sample.v"
    val parser = new Parser()
    val encoder = new Encoder(destFile)
    val ParseResult(code,_,data) = parser.parseFile(sourceFile)
    encoder.writeToFile(code,data)
  }
}
