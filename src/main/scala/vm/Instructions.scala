package vm

import scala.collection.mutable.ArrayBuffer

object Main {
  def pushInt(data: ArrayBuffer[Byte], int: Int): Unit = {
    data += (int & 0xFF).toByte
    data += ((int >> 8) & 0xFF).toByte
    data += ((int >> 8*2) & 0xFF).toByte
    data += ((int >> 8*3) & 0xFF).toByte
  }
  def pushShort(data: ArrayBuffer[Byte], int: Short): Unit = {
    data += (int & 0xFF).toByte
    data += ((int >> 8) & 0xFF).toByte
  }

  def popInt(data: ArrayBuffer[Byte]): Int = {
    val i = data.size
    val b3 = data.remove(i-1) & 0xFF
    val b2 = data.remove(i-2) & 0xFF
    val b1 = data.remove(i-3) & 0xFF
    val b0 = data.remove(i-4) & 0xFF
    b0 | (b1 << 8) | (b2 << 16) | (b3 << 24)
  }
  def popShort(data: ArrayBuffer[Byte]): Short = {
    val i = data.size
    val b1 = data.remove(i-1) & 0xFF
    val b0 = data.remove(i-2) & 0xFF
    (b0 | (b1 << 8)).asInstanceOf[Short]
  }

  def addInt(data: ArrayBuffer[Byte]): Int = {
    val x = popInt(data)
    val y = popInt(data)
    pushInt(data,x+y)
    x+y
  }
  def addShort(data: ArrayBuffer[Byte]): Short = {
    val x = popShort(data)
    val y = popShort(data)
    val z = (x + y).asInstanceOf[Short]
    pushShort(data,z)
    z
  }

  def main(args: Array[String]): Unit = {
    val data = ArrayBuffer[Byte]()

    pushInt(data,-1)
    pushInt(data,-2)
    addInt(data)
    val z = popInt(data)
    println(s"$z")
  }
}

/////////////////////////////////////////////////////////////////
sealed trait Size{
  val bytes: Int
  def cast(value: Long): Long

  def range: Seq[Int] = 0 until bytes
}
object I8 extends Size {
  override val bytes: Int = 1
  override def cast(value: Long): Long = value.toByte.toLong

  override def toString: String = "I8"
}
object I16 extends Size {
  override val bytes = 2
  override def cast(value: Long): Long = value.toShort.toLong
  override def toString: String = "I16"
}
object I32 extends Size {
  override val bytes = 4
  override def cast(value: Long): Long = value.toInt.toLong
  override def toString: String = "I32"
}
object I64 extends Size {
  override val bytes = 8
  override def cast(value: Long): Long = value
  override def toString: String = "I64"
}
/////////////////////////////////////////////////////////////////
sealed trait SysCall
object Print extends SysCall
object Read extends SysCall
/////////////////////////////////////////////////////////////////
sealed trait JumpType {
  def shouldJump(vm: VM): Boolean
}
object Jeq extends JumpType {
  override def shouldJump(vm: VM): Boolean =
    vm.sign == 0
}
object Jne extends JumpType {
  override def shouldJump(vm: VM): Boolean =
    vm.sign != 0
}
object Jlt extends JumpType {
  override def shouldJump(vm: VM): Boolean =
    vm.sign < 0
}
object Jgt extends JumpType {
  override def shouldJump(vm: VM): Boolean =
    vm.sign > 0
}
object Jle extends JumpType {
  override def shouldJump(vm: VM): Boolean =
    vm.sign <= 0
}
object Jge extends JumpType {
  override def shouldJump(vm: VM): Boolean =
    vm.sign >= 0
}
object Jmp extends JumpType {
  override def shouldJump(vm: VM): Boolean =
    true
}
/////////////////////////////////////////////////////////////////
sealed trait ArithType{
  def eval(lhs: Long, rhs: Long): Long
}
object Add extends ArithType {
  override def eval(lhs: Long, rhs: Long): Long =
    lhs + rhs
}
object Sub extends ArithType {
  override def eval(lhs: Long, rhs: Long): Long =
    lhs - rhs
}
object Mul extends ArithType {
  override def eval(lhs: Long, rhs: Long): Long =
    lhs * rhs
}
object Div extends ArithType {
  override def eval(lhs: Long, rhs: Long): Long =
    lhs / rhs
}
object Mod extends ArithType {
  override def eval(lhs: Long, rhs: Long): Long =
    lhs % rhs
}
object LS extends ArithType {
  override def eval(lhs: Long, rhs: Long): Long =
    lhs << rhs
}
object RS extends ArithType {
  override def eval(lhs: Long, rhs: Long): Long =
    lhs >> rhs
}
object And extends ArithType {
  override def eval(lhs: Long, rhs: Long): Long =
    lhs & rhs
}
object Or extends ArithType {
  override def eval(lhs: Long, rhs: Long): Long =
    lhs | rhs
}
object XOr extends ArithType {
  override def eval(lhs: Long, rhs: Long): Long =
    lhs ^ rhs
}
object Not extends ArithType {
  override def eval(lhs: Long, rhs: Long): Long =
    ~lhs
}
object Neg extends ArithType {
  override def eval(lhs: Long, rhs: Long): Long =
    -lhs
}
/////////////////////////////////////////////////////////////////
sealed trait Literal
case class NumLit(value: Long) extends Literal
case class LblLit(value: String) extends Literal
/////////////////////////////////////////////////////////////////
sealed trait Instruction {
  def bytes: Int
}

object Noop extends Instruction {
  override def bytes: Int = 1
}
object Halt extends Instruction {
  override def bytes: Int = 1
}
object Ret  extends Instruction {
  override def bytes: Int = 1
}

case class Pop(size: Size, dstReg: Int) extends Instruction {
  override def bytes: Int = 2
}

case class Push(size: Size, srcReg:Int) extends Instruction {
  override def bytes: Int = 2
}
case class PushLit(size: Size, srcLit: Literal) extends Instruction {
  override def bytes: Int = 1 + size.bytes
}

case class Call(address: Literal) extends Instruction {
  override def bytes: Int = 5
}

case class Trap(size: Size,sysCall: SysCall) extends Instruction {
  override def bytes: Int = 2
}

case class Cmp(size: Size, lhsReg: Int, rhsReg:Int) extends Instruction {
  override def bytes: Int = 2
}
case class CmpLit(size: Size, lhsReg: Int, rhsLit: Literal) extends Instruction {
  override def bytes: Int = 2 + size.bytes
}

case class Move(size: Size, dstReg: Int, srcReg: Int) extends Instruction {
  override def bytes: Int = 2
}
case class MoveLit(size: Size, dstReg: Int, srcLit: Literal) extends Instruction  {
  override def bytes: Int = 2 + size.bytes
}

case class Jump(size: Size,jumpT: JumpType, address: Literal) extends Instruction {
  override def bytes: Int = 2 + size.bytes
}

case class Arith(size: Size, arithT: ArithType, dstReg: Int, srcReg:Int) extends Instruction {
  override def bytes: Int = 3
}
case class ArithLit( size: Size, arithT: ArithType, dstReg: Int, srcLit: Literal) extends Instruction {
  override def bytes: Int = 2 + size.bytes
}

case class Load(size: Size, dstReg: Int, srcReg: Int, offsetReg: Int) extends Instruction {
  override def bytes: Int = 3
}
case class LoadLit(size: Size, dstReg: Int, srcReg: Int, offsetVal: Literal) extends Instruction {
  override def bytes: Int = 2 + size.bytes
}

case class Store(size: Size, dstReg: Int, srcReg: Int, offsetReg: Int) extends Instruction {
  override def bytes: Int = 3
}
case class StoreLit(size: Size, dstReg: Int, srcReg: Int, offsetVal: Literal) extends Instruction {
  override def bytes: Int = 2 + size.bytes
}


/////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////
/*
 *  8 bit word
 *  64 bit registers
 * /////////
 *  default size is 32
 * /////////
 *  all lits baed on size
 *    ex 1) move.8  R0  12  // 12 is an 8-bit value
 *    ex 2) push.32     12  // 12 is a 32-bit value
 *    ex 3) add     R0  12  // 12 is a 32-bit value
 * /////////
 *  bytes = table value assumes type is reg
 *        | except for call with counts +4 byte lbl
 *        | all other could be larger based on size
 *        | if type is lit then +1 for i8
 *        |                     +2 for i16
 *        |                     +4 for i32
 *        |                     +8 for i64
 *
 *  note: j** size cannot be 64, so the max bytes is 6 not 10
 *  note: load and store lit can be at most i16, so the max bytes is 5 not 11
 *
 *  type = 1 bit flag to test if second argument is a literal or regester
 *
 *  args  | name  | bytes | op-code | pattern
 *  ------|-------|-------|---------+----------
 *  0     | noop  | 1     | 0000    |
 *        | halt  | 1     | 0001    |
 *        | ret   | 1     | 0010    |
 *  ------+-------+-------+---------+------------
 *  1     | pop   | 2     | 0011    | [2-size] [4-reg]
 *        | push  | 2     | 0100    | [2-size] [1-type] [4-reg/lit]
 *        | call  | 5     | 0101    | [lbl]
 *        | trap  | 2     | 0110    | [8-tag]
 *  ------+-------+-------+---------+------------
 *  2     | cmp   | 2     | 0111    | [2-size] [1-type] [4-reg] [4-reg/lit]
 *        | move  | 2     | 1000    | [2-size] [1-type] [4-reg] [4-reg/lit]
 *        | j**   | 3     | 1001    | [2-size] [4-tag]  [lit]
 *        | arith | 3     | 1010    | [2-size] [1-type] [4-tag] [4-reg] [4-reg/lit]
 *  ------+------+--------+---------+------------
 *  3     | load  | 3     | 1011    | [2-size] [1-type] [4-reg] [4-reg] [4-reg/lit]
 *        | store | 3     | 1100    | [2-size] [1-type] [4-reg] [4-reg] [4-reg/lit]
 *
 * */
