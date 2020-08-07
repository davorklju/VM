package vm

object VM {
  val sp = 15
  val fp = 14
  val rv = 13

  def main(args: Array[String]): Unit = {
    println("lkasjdlkas")
  }
}

class VM(memSize: Int) {
  var pc = 0
  var sign = 0

  val mem  = new Array[Byte](memSize)
  val regs = new Array[Long](16)

  def write(at: Int, size: Size, value: Long): Unit = {
    val offset = 8*(8 - size.bytes)
    var sev = value << offset >> offset
    for( i <- size.range ){
      val byte = (sev >> 8*i) & 0xFF
      mem(at + i) = byte.toByte
    }
  }

  def read(at: Int, size: Size): Long = {
    var out: Long = 0
    for( i <- size.range ){
      val byte = (mem(at + i) & 0xFF).toLong
      out = out | (byte << 8*i)
    }
    size.cast(out)
  }
}
