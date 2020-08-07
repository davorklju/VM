import org.scalatest.flatspec.AnyFlatSpec
import vm._

class EncoderTest extends AnyFlatSpec {
  val encoder = new Encoder("")

  "noop" should "work" in {
    val instr = Noop
    val arr = encoder.encode(instr)
    val expected = Array[Byte](0)
    assert(arr sameElements expected)
  }

  "halt" should "work" in {
    val instr = Halt
    val arr = encoder.encode(instr).toList
    val expected = List(0x10)
    assert(arr == expected)
  }

  "ret" should "work" in {
    val instr = Ret
    val arr = encoder.encode(instr).toList
    val expected = List(0x20)
    assert(arr == expected)
  }

  "pop" should "encode 8 bits" in {
    val instr = Pop(I8,3)
    val arr = encoder.encode(instr).toList
    val expected = List(0x30,0x3)
    assert(arr == expected)
  }

  it should "encode 32 bits" in {
    val instr = Pop(I32,3)
    val arr = encoder.encode(instr).toList
    val expected = List((0x3 << 4) | (0x2 << 2) ,0x3)
    assert(arr == expected)
  }

  "push" should "encode 8 bits with reg" in {
    val instr = Push(I8,3)
    val arr = encoder.encode(instr).toList
    val expected = List((0x4 << 4) | (0x0 << 2) ,0x30)
    assert(arr == expected)
  }

  it should "encode 32 bits with reg" in {
    val instr = Push(I32,3)
    val arr = encoder.encode(instr).toList
    val expected = List((0x4 << 4) | (0x2 << 2) ,0x30)
    assert(arr == expected)
  }

  it should "encode 8 bits with lit" in {
    val instr = PushLit(I8,NumLit(12))
    val arr = encoder.encode(instr).toList
    val expected = List((0x4 << 4) | (0x0 << 2) | 1,0xC)
    assert(arr == expected)
  }

  it should "encode 32 bits with lit" in {
    val instr = PushLit(I32,NumLit(12))
    val arr = encoder.encode(instr).toList
    val expected = List((0x4 << 4) | (0x2 << 2) | 1,0xC,0,0,0)
    assert(arr == expected)
  }

  "call" should "work" in {
    val instr = Call(NumLit(12))
    val arr = encoder.encode(instr).toList
    val expected = List(0x5 << 4,0xC,0,0,0)
    assert(arr == expected)
  }

  "trap" should "encoded 8 bits" in {
    val instr = Trap(I8,Print)
    val arr = encoder.encode(instr).toList
    val expected = List((0x6 << 4) | (0x0 << 2),0x0)
    assert(arr == expected)
  }

  it should "encode 32 bits" in {
    val instr = Trap(I32,Read)
    val arr = encoder.encode(instr).toList
    val expected = List((0x6 << 4) | (0x2 << 2),0x1)
    assert(arr == expected)
  }

  "cmp" should "encode 8 bits with reg" in {
    val instr = Cmp(I8,3,2)
    val arr = encoder.encode(instr).toList
    val expected = List((0x7 << 4) | (0x0 << 2), 0x32)
    assert(arr == expected)
  }

  it should "encode 32 bits with reg" in {
    val instr = Cmp(I32,3,2)
    val arr = encoder.encode(instr).toList
    val expected = List((0x7 << 4) | (0x2 << 2), 0x32)
    assert(arr == expected)
  }

  it should "encode 8 bits with lit" in {
    val instr = CmpLit(I8,3,NumLit(2))
    val arr = encoder.encode(instr).toList
    val expected = List((0x7 << 4) | (0x0 << 2) | 1, 0x30,0x2)
    assert(arr == expected)
  }

  it should "encode 32 bits with lit" in {
    val instr = CmpLit(I32,3,NumLit(2))
    val arr = encoder.encode(instr).toList
    val expected = List((0x7 << 4) | (0x2 << 2) | 1, 0x30,0x2,0,0,0)
    assert(arr == expected)
  }

  "move" should "encode 8 bits with reg" in {
    val instr = Move(I8,3,2)
    val arr = encoder.encode(instr).toList
    val expected = List((0x8 << 4) | (0x0 << 2),0x32).map(_.toByte)
    assert(arr == expected)
  }

  it should "encode 32 bits with reg" in {
    val instr = Move(I32,3,2)
    val arr = encoder.encode(instr).toList
    val expected = List((0x8 << 4) | (0x2 << 2),0x32).map(_.toByte)
    assert(arr == expected)
  }

  it should "encode 8 with lit" in {
    val instr = MoveLit(I8,3,NumLit(12))
    val arr = encoder.encode(instr).toList
    val expected = List((0x8 << 4) | (0x0 << 2) | 1,0x30,0xC).map(_.toByte)
    assert(arr == expected)
  }

  it should "encode 32 with lit" in {
    val instr = MoveLit(I32,3,NumLit(12))
    val arr = encoder.encode(instr).toList
    val expected = List((0x8 << 4) | (0x2 << 2) | 1,0x30,0xC,0,0,0).map(_.toByte)
    assert(arr == expected)
  }

  "jump" should "encode 8 bit with eq" in {
    val instr = Jump(I8,Jeq,NumLit(12))
    val arr = encoder.encode(instr).toList
    val expected = List((0x9 << 4) | (0x0 << 2),0x0,0xC).map(_.toByte)
    assert(arr == expected)
  }

  it should "encode 32 bit with lt" in {
    val instr = Jump(I32,Jlt,NumLit(12))
    val arr = encoder.encode(instr).toList
    val expected = List((0x9 << 4) | (0x2 << 2),0x2,0xC,0,0,0).map(_.toByte)
    assert(arr == expected)
  }

  "arith" should "encode 8 bits with reg" in {
    val instr = Arith(I8,Mul,3,2)
    val arr = encoder.encode(instr).toList
    val expected = List((0xA << 4) | (0x0 << 2),0x23,0x2).map(_.toByte)
    assert(arr == expected)
  }

  it should "encode 32 bits with reg" in {
    val instr = Arith(I32,Mul,3,2)
    val arr = encoder.encode(instr).toList
    val expected = List((0xA << 4) | (0x2 << 2),0x23,0x2).map(_.toByte)
    assert(arr == expected)
  }

  it should "encode 8 bits with lit" in {
    val instr = ArithLit(I8,Mul,3,NumLit(12))
    val arr = encoder.encode(instr).toList
    val expected = List((0xA << 4) | (0x0 << 2) | 1,0x23,0xC).map(_.toByte)
    assert(arr == expected)
  }

  it should "encode 32 bits with lit" in {
    val instr = ArithLit(I32,Mul,3,NumLit(12))
    val arr = encoder.encode(instr).toList
    val expected = List((0xA << 4) | (0x2 << 2) | 1,0x23,0xC,0,0,0).map(_.toByte)
    assert(arr == expected)
  }

  "load" should "encode 8 bits with reg" in {
    val instr = Load(I8,3,4,5)
    val arr = encoder.encode(instr).toList
    val expected = List((0xB << 4) | (0x0 << 2),0x34,0x5).map(_.toByte)
    assert(arr == expected)
  }

  it should "encode 32 bits with reg" in {
    val instr = Load(I32,3,4,5)
    val arr = encoder.encode(instr).toList
    val expected = List((0xB << 4) | (0x2 << 2),0x34,0x5).map(_.toByte)
    assert(arr == expected)
  }

  it should "encode 8 bit with lit" in {
    val instr = LoadLit(I8,3,4,NumLit(5))
    val arr = encoder.encode(instr).toList
    val expected = List((0xB << 4) | (0x0 << 2) | 1,0x34,0x5).map(_.toByte)
    assert(arr == expected)
  }

  it should "encode 32 bit with lit" in {
    val instr = LoadLit(I32,3,4,NumLit(5))
    val arr = encoder.encode(instr).toList
    val expected = List((0xB << 4) | (0x2 << 2) | 1,0x34,0x5,0,0,0).map(_.toByte)
    assert(arr == expected)
  }

  "store" should "encode 8 bits with reg" in {
    val instr = Store(I8,3,4,5)
    val arr = encoder.encode(instr).toList
    val expected = List((0xC << 4) | (0x0 << 2),0x34,0x5).map(_.toByte)
    assert(arr == expected)
  }

  it should "encode 32 bits with reg" in {
    val instr = Store(I32,3,4,5)
    val arr = encoder.encode(instr).toList
    val expected = List((0xC << 4) | (0x2 << 2),0x34,0x5).map(_.toByte)
    assert(arr == expected)
  }

  it should "encode 8 bit with lit" in {
    val instr = StoreLit(I8,3,4,NumLit(5))
    val arr = encoder.encode(instr).toList
    val expected = List((0xC << 4) | (0x0 << 2) | 1,0x34,0x5).map(_.toByte)
    assert(arr == expected)
  }

  it should "encode 32 bit with lit" in {
    val instr = StoreLit(I32,3,4,NumLit(5))
    val arr = encoder.encode(instr).toList
    val expected = List((0xC << 4) | (0x2 << 2) | 1,0x34,0x5,0,0,0).map(_.toByte)
    assert(arr == expected)
  }
}
