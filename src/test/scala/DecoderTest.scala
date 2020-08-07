import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import vm._

class DecoderTest extends AnyFlatSpec with BeforeAndAfter {
  val vm = new VM(10)
  val decoder = new Decoder()
  val encoder = new Encoder("")

  before { vm.pc = 0 }

  "noop" should "decode" in {
    vm.mem(0) = 0x0
    val instr = decoder.decode(vm)
    val expected = Noop
    assert(instr == expected)
  }

  "halt" should "decode" in {
    vm.mem(0) = 0x10
    val instr = decoder.decode(vm)
    val expected = Halt
    assert(instr == expected)
  }

  "ret" should "decode" in {
    vm.mem(0) = 0x20
    val instr = decoder.decode(vm)
    val expected = Ret
    assert(instr == expected)
  }

  "pop" should "decode with 8 bit" in {
    val expected = Pop(I8,3)
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode with 32 bit" in {
    val expected = Pop(I32,3)
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  "push" should "decode 8 bits with reg" in {
    val expected = Push(I8,3)
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 32 bits with reg" in {
    val expected = Push(I32,3)
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 8 bits with lit" in {
    val expected = PushLit(I8,NumLit(3))
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 32 bits with lit" in {
    val expected = PushLit(I32,NumLit(3))
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  "call" should "decode" in {
    val expected = Call(NumLit(23))
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  "trap" should "decode 8 bits with Print" in {
    val expected = Trap(I8,Print)
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 32 bits with Read" in {
    val expected = Trap(I32,Print)
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  "cmp" should "decode 8 bits with reg" in {
    val expected = Cmp(I8,3,2)
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 32 bits with reg" in {
    val expected = Cmp(I32,3,2)
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 8 bits with Lit" in {
    val expected = CmpLit(I8,3,NumLit(2))
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 32 bits with Lit" in {
    val expected = CmpLit(I32,3,NumLit(2))
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  "move" should "decode 8 bits with reg" in {
    val expected = Move(I8,3,5)
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 32 bits with reg" in {
    val expected = Move(I32,3,5)
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 8 bits with lit" in {
    val expected = MoveLit(I8,3,NumLit(21))
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 32 bits with lit" in {
    val expected = MoveLit(I32,3,NumLit(21))
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  "jump" should "decode 8 bits" in {
    val expected = Jump(I8,Jeq,NumLit(21))
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 32 bits" in {
    val expected = Jump(I32,Jeq,NumLit(21))
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  "arith" should "decode 8 bits with reg" in {
    val expected = Arith(I8,Mul,4,5)
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 32 bits with reg" in {
    val expected = Arith(I32,Mul,4,5)
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 8 bits with lit" in {
    val expected = ArithLit(I8,Mul,4,NumLit(5))
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 32 bits with lit" in {
    val expected = ArithLit(I32,Mul,4,NumLit(5))
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  "load" should "decode 8 bits with reg" in {
    val expected = Load(I8,4,5,6)
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 32 bits with reg" in {
    val expected = Load(I32,4,5,6)
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 8 bits with lit" in {
    val expected = LoadLit(I8,4,5,NumLit(6))
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 32 bits with lit" in {
    val expected = LoadLit(I32,4,5,NumLit(6))
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  "store" should "decode 8 bits with reg" in {
    val expected = Store(I8,4,5,6)
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 32 bits with reg" in {
    val expected = Store(I32,4,5,6)
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 8 bits with lit" in {
    val expected = StoreLit(I8,4,5,NumLit(6))
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  it should "decode 32 bits with lit" in {
    val expected = StoreLit(I32,4,5,NumLit(6))
    val arr = encoder.encode(expected)
    for( i <- arr.indices ){ vm.mem(i) = arr(i) }
    val instr = decoder.decode(vm)
    assert(instr == expected)
  }

  "decoder" should "support byte max and min" in {
    for( value <- List(Byte.MaxValue,Byte.MinValue) ){
      vm.pc = 0
      val expected = PushLit(I8,NumLit(value))
      val arr = encoder.encode(expected)
      for( i <- arr.indices ){ vm.mem(i) = arr(i) }
      val instr = decoder.decode(vm)
      assert(instr.asInstanceOf[PushLit].srcLit == NumLit(value))
    }
  }

  it should "support short max and min" in {
    for( value <- List(Short.MaxValue,Short.MinValue) ){
      vm.pc = 0
      val expected = PushLit(I16,NumLit(value))
      val arr = encoder.encode(expected)
      for( i <- arr.indices ){ vm.mem(i) = arr(i) }
      val instr = decoder.decode(vm)
      assert(instr.asInstanceOf[PushLit].srcLit == NumLit(value))
    }
  }

  it should "support int max and min" in {
    for( value <- List(Int.MaxValue,Int.MinValue) ){
      vm.pc = 0
      val expected = PushLit(I32,NumLit(value))
      val arr = encoder.encode(expected)
      for( i <- arr.indices ){ vm.mem(i) = arr(i) }
      val instr = decoder.decode(vm)
      assert(instr.asInstanceOf[PushLit].srcLit == NumLit(value))
    }
  }

  it should "support long max and min" in {
    for( value <- List(Long.MaxValue,Long.MinValue) ){
      vm.pc = 0
      val expected = PushLit(I64,NumLit(value))
      val arr = encoder.encode(expected)
      for( i <- arr.indices ){ vm.mem(i) = arr(i) }
      val instr = decoder.decode(vm)
      assert(instr.asInstanceOf[PushLit].srcLit == NumLit(value))
    }
  }
}
