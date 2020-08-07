package vm

import scala.io.StdIn

class Eval(val labels: Map[String, Int]) {

  def runProgram(vm: VM, program: Array[Instruction]): Unit = {
    var continue = true
    while (continue) {
      val instruction = program(vm.pc)
      vm.pc += 1

      continue = instruction != Halt
      stepVM(vm, instruction)
    }
  }

  def getLitValue(literal: Literal): Long =
    literal match {
      case NumLit(value) =>
        value
      case LblLit(key) if labels.contains(key) =>
        labels(key)
      case LblLit(key) =>
        throw new Error(s"Unknown label: $key")
    }

  def stepVM(vm: VM, instruction: Instruction): Unit =
    instruction match {
      case Noop =>  case Halt =>  //do nothing

      case Ret => {
        vm.regs(VM.sp) -= 4
        val sp = vm.regs(VM.sp).toInt
        vm.pc = vm.read(sp,I32).toInt
      }

      case Pop(size, dstReg) => {
        vm.regs(VM.sp) -= size.bytes
        val sp = vm.regs(VM.sp).toInt
        vm.regs(dstReg) = vm.read(sp,size)
      }

      case Push(size, srcReg) => {
        val value = vm.regs(srcReg)
        val sp = vm.regs(VM.sp).toInt
        vm.write(sp,size,value)
        vm.regs(VM.sp) += size.bytes
      }

      case PushLit(size, literal) => {
        val srcLit = getLitValue(literal)
        val sp = vm.regs(VM.sp).toInt
        vm.write(sp, size, srcLit)
        vm.regs(VM.sp) += size.bytes
      }

      case Call(address) => {
        val sp = vm.regs(VM.sp).toInt
        vm.write(sp,I32,vm.pc)
        vm.regs(VM.sp) += 4
        vm.pc = getLitValue(address).toInt
      }

      case Trap(size,sysCall) =>
        sysCall match {
          case Print => print( vm.regs(0).toChar )
          case Read  => vm.regs(0) = StdIn.readLong()
        }

      case Cmp(size, lhsReg, rhsReg) => {
        val lhs = size.cast( vm.regs(lhsReg) )
        val rhs = size.cast( vm.regs(rhsReg) )
        vm.sign = Math.signum(lhs - rhs).toInt
      }

      case CmpLit(size, lhsReg, literal ) => {
        val rhsLit = getLitValue( literal )
        val lhs = size.cast( vm.regs(lhsReg) )
        val rhs = size.cast( rhsLit )
        vm.sign = Math.signum(lhs - rhs).toInt
      }

      case Move(size, dstReg, srcReg) => {
        vm.regs(dstReg) = size.cast( vm.regs(srcReg) )
      }

      case MoveLit(size, dstReg, literal ) => {
        val srcLit = getLitValue( literal )
        vm.regs(dstReg) = size.cast( srcLit )
      }

      case Jump(size, jumpT, address) => {
        if ( jumpT.shouldJump(vm) ) {
          vm.pc = getLitValue(address).toInt
        }
      }

      case Arith(size, arithT, dstReg, srcReg) => {
        val rhs = vm.regs(srcReg)
        val lhs = vm.regs(dstReg)
        val result = arithT.eval(lhs,rhs)
        vm.regs(dstReg) = size.cast(result)
      }

      case ArithLit(size, arithT, dstReg, srcLit) => {
        val lhs = vm.regs(dstReg)
        val result = arithT.eval(lhs,getLitValue(srcLit))
        vm.regs(dstReg) = size.cast(result)
      }

      case Load(size, dstReg, srcReg, offsetReg) => {
        val address = vm.regs(srcReg).toInt
        val offset = vm.regs(offsetReg).toInt
        val effectiveAddress = address + offset
        vm.regs(dstReg) = vm.read(effectiveAddress,size)
      }

      case LoadLit(size, dstReg, srcReg, offsetVal) => {
        val address = vm.regs(srcReg).toInt
        val effectiveAddress = address + getLitValue(offsetVal).toInt
        vm.regs(dstReg) = vm.read(effectiveAddress,size)
      }

      case Store(size, dstReg, srcReg, offsetReg) => {
        val address = vm.regs(dstReg).toInt
        val offset = vm.regs(offsetReg).toInt
        val effectiveAddress = address + offset
        val value = vm.regs(srcReg)
        vm.write(effectiveAddress,size,value)
      }

      case StoreLit(size, dstReg, srcReg, offsetVal) => {
        val address = vm.regs(dstReg).toInt
        val effectiveAddress = address + getLitValue(offsetVal).toInt
        val value = vm.regs(srcReg)
        vm.write(effectiveAddress,size,value)
      }
    }
}

object Eval {
  def main(args: Array[String]): Unit = {
    val vm = new VM(100)
    val eval = new Eval(Map(
      "function" -> 6,
      "loop" -> 2
    ))
    val program = Array[Instruction](
      MoveLit(I32,0,NumLit(0)),         // R0 = 0
      MoveLit(I32,1,NumLit(0)),         // R1 = 0
      CmpLit(I32,1,NumLit(10)),         // :loop_start
      Jump(I8,Jgt,LblLit("function")),  // exit if R1 > 10
      Call(NumLit(7)),
      Jump(I8,Jmp,LblLit("loop")),      // goto loop_start
      Halt,

      Push(I32,VM.fp),        // calling convection save fp, set new fp = sp
      Move(I32,VM.fp,VM.sp),

      MoveLit(I64,VM.rv,NumLit(-12)),
      Arith(I32,Add,0,1),     // R0 += R1
      ArithLit(I32,Add,1,NumLit(1)),  // R1 += 1

      Move(I32,VM.sp,VM.fp),  // reset calling convention to before call state
      Pop(I32,VM.fp),
      Ret
    )
    eval.runProgram(vm,program)
    vm.regs.foreach(println)
  }

  def pushPopMinMax(): Unit = {
    val vm = new VM(100)
    val eval = new Eval(Map.empty)
    val program = Array[Instruction](
      MoveLit(I8 ,0,NumLit(Byte.MaxValue)),
      MoveLit(I8 ,1,NumLit(Byte.MinValue)),
      MoveLit(I16,2,NumLit(Short.MaxValue)),
      MoveLit(I16,3,NumLit(Short.MinValue)),
      MoveLit(I32,4,NumLit(Int.MaxValue)),
      MoveLit(I32,5,NumLit(Int.MinValue)),
      MoveLit(I64,6,NumLit(Long.MaxValue)),
      MoveLit(I64,7,NumLit(Long.MinValue)),

      Push(I8 ,0),
      Push(I8 ,1),
      Push(I16,2),
      Push(I16,3),
      Push(I32,4),
      Push(I32,5),
      Push(I64,6),
      Push(I64,7),

      Pop(I64,0),
      Pop(I64,1),
      Pop(I32,2),
      Pop(I32,3),
      Pop(I16,4),
      Pop(I16,5),
      Pop(I8,6),
      Pop(I8,7),

      Halt
    )
    eval.runProgram(vm,program)
    vm.regs.foreach(println)
  }

}
