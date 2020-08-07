package vm

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Using

sealed trait Phase
object NonePhase extends Phase {
  override def toString: String = "Phase-None"
}
object TextPhase extends Phase {
  override def toString: String = "Phase-Text"
}
object DataPhase extends Phase {
  override def toString(): String = "Phase-Data"
}

case class ParseResult(
    code  : List[Instruction],
    labels: Map[String,LabelData],
    data  : List[Data]
)

case class Data(size: Size,index: Int, data: Array[Byte]){
  override def toString: String =
    s"Data($size, $index, [${data.mkString(",")}])"
}

case class LabelData(phase: Phase, index: Int)

class Parser {
  var sourceIdx = 0
  var lineCount = 0
  var phase: Phase = NonePhase

  val labels = new mutable.HashMap[String,LabelData]
  val code = new mutable.ArrayBuffer[Instruction]()
  val data = new mutable.ArrayBuffer[Data]()

  def setPhase(line: String): Unit =
    if( line.startsWith(".text") ) {
      sourceIdx = 0
      phase = TextPhase
    }
    else if( line.startsWith(".data") ){
      sourceIdx = 0
      phase = DataPhase
    }
    else
      throw new Error(s"Unknown phase ${line.takeWhile(!_.isWhitespace)} at $lineCount")

  def appendLabelAndContinue(line: String): Unit = {
    var idx = 1
    while( idx < line.length && !line(idx).isWhitespace )
      idx += 1
    val label = line.substring(1,idx)
    val lineRest = line.substring(idx).trim
    labels += (label -> LabelData(phase,sourceIdx))
    parseLine(lineRest)
  }

  def parseReg(str: String): Int =
    str match {
      case "rv" => VM.rv
      case "sp" => VM.sp
      case "fp" => VM.fp
      case _ if str.toLowerCase.startsWith("r") =>
        str.toLowerCase.substring(1).toInt
      case _ => throw new Error(s"Unknown register $str at $lineCount")
    }

  def isReg(str: String): Boolean =
    List("rv","sp","fp").contains(str) || str.toLowerCase.startsWith("r")

  def parseLit(str: String): Literal =
    if( str.startsWith("0x") &&
          str.substring(2).forall{ "0123456789ABCDEF".contains(_) } ){
      NumLit( java.lang.Long.parseUnsignedLong(str.substring(2),16) )
    }
    else if( str.forall(_.isDigit) )
      NumLit( str.toLong )
    else
      LblLit( str )

  def isJump(str: String): Boolean =
    List("jmp","jeq","jne","jlt","jgt","jle","jge") contains str

  def isArith(str: String): Boolean =
    List("add","sub","mul","div","mod"
      ,"ls","rs","and","or","xor","not","neg") contains str

  def parseArithT(str: String): ArithType = str match {
    case "add" => Add
    case "sub" => Sub
    case "mul" => Mul
    case "div" => Div
    case "mod" => Mod
    case "ls" => LS
    case "rs" => RS
    case "and" => And
    case "or" => Or
    case "xor" => XOr
    case "not" => Not
    case "neg" => Neg
    case _ => throw new Error(s"Unknown arith type: $str at $lineCount")
  }

  def parseJumpT(str: String): JumpType = str match {
    case "jmp" => Jmp
    case "jeq" => Jeq
    case "jne" => Jne
    case "jlt" => Jlt
    case "jgt" => Jgt
    case "jle" => Jle
    case "jge" => Jge
  }

  def parseInstruction(line: String): Unit = {
    val parts = line.split(" ")
    val instrParts = parts(0).split("\\.")
    val size =
      if(instrParts.length == 1) I32
      else instrParts(1) match{
        case "8"  => I8
        case "16" => I16
        case "32" => I32
        case "64" => I64
        case _    => throw new Error(s"Unexpected size: ${instrParts(1)} at $lineCount")
      }
    val instruction = instrParts(0) match {
      case "noop" => Noop
      case "halt" => Halt
      case "ret"  => Ret

      case "pop"  =>
        val reg = parseReg( parts(1) )
        Pop(size,reg)

      case "push" =>
        if( isReg( parts(1) ) ){
          val reg = parseReg( parts(1) )
          Push(size,reg)
        }
        else{
          val lit = parseLit( parts(1) )
          PushLit(size,lit)
        }

      case "call" =>
        val lit = parseLit( parts(1) )
        Call(lit)

      case "trap" =>
        val sysCall = parts(1) match {
          case "print" => Print
          case "read" => Read
        }
        Trap(size,sysCall)

      case "cmp" =>
        val lhs = parseReg( parts(1) )
        if( isReg(parts(2)) ){
          val rhs = parseReg( parts(2) )
          Cmp(size,lhs,rhs)
        }
        else {
          val rhs = parseLit( parts(2) )
          CmpLit(size,lhs,rhs)
        }

      case "move" =>
        val dstReg = parseReg( parts(1) )
        if( isReg(parts(2)) ){
          val srcReg = parseReg( parts(2) )
          Move(size,dstReg,srcReg)
        }
        else {
          val srcLit = parseLit( parts(2) )
          MoveLit(size,dstReg,srcLit)
        }

      case _ if isJump( instrParts(0) ) =>
        val jumpT = parseJumpT( instrParts(0) )
        val address = parseLit( parts(1) )
        Jump(size,jumpT,address)

      case _ if isArith( instrParts(0) ) =>
        val arithT = parseArithT( instrParts(0) )
        val lhs = parseReg(parts(1))
        if( arithT == Not || arithT == Neg ){
          Arith(size,arithT,lhs,0)
        }
        else if( isReg( parts(2) ) ){
          val rhs = parseReg(parts(2))
          Arith(size,arithT,lhs,rhs)
        }
        else {
          val rhs = parseLit(parts(2))
          ArithLit(size,arithT,lhs,rhs)
        }

      case "load" =>
        val dstReg = parseReg( parts(1) )
        val srcReg = parseReg( parts(2) )
        if( isReg(parts(3)) ){
          val offsetReg = parseReg( parts(3) )
          Load(size,dstReg,srcReg,offsetReg)
        }
        else {
          val offsetLit = parseLit( parts(3) )
          LoadLit(size,dstReg,srcReg,offsetLit)
        }

      case "store" =>
        val dstReg = parseReg( parts(1) )
        val srcReg = parseReg( parts(2) )
        if( isReg(parts(3)) ){
          val offsetReg = parseReg(parts(3))
          Store(size,dstReg,srcReg,offsetReg)
        }
        else {
          val offsetLit = parseLit(parts(3))
          StoreLit(size,dstReg,srcReg,offsetLit)
        }

      case _ => throw new Error(s"Unknown instruction: ${instrParts(0)} at $lineCount")
    }
    sourceIdx += instruction.bytes
    code += instruction
  }

  def parseData(line: String): Unit = {
    val parts = line.split(" ")
    val dataSize = parts.length - 1

    val size = parts(0) match {
      case "i8"  => I8
      case "i16" => I16
      case "i32" => I32
      case "i64" => I64
      case _     => throw new Error(s"Unkown data type: ${parts(0)} at $lineCount")
    }
    val offset = 8*(8 - size.bytes)

    val bytes = new Array[Byte](size.bytes * dataSize)
    var idx = 0

    for( dataTxt <- parts.slice(1, dataSize+1) ){
      val value = dataTxt.toLong << offset >> offset
      for( i <- size.range ){
        val byte = (value >> 8*i) & 0xFF
        bytes(idx+i) = byte.toByte
      }
      idx += size.bytes
    }
    data += Data(size,sourceIdx,bytes)
    sourceIdx += bytes.length
  }

  def parseLine(line: String): Unit =
    if( line.isEmpty || line.startsWith("//") )
      {/*do nothing*/}
    else if( line.startsWith(".") )
      setPhase(line)
    else if( line.startsWith(":") )
      appendLabelAndContinue(line)
    else if( phase == TextPhase )
      parseInstruction(line)
    else if( phase == DataPhase )
      parseData(line)
    else
      throw new Error(s"Phase not set, must be either .data or .text at $lineCount")

  def subLabels(initSP: Int)(literal: Literal): Literal = literal match {
    case NumLit(_) =>
      literal

    case LblLit(lbl) if labels.contains(lbl) =>
      val LabelData(phase,index) = labels(lbl)
      val value = index + (if(phase == TextPhase) initSP else 0)
      NumLit(value)

    case LblLit(value) =>
      throw new Error(s"Uknown label: $value at $lineCount")
  }

  def parseFile(sourceFile: String): ParseResult = {
    phase = NonePhase
    labels.clear()
    code.clear()
    data.clear()

    val source = Source.fromFile(sourceFile)
    source.getLines().foreach{ rawLine =>
      lineCount += 1
      val line = rawLine
        .replaceAll("[ \\t]+"," ")
        .trim
      parseLine(line)
    }
    source.close()

    val initSP: Int = data.map{_.data.length}.sum
    val fromLabel = subLabels(initSP) _

    val lblFreeCode = code.map{
        case PushLit(size, srcLit) => PushLit(size,fromLabel(srcLit))
        case Call(address) => Call(fromLabel(address))
        case CmpLit(size, lhsReg, rhsLit) =>
          CmpLit(size,lhsReg,fromLabel(rhsLit))
        case MoveLit(size, dstReg, srcLit) =>
          MoveLit(size,dstReg,fromLabel(srcLit))
        case Jump(size, jumpT, address) => Jump(size,jumpT,fromLabel(address))
        case ArithLit(size, arithT, dstReg, srcLit) =>
          ArithLit(size,arithT,dstReg,fromLabel(srcLit))
        case LoadLit(size, dstReg, srcReg, offsetVal) =>
          LoadLit(size,dstReg,srcReg,fromLabel(offsetVal))
        case StoreLit(size, dstReg, srcReg, offsetVal) =>
          StoreLit(size,dstReg,srcReg,fromLabel(offsetVal))
        case instruction => instruction
    }
    ParseResult(
      lblFreeCode.toList,
      labels.toMap,
      data.toList
    )
  }
}


object Parser {
  def main(args: Array[String]): Unit = {
    val sourceFile = "./VM/asm/sample.asm"
    val parser = new Parser()
    val ParseResult(code,labels,data) =
      parser.parseFile(sourceFile)
    println("--------------------------------------")
    println("code")
    code.foreach(println)
    println("--------------------------------------")
    println("labels")
    labels.foreach{ case (k,v) => println(s"$k => $v") }
    println("--------------------------------------")
    println("data")
    data.foreach(println)
  }
}
