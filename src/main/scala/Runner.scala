import java.io.File

import vm._

import scala.collection.mutable.ArrayBuffer

object Runner {
  def main(args: Array[String]): Unit = {
    val fileName = "Rule110"
    val path = new File("./src/main/scala/asm/")
    val asmFile = new File(path,fileName + ".asm")
    val vFile   = new File(path,fileName + ".v")

    val ParseResult(code,_,data) = new Parser().parseFile(asmFile.getAbsolutePath)
    new Encoder(vFile.getAbsolutePath).writeToFile(code,data)

    val vm = new VM(500)

    val decoder = new Decoder()
    decoder.loadVFile(vm,vFile.getAbsolutePath)

    val evaluator = new Eval(Map.empty)
    var instruction: Instruction = Noop

    var iter = 0
    printVM(vm,50)
    while( instruction != Halt ){
      instruction = decoder.decode(vm)

//      println(iter)
//      println(instruction)
      iter += 1

      evaluator.stepVM(vm,instruction)
//      printVM(vm,50)
    }
    println("---------------------")
    println(iter)
    printVM(vm,50)
  }

  def printVM(vm: VM,numCells: Int = 20): Unit = {
    println(s"pc   = ${vm.pc}")
    println(s"sign = ${vm.sign}")
    println("---------REG-----------")
    for( i <- vm.regs ){
      print(s"$i, ")
    }
    println()

    println("---------MEM-----------")
    var line = new ArrayBuffer[String]
    vm.mem.foreach{ byte =>
      line += byte.formatted("%02X ")
      if(line.length % numCells == 0){
        line.foreach{ print }
        println()
        line.clear()
      }
    }
    if( !line.isEmpty ){
      line.foreach{ print }
      println()
    }

    println("--------------------------------------------------")
  }

}
