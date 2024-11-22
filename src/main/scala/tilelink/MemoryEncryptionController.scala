
package freechips.rocketchip.tilelink

import chisel3._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

class TLMemoryEncryptionController()(implicit p: Parameters)
  extends LazyModule {

  val node: TLAdapterNode = TLAdapterNode(
    clientFn = { cp =>
      println(s"TLMemoryEncryptionController ClientF  $cp")
      cp
    },
    managerFn = { mp =>
      println(s"TLMemoryEncryptionController ManagerFn $mp")
      mp
    })

  lazy val module = new Impl

  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      println(s"out $out")
      println(s"in $in")
      out <> in
      when (in.a.bits.ee_a === true.B) {
        out.a.valid := false.B
      }
    }
  }
}

object TLMemoryEncryptionController
{
  def apply()(implicit p: Parameters): TLAdapterNode =
  {
    val mec2 = LazyModule(new TLMemoryEncryptionController())
    mec2.node
  }
  def apply(wrapper: TLBusWrapper)(implicit p: Parameters): TLAdapterNode = apply()
}