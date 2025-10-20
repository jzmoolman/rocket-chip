package freechips.rocketchip.tilelink

import chisel3._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.RegField
import org.chipsalliance.cde.config._

object MemoryControllerWrapperParams {
  val address: BigInt = 0x2010000
}

class MemoryControllerWrapper(beatBytes: Int)(implicit p: Parameters)
  extends LazyModule {

  val device =  new SimpleDevice("memory-controller", Seq("jzm,mc0", "mc"))

  val node: TLAdapterNode = TLAdapterNode(
    clientFn = { c => 
      c.copy(
      clients = c.clients.map { client =>
        client.copy(
          sourceId = IdRange(0, 1 << 10)  // force 10-bit source for each client
        )
      }
      )
     }
  )



  val ctlnode = TLRegisterNode(
    address     = Seq(AddressSet(MemoryControllerWrapperParams.address, 0xfff)),
    device      = device,
    beatBytes   = beatBytes)

  lazy val module = new Impl

  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      println("=======>")
      println(s"out $out")
      println(s"in $in")
      out <> in
      // when (in.a.bits.ee_a === true.B) {
      //  out.a.valid := false.B
      //}
    }
    val (in,edge) = ctlnode.in(0)

//    in.d.valid := in.a.valid
//    in.a.ready := in.d.ready

    val counter = RegInit(0.U(64.W))

    def readCounter(ready: Bool): (Bool, UInt) = {
      when (ready) { counter := counter - 1.U }
      // (ready, bits)
      (true.B, counter)
    }

    def writeCounter(valid: Bool, bits: UInt): Bool = {
      when (valid) { counter := counter + 1.U }
      // Ignore bits
      // Return ready
      true.B
    }
    dontTouch(in.a.bits.data)

    ctlnode.regmap(
      0x00 -> Seq(RegField.r(64, readCounter(_))),
      0x08 -> Seq(RegField.w(64, writeCounter(_, _)))
    )
  }
}

object MemoryControllerWrapper
{
  def apply(cbus: TLBusWrapper )(implicit p: Parameters): TLAdapterNode = {
    val mc = LazyModule(new MemoryControllerWrapper(cbus.beatBytes))
    mc.ctlnode := cbus.coupleTo("mc_ctl_port") { TLBuffer(1) := TLFragmenter(cbus) := _}
    mc.node
  }
}
