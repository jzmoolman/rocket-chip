// See LICENSE.SiFive for license details.

package devices.tilelink

import chisel3._
import chisel3.util.log2Ceil
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.subsystem.{BaseSubsystem, HierarchicalLocation, HasTiles, TLBusWrapperLocation}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.prci.{ClockSinkDomain}
import freechips.rocketchip.util.DontTouch

import java.nio.ByteBuffer
import java.nio.file.{Files, Paths}

/** Size, location and contents of the boot rom. */
case class PUFParams(
                       address: BigInt = 0x20000,
                       size: Int = 0x010000)

class TLPUF(val base: BigInt, val size: Int, beatBytes: Int = 4,
             resources: Seq[Resource] = new SimpleDevice("puf", Seq("jzm,puf0")).reg("mem"))(implicit p: Parameters) extends LazyModule
{
  val node = TLManagerNode(Seq(TLSlavePortParameters.v1(
    Seq(TLSlaveParameters.v1(
      address     = List(AddressSet(base, size-1)),
      resources   = resources,
      regionType  = RegionType.UNCACHED,
      executable = true,
      supportsGet = TransferSizes(1, beatBytes),
      fifoId      = Some(0))),
    beatBytes = beatBytes)))

  println(s"beatBytes = $beatBytes")

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {

    val (in, edge) = node.in(0)

//    val w_data = Wire(UInt((8*beatBytes).W))
    val r_data = RegInit(0.U((8*beatBytes).W))
    val r_state = RegInit(0.U(1.W))
    val r_valid = RegInit(false.B)
    val r_source = RegInit(in.a.bits.source)

    in.a.ready := in.d.ready

//    dontTouch(w_data)

    when (r_state === 0.U && in.a.fire === true.B) {
      r_state := true.B
      r_data := 0xff.U
      r_valid := true.B
      r_source := in.a.bits.source
    } .otherwise {
        r_state := false.B
        r_data := 0.U
        r_valid := false.B
      }

    in.d.valid := r_valid
    in.d.bits := edge.AccessAck(in.a.bits, r_data)
    in.d.bits.source := r_source

    // Tie off unused channel
    in.b.valid := false.B
    in.c.ready := true.B
    in.e.ready := true.B
  }
}

case class PUFLocated(loc: HierarchicalLocation) extends Field[Option[PUFParams]](None)

object PUF {
  /** BootROM.attach not only instantiates a TLROM and attaches it to the tilelink interconnect
   *    at a configurable location, but also drives the tiles' reset vectors to point
   *    at its 'hang' address parameter value.
   */
  def attach(params: PUFParams, subsystem: BaseSubsystem with HasTiles, where: TLBusWrapperLocation)
            (implicit p: Parameters): TLPUF = {
    println("Puff attaching")
    val tlbus = subsystem.locateTLBusWrapper(where)
    val pufDomainWrapper = LazyModule(new ClockSinkDomain(take = None))
    pufDomainWrapper.clockNode := tlbus.fixedClockNode

    //    val puffResetVectorSourceNode = BundleBridgeSource[UInt]()
    val puf = pufDomainWrapper {
      LazyModule(new TLPUF(params.address, params.size, tlbus.beatBytes))
    }

    puf.node := tlbus.coupleTo("puf"){ TLFragmenter(tlbus) := _ }
    // Drive the `subsystem` reset vector to the `hang` address of this Boot ROM.
    //    subsystem.tileResetVectorNexusNode := puffResetVectorSourceNode
    //    InModuleBody {
    //      puffResetVectorSourceNode.bundle := 0.U
    //    }
    puf
  }
}
