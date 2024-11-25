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
    val contents : Seq[Byte] = Seq(0,1,2,3,4,6,7,0,1,2,3,4,6,7 )
    val wrapSize = 1 << log2Ceil(contents.size)

    val (in, edge) = node.in(0)

    val words = (contents ++ Seq.fill(wrapSize-contents.size)(0.toByte)).grouped(beatBytes).toSeq
    val bigs = words.map(_.foldRight(BigInt(0)){ case (x,y) => (x.toInt & 0xff) | y << 8})
    val rom = VecInit(bigs.map(_.U((8*beatBytes).W)))

    in.d.valid := in.a.valid
    in.a.ready := in.d.ready


    val index = in.a.bits.address(log2Ceil(wrapSize)-1,log2Ceil(beatBytes))
    val high = if (wrapSize == size) 0.U else in.a.bits.address(log2Ceil(size)-1, log2Ceil(wrapSize))
//    in.d.bits := edge.AccessAck(in.a.bits, Mux(high.orR, 0.U, rom(index)))

//    in.d.bits := edge.AccessAck(in.a.bits,rom(index))
    val w = Wire(UInt((8*beatBytes).W))
    dontTouch(w)
    w := 255.U
    in.d.bits := edge.AccessAck(in.a.bits,w)

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
