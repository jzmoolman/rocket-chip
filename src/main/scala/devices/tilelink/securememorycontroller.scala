package freechips.rocketchip.devices.tilelink

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.subsystem.{BaseSubsystem, HierarchicalLocation, HasTiles, TLBusWrapperLocation}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

case class SecureMemoryControllerParams(
  address: BigInt = 0x2020000,
  size: Int = 0x10000,
)

class SecureMemoryController(
  val base: BigInt, val size: Int, beatBytes: Int)(implicit p: Parameters)
  extends LazyModule {
    println(s"==> SecureMemoryController")
    println(s"==> beatBytes $beatBytes")


  val resources: Seq[Resource] = new SimpleDevice("secure-memory-controller", Seq("jzm,smc0")).reg("mem")
  val configNode = TLManagerNode(Seq(TLSlavePortParameters.v1(
    Seq(TLSlaveParameters.v1(
      address     = List(AddressSet(base, size-1)),
      resources   = resources,
      regionType  = RegionType.UNCACHED,
      executable  = false,
      supportsGet  = TransferSizes(1, beatBytes),
      supportsPutPartial = TransferSizes(1, beatBytes),
      supportsPutFull    = TransferSizes(1, beatBytes),
      fifoId      = Some(0))),
    beatBytes = beatBytes)))

    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) {

    def bigBits(x: BigInt, tail: List[Boolean] = List.empty[Boolean]): List[Boolean] =
      if (x == 0) tail.reverse else bigBits(x >> 1, ((x & 1) == 1) :: tail)

    val mask = bigBits(size-1 >> log2Ceil(beatBytes))

    val (in, edge) = configNode.in(0)

    val addrBits = (mask zip edge.addr_hi(in.a.bits).asBools).filter(_._1).map(_._2)
    val memAddress = Cat(addrBits.reverse)

    val mem = Mem(1 << addrBits.size, Vec(beatBytes, Bits(8.W)))
    val bad = Mem(1 << addrBits.size, Bool())

    in.d.valid := in.a.valid
    in.a.ready := in.d.ready

    val hasData = edge.hasData(in.a.bits)
    val wdata = VecInit(Seq.tabulate(beatBytes) { i => in.a.bits.data(8*(i+1)-1, 8*i)})

    in.d.bits := edge.AccessAck(in.a.bits)
    in.d.bits.data := Cat(mem(memAddress).reverse)
    //in.d.corrupt := !hasdata && bas(memAddress) && tra false.B
    in.d.bits.corrupt := false.B
    in.d.bits.opcode := Mux(hasData, TLMessages.AccessAck, TLMessages.AccessAckData)

    when ( in.a.fire && hasData) {
      mem.write(memAddress, wdata, in.a.bits.mask.asBools);
      bad.write(memAddress, in.a.bits.corrupt);

    }
    
    in.b.valid := false.B
    in.c.ready := true.B
    in.e.ready := true.B
  }
}

case class SecureMemoryControllerLocated(loc: HierarchicalLocation) extends Field[Option[SecureMemoryControllerParams]](None)


object SecureMemoryController
{
  def attach(params: SecureMemoryControllerParams, subsystem: BaseSubsystem with HasTiles, where: TLBusWrapperLocation) 
       (implicit p: Parameters): SecureMemoryController = {
   
    println("Print attaching SecureMemoryController")
    val tlbus = subsystem.locateTLBusWrapper(where)
    val smc = LazyModule(new SecureMemoryController(params.address, params.size, tlbus.beatBytes))
    smc.configNode := tlbus.coupleTo("smc_port") { TLFragmenter(tlbus) := _ }
    smc
  }
}
