// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import org.chipsalliance.cde.config._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

/** Parameterization of the memory-side bus created for each memory channel */
case class MemoryControllerParams(
                            beatBytes: Int,
                            blockBytes: Int,
                            dtsFrequency: Option[BigInt] = None,
                            zeroDevice: Option[BuiltInZeroDeviceParams] = None,
                            errorDevice: Option[BuiltInErrorDeviceParams] = None,
                            replication: Option[ReplicatedRegion] = None)
  extends HasTLBusParams
    with HasBuiltInDeviceParams
    with HasRegionReplicatorParams
    with TLBusWrapperInstantiationLike
{
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): MemoryController = {
    val mcontroller = LazyModule(new MemoryController(this, loc.name))
    mcontroller.suggestName(loc.name)
    context.tlBusWrapperLocationMap += (loc -> mcontroller)
    mcontroller
  }
}

/** Wrapper for creating TL nodes from a bus connected to the back of each mem channel */
class MemoryController(params: MemoryControllerParams, name: String = "memory_controller")(implicit p: Parameters)
  extends TLBusWrapper(params, name)(p)
{
  private val replicator = params.replication.map(r => LazyModule(new RegionReplicator(r)))
  val prefixNode = replicator.map { r =>
    r.prefix := addressPrefixNexusNode
    addressPrefixNexusNode
  }

  private val xbar = LazyModule(new TLXbar).suggestName(busName + "_xbar")
  val inwardNode: TLInwardNode =
    replicator.map(xbar.node :*=* TLFIFOFixer(TLFIFOFixer.all) :*=* _.node)
      .getOrElse(xbar.node :*=* TLFIFOFixer(TLFIFOFixer.all))

  val outwardNode: TLOutwardNode = ProbePicker() :*= xbar.node
  def busView: TLEdge = xbar.node.edges.in.head

  val builtInDevices: BuiltInDevices = BuiltInDevices.attach(params, outwardNode)
}
