package mymini

import mini._

class MiniCoverageSpec extends TileTests(
  SimpleTests,
  annotations = Seq(mymini.Coverages.testerALUCoverageAspect, mymini.TileTesterALUAspects.instanced),
  params = Some((new MiniConfig).toInstance alter { (site, here, up) => {
    case CacheBlockBytes => up(CacheBlockBytes)
    case Trace => false
  }})
)
