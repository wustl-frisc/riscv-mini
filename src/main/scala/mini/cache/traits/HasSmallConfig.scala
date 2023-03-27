package mini
package cache

trait HasSmallConfig extends Cache {
  lazy override val c = CacheConfig(
    nWays = 1,
    nSets = 256,
    blockBytes = 4 * (xlen / 8) // 4 * 32 bits = 16B
  )
}
