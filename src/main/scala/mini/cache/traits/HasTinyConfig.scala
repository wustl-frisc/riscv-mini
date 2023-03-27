package mini
package cache

trait HasTinyConfig extends Cache {
  lazy override val c = CacheConfig(
    nWays = 1,
    nSets = 1,
    blockBytes = 4 * (xlen / 8) // 4 * 32 bits = 16B
  )
}

