package aoplib.histogram

import chisel3.aop.injecting.InjectingTransform
import chisel3.aop.{AdditionalTransforms, Concern, ConcernTransform}
import chisel3.experimental.RawModule
import firrtl.Transform
import firrtl.passes.wiring.WiringTransform

import scala.reflect.runtime.universe.TypeTag

/** Contains all histogram aspects for a given design-under-test
  *
  * @param tag Needed to prevent type erasure of the design's type
  * @tparam T Type of the design-under-test
  * @tparam R Type of Histogram Aspect
  */
abstract class HistogramConcern[T <: RawModule, R <: HistogramAspect[T, _]](implicit tag: TypeTag[T]) extends Concern[T, R] {
  def aspects: Seq[R]
  override def additionalTransformClasses: Seq[Class[_ <: Transform]] = Seq(classOf[InjectingTransform], classOf[WiringTransform])
}
