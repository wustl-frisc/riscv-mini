package aoplib

import chisel3.Data
import chisel3.aop.Aspect
import chisel3.experimental.RawModule
import firrtl.annotations.{Annotation, ReferenceTarget}
import firrtl.{AnnotationSeq, CircuitForm, CircuitState, LowForm, RenameMap, Transform}

import scala.reflect.runtime.universe.TypeTag

//abstract class HistogramAspect[T <: RawModule](implicit tag: TypeTag[T]) extends Aspect[T, Seq[Data]] {
//  def histogramSignals(dut: T): Seq[Data]
//
//  override def collectAspectInfo(dut: T): Seq[Data] = histogramSignals(dut)
//
//  override def resolveAspectInfo(aspectInfo: Seq[Data]): AnnotationSeq = {
//    Seq(HistogramSignals(aspectInfo.map(_.toTarget)))
//  }
//
//  override def transformClass: Class[_ <: Transform] = classOf[HistogramTransform]
//
//}
//
//case class HistogramSignals(targets: Seq[ReferenceTarget]) extends Annotation {
//  override def update(renames: RenameMap): Seq[Annotation] = {
//    val newTargets = targets.flatMap(t => renames.get(t) match {
//      case Some(seq) => seq.map {
//        case x: ReferenceTarget => x
//        case x => sys.error(s"Cannot update $this when $t is renamed to $x.")
//      }
//      case None => Seq(t)
//    })
//    Seq(HistogramSignals(newTargets))
//  }
//}
//
//class HistogramTransform extends Transform {
//  override def inputForm: CircuitForm = LowForm
//  override def outputForm: CircuitForm = LowForm
//
//  override protected def execute(state: CircuitState): CircuitState = {
//    val refTargets = state.annotations.collect{ case a: HistogramSignals => a}.flatMap( a => a.targets )
//    println("Executing Histogram Transform on the following:")
//    refTargets.foreach { r => println(r.prettyPrint("  ")) }
//    state
//  }
//}