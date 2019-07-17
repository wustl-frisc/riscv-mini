package aoplib.floorplan
import _root_.floorplan._
import chisel3._
import chisel3.aop.Aspect
import chisel3.core.Reset
import chisel3.experimental.{RawModule, RunFirrtlTransform}
import firrtl.annotations._
import firrtl.{AnnotationSeq, CircuitForm, CircuitState, LowForm, MidForm, RenameMap, Transform}
import floorplan.WebVisualization.Parameters

import scala.reflect.runtime.universe.TypeTag

case class MemberTracker(name: String, targets: Seq[IsMember], finalSelection: Seq[IsMember] => Option[IsMember]) extends Annotation {
  override def update(renames: RenameMap): Seq[Annotation] = {
    val newMembers = targets.flatMap { m: IsMember =>
      renames.get(m) match {
        case Some(seq) => seq
        case None => Seq(m)
      }
    }
    Seq(this.copy(targets = newMembers))
  }
}

case class FloorplanAspect[T <: RawModule](name: String, dir: String, buildFloorplan: T => LayoutBase)
                                          (implicit tTag: TypeTag[T]) extends Aspect[T] with RunFirrtlTransform {
  def collectTrackers(layout: LayoutBase): Seq[MemberTracker] = {
    def visit(layout: LayoutBase): Seq[MemberTracker] = {
      val trackers = if(layout.properties.get(TargetKey).nonEmpty) {
        val (target, finalMapping) = layout.get(TargetKey).asInstanceOf[(IsMember, Seq[IsMember] => Option[IsMember])]
        Seq(MemberTracker(layout.name, Seq(target), finalMapping))
      } else Nil
      layout match {
        case arr: ArrayLayout => trackers ++ arr.elements.flatMap(visit)
        case other => trackers
      }
    }
    visit(layout)
  }
  override def toAnnotation(top: T): AnnotationSeq = {
    val layout = buildFloorplan(top)
    val trackers = collectTrackers(layout)
    Seq(FloorplanInfo(layout, dir, name)) ++ trackers
  }
  override def transformClass: Class[_ <: Transform] = classOf[FloorplanTransform]
}


case class FloorplanInfo(layout: LayoutBase, dir: String = "test_run_dir/html", name: String = "layout") extends NoTargetAnnotation

class FloorplanTransform extends Transform {
  override def inputForm: CircuitForm = LowForm
  override def outputForm: CircuitForm = LowForm

  def updateLayout(layout: LayoutBase, remap: Map[String, Option[IsMember]]): LayoutBase = {
    def visit(layout: LayoutBase): LayoutBase = {
      val renamedLayout = if(layout.properties.get(NameKey).isDefined && layout.properties(NameKey) != None && remap.contains(layout.name)) {
        remap(layout.name) match {
          case Some(x) =>
            val newName = x match {
              case r: ReferenceTarget => r.ref
              case r: InstanceTarget => "instance " + r.instance + " of " + r.ofModule
              case r: ModuleTarget => r.module
            }
            layout.replaceTargets(x, (seq) => None)
          case None => EmptyLayout()
        }
      } else layout
      renamedLayout match {
        case arr: ArrayLayout =>
          arr.replaceElements(arr.elements.map(visit))
        case other => other
      }
    }
    visit(layout)
  }

  override def execute(state: CircuitState): CircuitState = {
    val groupedAnnotations = state.annotations.groupBy {
      case b: FloorplanInfo => "info"
      case s: MemberTracker => "tracker"
      case other => "other"
    }

    val renames = groupedAnnotations.getOrElse("tracker", Nil).foldLeft(Map.empty[String, Option[IsMember]]) {
      case (map, s: MemberTracker) => map ++ Map(s.name -> s.finalSelection(s.targets))
    }

    val returnedState = state.copy(annotations = groupedAnnotations.getOrElse("other", Nil).asInstanceOf[Seq[Annotation]])

    groupedAnnotations.getOrElse("info", Nil) match {
      case Nil =>
      case Seq(info: FloorplanInfo) =>
        val updatedLayout = updateLayout(info.layout, renames).resolve(true)
        WebVisualization.generateFiles(updatedLayout, info.dir, Some(info.name))(Parameters(width = 1300, height = 1000))
        PlacementJSON.generateFiles(updatedLayout, info.dir, Some(info.name))
    }
    returnedState
  }
}
