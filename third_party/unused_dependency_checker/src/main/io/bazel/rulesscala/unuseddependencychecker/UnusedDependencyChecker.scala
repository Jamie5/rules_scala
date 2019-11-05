package third_party.unused_dependency_checker.src.main.io.bazel.rulesscala.unused_dependency_checker

import scala.reflect.io.AbstractFile
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.{Global, Phase}
import UnusedDependencyChecker._
import scala.tools.nsc.typechecker.StdAttachments

class UnusedDependencyChecker(val global: Global) extends Plugin { self =>
  val name = "unused-dependency-checker"
  val description = "Errors if there exists dependencies that are not used"

  override val components: List[PluginComponent] = List[PluginComponent](PlusOneComponent, NonPlusOneComponent)

  var indirect: Map[String, String] = Map.empty
  var direct: Map[String, String] = Map.empty
  var ignoredTargets: Set[String] = Set.empty
  var analyzerMode: AnalyzerMode = Error
  var currentTarget: String = "NA"
  var plusOneDeps: Boolean = false

  val isWindows: Boolean = System.getProperty("os.name").toLowerCase.contains("windows")

  override def init(options: List[String], error: (String) => Unit): Boolean = {
    var directJars: Seq[String] = Seq.empty
    var directTargets: Seq[String] = Seq.empty
    var indirectJars: Seq[String] = Seq.empty
    var indirectTargets: Seq[String] = Seq.empty

    for (option <- options) {
      option.split(":").toList match {
        case "direct-jars" :: data => directJars = data.map(decodeTarget)
        case "direct-targets" :: data => directTargets = data.map(decodeTarget)
        case "indirect-jars" :: data => indirectJars = data.map(decodeTarget)
        case "indirect-targets" :: data => indirectTargets = data.map(decodeTarget)
        case "ignored-targets" :: data => ignoredTargets = data.map(decodeTarget).toSet
        case "current-target" :: target :: _ => currentTarget = decodeTarget(target)
        case "mode" :: mode :: _ => parseAnalyzerMode(mode).foreach(analyzerMode = _)
        case "plus-one-deps" :: mode :: Nil => plusOneDeps = mode == "yes"
        case unknown :: _ => error(s"unknown param $unknown")
        case Nil =>
      }
    }

    direct = directJars.zip(directTargets).toMap
    indirect = indirectJars.zip(indirectTargets).toMap

    true
  }

  private object PlusOneComponent extends PluginComponent {
    val global: Global = self.global

    import global._

    override val runsAfter = List("typer")

    val phaseName: String = s"${self.name}-plus-one"

    private def warnOrError(messages: Set[String]): Unit = {
      val reportFunction: String => Unit = analyzerMode match {
        case Error => reporter.error(NoPosition, _)
        case Warn => reporter.warning(NoPosition, _)
      }

      messages.foreach(reportFunction)
    }

    private def warnOrError(messages: Map[String, Position]): Unit = {
      val reportFunction: ((String, Position)) => Unit = analyzerMode match {
        case Error =>
          { case (msg, pos) =>
            reporter.error(pos, msg)
          }
        case Warn =>
        { case (msg, pos) =>
          reporter.warning(pos, msg)
        }
      }

      messages.foreach(reportFunction)
    }

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def run(): Unit = {
        super.run()
        if (plusOneDeps) {
          analyzeCode()
        }
      }

      override def apply(unit: CompilationUnit): Unit = {
      }

      private def analyzeCode(): Unit = {
        val usedJarsAndUseLocation: Map[AbstractFile, Position] = findUsedJars
        val usedJars = usedJarsAndUseLocation.keySet
        val directJarPaths = direct.keys.toSet
        val usedJarPaths = if (!isWindows) usedJars.map(_.path) else usedJars.map(_.path.replaceAll("\\\\", "/"))

        val usedTargets = usedJarPaths
          .map(direct.get)
          .collect {
            case Some(target) => target
          }

        val unusedTargets = directJarPaths
          .filter(jar => !usedTargets.contains(direct(jar)))
          .map(direct.get)
          .collect {
            case Some(target) if !ignoredTargets.contains(target) => target
          }

        val unusedDependenciesFound =
          unusedTargets.map { target =>
            s"""Target '$target' is specified as a dependency to $currentTarget but isn't used, please remove it from the deps.
               |You can use the following buildozer command:
               |buildozer 'remove deps $target' $currentTarget
               |""".stripMargin
          }

        warnOrError(unusedDependenciesFound)

        val indirectDependenciesFound =
          usedJarsAndUseLocation.flatMap { case (usedJar, useLocation) =>
            val usedJarPath = usedJar.path
            if (!direct.contains(usedJarPath)) {
              indirect.get(usedJarPath).flatMap { target =>
                Some(
                  s"""Target '$target' is used but isn't explicitly declared, please add it to the deps.
                     |You can use the following buildozer command:
                     |buildozer 'add deps $target' $currentTarget""".stripMargin -> useLocation
                )
              }
            } else {
              None
            }
          }
        warnOrError(indirectDependenciesFound)
      }
    }

    def findUsedJars: Map[AbstractFile, Position] = {
      val jars = collection.mutable.Map[AbstractFile, Position]()

      def handleType(tpe: Type, pos: Position): Unit = {
        val sym = tpe.typeSymbol
        val assocFile = sym.associatedFile
        if (assocFile.path.endsWith(".class"))
          assocFile.underlyingSource.foreach { source =>
            // Prefer the first one we find
            jars.getOrElseUpdate(source, pos)
          }
      }

      def exploreType(tpe: Type, pos: Position): Unit = {
        handleType(tpe, pos)
        tpe.typeArgs.foreach(exploreType(_, pos))
      }

      def fullyExploreTree(tree: Tree): Unit = {
        exploreTree(tree)
        tree.foreach(exploreTree)
      }

      def exploreTree(tree: Tree): Unit = {
        tree match {
          case node: TypeTree =>
            if (node.original != null) {
              node.original.foreach(fullyExploreTree)
            }
          case node: Literal =>
            // XXX need to examine OriginalTreeAttachemnt for folded
            // constants.
            // See https://github.com/scala/bug/issues/7173
            // https://github.com/scala/scala/commit/2e9a5853e9886fd76f7a5c78a9df0b16a7d5f74e
            // (this might exist 2.12.4 on??? unsure...)
            node.attachments.get[global.treeChecker.OriginalTreeAttachment].foreach { attach =>
              fullyExploreTree(attach.original)
            }
            node.value.value match {
              case tpe: Type =>
                exploreType(tpe, tree.pos)
              case _ =>
            }
          case _ =>
        }

        if (tree.hasSymbolField) {
          tree.symbol.annotations.foreach { annot =>
            annot.tree.foreach(fullyExploreTree)
          }
        }
        if (tree.tpe != null) {
          exploreType(tree.tpe, tree.pos)
        }
      }

      currentRun.units.foreach { unit =>
        unit.body.foreach(fullyExploreTree)
      }
      jars.toMap
    }
  }

  private object NonPlusOneComponent extends PluginComponent {
    val global: Global = self.global

    import global._

    override val runsAfter = List("jvm")

    val phaseName: String = s"${self.name}-non-plus-one"

    private def warnOrError(messages: Set[String]): Unit = {
      val reportFunction: String => Unit = analyzerMode match {
        case Error => reporter.error(NoPosition, _)
        case Warn => reporter.warning(NoPosition, _)
      }

      messages.foreach(reportFunction)
    }

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def run(): Unit = {
        super.run()

        if (!plusOneDeps) {
          warnOrError(unusedDependenciesFound)
        }
      }

      private def unusedDependenciesFound: Set[String] = {
        val usedJars: Set[AbstractFile] = findUsedJars
        val directJarPaths = direct.keys.toSet
        val usedJarPaths = if (!isWindows) usedJars.map(_.path) else usedJars.map(_.path.replaceAll("\\\\", "/"))

        val usedTargets = usedJarPaths
          .map(direct.get)
          .collect {
            case Some(target) => target
          }

        val unusedTargets = directJarPaths
          .filter(jar => !usedTargets.contains(direct(jar)))
          .map(direct.get)
          .collect {
            case Some(target) if !ignoredTargets.contains(target) => target
          }

        unusedTargets.map { target =>
          s"""Target '$target' is specified as a dependency to $currentTarget but isn't used, please remove it from the deps.
             |You can use the following buildozer command:
             |buildozer 'remove deps $target' $currentTarget
             |""".stripMargin
        }
      }

      override def apply(unit: CompilationUnit): Unit = ()
    }

    def findUsedJars: Set[AbstractFile] = {
      val jars = collection.mutable.Set[AbstractFile]()

      def walkTopLevels(root: Symbol): Unit = {
        def safeInfo(sym: Symbol): Type =
          if (sym.hasRawInfo && sym.rawInfo.isComplete) sym.info else NoType

        def packageClassOrSelf(sym: Symbol): Symbol =
          if (sym.hasPackageFlag && !sym.isModuleClass) sym.moduleClass else sym

        for (x <- safeInfo(packageClassOrSelf(root)).decls) {
          if (x == root) ()
          else if (x.hasPackageFlag) walkTopLevels(x)
          else if (x.owner != root) { // exclude package class members
            if (x.hasRawInfo && x.rawInfo.isComplete) {
              val assocFile = x.associatedFile
              if (assocFile.path.endsWith(".class") && assocFile.underlyingSource.isDefined)
                assocFile.underlyingSource.foreach(jars += _)
            }
          }
        }
      }

      exitingTyper {
        walkTopLevels(RootClass)
      }
      jars.toSet
    }
  }

}

object UnusedDependencyChecker {

  sealed trait AnalyzerMode

  case object Error extends AnalyzerMode

  case object Warn extends AnalyzerMode

  def parseAnalyzerMode(mode: String): Option[AnalyzerMode] = mode match {
    case "error" => Some(Error)
    case "warn" => Some(Warn)
    case _ => None
  }

  def decodeTarget(target: String): String = target.replace(";", ":")
}
