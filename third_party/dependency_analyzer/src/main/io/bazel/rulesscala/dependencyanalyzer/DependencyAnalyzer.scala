package third_party.dependency_analyzer.src.main.io.bazel.rulesscala.dependencyanalyzer

import scala.reflect.io.AbstractFile
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
import scala.tools.nsc.Phase

class DependencyAnalyzer(val global: Global) extends Plugin {

  override val name = "dependency-analyzer"
  override val description =
    "Analyzes the used dependencies. Can check and warn or fail the " +
      "compilation for issues including not directly including " +
      "dependencies which are directly included in the code, or " +
      "including unused dependencies."
  override val components =
    List[PluginComponent](
      new AnalyzerComponent(
        runsAfterPhase = "typer",
        handles = DependencyTrackingMethod.Ast
      ),
      new AnalyzerComponent(
        runsAfterPhase = "jvm",
        handles = DependencyTrackingMethod.HighLevel
      )
    )

  private val isWindows: Boolean = System.getProperty("os.name").toLowerCase.contains("windows")
  private var settings: DependencyAnalyzerSettings = null

  override def init(
    options: List[String],
    error: String => Unit
  ): Boolean = {
    settings = DependencyAnalyzerSettings.parseSettings(options = options, error = error)
    true
  }

  private class AnalyzerComponent(
    // Typer seems to be the better method at least for AST - it seems like
    // some things get eliminated in later phases. However, due to backwards
    // compatibility we have to preserve using jvm for the high-level-crawl
    // dependency tracking method
    runsAfterPhase: String,
    handles: DependencyTrackingMethod
  ) extends PluginComponent {
    override val global: DependencyAnalyzer.this.global.type =
      DependencyAnalyzer.this.global

    override val runsAfter = List(runsAfterPhase)

    val phaseName = s"${DependencyAnalyzer.this.name}-post-$runsAfterPhase"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def run(): Unit = {
        super.run()
        if (settings.dependencyTrackingMethod == handles) {
          runAnalysis()
        }
      }

      override def apply(unit: global.CompilationUnit): Unit = ()
    }
  }

  private def runAnalysis(): Unit = {
    val usedJars = findUsedJars
    val usedJarPaths = usedJars.map { case (x, y) => x.path -> y }

    if (settings.unusedDepsMode != AnalyzerMode.Off) {
      reportUnusedDepsFoundIn(usedJarPaths.keySet)
    }

    if (settings.strictDepsMode != AnalyzerMode.Off) {
      reportIndirectTargetsFoundIn(usedJarPaths)
    }
  }

  private def reportIndirectTargetsFoundIn(usedJarPaths: Map[String, global.Position]): Unit = {
    val errors =
      usedJarPaths
        .filterNot { case (k, v) => settings.directTargetSet.jarSet.contains(k) }
        .flatMap { case (k, v) =>
          settings.indirectTargetSet.targetFromJarOpt(k).map { target =>
            s"""Target '$target' is used but isn't explicitly declared, please add it to the deps.
               |You can use the following buildozer command:
               |buildozer 'add deps $target' ${settings.currentTarget}""".stripMargin -> v
          }
        }
        .toSet

    warnOrError(settings.strictDepsMode, errors)
  }

  private def reportUnusedDepsFoundIn(usedJarPaths: Set[String]): Unit = {
    val directJarPaths = settings.directTargetSet.jarSet

    val usedTargets =
      usedJarPaths.flatMap(settings.directTargetSet.targetFromJarOpt)

    val unusedTargets = directJarPaths
      // This .get is safe because [jar] was gotten from [directJarPaths]
      // which is the set of keys of the direct targets.
      .filter(jar => !usedTargets.contains(settings.directTargetSet.targetFromJarOpt(jar).get))
      .flatMap(settings.directTargetSet.targetFromJarOpt)
      .diff(settings.ignoredUnusedDependencyTargets)

    val toWarnOrError =
      unusedTargets.map { target =>
        s"""Target '$target' is specified as a dependency to ${settings.currentTarget} but isn't used, please remove it from the deps.
           |You can use the following buildozer command:
           |buildozer 'remove deps $target' ${settings.currentTarget}
           |""".stripMargin -> (global.NoPosition: global.Position)
      }

    warnOrError(settings.unusedDepsMode, toWarnOrError)
  }

  private def warnOrError(
    analyzerMode: AnalyzerMode,
    errors: Set[(String, global.Position)]
  ): Unit = {
    val reportFunction: (String, global.Position) => Unit = analyzerMode match {
      case AnalyzerMode.Error => (s, p) => global.reporter.error(p, s)
      case AnalyzerMode.Warn => (s, p) => global.reporter.warning(p, s)
      case AnalyzerMode.Off => (_, _) => ()
    }

    errors.foreach { case (a, b) => reportFunction(a, b) }
  }

  private def findUsedJars: Map[AbstractFile, global.Position] = {
    settings.dependencyTrackingMethod match {
      case DependencyTrackingMethod.HighLevel =>
        new HighLevelCrawlUsedJarFinder(global).findUsedJars.map(x => x -> global.NoPosition).toMap
      case DependencyTrackingMethod.Ast =>
        new AstUsedJarFinder(global).findUsedJars
    }
  }
}
