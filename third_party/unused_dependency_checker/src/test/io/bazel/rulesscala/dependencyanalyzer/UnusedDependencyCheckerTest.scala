package third_party.unused_dependency_checker.src.test.io.bazel.rulesscala.dependencyanalyzer

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import org.scalatest._
import scala.reflect.io.AbstractFile
import scala.reflect.io.Directory
import scala.reflect.io.PlainDirectory
import third_party.utils.src.test.io.bazel.rulesscala.utils.JavaCompileUtil
import third_party.utils.src.test.io.bazel.rulesscala.utils.TestUtil
import third_party.utils.src.test.io.bazel.rulesscala.utils.TestUtil._

class UnusedDependencyCheckerTest extends FunSuite {
  def compileWithUnusedDependencyChecker(code: String, plusOne: Boolean, withDirect: List[(String, String)] = Nil, extraClasspathXX: Vector[Path] = Vector.empty): List[String] = {
    val toolboxPluginOptions: String = {
      val jar = System.getProperty("plugin.jar.location")
      val start = jar.indexOf("/third_party/unused_dependency_checker")
      // this substring is needed due to issue: https://github.com/bazelbuild/bazel/issues/2475
      val jarInRelationToBaseDir = jar.substring(start, jar.length)
      val pluginPath = Paths.get(baseDir, jarInRelationToBaseDir).toAbsolutePath
      s"-Xplugin:$pluginPath -Jdummy=${pluginPath.toFile.lastModified}"
    }

    val constructParam: (String, Iterable[String]) => String = constructPluginParam("unused-dependency-checker")
    val compileOptions = List(
      constructParam("direct-jars", withDirect.map(_._1)),
      constructParam("direct-targets", withDirect.map(_._2)),
      constructParam("current-target", Seq(defaultTarget)),
      constructParam("plus-one-deps", if (plusOne) { Seq("yes") } else { Seq("no") })
    ).mkString(" ")

    val extraClasspath = withDirect.map(_._1) ++ extraClasspathXX.map(_.toString).toList

    runCompiler(code, compileOptions, extraClasspath, toolboxPluginOptions)
  }

  test("error on unused direct dependencies") {
    val testCode =
      """object Foo {
        |}
      """.stripMargin

    val commonsTarget = "//commons:Target"

    val direct = List(apacheCommonsClasspath -> encodeLabel(commonsTarget))
    val errorMesssages = compileWithUnusedDependencyChecker(testCode, plusOne = false, withDirect = direct)

    assert(errorMesssages.exists { msg =>
      msg.contains(commonsTarget) &&
        msg.contains(s"buildozer 'remove deps $commonsTarget' $defaultTarget")
    })
  }

  test("do not error on used direct dependencies") {
    val testCode =
      """object Foo {
        |  org.apache.commons.lang3.ArrayUtils.EMPTY_BOOLEAN_ARRAY.length
        |}
      """.stripMargin

    val commonsTarget = "commonsTarget"

    val direct = List(apacheCommonsClasspath -> commonsTarget)

    val errorMessages = compileWithUnusedDependencyChecker(testCode, plusOne = false, withDirect = direct)
    assert(errorMessages.isEmpty)
  }

  test("error on unused direct dependenciesXXX") {
    val testCode =
      """object Foo {
        |}
      """.stripMargin

    val commonsTarget = "//commons:Target"

    val direct = List(apacheCommonsClasspath -> encodeLabel(commonsTarget))
    val errorMesssages = compileWithUnusedDependencyChecker(testCode, plusOne = true, withDirect = direct)

    assert(errorMesssages.exists { msg =>
      msg.contains(commonsTarget) &&
        msg.contains(s"buildozer 'remove deps $commonsTarget' $defaultTarget")
    })
  }

  test("do not error on used direct dependenciesXXX") {
    val testCode =
      """object Foo {
        |  org.apache.commons.lang3.ArrayUtils.EMPTY_BOOLEAN_ARRAY.length
        |}
      """.stripMargin

    val commonsTarget = "commonsTarget"

    val direct = List(apacheCommonsClasspath -> commonsTarget)

    val errorMessages = compileWithUnusedDependencyChecker(testCode, plusOne = true, withDirect = direct)
    assert(errorMessages.isEmpty)
  }

  def withTmpDir(action: Path => Unit): Unit = {
    val tmpDir = Files.createTempDirectory("foo")
    val file = tmpDir.toFile
    try {
      action(tmpDir)
    } finally {
      // Since this runs in bazel maybe it's unnecessary to cleanup?
      //XXXFileUtils.deleteDirectory(file)
    }
  }

  /**
   * Suppose C depends on both A and B directly, we want to
   * recognize that and not emit any errors.
   */
  private def checkDirectDependencyRecognized(
    aCode: String,
    bCode: String,
    cCode: String
  ): Unit = {
    withTmpDir { tmpDir =>
      TestUtil.compileSourcesForUse(
        aCode,
        Vector.empty,
        output = tmpDir
      )
      TestUtil.compileSourcesForUse(
        bCode,
        Vector(tmpDir),
        output = tmpDir
      )
      val direct = List(tmpDir.resolve("A.class").toString -> "TheAThing", tmpDir.resolve("B.class").toString -> "TheBThing")
      val errorMesssages =
        compileWithUnusedDependencyChecker(
          cCode,
          plusOne = true,
          direct,
          extraClasspathXX = Vector(tmpDir)
        )

      assert(errorMesssages.isEmpty)
    }
  }

  /**
   * Suppose C depends on B, but not directly on A (and maybe only
   * indirectly on A). Then we should detect that and emit an error.
   */
  private def checkIndirectDependencyDetected(
    aCode: String,
    bCode: String,
    cCode: String
  ): Unit = {
    withTmpDir { tmpDir =>
      TestUtil.compileSourcesForUse(
        aCode,
        Vector.empty,
        output = tmpDir
      )
      TestUtil.compileSourcesForUse(
        bCode,
        Vector(tmpDir),
        output = tmpDir
      )
      val direct = List(tmpDir.resolve("A.class").toString -> "TheAThing", tmpDir.resolve("B.class").toString -> "TheBThing")
      val errorMesssages =
        compileWithUnusedDependencyChecker(
          cCode,
          plusOne = true,
          direct,
          extraClasspathXX = Vector(tmpDir)
        )

      assert(errorMesssages.size == 1)
      assert(errorMesssages(0).contains(s"buildozer 'remove deps TheAThing' $defaultTarget"))
    }
  }

  test("basic") {
    checkIndirectDependencyDetected(
      aCode =
        """
          |class A
          |""".stripMargin,
      bCode =
        """
          |class B(a: A)
          |""".stripMargin,
      cCode =
        """
          |class C(b: B)
          |""".stripMargin
    )
  }

  test("basicFooBarA") {
    checkDirectDependencyRecognized(
      aCode =
        """
          |class A
          |""".stripMargin,
      bCode =
        """
          |class B {
          |  def foo(a: A = new A()): Unit = {}
          |}
          |""".stripMargin,
      cCode =
        """
          |class C {
          |  def bar(): Unit = {
          |    new B().foo(new A())
          |  }
          |}
          |""".stripMargin
    )
  }

  /**
   * Ensure that if we have class C depending on class B, that the
   * unused dependency checkers realizes that and doesn't give a false
   * positive.
   */
  private def checkDirectDependencyRecognized(
    bCode: String,
    cCode: String
  ): Unit = {
    withTmpDir { tmpDir =>
      TestUtil.compileSourcesForUse(
        bCode,
        Vector(tmpDir),
        output = tmpDir
      )
      val direct = List(tmpDir.resolve("B.class").toString -> "TheBThing")
      val errorMesssages =
        compileWithUnusedDependencyChecker(
          cCode,
          plusOne = true,
          direct,
          extraClasspathXX = Vector(tmpDir)
        )

      assert(errorMesssages.isEmpty)
    }
  }

  test("basic2") {
    checkDirectDependencyRecognized(
      bCode =
        s"""
           |class B(
           |)
           |""".stripMargin,
      cCode =
        s"""
           |class C(
           |  b: Option[B]
           |)
           |""".stripMargin
    )
  }

  test("basic3") {
    checkDirectDependencyRecognized(
      bCode =
        s"""
           |class B(
           |) extends scala.annotation.StaticAnnotation
           |""".stripMargin,
      cCode =
        s"""
           |@B
           |class C(
           |)
           |""".stripMargin
    )
  }

  test("basic3a") {
    checkDirectDependencyRecognized(
      bCode =
        s"""
           |class B(
           |)
           |""".stripMargin,
      cCode =
        s"""
           |@B
           |class C(
           |)
           |""".stripMargin
    )
  }

  test("basic3b") {
    checkDirectDependencyRecognized(
      bCode =
        s"""
           |class B(
           |)
           |""".stripMargin,
      cCode =
        s"""
           |class C {
           |  @B
           |  def foo(): Unit = {
           |  }
           |}
           |""".stripMargin
    )
  }

  test("basic4") {
    checkDirectDependencyRecognized(
      bCode =
        s"""
           |class B(
           |)
           |""".stripMargin,
      cCode =
        s"""
           |class C[T <: B](
           |)
           |""".stripMargin
    )
  }

  test("basic5") {
    checkDirectDependencyRecognized(
      bCode =
        s"""
           |class B(
           |)
           |""".stripMargin,
      cCode =
        s"""
           |class C(
           |) {
           |  val x: Class[_] = classOf[B]
           |}
           |""".stripMargin
    )
  }

  test("basic6") {
    checkDirectDependencyRecognized(
      bCode =
        s"""
           |class B(
           |)
           |
           |class D(a: Any)
           |""".stripMargin,
      cCode =
        s"""
           |@D(classOf[B])
           |class C
           |""".stripMargin
    )
  }

  test("basic7") {
    checkDirectDependencyRecognized(
      bCode =
        s"""
           |object B {
           |  final val a: Int = 123
           |}
           |""".stripMargin,
      cCode =
        s"""
           |object C {
           |  val d: Int = B.a
           |}
           |""".stripMargin
    )
  }

  test("basic8") {
    withTmpDir { tmpDir =>
      JavaCompileUtil.compile(
        tmpDir = tmpDir.toString,
        className = "B",
        code = "public interface B { }"
      )
      val direct = List(tmpDir.resolve("B.class").toString -> "TheBThing")
      val errorMesssages =
        compileWithUnusedDependencyChecker(
          """
            |class C {
            |  def foo(x: B): Unit = {}
            |}
            |""".stripMargin,
          plusOne = true,
          direct,
          extraClasspathXX = Vector(tmpDir)
        )

      assert(errorMesssages.isEmpty)
    }
  }

  test("basic9") {
    withTmpDir { tmpDir =>
      JavaCompileUtil.compile(
        tmpDir = tmpDir.toString,
        className = "B",
        code = "public interface B { int a = 42; }"
      )
      val direct = List(tmpDir.resolve("B.class").toString -> "TheBThing")
      val errorMesssages =
        compileWithUnusedDependencyChecker(
          """
            |class C {
            |  def foo(x: B): Unit = {}
            |  val b = B.a
            |}
            |""".stripMargin,
          plusOne = true,
          direct,
          extraClasspathXX = Vector(tmpDir)
        )

      assert(errorMesssages.isEmpty)
    }
  }

  test("basic10") {
    withTmpDir { tmpDir =>
      JavaCompileUtil.compile(
        tmpDir = tmpDir.toString,
        className = "B",
        code = "public interface B { int a = 42; }"
      )
      val direct = List(tmpDir.resolve("B.class").toString -> "TheBThing")
      val errorMesssages =
        compileWithUnusedDependencyChecker(
          """
            |class C {
            |  val b = B.a
            |}
            |""".stripMargin,
          plusOne = true,
          direct,
          extraClasspathXX = Vector(tmpDir)
        )

      assert(errorMesssages.isEmpty)
    }
  }
}
