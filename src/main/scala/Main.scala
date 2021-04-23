object Main extends App {
  new TestRunner(Seq(MyTestSuite1)).runAll()
}


// the user TestSuite

object MyTestSuite1 extends TestSuite("My 1st TestSuite") {
    var l : Seq[Int] = _
    beforeEach({
        println("before each suite 1")
        l = Seq(1, 2, 3, 4)
    })

    afterEach({
        println("after each suite 1")
        l = Seq.empty[Int]
    })

    test("test 1") {
        println("in test 1")
        println(l)
        assert("sas" == "sas")
    }

    test("test 2") {
        println("in test 2")
        l.foreach(println)
        assert("sas" != "sas")
    }
}


// base template of Test Suite

class TestSuite(val name: String) {
    var before: () => Unit = _
    def beforeEach(body: => Unit): Unit = {
        before = () => body
    }

    var after: () => Unit = _
    def afterEach(body: => Unit): Unit = {
        after = () => body
    }

    var tests = Vector.empty[(String, () => Unit)]
    def test(name: String)(body: => Unit): Unit = {
        tests = tests :+ (name, () => body)
    }

    var failures = Vector.empty[(String, Throwable)]
    def reportFails(): Unit = {
        println("===== Failures =====")
        failures.foreach { case (name, ex) => 
            println(s"Failed : ${name}")
            ex.printStackTrace()
        }
    }

    def runAll(): Unit = {
        tests.foreach { case (name, body) =>

            println(s"running ${name}")

            before()

            try {
                body()
            } catch {
                case ex: Throwable => failures = failures :+ (name, ex)
            }

            after()
        }
    }
}


// the TestRunner and reporter 

class TestRunner(suites: Seq[TestSuite]) {
    def runAll() {
        suites.foreach(suite => {
            println(s"Running ${suite.name}")
            suite.runAll()
            suite.reportFails()
        })
    }
}
