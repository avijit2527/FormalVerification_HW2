
object Main {

    def and(args: Expr*): Expr = And(args.toList)
    def or(args: Expr*): Expr = Or(args.toList)
    def not(arg: Expr): Expr = Not(arg)
    def var_(name: String) : Variable = Variable(name)
    def lit(value: Boolean): Expr = BoolLit(value)

    def testEquivalence = {
        def a = var_("a")
        def b = var_("b")
        def c = var_("c")
        def d = var_("d")

        val exp1 = and(a,b)
        val exp2 = not(or(not(a), not(b)))
        Evaluation.areEquivalent(not(c), not(d))
        assert(Evaluation.areEquivalent(exp1, exp2) == true)
        assert(Evaluation.areEquivalent(exp1, not(exp2)) == false)

        val exp3 = and(a,b,c,d)
        val exp4 = and(and(a,b), and(c,d), lit(true))
        assert(Evaluation.areEquivalent(exp3, exp4) == true)
        assert(Evaluation.areEquivalent(exp3, not(exp4)) == false)

        val exp5 = and(or(a,b),and(c,not(d)))
        val exp6 = and(a,b,c,not(d))
        assert(Evaluation.areEquivalent(exp5, not(exp6)) == false)
    }
    
    def testSolver(): Unit = {
        println("============ test solver ============")
        val a = Literal.create(1)
        val b = Literal.create(2)
        val an = ~a
        val bn = ~b

        val S = new Solver()
        S.addClause(a, b)
        S.addClause(~a, ~b, b)
        // Solve
        println(S.solve())
        // Extract model.
        val va = S.modelValue(a)
        val vb = S.modelValue(b)
        println("model: [a -> %s, b -> %s]".format(va.toString(), vb.toString()))
    }
    
    def testCheckEquivUsingSat(): Unit = {
        def a = var_("a")
        def b = var_("b")
        def c = var_("c")
        def d = var_("d")

        val exp1 = and(a,b)
        val exp2 = not(or(not(a), not(b)))
        val exp3 = or(a,b,c,d)
        val exp4 = and(and(a,b), and(c,d), lit(true))
        Evaluation.checkEquivUsingSat(exp4,(exp4))
    }

    def main(args: Array[String])
    {
        //testSolver
        //testCheckEquivUsingSat
        // uncomment following to run corresponding sample tests
         testEquivalence
        // Firewall.test1()
        // CircuitTest.sampleTest
    }
}
