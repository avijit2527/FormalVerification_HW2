
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

        val exp1 = or(lit(true))
        val exp2 = not(or(not(a), not(b)))
        Evaluation.areEquivalent(not(c), not(d))
        assert(Evaluation.areEquivalent(exp1, exp1) == true)
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
    
        def e = var_("e")
        def f = var_("f")
        def g = var_("g")
        def h = var_("h")
    
        def i = var_("i")
        def j = var_("j")
        def k = var_("k")
        def l = var_("l")
    
        def m = var_("m")
        def n = var_("n")
        def o = var_("o")
        def p = var_("p")

        
        val exp1 = and(and(and(a,b),or(c,d)),and(and(e,f),or(g,h)))
        val exp2 = and(and(and(i,j),or(k,l)),and(and(m,n),or(o,p)))
        
        val exp3 = or(exp1,exp2)
        val exp4 = and(exp1,exp2)
        
        val exp5 = or(exp3,exp4)
        val exp6 = and(exp3,exp4)
        
        val exp7 = or(exp5,exp6)
        val exp8 = and(exp5,exp6)
        
        
        val exp9 = or(exp7,exp8)
        val exp10 = and(exp7,exp8)
        
        val exp11 = or(exp9,exp10)
        val exp12 = and(exp9,exp10)
        
        
        val exp13 = or(exp11,exp12)
        val exp14 = and(exp11,exp12)
        
        
        val exp15 = or(exp13,exp14)
        val exp16 = and(exp13,exp14)
        
        
        val exp17 = or(exp15,exp16)
        val exp18 = and(exp15,exp16)
        
        
        val exp19 = or(exp17,exp18)
        val exp20 = and(exp17,exp18)
        
        println(Evaluation.checkEquivUsingSat(exp13,not(exp12))_1)
    }

    def main(args: Array[String])
    {
        //testSolver
        // uncomment following to run corresponding sample tests
       //testEquivalence
       testCheckEquivUsingSat
         Firewall.test1()
         //CircuitTest.sampleTest
    }
}
