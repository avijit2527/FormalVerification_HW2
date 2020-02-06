 

import scala.util.Try
import scala.io.Source

object CircuitTest
{

    def and(args: Expr*): Expr = And(args.toList)
    def or(args: Expr*): Expr = Or(args.toList)
    def not(arg: Expr): Expr = Not(arg)
    def var_(name: String) : Variable = Variable(name)
    def lit(value: Boolean): Expr = BoolLit(value)

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


    //This is just a sample and not part of the actual testcases. 
    def sampleTest = 
    {
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
        
        val exp = and(exp15, exp16)
        
        
        val modifiedExp1 = or(exp15, exp16)
        val modifiedExp2 = and(exp15, exp16)
        val modifiedExp3 = and(and(a,b),and(c,d),or(e,f),and(g,h))
        def evaluateWrapper1(m: Map[Variable, Boolean]) = Evaluation.evaluate(modifiedExp1, m)
        def evaluateWrapper2(m: Map[Variable, Boolean]) = Evaluation.evaluate(modifiedExp2, m)
        def evaluateWrapper3(m: Map[Variable, Boolean]) = Evaluation.evaluate(modifiedExp3, m)


        

        assert(!Circuit.checkEquivalenceOfCircuits(exp, evaluateWrapper1))
        assert(Circuit.checkEquivalenceOfCircuits(exp, evaluateWrapper2))
        assert(Circuit.checkEquivalenceOfCircuits(modifiedExp3, evaluateWrapper3))

    }
    
}
