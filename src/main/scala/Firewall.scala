import scala.collection.mutable.HashMap

import scala.util.control.Breaks._


case class CIDR(A: Int, B: Int, C: Int, D: Int, n: Int) {

    def toBitString: String = (D + (C << 8) + (B << 16) + (A << 24)).toBinaryString

    override def toString: String = A +"."+ B +"."+ C +"."+ D +"/"+n

    def toBooleanExpr(varList : Array[Variable]): Expr = {
        val W: Int = D + (C << 8) + (B << 16) + (A << 24)
        var formula : Expr = BoolLit(true)

        assert(n <= 32 && n >= 0)
        var idx = 0
        while (idx < n) {
            val bitval = (W >>> (31-idx)) & 1
            formula = Evaluation.simplify(formula)
            if(bitval != 0 )
                formula = And(List(formula, varList(31-idx)))
            else
                formula = And(List(formula, Not(varList(31-idx))))

            idx += 1
        }
        formula
    }
}

object Firewall {

    var variableList : Array[Variable] = new Array[Variable](32)
    for( w <- 0 until 32) {
        variableList(w) = Variable("c" + w.toString)
    }


    def firewall2BoolFcn(cidrs: Array[CIDR]): Expr = {

        // TODO : converts a given firewall table to a boolean expression
        var finalExpressionList : List[Expr] = List.empty

        for(cidr <- cidrs){
            //println(cidr,cidr.toBooleanExpr(variableList))
            finalExpressionList = finalExpressionList :+ cidr.toBooleanExpr(variableList)
        }
        Or(finalExpressionList)
    }
    
    def bool2int(b:Boolean) = if (b) 1 else 0
        
    def getAllVariables(e: Expr) : Set[Variable] = {
        e match {
            case Variable(name) => Set(Variable(name))
            case BoolLit(value) => Set.empty
            case And(args) =>  var retval: Set[Variable] =  Set.empty ; for(a <- args) {retval = getAllVariables(a) ++ retval}; retval
            case Or(args) => var retval: Set[Variable] = Set.empty ; for(a <- args) {retval = getAllVariables(a) ++ retval}; retval
            case Not(v) => getAllVariables(v)
            case _ => Set.empty
        }
    }

    def toFirewallFormat(resultMap : Map[Variable, Boolean], subnetMask : Int) : CIDR = {
        var resultString = ""
        for(a <- 31 to subnetMask by -1){
            resultString += (bool2int(resultMap(Variable("c" + a.toString))).toString)
        }
        for(a <- (subnetMask - 1) to 0 by -1){
            resultString += 0.toString
        }
        //println(Integer.parseInt(resultString.slice(0,8), 2))
        
        
        CIDR(Integer.parseInt(resultString.slice(0,8), 2),Integer.parseInt(resultString.slice(8,16), 2),Integer.parseInt(resultString.slice(16,24), 2),Integer.parseInt(resultString.slice(24,32), 2),32 - subnetMask)
    }
    
    

    def firewallDifference(f1: Array[CIDR], f2: Array[CIDR]) : Array[CIDR] = {

        //////////////////////////////////////////////////////////////////////
        // TODO: implement firewallDifference.                              //
        //     - it takes two ip-table as input                             //
        //     - returns a list of generalized ip addresses which represent //
        // set of all ips which are in "f1" but not in "f2"                 //
        //                                                                  //
        // * you may write helper functions for your convenience            //
        //////////////////////////////////////////////////////////////////////


        var differenceFirewall : Array[CIDR] = Array()
        val expr1 = firewall2BoolFcn(f1)
        val expr2 = firewall2BoolFcn(f2)
        val finalExpr = And(List(expr1,Not(expr2))) 
        //val simlifiedFinalExpr = Evaluation.simplify(finalExpr)
        //println(finalExpr)
        if(simlifiedFinalExpr == BoolLit(false)){
            return differenceFirewall
        }
        val (allClauses,countVariable,mapVarToInt) = CNFConverter.toCNF(simlifiedFinalExpr,1)
        var resultMap : Map[Variable, Boolean] = Map.empty
        
        val S = new Solver()
        for(clause <- allClauses){
            S.addClause(clause)
        }
        
        var isSAT = S.solve()
        while(isSAT){
            if (isSAT){
                for((x,y) <- mapVarToInt){
                    resultMap = resultMap + (x -> S.modelValue(y))
                }
            }
        
        
            println(resultMap)
            var subnetCount = 0
            breakable{
                for(variable <- variableList){
                    if(Evaluation.evaluate(finalExpr,resultMap + (variable -> true)) == Evaluation.evaluate(finalExpr,resultMap + (variable -> false))){
                        subnetCount += 1
                    }else{
                        break
                    }
                }
            }
            println(subnetCount)
            //subnetCount -= 1
            var resultingFirewall = toFirewallFormat(resultMap, subnetCount)
            //println("----------")
            //println(subnetCount)
            //println(resultMap)
            //println(resultingFirewall)
            //println("----------")
            differenceFirewall = differenceFirewall :+ resultingFirewall
            var blockingClause : List[Literal] = List.empty
            for(a <- subnetCount until 32){
                val tempLookup = Variable("c" + a.toString)
                if(S.modelValue(mapVarToInt(tempLookup))){
                    blockingClause = blockingClause :+ ~Literal.create(mapVarToInt(tempLookup))
                }else{
                    blockingClause = blockingClause :+ Literal.create(mapVarToInt(tempLookup))
                }
            }
            S.addClause(Clause(blockingClause))
            //println(31 - subnetCount)
            isSAT = S.solve()
        }
        //println(Evaluation.areEquivalent(And(List(firewall2BoolFcn(differenceFirewall),expr2)),Or(List.empty[Expr])))
        differenceFirewall
    }


    def test1(): Unit = {

        println("======= Firewall : test1() =======")
        val f1: Array[CIDR] = Array(CIDR(14, 110, 0, 0, 15), CIDR(15, 110, 0, 0, 15), CIDR(30, 216, 0, 0, 13),
            CIDR(31, 208, 0, 0, 12), CIDR(94, 83, 236, 144, 30), CIDR(95, 82, 236, 144, 30),
            CIDR(112, 0, 0, 0, 5), CIDR(128, 0, 0, 0, 5), CIDR(136, 10, 0, 0, 15),
            CIDR(136, 12, 0, 0, 16), CIDR(163, 217, 224, 0, 20), CIDR(173, 211, 0, 0, 17),
            CIDR(174, 210, 0, 0, 17), CIDR(176, 56, 0, 0, 15), CIDR(176, 58, 0, 0, 17),
            CIDR(176, 128, 0, 0, 9), CIDR(177, 58, 0, 0, 15), CIDR(184, 0, 0, 0, 6),
            CIDR(213, 184, 0, 0, 14), CIDR(214, 180, 0, 0, 14), CIDR(241, 31, 223, 104, 29),
            CIDR(241, 31, 224, 104, 30), CIDR(245, 171, 224, 0, 19), CIDR(245, 172, 224, 0, 19))

        val f2: Array[CIDR] = Array(CIDR(14, 110, 0, 0, 15), CIDR(31, 208, 0, 0, 12), CIDR(94, 83, 235, 144, 31),
            CIDR(94, 83, 236, 144, 30), CIDR(95, 82, 236, 144, 30), CIDR(112, 0, 0, 0, 5),
            CIDR(128, 0, 0, 0, 4), CIDR(163, 216, 224, 0, 19), CIDR(163, 217, 224, 0, 19),
            CIDR(174, 210, 0, 0, 17), CIDR(176, 58, 0, 0, 17), CIDR(176, 128, 0, 0, 9),
            CIDR(177, 0, 0, 0, 8), CIDR(184, 0, 0, 0, 6), CIDR(213, 182, 0, 0, 15),
            CIDR(213, 184, 0, 0, 14), CIDR(241, 31, 223, 104, 29), CIDR(241, 31, 224, 104, 30),
            CIDR(245, 172, 240, 0, 20), CIDR(249, 82, 96, 0, 20), CIDR(249, 83, 96, 0, 20))

        val d12: Array[CIDR] = Array(CIDR(30, 216, 0, 0, 13), CIDR(15, 110, 0, 0, 15),
            CIDR(245, 172, 224, 0, 20), CIDR(245, 171, 224, 0, 19), CIDR(176, 56, 0, 0, 15),
            CIDR(173, 211, 0, 0, 17), CIDR(214, 180, 0, 0, 14))


        //val cubeList = firewallDifference(f1, f2)
        //val eqv = Evaluation.areEquivalent(Firewall.firewall2BoolFcn(d12), Firewall.firewall2BoolFcn(cubeList))
        //println("d12 check: " + (if(eqv) "pass" else "fail"))
        // validate that each of these blocks are in f1 but not in f2

        val f3 = Array(CIDR(5, 176, 176, 168, 31), CIDR(6, 176, 176, 168, 31), CIDR(10, 0, 0, 0, 7),
            CIDR(14, 19, 32, 0, 20), CIDR(19, 164, 203, 68, 30), CIDR(19, 165, 202, 70, 31),
            CIDR(20, 165, 202, 70, 31), CIDR(53, 209, 0, 0, 16), CIDR(54, 210, 0, 0, 16),
            CIDR(56, 214, 174, 96, 27), CIDR(56, 214, 175, 96, 27), CIDR(57, 213, 174, 96, 27),
            CIDR(68, 0, 0, 0, 6), CIDR(99, 71, 242, 160, 28), CIDR(112, 102, 185, 0, 24),
            CIDR(112, 103, 184, 0, 22), CIDR(113, 102, 184, 0, 22), CIDR(128, 0, 0, 0, 1))

        val f4 = Array(CIDR(5, 176, 176, 168, 31), CIDR(6, 176, 176, 168, 31), CIDR(6, 176, 177, 168, 31),
            CIDR(10, 0, 0, 0, 7), CIDR(14, 19, 32, 0, 19), CIDR(14, 20, 32, 0, 20),
            CIDR(19, 164, 203, 68, 30), CIDR(19, 165, 202, 70, 31), CIDR(54, 209, 0, 0, 16),
            CIDR(56, 213, 175, 96, 27), CIDR(56, 214, 174, 96, 27), CIDR(69, 0, 0, 0, 8),
            CIDR(76, 52, 32, 0, 21), CIDR(77, 52, 40, 0, 21), CIDR(77, 53, 32, 0, 20),
            CIDR(98, 72, 242, 160, 30), CIDR(99, 71, 242, 160, 28), CIDR(112, 102, 185, 0, 24),
            CIDR(112, 103, 184, 0, 22), CIDR(113, 102, 184, 0, 22), CIDR(128, 0, 0, 0, 1))

        val d34 = Array(CIDR(70, 0, 0, 0, 7), CIDR(68, 0, 0, 0, 8), CIDR(53, 209, 0, 0, 16),
            CIDR(57, 213, 174, 96, 27), CIDR(54, 210, 0, 0, 16), CIDR(20, 165, 202, 70, 31),
            CIDR(56, 214, 175, 96, 27))

        //val cb2 = firewallDifference(f3, f4)
        //val eqv2 = Evaluation.areEquivalent(Firewall.firewall2BoolFcn(d34), Firewall.firewall2BoolFcn(cb2))
        
        //println("d34 check: " + (if(eqv2) "pass" else "fail"))
        println("=================\n")
        
        val f5 = Array(CIDR(29, 20, 64, 0, 14), CIDR(128, 0, 0, 0, 2), CIDR(188, 181, 231, 128, 27),
            CIDR(188, 181, 234, 136, 32), CIDR(188, 180, 0, 0, 15))

        val f6 = Array(CIDR(29, 20, 64, 0, 14), CIDR(128, 0, 0, 0, 4), CIDR(188, 181, 231, 128, 27),
            CIDR(188, 181, 234, 136, 29), CIDR(189, 181, 231, 136, 29))

        val cb3 = firewallDifference(f5, f6)
        
        
        for (a <- cb3)
            println(a)
        

        
    }
}
