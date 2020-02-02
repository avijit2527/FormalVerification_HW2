import scala.collection.mutable.HashMap

object Evaluation {

    def evaluate(e: Expr, m: Map[Variable, Boolean]) : Option[Boolean] = {
        //returns true if the map has all required variables
        def checkMap(e: Expr, memo : HashMap[Expr, Boolean]): Boolean = {
            memo.get(e) match {
                case Some(r) => r
                case None =>
                    val r = e match {
                        case BoolLit(_) =>  true
                        case Variable(name) =>
                            m.get(e.asInstanceOf[Variable]) match {
                                case Some(value) => true
                                case None => false
                            }
                        case And(args) => args.foldLeft(true)( (x, y) => x && checkMap(y, memo))
                        case Or(args) => args.foldLeft(true)( (x, y) => x && checkMap(y, memo))
                        case Not(arg) => checkMap(arg, memo)
                    }
                    memo.put(e, r)
                    r
            }
        }

        def eval(e : Expr, memo : HashMap[Expr, Boolean]) : Boolean = {
            val memoRes = memo.get(e)
            val finalResult = memoRes match {
                case Some(v) => v
                case None =>
                    val result = e match {
                        case BoolLit(v) => v
                        case v : Variable => m.get(v).get
                        case And(args) =>
                            args.foldLeft(true)((r, x) => r && eval(x, memo))
                        case Or(args) =>
                            args.foldLeft(false)((r, x) => r || eval(x, memo))
                        case Not(arg) =>
                            !eval(arg, memo)
                    }
                    memo.put(e, result)
                    result
            }
            finalResult
        }

        if (!checkMap(e, new HashMap())) None else Some(eval(e, new HashMap()))
    }

    def simplify(e: Expr) : Expr = {

        //returns true if any expression is BoolLit(false)
        def checkFalse(a: Any) = a match {
            case BoolLit(value) => if (value==false) true else false
            case _ => false
        }

        //returns true if any expression is BoolLit(true)
        def checkTrue(a: Any) = a match {
            case BoolLit(value) => if (value==true) true else false
            case _ => false
        }


        def findNegate(xs: List[Expr], mem: HashMap[Expr, Boolean]): Boolean = {
            if(!xs.isEmpty) {
                val exp = xs.head
                val isNegPresent = if (exp.isInstanceOf[Not]) mem.get(exp.asInstanceOf[Not].arg) else mem.get(Not(exp))
                isNegPresent match {
                    case Some(v) => true
                    case None =>
                        mem.put(exp, true)
                        findNegate(xs.tail, mem)
                }
            }
            else false
        }

        def simplifyMemo(e: Expr, memo: HashMap[Expr, Expr]): Expr = {
            val memoVal = memo.get(e)
            val finalResult = memoVal match {
                case Some(v) => v
                case None =>
                    val result = e match {
                        case And(args) => args match {
                            case List(x) => simplify(x) //And(e) = e
                            case _ =>
                                val sim = args.map(x => simplify(x))  //sim is e with simplified subexpressions
                                val list = sim.filterNot(checkTrue)  //remove expressions evaluating to true from sim
                                if (sim.filter(checkFalse).length > 0) BoolLit(false) //if there is an expression = false, AND evaluates to false
                                else if(list.length == 0) BoolLit(true) //e has all true subexpressions or is empty
                                else if(list.length == 1) list(0) // And(e) = e after simplification
                                else if (findNegate(list, new HashMap())) BoolLit(false) // And(e, ~e) = false
                                else {
                                    val rem_dupli = list.distinct
                                    if(rem_dupli.length == 1) rem_dupli(0) else And(rem_dupli)
                                }

                        }

                        case Or(args) => args match {
                            case List(x) => simplify(x)
                            case _ =>
                                val sim = args.map(x => simplify(x))
                                val list = sim.filterNot(checkFalse)  //remove expressions evaluating to false
                                if (sim.filter(checkTrue).length > 0) BoolLit(true) //if there is an expression = true, OR evaluates to true
                                else if(list.length == 0) BoolLit(false) 
                                else if(list.length == 1) list(0) // Or(e) = e
                                else if (findNegate(list, new HashMap())) BoolLit(true)
                                else {
                                    val rem_dupli = list.distinct
                                    if(rem_dupli.length == 1) rem_dupli(0) else Or(rem_dupli)
                                }

                        }
                        case Not(arg) =>
                            val newarg = simplify(arg)
                            newarg match {
                                case BoolLit(value) => BoolLit(!value)
                                case Not(x) => x
                                case _ => Not(newarg)
                            }
                        case _ => e
                    }
                    memo.put(e, result)
                    result
            }
            finalResult
        }

        simplifyMemo(e, new HashMap())
    }
    
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




    def checkEquivUsingSat(e1: Expr, e2: Expr) : (Boolean, Option[Map[Variable, Boolean]]) = {

        // TODO : implement equivalent checking using sat solver
        // if the two expression are not equivalent then return (false, sat-model)
        var resultMap : Map[Variable, Boolean] = Map.empty

        val evalutateExpr = And(List(Or(List(Not(e1),Not(e2))),Or(List(e1,e2))))
        val simlifiedEvalutateExpr = CNFConverter.simplify(evalutateExpr)
        val allClauses = CNFConverter.toCNF(simlifiedEvalutateExpr)
        val allVariables = getAllVariables(And(allClauses)) ++ getAllVariables(evalutateExpr)


        //println(simlifiedEvalutateExpr)
        if (simlifiedEvalutateExpr == BoolLit(false)){
            return (true,Some(resultMap))
        }
        if (simlifiedEvalutateExpr == BoolLit(true)){
            for(x <- allVariables){
                resultMap = resultMap + (x -> true)
            }
            return (false,Some(resultMap))
        }

        var mapVarToInt : HashMap[Expr,Int] = HashMap.empty
        var varCount = 1
        for (variables <- allVariables){
            mapVarToInt += (variables -> varCount)
            varCount += 1
        }
        val S = new Solver()
        for (clause <- allClauses){
            var finalClause : List[Literal] = List.empty
            clause match {
                case Or(args) => {
                    for(arg <- args){
                        arg match {
                            case Not(value) => {
                                finalClause = finalClause :+ ~Literal.create(mapVarToInt(value))
                            }
                            case _ => {
                                finalClause = finalClause :+ Literal.create(mapVarToInt(arg))
                            }
                        }
                    }
                }
            }
            S.addClause(Clause(finalClause))
        }
        val isSAT = S.solve()
        //var resultMap : Map[Variable, Boolean] = Map.empty
        if (isSAT){
            for(x <- allVariables){
                resultMap = resultMap + (x -> S.modelValue(Literal.create(mapVarToInt(x))))
            }
        }
        
        
        (!isSAT,Some(resultMap))
    }

    def areEquivalent(e1: Expr, e2: Expr) : Boolean = {

        val satResult = checkEquivUsingSat(e1, e2)
        val areEqual = satResult._1
        val model = satResult._2.getOrElse(Map[Variable, Boolean]())

        if (!areEqual) {
            // get a input assignment and simulate two circuits to check
            // if the result from sat solver is indeed correct

            assert(model.nonEmpty)

            val sim1 = evaluate(e1, model)
            val sim2 = evaluate(e2, model)

            sim1 == sim2 // returns false if sat model is a right counter example

        } else {
            true
        }
    }
}
