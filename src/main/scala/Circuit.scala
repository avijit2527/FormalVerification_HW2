import scala.collection.mutable.HashMap


object Circuit
{

    def countNumOfAnds(e: Expr): Int = {
        var result = 0
        e match {
            case And(args) => {
                var count = 0
                for (a <- args){
                    count = count + countNumOfAnds(a)
                }
                return count + 1
            }
            case Or(args) => {
                var count = 0
                for (a <- args){
                    count = count + countNumOfAnds(a)
                }
                return count
            }
            case Not(arg) => return countNumOfAnds(arg)
            case _ => return 0
        }
        
        result
    }
    
    def solveEquation(e: Expr) : Map[Variable, Boolean] = {
        var resultMap : Map[Variable, Boolean] = Map.empty
        val simlifiedEvalutateExpr = CNFConverter.simplify(e)
        val allClauses = CNFConverter.toCNF(simlifiedEvalutateExpr)
        val allVariables = Evaluation.getAllVariables(And(allClauses)) ++ Evaluation.getAllVariables(e)
        if (simlifiedEvalutateExpr == BoolLit(false)){
            return resultMap
        }
        if (simlifiedEvalutateExpr == BoolLit(true)){
            return resultMap
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
        if (isSAT){
            for(x <- allVariables){
                resultMap = resultMap + (x -> S.modelValue(Literal.create(mapVarToInt(x))))
            }
        }
        
        
        resultMap
    }
    
    var count =  0
    
    def getChangedCircuit(e: Expr, num : Int) : Expr = {
        //assert(count <= num)
        e match {
            case BoolLit(value) => return e
            case Variable(name) => return e
            case And(args) => {
                count = count + 1
                var finalExpr = e
                var listOfElem : List[Expr] = List.empty
                //println(e)
                //println (count , num)
                if ((count - 1) == num){
                    return Or(args)
                }
                for (arg <- args){
                    listOfElem = listOfElem :+  getChangedCircuit(arg,num)
                }
                return And(listOfElem)                
            }
            case Or(args) => {
                var finalExpr = e
                var listOfElem : List[Expr] = List.empty
                for (arg <- args){
                    listOfElem = listOfElem :+  getChangedCircuit(arg,num)
                }
                return Or(listOfElem)                
            }
            case Not(arg) => {
                return Not(getChangedCircuit(arg,num))              
            }
        }
    }

    def checkEquivalenceOfCircuits(actualCircuit: Expr, givenCircuitEval: (Map[Variable, Boolean]) => Option[Boolean]): Boolean =
    {

        //////////////////////////////////////////////////////////////////////////////////////////////////////////
        //  TODO - This function returns True if actualCircuit and the provided circuit are equivalent and      //
        //  false if they aren't.                                                                               //
        //  The parameter givenCircuitEval is a function which when passed an assignment to circuit variables   //
        //  returns the output of the provided circuit which you are trying to verify. It returns None if the   //
        //  circuit is unable to produce an output with the given assignment.                                   //
        //  The implementation provided below will not work for any of the test cases. You have to change this  //
        //  implementation.                                                                                     //
        //////////////////////////////////////////////////////////////////////////////////////////////////////////

        ////////////////////////////////////////////////////////
        //THE FOLLOWING CODE IS JUST FOR ILLUSTRATION PURPOSES//
        //REPLACE THE CODE BELOW WITH YOUR IMPLEMENTATION     //
        ////////////////////////////////////////////////////////
        var noOfAnds = countNumOfAnds(actualCircuit)
        //println(noOfAnds)
  
        for(a <- 0 until noOfAnds){
            val S = new Solver()
            count = 0
            var tempCircuit = getChangedCircuit(actualCircuit,a)
            //println(a)
            //println(noOfAnds)
            //println(actualCircuit)
            //println(tempCircuit)
            var tempAssignment = solveEquation(And(List(tempCircuit,Not(actualCircuit))))
            if(givenCircuitEval(tempAssignment).getOrElse(true)){
                return false
            }
        }
        
        true

    }



}
