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
    var totalTime = 0.0
    var totalCount = 0
    
    
    
    def solveEquation(e: Expr, allVariables : Set[Variable], initialVariables : Set[Variable], mapVarToInt : HashMap[Expr,Int]) : Map[Variable, Boolean] = {
        val start = System.nanoTime()
        var resultMap : Map[Variable, Boolean] = Map.empty
        val simlifiedEvalutateExpr = CNFConverter.simplify(e)
        
        val allClauses = CNFConverter.toCNF(simlifiedEvalutateExpr,1)._1
        //val allVariables = Evaluation.getAllVariables(And(allClauses)) ++ Evaluation.getAllVariables(e)
        
        
        val end = System.nanoTime()
        totalCount += 1
        totalTime += ((end -start)/1000000)
        println(totalTime/totalCount)
        
        
        
        if (simlifiedEvalutateExpr == BoolLit(false)){
            for(x <- initialVariables){
                resultMap = resultMap + (x -> true)
            }
            return resultMap
        }
        if (simlifiedEvalutateExpr == BoolLit(true)){
            for(x <- initialVariables){
                resultMap = resultMap + (x -> true)
            }
            return resultMap
        }

       /* var mapVarToInt : HashMap[Expr,Int] = HashMap.empty
        var varCount = 1
        for (variables <- allVariables){
            mapVarToInt += (variables -> varCount)
            varCount += 1
        }*/
        val S = new Solver()
        for (clause <- allClauses){
            var finalClause : List[Literal] = List.empty
            clause match {
                case Or(args) => {
                    for(arg <- args){
                        arg match {
                            case Not(value) => {
                            //println(value,mapVarToInt(value))
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
        //println("here")
        //println(allVariables)
        if (isSAT){
            for(x <- initialVariables){
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
    
    def createAllVariables(initialVariables : Set[Variable]) : (Set[Variable],HashMap[Expr,Int]) = {
    
        var mapVarToInt : HashMap[Expr,Int] = HashMap.empty
        var retval : Set[Variable] = Set.empty
        var count = 1
        for (x <- initialVariables){
            mapVarToInt += (x -> count)
            //println(mapVarToInt(x))
            count += 1
        }
        for (i <- 1 until 20000){
            var tempVar = Variable("new" + i.toString())
            retval += tempVar
            mapVarToInt += (tempVar -> count)
            count += 1
        }
        //println(mapVarToInt)
        (retval,mapVarToInt)
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
        val initialVariables = Evaluation.getAllVariables(actualCircuit)
        //println(noOfAnds)
        val varsAndMap = createAllVariables(initialVariables)
        val allVariables = varsAndMap._1 ++ initialVariables
        val mapVarToInt = varsAndMap._2
        val retFromToCNFActual = CNFConverter.toCNF(actualCircuit,1)
        val actualCircuitCNF = retFromToCNFActual._1
        val newVarCountActual = retFromToCNFActual._2
       
  
        for(a <- 0 until noOfAnds){
            var allClauses : List[Expr] = actualCircuitCNF
            val S = new Solver()
            count = 0
            var tempCircuit = getChangedCircuit(actualCircuit,a)
    //        println(a)
        //    println(noOfAnds)
        //    println(actualCircuit)
          //  println(tempCircuit)
            val retFromToCNFtemp = CNFConverter.toCNF(tempCircuit,newVarCountActual)
            val tempCircuitCNF = retFromToCNFtemp._1
            val newVarCountTemp = retFromToCNFtemp._2
            allClauses = allClauses ++ tempCircuitCNF
            
            var tempAssignment = solveEquation(And(List(tempCircuit,Not(actualCircuit))),allVariables,initialVariables,mapVarToInt)
            //println(tempAssignment)
            var tempCircuitVal = givenCircuitEval(tempAssignment)
            if(tempCircuitVal != None){
                if(tempCircuitVal.get){
                    return false
                }
            }else{
                var tempAssignmentOpposite = solveEquation(And(List(Not(tempCircuit),actualCircuit)),allVariables,initialVariables,mapVarToInt)
                var tempCircuitValOpposite = givenCircuitEval(tempAssignmentOpposite)
                if(tempCircuitValOpposite != None){
                    if(!tempCircuitValOpposite.get){
                        return false
                    }
                }
            
            }
        }
        println(totalCount)
        true

    }



}
