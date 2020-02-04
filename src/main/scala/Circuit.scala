import scala.collection.mutable.HashMap


object Circuit
{
    var countVariable = 1
    var allClauses : List[Expr] = List.empty
    
   
    def toConjunction(e : Expr) : Expr = {
        var retVariable : Expr = e match {
            case BoolLit(value) => BoolLit(value)
            case Variable(name) => Variable(name)
            case And(args) => {
                var temp : List[Expr] = List.empty
                var finalClauseArg : List[Expr] = List.empty
                var newVar : Variable = Variable("new" + countVariable.toString)
                countVariable += 1
                for(arg <- args){
                    var gateVar = toConjunction(arg)
                    temp = temp :+ gateVar
                    finalClauseArg = finalClauseArg :+ Not(gateVar)
                }
                finalClauseArg = finalClauseArg :+ newVar
                for(tempVar <- temp){
                    allClauses = allClauses :+ Or(List(Not(newVar),tempVar))
                }
                allClauses = allClauses :+ Or(finalClauseArg)
                newVar
            }
            case Or(args) => {
                var temp : List[Expr] = List.empty
                var finalClauseArg : List[Expr] = List.empty
                var newVar : Variable = Variable("new" + countVariable.toString)
                countVariable += 1
                for(arg <- args){
                    var gateVar = toConjunction(arg)
                    temp = temp :+ gateVar
                    finalClauseArg = finalClauseArg :+ gateVar
                }
                finalClauseArg = finalClauseArg :+ Not(newVar)
                for(tempVar <- temp){
                    allClauses = allClauses :+ Or(List(newVar,Not(tempVar)))
                }
                allClauses = allClauses :+ Or(finalClauseArg)
                newVar
            }
            case Not(arg) => {
                var newVar : Variable = Variable("new" + countVariable.toString)
                countVariable += 1
                var gateVar = toConjunction(arg)
                allClauses = allClauses :+ Or(List(Not(newVar),Not(gateVar)))
                allClauses = allClauses :+ Or(List(newVar,gateVar))
                newVar
            }
        }
        
        retVariable
    }
    def toCNF(e : Expr, countVar : Int) : (List[Expr], Int, Expr) = {
        countVariable = countVar
        allClauses = List.empty
        var result = toConjunction(e)
        (allClauses,countVariable,result)
    }
    
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
    
    
    
    def solveEquation(e: Expr, allVariables : Set[Variable], initialVariables : Set[Variable], mapVarToInt : HashMap[Expr,Int], solveAllClauses : List[Expr]) : Map[Variable, Boolean] = {
        var resultMap : Map[Variable, Boolean] = Map.empty
        val simlifiedEvalutateExpr = Evaluation.simplify(e)  
        
        
        
       
       
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

        val S = new Solver()
        for (clause <- solveAllClauses){
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
            for(x <- initialVariables){
                resultMap = resultMap + (x -> S.modelValue(Literal.create(mapVarToInt(x))))
            }
        }
         
        resultMap
    } 
    
   
    var count =  0
    
    def getChangedCircuit(e: Expr, num : Int) : Expr = {
        e match {
            case BoolLit(value) => return e
            case Variable(name) => return e
            case And(args) => {
                count = count + 1
                var finalExpr = e
                var listOfElem : List[Expr] = List.empty
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
        var xcount = 1
        for (x <- initialVariables){
            mapVarToInt += (x -> xcount)
            xcount += 1
        }
        for (i <- 1 until 20000){
            var tempVar = Variable("new" + i.toString())
            retval += tempVar
            mapVarToInt += (tempVar -> xcount)
            xcount += 1
        }
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
        val varsAndMap = createAllVariables(initialVariables)
        val allVariables = varsAndMap._1 ++ initialVariables
        val mapVarToInt = varsAndMap._2
        val retFromToCNFActual = toCNF(actualCircuit,1)
        val actualCircuitCNF = retFromToCNFActual._1
        val newVarCountActual = retFromToCNFActual._2
        val gateActualVar = retFromToCNFActual._3
       
  
        for(a <- 0 until noOfAnds){
            var allClausesTemp : List[Expr] = actualCircuitCNF
            val S = new Solver()
            count = 0
            var tempCircuit = getChangedCircuit(actualCircuit,a)
            val (tempCircuitCNF,newVarCountTemp,gateTempVar) = toCNF(tempCircuit,newVarCountActual)
            allClausesTemp = allClausesTemp ++ tempCircuitCNF
            var newVar1 : Variable = Variable("new" + newVarCountTemp.toString)
            allClausesTemp = allClausesTemp :+ Or(List(Not(newVar1),Not(gateActualVar)))
            allClausesTemp = allClausesTemp :+ Or(List(newVar1,gateActualVar))
            
            var newVar2 : Variable = Variable("new" + (newVarCountTemp+1).toString)
            allClausesTemp = allClausesTemp :+ Or(List(Not(newVar2),newVar1))
            allClausesTemp = allClausesTemp :+ Or(List(Not(newVar2),gateTempVar))
            allClausesTemp = allClausesTemp :+ Or(List(newVar2,Not(gateTempVar),Not(newVar1)))
            
            var newVar3 : Variable = Variable("new" + (newVarCountTemp+2).toString)
            allClausesTemp = allClausesTemp :+ Or(List(Not(newVar3),Not(gateTempVar)))
            allClausesTemp = allClausesTemp :+ Or(List(newVar3,gateTempVar))
              
            var newVar4 : Variable = Variable("new" + (newVarCountTemp+3).toString)
            allClausesTemp = allClausesTemp :+ Or(List(Not(newVar4),newVar3))
            allClausesTemp = allClausesTemp :+ Or(List(Not(newVar4),gateActualVar))
            allClausesTemp = allClausesTemp :+ Or(List(newVar4,Not(gateActualVar),Not(newVar3)))
            
            
            var newVar5 : Variable = Variable("new" + (newVarCountTemp+4).toString)
            allClausesTemp = allClausesTemp :+ Or(List(Not(newVar2),newVar5))
            allClausesTemp = allClausesTemp :+ Or(List(Not(newVar4),newVar5))
            allClausesTemp = allClausesTemp :+ Or(List(newVar2,newVar4,Not(newVar5)))
            allClausesTemp = allClausesTemp :+ Or(List(newVar5))
            
            
            
            var tempAssignment = solveEquation(Or(List(And(List(tempCircuit,Not(actualCircuit))),And(List(Not(tempCircuit),actualCircuit)))),allVariables,initialVariables,mapVarToInt,allClausesTemp)
            var tempCircuitVal = givenCircuitEval(tempAssignment)
            var actualCircuitEval = Evaluation.evaluate(actualCircuit,tempAssignment)
            if(tempCircuitVal != actualCircuitEval){
                return false
            }
        }
        true

    }



}
