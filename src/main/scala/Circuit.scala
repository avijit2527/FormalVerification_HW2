import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList



object Circuit
{
    var countVariable = 1
    var allClauses : MutableList[Clause] = MutableList.empty
    
    var mapVarToInt : HashMap[Variable,Int] = HashMap.empty
    
   
    def toConjunction(e : Expr) : Literal = {
        var retVariable : Literal = e match {
            case Variable(name) => {
                var cacheLiteral = mapVarToInt.get(Variable(name))
                var tempLiteral = Literal.create(countVariable)
                if(cacheLiteral == None){
                    mapVarToInt += (Variable(name) -> countVariable)
                    tempLiteral = Literal.create(countVariable)
                    countVariable += 1
                }else{
                    tempLiteral = Literal.create(cacheLiteral.get)
                }
                tempLiteral
            }
            case And(args) => {
                var temp : List[Literal] = List.empty
                var finalClauseArg : List[Literal] = List.empty
                var newVar : Literal = Literal.create(countVariable)
                countVariable += 1
                for(arg <- args){
                    var gateVar = toConjunction(arg)
                    temp = temp :+ gateVar
                    finalClauseArg = finalClauseArg :+ ~gateVar
                }
                finalClauseArg = finalClauseArg :+ newVar
                for(tempVar <- temp){
                    allClauses += Clause(List(~newVar,tempVar))
                }
                allClauses += Clause(finalClauseArg)
                newVar
            }
            case Or(args) => {
                var temp : List[Literal] = List.empty
                var finalClauseArg : List[Literal] = List.empty
                var newVar : Literal = Literal.create(countVariable)
                countVariable += 1
                for(arg <- args){
                    var gateVar = toConjunction(arg)
                    temp = temp :+ gateVar
                    finalClauseArg = finalClauseArg :+ gateVar
                }
                finalClauseArg = finalClauseArg :+ ~newVar
                for(tempVar <- temp){
                    allClauses += Clause(List(newVar,~tempVar))
                }
                allClauses += Clause(finalClauseArg)
                newVar
            }
            case Not(arg) => {
                var newVar : Literal = Literal.create(countVariable)
                countVariable += 1
                var gateVar = toConjunction(arg)
                allClauses += Clause(List(~newVar,~gateVar))
                allClauses += Clause(List(newVar,gateVar))
                newVar
            }
        }
        
        retVariable
    }
    def toCNF(e : Expr, countVar : Int) : (MutableList[Clause], Int, Literal) = {
        mapVarToInt = HashMap.empty
        countVariable = countVar
        allClauses = MutableList.empty
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
    
    
    def solveEquation(initialVariables : Set[Variable], solveAllClauses : MutableList[Clause]) : Map[Variable, Boolean] = {
        var resultMap : Map[Variable, Boolean] = Map.empty
        
        val S = new Solver()
        for (clause <- solveAllClauses){
            S.addClause(clause)
        }
        val isSAT = S.solve()
        if (isSAT){
            for(x <- initialVariables){
                resultMap = resultMap + (x -> S.modelValue(Literal.create(mapVarToInt(x))))
            }
        }
         
        resultMap
    } 
    
   
    var countChangedCircuit =  0
    
    def getChangedCircuit(e: Expr, num : Int) : Expr = {
        e match {
            case BoolLit(value) => return e
            case Variable(name) => return e
            case And(args) => {
                countChangedCircuit = countChangedCircuit + 1
                var finalExpr = e
                var listOfElem : MutableList[Expr] = MutableList.empty
                if ((countChangedCircuit - 1) == num){
                    return Or(args)
                }
                for (arg <- args){
                    listOfElem +=  getChangedCircuit(arg,num)
                }
                return And(listOfElem.toList)                
            }
            case Or(args) => {
                var finalExpr = e
                var listOfElem : MutableList[Expr] = MutableList.empty
                for (arg <- args){
                    listOfElem = listOfElem :+  getChangedCircuit(arg,num)
                }
                return Or(listOfElem.toList)                
            }
            case Not(arg) => {
                return Not(getChangedCircuit(arg,num))              
            }
        }
    }
    
    

    def checkEquivalenceOfCircuits(actualCircuit: Expr, givenCircuitEval: (Map[Variable, Boolean]) => Option[Boolean]): Boolean =
    {
        var noOfAnds = countNumOfAnds(actualCircuit)
        println(noOfAnds)
        val simplifiedActualCircuit = Evaluation.simplify(actualCircuit)
        val initialVariables = Evaluation.getAllVariables(actualCircuit)
        val (actualCircuitCNF, newVarCountActual, gateActualVar) = toCNF(simplifiedActualCircuit,1)
       
  
        for(a <- 0 until noOfAnds){
            countChangedCircuit = 0
            var tempCircuit = getChangedCircuit(actualCircuit,a)
            val simplifiedTempCircuit = Evaluation.simplify(tempCircuit)
            val (tempCircuitCNF,newVarCountTemp,gateTempVar) = toCNF(simplifiedTempCircuit,newVarCountActual)
            var allClausesTemp = actualCircuitCNF ++ tempCircuitCNF
            var newVar1 : Literal = Literal.create(newVarCountTemp)
            allClausesTemp += Clause(List(~newVar1,~gateActualVar))
            allClausesTemp += Clause(List(newVar1,gateActualVar))
            
            var newVar2 : Literal = Literal.create(newVarCountTemp + 1)
            allClausesTemp += Clause(List(~newVar2,newVar1))
            allClausesTemp += Clause(List(~newVar2,gateTempVar))
            allClausesTemp += Clause(List(newVar2,~gateTempVar,~newVar1))
            
            var newVar3 : Literal = Literal.create(newVarCountTemp + 2)
            allClausesTemp += Clause(List(~newVar3,~gateTempVar))
            allClausesTemp += Clause(List(newVar3,gateTempVar))
              
            var newVar4 : Literal = Literal.create(newVarCountTemp + 3)
            allClausesTemp += Clause(List(~newVar4,newVar3))
            allClausesTemp += Clause(List(~newVar4,gateActualVar))
            allClausesTemp += Clause(List(newVar4,~gateActualVar,~newVar3))
            
            
            var newVar5 : Literal = Literal.create(newVarCountTemp + 4)
            allClausesTemp += Clause(List(~newVar2,newVar5))
            allClausesTemp += Clause(List(~newVar4,newVar5))
            allClausesTemp += Clause(List(newVar2,newVar4,~newVar5))
            allClausesTemp += Clause(List(newVar5))
            
            
            
            
            var tempAssignment : Map[Variable, Boolean] = Map.empty
        
            val S = new Solver()
            for (clause <- allClausesTemp){
                S.addClause(clause)
            }
            val isSAT = S.solve()
            if (isSAT){
                for(x <- initialVariables){
                    tempAssignment = tempAssignment + (x -> S.modelValue(Literal.create(mapVarToInt(x))))
                }
            }
         
            
            
            
            
            var tempCircuitVal = givenCircuitEval(tempAssignment)
            var actualCircuitEval = Evaluation.evaluate(actualCircuit,tempAssignment)
            if(tempCircuitVal != actualCircuitEval){
                return false
            }
        }
        true

    }



}
