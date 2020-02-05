import scala.collection.mutable.HashMap
import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer



object Circuit
{
    var countVariable = 1
    var allClauses : List[Clause] = List.empty
    var mapVarToInt : HashMap[Variable,Int] = HashMap.empty
    var selectVariables : List[Literal] = List.empty



    def createMasterCircuit(e : Expr) : Literal = {
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
                var tempInputs : List[Literal] = List.empty
                var finalClauseArgAnd : List[Literal] = List.empty
                var finalClauseArgOr : List[Literal] = List.empty
                var newVarAnd : Literal = Literal.create(countVariable)
                countVariable += 1
                var newVarOr : Literal = Literal.create(countVariable)
                countVariable += 1
                for(arg <- args){
                    var gateVar = createMasterCircuit(arg)
                    tempInputs = tempInputs :+ gateVar
                    finalClauseArgAnd = finalClauseArgAnd :+ ~gateVar
                    finalClauseArgOr = finalClauseArgOr :+ gateVar
                }
                finalClauseArgAnd = finalClauseArgAnd :+ newVarAnd
                finalClauseArgOr = finalClauseArgOr :+ ~newVarOr
                for(tempVar <- tempInputs){
                    allClauses = allClauses :+ Clause(List(~newVarAnd,tempVar))
                    allClauses = allClauses :+ Clause(List(newVarOr,~tempVar))
                }
                allClauses = allClauses :+ Clause(finalClauseArgAnd)
                allClauses = allClauses :+ Clause(finalClauseArgOr)
                
                
                var selectVar : Literal = Literal.create(countVariable)
                countVariable += 1
                selectVariables = selectVariables :+ selectVar
                var outputVar : Literal = Literal.create(countVariable)
                countVariable += 1
                
                allClauses = allClauses :+ Clause(List(selectVar,newVarAnd,~outputVar))
                allClauses = allClauses :+ Clause(List(selectVar,~newVarAnd,outputVar))
                allClauses = allClauses :+ Clause(List(~selectVar,newVarOr,~outputVar))
                allClauses = allClauses :+ Clause(List(~selectVar,~newVarOr,outputVar))
                
                outputVar

            }
            case Or(args) => {
                var temp : List[Literal] = List.empty
                var finalClauseArg : List[Literal] = List.empty
                var newVar : Literal = Literal.create(countVariable)
                countVariable += 1
                for(arg <- args){
                    var gateVar = createMasterCircuit(arg)
                    temp = temp :+ gateVar
                    finalClauseArg = finalClauseArg :+ gateVar
                }
                finalClauseArg = finalClauseArg :+ ~newVar
                for(tempVar <- temp){
                    allClauses = allClauses :+ Clause(List(newVar,~tempVar))
                }
                allClauses = allClauses :+ Clause(finalClauseArg)
                newVar
            }
            case Not(arg) => {
                var newVar : Literal = Literal.create(countVariable)
                countVariable += 1
                var gateVar = createMasterCircuit(arg)
                allClauses = allClauses :+ Clause(List(~newVar,~gateVar))
                allClauses = allClauses :+ Clause(List(newVar,gateVar))
                newVar
            }
        }
        
        retVariable
        
    }
   
   
   
   
   
   
   
   
    var allClausesOriginal : List[Clause] = List.empty
   
   
   
   
      
    
    def createOriginalCircuit(e : Expr) : Literal = {
        var retVariable : Literal = e match {
            case Variable(name) => {
                var cacheLiteral = mapVarToInt.get(Variable(name))
                var tempLiteral = Literal.create(cacheLiteral.get)
               
                tempLiteral
            }
            case And(args) => {
                var temp : List[Literal] = List.empty
                var finalClauseArg : List[Literal] = List.empty
                var newVar : Literal = Literal.create(countVariable)
                countVariable += 1
                for(arg <- args){
                    var gateVar = createOriginalCircuit(arg)
                    temp = temp :+ gateVar
                    finalClauseArg = finalClauseArg :+ ~gateVar
                }
                finalClauseArg = finalClauseArg :+ newVar
                for(tempVar <- temp){
                    allClausesOriginal = allClausesOriginal :+ Clause(List(~newVar,tempVar))
                }
                allClausesOriginal = allClausesOriginal :+ Clause(finalClauseArg)
                newVar
            }
            case Or(args) => {
                var temp : List[Literal] = List.empty
                var finalClauseArg : List[Literal] = List.empty
                var newVar : Literal = Literal.create(countVariable)
                countVariable += 1
                for(arg <- args){
                    var gateVar = createOriginalCircuit(arg)
                    temp = temp :+ gateVar
                    finalClauseArg = finalClauseArg :+ gateVar
                }
                finalClauseArg = finalClauseArg :+ ~newVar
                for(tempVar <- temp){
                    allClausesOriginal = allClausesOriginal :+ Clause(List(newVar,~tempVar))
                }
                allClausesOriginal = allClausesOriginal :+ Clause(finalClauseArg)
                newVar
            }
            case Not(arg) => {
                var newVar : Literal = Literal.create(countVariable)
                countVariable += 1
                var gateVar = createOriginalCircuit(arg)
                allClausesOriginal = allClausesOriginal :+ Clause(List(~newVar,~gateVar))
                allClausesOriginal = allClausesOriginal :+ Clause(List(newVar,gateVar))
                newVar
            }
        }
        
        retVariable
    }
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
    def checkEquivalenceOfCircuits(actualCircuit: Expr, givenCircuitEval: (Map[Variable, Boolean]) => Option[Boolean]): Boolean =
    {
        countVariable = 1
        mapVarToInt = HashMap.empty
        allClauses = List.empty
        selectVariables = List.empty
        allClausesOriginal = List.empty
        
        
        
        var duplicateVariable = createMasterCircuit(actualCircuit)
        var originalCircuitVariable = createOriginalCircuit(actualCircuit)
        //println(duplicateVariable,originalCircuitVariable,countVariable)
        
        var tempXorClauses = allClauses ++ allClausesOriginal
        
        println(selectVariables.length)
        for(selectVariable <- selectVariables){
            var xorClauses = tempXorClauses
            for(tempSelectVariable <- selectVariables){
                if(tempSelectVariable == selectVariable){
                    xorClauses = xorClauses :+ Clause(List(tempSelectVariable))
                }else{
                    xorClauses = xorClauses :+ Clause(List(~tempSelectVariable))
                }
            }
            
            var newVar1 : Literal = Literal.create(countVariable)
            countVariable += 1
            xorClauses = xorClauses :+ Clause(List(~newVar1,~originalCircuitVariable))
            xorClauses = xorClauses :+ Clause(List(newVar1,originalCircuitVariable))
            
            var newVar2 : Literal = Literal.create(countVariable)
            countVariable += 1
            xorClauses = xorClauses :+ Clause(List(~newVar2,newVar1))
            xorClauses = xorClauses :+ Clause(List(~newVar2,duplicateVariable))
            xorClauses = xorClauses :+ Clause(List(newVar2,~duplicateVariable,~newVar1))
            
            var newVar3 : Literal = Literal.create(countVariable)
            countVariable += 1
            xorClauses = xorClauses :+ Clause(List(~newVar3,~duplicateVariable))
            xorClauses = xorClauses :+ Clause(List(newVar3,duplicateVariable))
              
            var newVar4 : Literal = Literal.create(countVariable)
            countVariable += 1
            xorClauses = xorClauses :+ Clause(List(~newVar4,newVar3))
            xorClauses = xorClauses :+ Clause(List(~newVar4,originalCircuitVariable))
            xorClauses = xorClauses :+ Clause(List(newVar4,~originalCircuitVariable,~newVar3))
            
            
            
            var newVar5 : Literal = Literal.create(countVariable)
            countVariable += 1
            xorClauses = xorClauses :+ Clause(List(~newVar2,newVar5))
            xorClauses = xorClauses :+ Clause(List(~newVar4,newVar5))
            xorClauses = xorClauses :+ Clause(List(newVar2,newVar4,~newVar5))
            xorClauses = xorClauses :+ Clause(List(newVar5))
            
            
            //println(xorClauses)
            val S = new Solver()
            for(clause <- xorClauses){
                S.addClause(clause)
            }
            val isSAT = S.solve()
            var resultMap : Map[Variable, Boolean] = Map.empty
            if (isSAT){
                for((x,y) <- mapVarToInt){
                    resultMap = resultMap + (x -> S.modelValue(y))
                }
            }
            if(Evaluation.evaluate(actualCircuit, resultMap) != givenCircuitEval(resultMap)){
                return false
            }
            
        }
        
        true
    }



}
