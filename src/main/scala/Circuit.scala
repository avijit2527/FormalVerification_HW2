import scala.collection.mutable.HashMap
import scala.util.control.Breaks._
import scala.collection.mutable.MutableList




object Circuit
{
    var countVariable = 1
    var allClauses : MutableList[Clause] = MutableList.empty
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
                    allClauses += Clause(List(~newVarAnd,tempVar))
                    allClauses += Clause(List(newVarOr,~tempVar))
                }
                allClauses += Clause(finalClauseArgAnd)
                allClauses += Clause(finalClauseArgOr)
                
                
                var selectVar : Literal = Literal.create(countVariable)
                countVariable += 1
                selectVariables = selectVariables :+ selectVar
                var outputVar : Literal = Literal.create(countVariable)
                countVariable += 1
                
                allClauses += Clause(List(selectVar,newVarAnd,~outputVar))
                allClauses += Clause(List(selectVar,~newVarAnd,outputVar))
                allClauses += Clause(List(~selectVar,newVarOr,~outputVar))
                allClauses += Clause(List(~selectVar,~newVarOr,outputVar))
                
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
                    allClauses += Clause(List(newVar,~tempVar))
                }
                allClauses += Clause(finalClauseArg)
                newVar
            }
            case Not(arg) => {
                var newVar : Literal = Literal.create(countVariable)
                countVariable += 1
                var gateVar = createMasterCircuit(arg)
                allClauses += Clause(List(~newVar,~gateVar))
                allClauses += Clause(List(newVar,gateVar))
                newVar
            }
        }
        
        retVariable
        
    }
   
   
   
   
   
   
   
   
    var allClausesOriginal : MutableList[Clause] = MutableList.empty
   
   
   
   
      
    
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
                    allClausesOriginal += Clause(List(~newVar,tempVar))
                }
                allClausesOriginal += Clause(finalClauseArg)
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
                    allClausesOriginal += Clause(List(newVar,~tempVar))
                }
                allClausesOriginal += Clause(finalClauseArg)
                newVar
            }
            case Not(arg) => {
                var newVar : Literal = Literal.create(countVariable)
                countVariable += 1
                var gateVar = createOriginalCircuit(arg)
                allClausesOriginal += Clause(List(~newVar,~gateVar))
                allClausesOriginal += Clause(List(newVar,gateVar))
                newVar
            }
        }
        
        retVariable
    }
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
    def checkEquivalenceOfCircuits(actualCircuit: Expr, givenCircuitEval: (Map[Variable, Boolean]) => Option[Boolean]): Boolean =
    {
        countVariable = 1
        mapVarToInt = HashMap.empty
        allClauses = MutableList.empty
        selectVariables = List.empty
        allClausesOriginal = MutableList.empty
        
        
        
        var duplicateVariable = createMasterCircuit(actualCircuit)
        var originalCircuitVariable = createOriginalCircuit(actualCircuit)
        
        var orListSelectVar : MutableList[Literal] = MutableList.empty
        
        for(selectVariable1 <- selectVariables){
            var finalClauseLiterals : MutableList[Literal] = MutableList.empty
            var newVarSelect : Literal = Literal.create(countVariable)
            countVariable += 1
            orListSelectVar += newVarSelect
            for(selectVariable2 <- selectVariables){
                if(selectVariable1 == selectVariable2){
                    allClauses += Clause(List(~newVarSelect,selectVariable2))
                    finalClauseLiterals += selectVariable2
                }else{
                    allClauses += Clause(List(~newVarSelect,~selectVariable2))
                    finalClauseLiterals += ~selectVariable2
                }
            }
            finalClauseLiterals += newVarSelect
            allClauses += Clause(finalClauseLiterals.toList)
            var newVarOrSelect : Literal = Literal.create(countVariable)
            countVariable += 1
        }
        
        
        
        
        var finalClauseOrLiterals : Literal = Literal.create(countVariable)
        countVariable += 1
        for(a <- orListSelectVar){
            allClauses += Clause(List(~a,finalClauseOrLiterals))
        }
        orListSelectVar += ~finalClauseOrLiterals
        allClauses += Clause(orListSelectVar.toList)
        allClauses += Clause(List(finalClauseOrLiterals))
        
        
        var xorClauses = allClauses ++ allClausesOriginal
        
        
        
        var newVar1 : Literal = Literal.create(countVariable)
        countVariable += 1
        xorClauses += Clause(List(~newVar1,~originalCircuitVariable))
        xorClauses += Clause(List(newVar1,originalCircuitVariable))
        
        var newVar2 : Literal = Literal.create(countVariable)
        countVariable += 1
        xorClauses += Clause(List(~newVar2,newVar1))
        xorClauses += Clause(List(~newVar2,duplicateVariable))
        xorClauses += Clause(List(newVar2,~duplicateVariable,~newVar1))
        
        var newVar3 : Literal = Literal.create(countVariable)
        countVariable += 1
        xorClauses += Clause(List(~newVar3,~duplicateVariable))
        xorClauses += Clause(List(newVar3,duplicateVariable))
              
        var newVar4 : Literal = Literal.create(countVariable)
        countVariable += 1
        xorClauses += Clause(List(~newVar4,newVar3))
        xorClauses += Clause(List(~newVar4,originalCircuitVariable))
        xorClauses += Clause(List(newVar4,~originalCircuitVariable,~newVar3))
           
            
            
        var newVar5 : Literal = Literal.create(countVariable)
        countVariable += 1
        xorClauses += Clause(List(~newVar2,newVar5))
        xorClauses += Clause(List(~newVar4,newVar5))
        xorClauses += Clause(List(newVar2,newVar4,~newVar5))
        xorClauses += Clause(List(newVar5))
            
            
        
        
        val S = new Solver()
        for(clause <- xorClauses){
            S.addClause(clause)
        }
        
        var isSAT = S.solve()
        while(isSAT){
            var resultMap : Map[Variable, Boolean] = Map.empty
            if (isSAT){
                for((x,y) <- mapVarToInt){
                    resultMap = resultMap + (x -> S.modelValue(y))
                }
            }
            if(Evaluation.evaluate(actualCircuit, resultMap) != givenCircuitEval(resultMap)){
                return false
            }
            
            var blockingClause : MutableList[Literal] = MutableList.empty
            for((x,y) <- mapVarToInt){
                if(S.modelValue(y)){
                    blockingClause += ~Literal.create(y)
                }else{
                    blockingClause += Literal.create(y)
                }
            }
            S.addClause(Clause(blockingClause.toList))
            
            isSAT = S.solve()
            
        }
        
        
        true
    }



}
