import scala.collection.mutable.HashMap
import scala.util.control.Breaks._



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
   
    def checkEquivalenceOfCircuits(actualCircuit: Expr, givenCircuitEval: (Map[Variable, Boolean]) => Option[Boolean]): Boolean =
    {

        var fianlVariable = createMasterCircuit(actualCircuit)
        allClauses = allClauses :+ Clause(List(fianlVariable))
        println(selectVariables)
        
        val S1 = new Solver()
        for(clause <- allClauses){
            S1.addClause(clause)
        }
        
        
        for(selectVariable <- selectVariables){
            S1.addClause(Clause(List(~selectVariable)))
        }
        
        var isSAT = S1.solve()
        for(selectVariable <- selectVariables){
            val S2 = new Solver()
            for(clause <- allClauses){
                S2.addClause(clause)
            }
        
        
            for(tempSelectVariable <- selectVariables){
                if(tempSelectVariable == selectVariable){
                    S2.addClause(Clause(List(tempSelectVariable)))
                }else{
                    S2.addClause(Clause(List(~tempSelectVariable)))
                }
            }
            
            var isSATS2 = S2.solve()
            
            breakable{
                while(isSATS2){
                    var blockingClause : List[Literal] = List.empty
                    var tempAssignment : Map[Variable,Boolean] = Map.empty
                    for((a,b) <- mapVarToInt){
                        tempAssignment = tempAssignment + (a -> S2.modelValue(b))
                        if(S2.modelValue(b)){
                            blockingClause = blockingClause :+ ~Literal.create(b)
                        }else{
                            blockingClause = blockingClause :+ Literal.create(b)
                        }
                    }
                    if(!Evaluation.evaluate(actualCircuit,tempAssignment).get){
                        if(givenCircuitEval(tempAssignment).get)
                            return false
                    }
                        S2.addClause(Clause(blockingClause))
                        //println(tempAssignment)  
                        //println(Evaluation.evaluate(actualCircuit,tempAssignment))
                        //println(S.modelValue(fianlVariable))
                        isSATS2 = S2.solve()
                        
                    }
                }
            }
        
        println(isSAT)
        
        
        
        
        true
    }



}
