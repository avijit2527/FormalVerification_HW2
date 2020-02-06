import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList

object CNFConverter{
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
                    //println((Variable(name) , countVariable))
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
    def toCNF(e : Expr, countVar : Int) : (List[Clause], Int, HashMap[Variable,Int]) = {
        mapVarToInt = HashMap.empty
        countVariable = countVar
        allClauses = MutableList.empty
        var result = toConjunction(e)
        allClauses += Clause(List(result))
        (allClauses.toList,countVariable,mapVarToInt)
    }
    
    
    
    
    
}
