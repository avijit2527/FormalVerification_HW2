


object AllCNFConverter{
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
    def toCNF(e : Expr, countVar : Int) : (List[Expr], Int) = {
        countVariable = countVar
        allClauses = List.empty
        var result = toConjunction(e)
        allClauses = allClauses :+ Or(List(result))
        (allClauses,countVariable)
    }
    
}
