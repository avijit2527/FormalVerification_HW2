


object CNFConverter{
    var countVariable = 1
    var allClauses : List[Expr] = List.empty
    
    

    def simplify(e: Expr) : Expr = { 
        ///////////////////////////////////////////////////////////////
        // TODO - This function should return the simplified         //
        // expression e reduces to given the simplification rules    //
        // mentioned in the assignment document                      //
        ///////////////////////////////////////////////////////////////

        e match {        
            case Variable(name) => Variable(name)
            
            case BoolLit(value) => BoolLit(value)
            
            case And(args) => {
                if(args.length == 0){return BoolLit(true)}

                for (a <- args){
                    if (args.contains(Not(a)) || a == BoolLit(false)) {return BoolLit(false)}
                } 
                var unique_args = args
                
                unique_args = args.filter(_ != BoolLit(true))
                unique_args = unique_args.distinct
                var argumentList : List[Expr] = List.empty[Expr]
                for (a <- unique_args){
                    argumentList = argumentList :+ simplify(a)
                } 
                

                if (e == And(argumentList)){
                    if (argumentList.length == 1){
                        return argumentList(0)
                    }else{
                        return And(argumentList)
                    }
                }
                var simlifiedArgument = simplify(And(argumentList))
                return simlifiedArgument                              
            }
            
            
            case Or(args) =>  {
                if(args.length == 0){return BoolLit(false)}
                for (a <- args){
                    if (args.contains(Not(a)) || a == BoolLit(true)) {return BoolLit(true)}                
                } 
                var unique_args = args
                
                unique_args = args.filter(_ != BoolLit(false))
                unique_args = unique_args.distinct
                var argumentList : List[Expr] = List.empty[Expr]
                for (a <- unique_args){
                    argumentList = argumentList :+ simplify(a)
                } 
                
                


                if (e == Or(argumentList)){
                    if (argumentList.length == 1){
                        return argumentList(0)
                    }else{
                        return Or(argumentList)
                    }
                }
                var simlifiedArgument = simplify(Or(argumentList))
                return simlifiedArgument                              
            }
            
            case Not(arg) => {
                var ret = arg match {
                    case Not(s) => s
                    case BoolLit(value) => BoolLit(!value)
                    case Variable(name) => Not(Variable(name))
                    case _ => Not(simplify(arg))
                }
                if (e == ret){                
                    return ret
                }
                var simplified = simplify(ret)
                return simplified
            }

        }
    }
    
    
   
    
    
    
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
