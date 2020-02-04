import scala.collection.mutable.HashMap


object CNFConverter{
    var countVariable = 1
    var allClauses : List[Clause] = List.empty
    var mapVarToInt : HashMap[Variable,Int] = HashMap.empty
    
    

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
    
    
   
    
    
    
    def toConjunction(e : Expr) : Literal = {
        var retVariable : Literal = e match {
            case Variable(name) => {
                var tempLiteral = Literal.create(countVariable)
                mapVarToInt += (Variable(name) -> countVariable)
                countVariable += 1
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
                    allClauses = allClauses :+ Clause(List(~newVar,tempVar))
                }
                allClauses = allClauses :+ Clause(finalClauseArg)
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
                    allClauses = allClauses :+ Clause(List(newVar,~tempVar))
                }
                allClauses = allClauses :+ Clause(finalClauseArg)
                newVar
            }
            case Not(arg) => {
                var newVar : Literal = Literal.create(countVariable)
                countVariable += 1
                var gateVar = toConjunction(arg)
                allClauses = allClauses :+ Clause(List(~newVar,~gateVar))
                allClauses = allClauses :+ Clause(List(newVar,gateVar))
                newVar
            }
        }
        
        retVariable
    }
    def toCNF(e : Expr, countVar : Int) : (List[Clause], Int, HashMap[Variable,Int]) = {
        mapVarToInt = HashMap.empty
        countVariable = countVar
        allClauses = List.empty
        var result = toConjunction(e)
        allClauses = allClauses :+ Clause(List(result))
        (allClauses,countVariable,mapVarToInt)
    }
    
    
    
    
    
}
