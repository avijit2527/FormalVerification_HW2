
object Circuit
{

    def checkEquivalenceOfCircuits(actualCircuit: Expr, givenCircuitEval: (Map[Variable, Boolean]) => Option[Boolean]): Boolean =
    {

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
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

        //Take a look at CircuitTest.scala for a sample test.

        def findVariablesInExpr(e: Expr): Set[Variable] = e match {
            case v : Variable =>  Set(v)
            case And(args) => args.map(findVariablesInExpr(_)).foldLeft(Set.empty[Variable])(_ ++ _)
            case Or(args) => args.map(findVariablesInExpr(_)).foldLeft(Set.empty[Variable])(_ ++ _)
            case Not(arg) => findVariablesInExpr(arg)
            case BoolLit(lit) => Set.empty[Variable]
        }

        //  It assigns boolean values to variables and checks if evaluation of e and provided Circuit is the same
        //  for all assignments. This blows up for large testcases.
        def compareTruthTable(vars: Set[Variable], m: Map[Variable, Boolean], e: Expr): Boolean = {
            if(!vars.isEmpty) 
            {
                val variable = vars.head
                compareTruthTable(vars.tail, m + ( variable -> true), e) &&
                compareTruthTable(vars.tail, m + ( variable -> false), e)
            }
            else if (Evaluation.evaluate(e,m) == givenCircuitEval(m)) true else false
        }

        val simplifiedCircuit = Evaluation.simplify(actualCircuit)
        val vars = findVariablesInExpr(simplifiedCircuit)
        compareTruthTable(vars, Map.empty[Variable, Boolean],simplifiedCircuit)

    }



}