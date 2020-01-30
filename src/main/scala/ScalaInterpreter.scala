import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map


object ScalaInterpreter 
{

    var inputVarSet = Set[Variable]()
    var outputVarMap = Map[String, Expr]()
    var outputVarSet = Set[String]()
    var gateMap = Map[String, Expr]()
    var outputVarCounter = Map[Int, String]()

    def declare(variable: Variable, typ: Boolean) = {
        def declareInp = {
            inputVarSet += variable
            gateMap += ( variable.name -> variable)
        }

        def declareOut = {
            outputVarSet += variable.name
            val siz = outputVarCounter.size
            outputVarCounter += (siz -> variable.name)
        }

        if(typ == true) declareInp else declareOut
    }

    def expandExpr(e:Expr): Expr = {
        def getExpr(name: String) = {
            val exp = gateMap.get(name)
            exp match {
            case Some(gate) => gate
            case None => 
                val outexp = outputVarMap.get(name)
                outexp match {
                    case Some(value) => value
                    case None => throw new Error("Variable doesn't have corresponding value")
                } 
            }
        }

        e match {
        case Not(GateVariable(value)) => 
            Not(getExpr(value))
        case And(args) => 
            And(args.map( x => getExpr(x.asInstanceOf[GateVariable].name)))
        case Or(args) => 
            Or(args.map( x => getExpr(x.asInstanceOf[GateVariable].name)))
        case Buf(GateVariable(value)) => 
            getExpr(value)
        case Nand(args) =>
            Not(And(args.map( x => getExpr(x.asInstanceOf[GateVariable].name))))
        case Nor(args) =>
            Not(Or(args.map( x => getExpr(x.asInstanceOf[GateVariable].name))))
        case Xor(args) =>
            val a = getExpr(args(0).asInstanceOf[GateVariable].name)
            val b = getExpr(args(1).asInstanceOf[GateVariable].name)
            Or(List(And(List(Not(a), b)), And(List(a, Not(b)))))
        }
    }

    def addgate(lhs: GateVariable, rhs: Expr) {
        val expansion = expandExpr(rhs)
        val isOutputVar = outputVarSet contains(lhs.name) 
        if(isOutputVar) 
            outputVarMap += ( lhs.name -> expansion)
        else 
            gateMap += (lhs.name -> expansion)
        
    }

    def interpretStmt(code: List[Statement]): Unit = {
        if(!code.isEmpty) {
            code.head match {
                case VariableDeclaration(variable, typ) => declare(variable, typ)
                case GateDefinition(lhs, rhs) => addgate(lhs, rhs)
            }
            interpretStmt(code.tail)
        }
    }

    def cleanup = {
        inputVarSet = Set[Variable]()
        outputVarMap =  Map[String, Expr]()
        outputVarSet = Set[String]()
        gateMap = Map[String, Expr]()
        outputVarCounter = Map[Int, String]()
    }
}