import scala.util.Try
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.token.StdTokens
object ScalaParser extends StdTokenParsers with StdTokens with PackratParsers {

    type Tokens = StdLexical
    val lexical = new StdLexical

    lexical.delimiters ++= List("=", "(", ")", ",", "#")
    lexical.reserved ++= List("INPUT", "OUTPUT", "AND", "OR", "NOT", "BUF", "NAND", "NOR","XOR")

    def parseExpression(text: String) =    
    {
                val tokens = new PackratReader(new lexical.Scanner(text))
                phrase(expr)(tokens) match 
                {
                case Success(exp, _) => Some(exp)
                case NoSuccess(msg, next) => 
                    println("Data Type Mismatched")
                    None
                }
    }

    lazy val expr: PackratParser[Expr] = term

    lazy val term: PackratParser[Expr] =  gate | gateId | identifier 

    lazy val gateId: PackratParser[GateVariable] = ident ^^ {  n => GateVariable(n) }

    lazy val identifier: PackratParser[Variable] = ident ^^ {  n => Variable(n) }

    lazy val gate: PackratParser[Expr] = andgate | orgate | notgate | bufgate | nandgate | norgate | xorgate

    lazy val andgate: PackratParser[And] = "AND" ~ "(" ~  gateId ~ "," ~  gateId  ~ opt(rep("," ~> gateId)) <~ ")" ^^ {
        case a ~ "(" ~ c ~ "," ~ e ~ f => f match {
            case Some(values) => And(List(c,e) ++ values)
            case None => And(List(c, e))
        }   
    }


    lazy val orgate: PackratParser[Or] = "OR" ~ "(" ~  gateId ~ "," ~ gateId  ~ opt(rep("," ~> gateId)) <~ ")" ^^ {
        case a ~ "(" ~ c ~ "," ~ e ~ f => f match {
            case Some(values) => Or(List(c,e) ++ values)
            case None => Or(List(c, e))
        }   
    }


    lazy val notgate: PackratParser[Not] = "NOT" ~ "(" ~  gateId  <~ ")" ^^ {
        case a ~ "(" ~ c => 
                Not(c)
    }

    lazy val bufgate: PackratParser[Buf] = "BUF" ~ "(" ~   gateId  <~ ")" ^^ {
        case a ~ "(" ~ c => 
                Buf(c)
    }

    lazy val nandgate: PackratParser[Nand] = "NAND" ~ "(" ~  gateId ~ "," ~  gateId  ~ opt(rep("," ~> gateId)) <~ ")" ^^ {
        case a ~ "(" ~ c ~ "," ~ e ~ f => f match {
            case Some(values) => Nand(List(c,e) ++ values)
            case None => Nand(List(c, e))
        }   
    }

    lazy val xorgate: PackratParser[Xor] = "XOR" ~ "(" ~ gateId ~ "," ~ gateId <~ ")" ^^ {
        case a ~ "(" ~ c ~ "," ~ e =>
            Xor(List(c, e))
    }


    lazy val norgate: PackratParser[Nor] = "NOR" ~ "(" ~  gateId ~ "," ~ gateId  ~ opt(rep("," ~> gateId)) <~ ")" ^^ {
        case a ~ "(" ~ c ~ "," ~ e ~ f => f match {
            case Some(values) => Nor(List(c,e) ++ values)
            case None => Nor(List(c, e))
        }   
    }


    def parseStatement(text: String) = 
    {
        val tokens = new PackratReader(new lexical.Scanner(text))
        phrase(statement)(tokens) match 
        {
            case Success(exp, _) => Some(exp)
            case NoSuccess(msg, next) =>
                                    println("Error: DATA MISMATCH " + msg.toString())
                                    None
        }
    }

    lazy val statement: PackratParser[List[Statement]] = blockstatement

    lazy val blockstatement : PackratParser[List[Statement]] = rep(stmt)  ^^ { x => x}

    lazy val stmt: PackratParser[Statement] = vardecl | gatedef

    lazy val vardecl: PackratParser[VariableDeclaration] = ( "INPUT" | "OUTPUT") ~ "(" ~ identifier <~ ")" ^^ {
        case a ~ "(" ~ c => a match {
            case "INPUT" => VariableDeclaration(c, true)
            case "OUTPUT" => VariableDeclaration(c, false)
        }
    }

    lazy val gatedef: PackratParser[GateDefinition] = gateId ~ "=" ~ expr ^^ {
        case a ~ "=" ~ b => GateDefinition(a, b)
    }


}