import java.security.{MessageDigest => MD}
import java.nio.ByteBuffer

abstract class Expr {
  def computeDigest() : Array[Byte]
  val digest : Array[Byte] = computeDigest()
  override val hashCode : Int = ByteBuffer.wrap(digest.slice(0, 4)).getInt
  
  def hash(baseName : String, args: List[Expr]) : Array[Byte] = {
    val md = MD.getInstance("SHA-256")
    md.reset()
    md.update(baseName.getBytes("UTF-8"))
    args.foreach { arg => md.update(arg.digest) }
    md.digest()
  }

  override def equals(that : Any) : Boolean = {
    that match {
      case e : Expr => hashCode == that.hashCode && MD.isEqual(digest, e.digest)
      case _        => false
    }
  }
}

case class BoolLit(value: Boolean) extends Expr {
    override def toString = value.toString()
    override def computeDigest() : Array[Byte] = hash("$" + value.toString(), List.empty)
}

case class Variable(name: String) extends Expr {
    override def toString(): String = name
    override def computeDigest() : Array[Byte] = hash("_VAR" + name, List.empty)
} 

case class And(args: List[Expr]) extends Expr {
    override def toString(): String = {
        args.map(x => x.toString()).mkString("("," & ",")")
    }
    override def computeDigest() : Array[Byte] = hash("#AND", args)
}

case class Or(args: List[Expr]) extends Expr {
    override def toString(): String = {
        args.map(x => x.toString()).mkString("("," | ",")")
    }
    override def computeDigest() : Array[Byte] = hash("#OR", args)
}

case class Not(arg: Expr) extends Expr {
    override def toString(): String = {
        "~" + "(" + arg.toString() + ")"
    }
    override def computeDigest() : Array[Byte] = hash("#NOT", List(arg))
}

////////////////////////////////////////////////
//   IGNORE THE CASE CLASSES BELOW THIS LINE  //
////////////////////////////////////////////////


case class GateVariable(name: String) extends Expr {
    override def toString(): String = name
    override def computeDigest() : Array[Byte] = hash("_GATEVAR" + name, List.empty)
}


case class Buf(arg: Expr) extends Expr {
    override def toString(): String = {
        "(" + arg.toString() + ")"
    }
    override def computeDigest() : Array[Byte] = hash("#BUF", List(arg))
}
case class Nand(args: List[Expr]) extends Expr {
    override def toString(): String = {
        args.map(x => x.toString()).mkString("~("," & ",")")
    }
    override def computeDigest() : Array[Byte] = hash("#NAND", args)
}
case class Nor(args: List[Expr]) extends Expr {
    override def toString(): String = {
        args.map(x => x.toString()).mkString("~("," | ",")")
    }
    override def computeDigest() : Array[Byte] = hash("#NOR", args)
}

case class Xor(args: List[Expr]) extends Expr {
    override def toString(): String = {
        args.map(x => x.toString()).mkString("("," ^ ",")")
    }
    override def computeDigest() : Array[Byte] = hash("#XOR", args)
}


abstract class Statement 


case class VariableDeclaration(varname: Variable, typ: Boolean) extends Statement {
    override def toString = "var: " + ( if (typ == true) "input" else "output") + " " + varname.toString + "\n"
}


case class GateDefinition(lhs: GateVariable, rhs: Expr) extends Statement {
    override def toString = lhs.toString() + " = " + rhs.toString() + "\n"
}
