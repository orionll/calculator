package temp

import scala.math._
import scala.annotation.tailrec

final class CalculatorException(msg: String, expr: String, loc: Int) extends RuntimeException {
  private val (subExpr, subLoc) = {
    val maxWidth = 30
    val right = loc + maxWidth/2
    val left = loc - maxWidth/2
    if (right >= expr.length) {
      val l = expr.length - maxWidth
      if (l < 0) (expr, loc) else ("..." + expr.substring(l, expr.length), loc - l + 3)
    } else if (left < 0) {
      val r = maxWidth
      if (r >= expr.length) (expr, loc) else (expr.substring(0, r) + "...", loc)
    } else {
      ("..." + expr.substring(left, right) + "...", loc - left + 3)
    }
  }
  
  override def getMessage = msg + String.format(":%n") + subExpr + String.format("%n") + (" " * subLoc) + "^"
}

sealed trait Token { def loc: Int }
case class Number(str: String, loc: Int) extends Token
case class Identifier(str: String, loc: Int) extends Token
case class Operator(c: Char, loc: Int) extends Token
case class OpenParen(loc: Int) extends Token
case class CloseParen(loc: Int) extends Token

sealed trait BinaryOperator
case object Assign extends BinaryOperator
case object Plus extends BinaryOperator
case object Minus extends BinaryOperator
case object Multiply extends BinaryOperator
case object Divide extends BinaryOperator
case object Power extends BinaryOperator

case class BinaryOperatorInfo(op: BinaryOperator, char: Char, precedence: Int, func: (String, Int, Double, Double) => Double)

sealed trait AST
case class ASTNumber(num: Double) extends AST
case class ASTIdentifier(i: String, loc: Int) extends AST
case class ASTBinaryOperator(op: BinaryOperator, l: AST, r: AST, loc: Int) extends AST
case class ASTFunction(i: String, arg: AST, loc: Int) extends AST

object Evaluator extends App {
  def divide(expr: String, loc: Int, l: Double, r: Double) = if (r == 0.0)
    throw new CalculatorException("Evaluation error. Division by zero", expr, loc)
    else l / r
  
  val binaryOperators: List[BinaryOperatorInfo] = List(
    BinaryOperatorInfo(Assign, '=', 0, (_, _, _, _) => ???),
    BinaryOperatorInfo(Plus, '+', 1, (_, _, l, r) => l + r),
    BinaryOperatorInfo(Minus , '-', 1, (_, _, l, r) => l - r),
    BinaryOperatorInfo(Multiply, '*', 2, (_, _, l, r) => l * r),
    BinaryOperatorInfo(Divide, '/', 2, divide),
    BinaryOperatorInfo(Power, '^', 3, (_, _, l, r) => pow(l, r))
  )
  
  val operators: List[Char] = binaryOperators.map(_.char)
  
  val minPrecedence: Int = binaryOperators.map(_.precedence).min
  def findBinOpInfoByChar(op: Char): BinaryOperatorInfo = binaryOperators.find(_.char == op).head
  def findBinOpInfoByOp(op: BinaryOperator): BinaryOperatorInfo = binaryOperators.find(_.op == op).head
  
  val functions: Map[String, Double => Double] = Map(
    ("exp", exp), ("sqrt", sqrt), ("log", log), ("sin", sin), ("tan", tan), ("cos", cos), ("abs", abs),
    ("asin", asin), ("atan", atan), ("acos", acos), ("sinh", sinh), ("tanh", tanh), ("cosh", cosh)
  )
  
  val constants: Map[String, Double] = Map(("pi", Pi), ("e", E))
  
  // Lexer
  
  def tokenLength(t: Token): Int = t match {
    case Number(n, _) => n.length
    case Identifier(i, _) => i.length
    case _ => 1
  }
  
  def isDigit(n: Char) = n >= '0' && n <= '9'
  def isLetter(c: Char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  def isOperator(c: Char) = operators.contains(c)
  
  def scanToken(expr: String, loc: Int, chars: List[Char]): Option[Token] = chars match {
    case ('(' :: _) => Some(OpenParen(loc))
    case (')' :: _) => Some(CloseParen(loc))
    case (c :: _) if isOperator(c) => Some(Operator(c, loc))
    case _ if isLetter(chars.head) => Some(Identifier(chars.takeWhile(isLetter).mkString, loc))
    case _ if isDigit(chars.head) => {
      val intPart = chars.takeWhile(isDigit).mkString
      val n = chars.drop(intPart.length) match {
        case ('.' :: d :: tail) if isDigit(d) => intPart + '.' + d + tail.takeWhile(isDigit).mkString
        case ('.' :: d :: _) => throw new CalculatorException("Lexical error. Unexpected symbol", expr, loc + intPart.length + 1)
        case ('.' :: _) => throw new CalculatorException("Lexical error. Unexpected end of input", expr, loc + intPart.length + 1)
        case _ => intPart
      }
      
      Some(Number(n, loc))
    }
    case _ => None
  }
  
  def tokenize(expr: String): List[Token] = {
    @tailrec
    def scan(loc: Int, chars: List[Char], tokens: List[Token]): List[Token] = chars match {
      case Nil => tokens
      case (' ' :: tail) => scan(loc + 1, tail, tokens)
      case _ => {
        val t = scanToken(expr, loc, chars).getOrElse(throw new CalculatorException("Lexical error. Unexpected symbol", expr, loc))
        val n = tokenLength(t)
        scan(loc + n, chars.drop(n), t :: tokens)
      }
    }
    
    scan(0, expr.toList, Nil).reverse
  }
  
  def parsePrimary(expr: String, tokens: List[Token]): (AST, List[Token]) = tokens match {
    case Nil => throw new CalculatorException("Syntax error. Unexpected end of input", expr, expr.length)
    case (Number(n, _) :: tail) => (ASTNumber(n.toDouble), tail)
    case (OpenParen(_) :: tail) => parseParens(expr, tail)
    case (Identifier(i, loc) :: OpenParen(_) :: tail) => {
      val (arg, newTail) = parseParens(expr, tail)
      (ASTFunction(i, arg, loc), newTail)
    }
    case (Identifier(i, loc) :: tail) => (ASTIdentifier(i, loc), tail)
    case (t :: _) => throw new CalculatorException("Syntax error. Unexpected token", expr, t.loc)
  }
  
  def parseParens(expr: String, tokens: List[Token]): (AST, List[Token]) = parseExpression(expr, tokens) match {
    case (ast, CloseParen(_) :: tail) => (ast, tail)
    case (_, t :: _) => throw new CalculatorException("Syntax error. Unexpected token", expr, t.loc)
    case _ => throw new CalculatorException("Syntax error. Expected ')'", expr, expr.length)
  }
  
  def parseExpression(expr: String, tokens: List[Token]): (AST, List[Token]) = {
    val (ast, tail) = parsePrimary(expr, tokens)
    parseExpression1(expr, ast, minPrecedence, tail)
  }
  
  @tailrec
  def innerLoop(expr: String, rhs: AST, opPrecedence: Int, tokens: List[Token]): (AST, List[Token]) = tokens match {
    case (Operator(op, _) :: _) => {
      val precedence = findBinOpInfoByChar(op).precedence
      if (precedence > opPrecedence) {
        val (newRhs, tail) = parseExpression1(expr, rhs, precedence, tokens)
        innerLoop(expr, newRhs, opPrecedence, tail)
      } else (rhs, tokens)
    }
    
    case _ => (rhs, tokens)
  }
  
  @tailrec
  def parseExpression1(expr: String, lhs: AST, minPrecedence: Int, tokens: List[Token]): (AST, List[Token]) = tokens match {
    case (Operator(op, loc) :: tail) => {
      val opInfo = findBinOpInfoByChar(op)
      if (opInfo.precedence >= minPrecedence) {
        val (rhs1, tail1) = parsePrimary(expr, tail)
        val (rhs2, tail2) = innerLoop(expr, rhs1, opInfo.precedence, tail1)
        parseExpression1(expr, ASTBinaryOperator(opInfo.op, lhs, rhs2, loc), minPrecedence, tail2)
      } else {
        (lhs, tokens)
      }
    }
    
    case _ => (lhs, tokens)
  }
  
  def parse(expr: String, tokens: List[Token]): AST = {
    val (ast, tail) = parseExpression(expr, tokens)
    tail match {
      case (t :: _) => throw new CalculatorException("Syntax error. Unexpected token", expr, t.loc)
      case _ => ast
    }
  }
  
  // Interpreter
  
  type VarTable = Map[String, Double]
  
  def calc(expr: String, varTable: VarTable, ast: AST): Double = ast match {
    case ASTNumber(d) => d
    case ASTBinaryOperator(Assign, _, _, loc) =>
      throw new CalculatorException("Evaluation error. Assignment operator cannot be used in expressions", expr, loc)
    case ASTBinaryOperator(op, l, r, loc) =>
      findBinOpInfoByOp(op).func(expr, loc, calc(expr, varTable, l), calc(expr, varTable, r))
    case ASTIdentifier(i, loc) => varTable.get(i).getOrElse(
      throw new CalculatorException("Evaluation error. Variable '" + i + "' is undefined", expr, loc))
    case ASTFunction(i, arg, loc) => functions.get(i).getOrElse(
      throw new CalculatorException("Evaluation error. Function '" + i + "' is undefined", expr, loc))(calc(expr, varTable, arg))
  }
  
  def evalAndUpdateTable(ast: AST, expr: String, ids: List[String], varTable: VarTable): (VarTable, Double) = {
    val value = calc(expr, varTable, ast)
    (ids.foldLeft(varTable)((table, i) => table + (i -> value)), value)
  }
}

// Stateful evaluator (maintains variable table)
final class Evaluator {
  import Evaluator._
  
  private var varTable: VarTable = constants
  
  private val it = List("it")
  
  def evaluate(expr: String): (Double, String) = {
    val tokens = tokenize(expr)
    val ast = parse(expr, tokens)
    
    def eval(ast: AST, ids: List[String]): (Double, String) = {
      val (modifiedVarTable, result) = evalAndUpdateTable(ast, expr, ids, varTable)
      varTable = modifiedVarTable
      (result, ids.head)
    }
    
    ast match {
      case ASTBinaryOperator(Assign, (ASTIdentifier(i, _)), rhs, _) => eval(rhs, i :: it)
      case _ => eval(ast, it)
    }
  }
  
  def reset(): Unit = {
    varTable = constants;
  }
}