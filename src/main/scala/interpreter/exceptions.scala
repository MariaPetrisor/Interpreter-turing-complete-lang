package interpreter

case class expectedIntegerException(message: String)  extends Exception(message)
case class emptyListException(message: String)  extends Exception(message)
