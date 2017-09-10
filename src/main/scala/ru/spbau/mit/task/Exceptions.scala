package ru.spbau.mit.task

/**
  * Class implements calculator base exception
  *
  * @param message -- the detail message
  */
class BaseException(message: String) extends RuntimeException(message)

class FatalException(message: String) extends BaseException(message)

class ParserException(message: String) extends BaseException(message)