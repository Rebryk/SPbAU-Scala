package ru.spbau.mit.wiki

trait WikiParser {
  def loadContent(url: String): TreeNode
}