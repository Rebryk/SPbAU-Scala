package ru.spbau.mit.wiki

trait TreeNode {
  val name: String

  def find(name: String): Option[TreeNode]
}

case class SectionNode(name: String, childNodes: List[TreeNode] = List.empty) extends TreeNode {
  override def find(name: String): Option[TreeNode] = {
    if (this.name == name) {
      Some(this)
    } else {
      this.childNodes.map(_.find(name)).find(_.isDefined) match {
        case Some(node) => node
        case _ => None
      }
    }
  }
}

case class PageNode(name: String, title: String, version: String) extends TreeNode {
  override def find(name: String): Option[TreeNode] = {
    if (this.name == name) {
      Some(this)
    } else {
      None
    }
  }
}