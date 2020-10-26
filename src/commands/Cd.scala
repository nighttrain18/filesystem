package commands
import files.{DirEntry, Directory}
import filesystem.State
import scala.collection.mutable.Stack

import scala.annotation.tailrec

class Cd(dirName: String) extends Command {
  override def apply(state: State): State = {
    val root = state.root
    val absolutePath = {
      if(dirName.startsWith(Directory.SEPARATOR)) dirName
      else {
        val isSeparatorNeeded = state.workingDirectory.path != Directory.ROOT.path
        val separator = if(isSeparatorNeeded) Directory.SEPARATOR else ""
        s"${state.workingDirectory.path}$separator$dirName"
      }
    }
    var tokens = absolutePath
      .split(Directory.SEPARATOR)
      .toList
    tokens = if (tokens.head == "") tokens.tail else tokens
    val collapsedTokens = collapseRelativeTokens(tokens)
    try {
      val destinationDirectory = root.findDescendant(collapsedTokens)
      State(root, destinationDirectory)
    }
    catch {
      case _: Exception => state.setMessage(dirName + ": no such directory")
    }
  }

  def collapseRelativeTokens(path: List[String]): List[String] = {
    def helper(path: List[String], result: Stack[String]): List[String] = {
      if(path.isEmpty) result.toList.reverse
      else if(path.head.equals(".")) helper(path.tail, result)
      else if(path.head.equals("..")) {
        if(result.isEmpty) helper(path.tail, result)
        else helper(path.tail, result.drop(1))
      }
      else helper(path.tail, result.push(path.head))
    }

    helper(path, Stack())
  }
}
