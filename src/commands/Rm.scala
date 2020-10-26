package commands
import files.Directory
import filesystem.{FileSystemException, State}

class Rm(name: String) extends Command {
  override def apply(state: State): State = {
    // /dir1/dir2/dir3 /dir1 dir1/dir2
    val absolutePath = {
      if(name.startsWith(Directory.SEPARATOR)) name
      else {
        val isSeparatorNeeded = state.workingDirectory.path != Directory.ROOT.path
        val separator = if(isSeparatorNeeded) Directory.SEPARATOR else ""
        s"${state.workingDirectory.path}$separator$name"
      }
    }
    if(checkIllegal(absolutePath)) state.setMessage("No such file or directory")
    else {
      rm(state, absolutePath)
    }
  }

  def rm(state: State, path: String): State = {
    def helper(root: Directory, path: List[String]): Directory = {
      if(path.length == 1) new Directory(root.parentPath, root.name, root.contents.filter(_.name != path.head))
      else {
        if(root.hasEntry(path.head)) {
          new Directory(
            root.parentPath,
            root.name,
            root.contents.map(c => {
              if(c.name == path.head) helper(c.asDirectory, path.tail)
              else c
            })
          )
        }
        else null
      }
    }

    if(path == Directory.SEPARATOR) state.setMessage("Cannot remove root directory")
    val nextWdPath = {
      val currentWdPath = state.workingDirectory.path
      if(isFileRemoving(path)) currentWdPath
      else if(currentWdPath == path) makeUpperDirectoryPath(path)
      else if(isPathDeeper(currentWdPath, path)) makeUpperDirectoryPath(path)
      else currentWdPath
    }
    val pathTokens = tokenizeAbsolutePath(path)
    val nextRoot = helper(state.root, pathTokens)
    if(nextRoot == null) state.setMessage("No such file or directory")
    val nextWdPathTokens = tokenizeAbsolutePath(nextWdPath)
    val nextWd = nextRoot.findDescendant(nextWdPathTokens)
    if(nextWd == null) state.setMessage("No such file or directory")
    else State(nextRoot, nextWd)
  }

  def isPathDeeper(comparablePath: String, etalonPath: String): Boolean = {
    val separatorCounter = (s: Char) => s == Directory.SEPARATOR
    if(etalonPath.startsWith(comparablePath)) {
      val comparablePathSeparatorsCount = comparablePath.count(separatorCounter)
      val etalonPathSeparatorsCount = etalonPath.count(separatorCounter)
      return etalonPathSeparatorsCount < comparablePathSeparatorsCount
    }
    false
  }

  def checkIllegal(name: String): Boolean = {
    name.contains(".")
  }

  def tokenizeAbsolutePath(path: String): List[String] =
    path
      .substring(1)
      .split(Directory.SEPARATOR)
      .toList

  def isFileRemoving(path: String): Boolean = tokenizeAbsolutePath(path).last.contains('.')

  def makeUpperDirectoryPath(path: String): String = {
    val tokens = tokenizeAbsolutePath(path)
    if(tokens.length == 1) path
    else tokens.dropRight(1).mkString(Directory.SEPARATOR)
  }
}
