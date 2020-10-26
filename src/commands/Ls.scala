package commands
import files.DirEntry
import filesystem.State

class Ls extends Command {
  override def apply(state: State): State = {
    val contents = state.workingDirectory.contents
    val contentOutput = createContentOutput(contents)
    state.setMessage(contentOutput)
  }

  def createContentOutput(contents: List[DirEntry]): String = {
    def helper(contents: List[DirEntry], acc: String): String =
      if(contents.isEmpty) acc
      else {
        val newLine = if(!contents.tail.isEmpty) "\n" else ""
        helper(contents.tail, s"$acc ${contents.head.name}[${contents.head.getType}] $newLine")
      }

    helper(contents, "")
  }
}
