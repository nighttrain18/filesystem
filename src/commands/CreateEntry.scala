package commands
import files.{DirEntry, Directory}
import filesystem.State

abstract class CreateEntry(entryName: String) extends Command {
  override def apply(state: State): State = {
    val wd = state.workingDirectory
    if(wd.hasEntry(entryName)) {
      state.setMessage(s"Entry $entryName already exists!")
    } else if (entryName.contains(Directory.SEPARATOR)) {
      state.setMessage(s"$entryName mustn't contain separators!")
    } else if(checkIllegal(entryName)) {
      state.setMessage(s"$entryName: illegal entry name")
    } else {
      doCreateEntry(state, entryName)
    }
  }

  def createSpecificEntry(state: State): DirEntry

  def checkIllegal(name: String): Boolean = {
    name.contains(".")
  }

  def doCreateEntry(state: State, name: String): State = {
    def updateStructure(currentDirectory: Directory, path: List[String], newEntry: DirEntry): Directory = {
      if(path.isEmpty) currentDirectory.addEntry(newEntry)
      else {
        val oldEntry = currentDirectory.findEntry(path.head).asDirectory
        currentDirectory.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
      }
    }
    val wd = state.workingDirectory
    val allDirsInPath = wd.getAllFoldersInPath
    val newEntry: DirEntry = createSpecificEntry(state)
    // update the whole directory structure starting from the root
    // the directory structure is IMMUTABLE
    val newRoot = updateStructure(state.root, allDirsInPath, newEntry)
    val newWd = newRoot.findDescendant(allDirsInPath)
    State(newRoot, newWd)
  }
}
