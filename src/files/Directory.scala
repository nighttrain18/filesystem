package files

import filesystem.FileSystemException

import scala.annotation.tailrec

class Directory(
                 override val parentPath: String,
                 override val name: String,
                 val contents: List[DirEntry]
               )
  extends DirEntry(parentPath, name) {
  def hasEntry(name: String): Boolean = findEntry(name) != null

  def getAllFoldersInPath: List[String] =
    path
      .substring(1)
      .split(Directory.SEPARATOR)
      .toList
      .filter(x => !x.isEmpty)

  def findDescendant(path: List[String]): Directory =
    if(path.isEmpty) this
    else findEntry(path.head).asDirectory.findDescendant(path.tail)

  def addEntry(newEntry: DirEntry): Directory =
    new Directory(parentPath, name, contents :+ newEntry)

  def findEntry(entryName: String): DirEntry = {
    @tailrec
    def findEntryHelper(name: String, contentList: List[DirEntry]): DirEntry =
      if(contentList.isEmpty) null
      else if (contentList.head.name == name) contentList.head
      else findEntryHelper(name, contentList.tail)

    findEntryHelper(entryName, contents)
  }

  def replaceEntry(entryName: String, newEntry: DirEntry): Directory = {
    val newContents = contents.filter(dirEntry => dirEntry.name != entryName) :+ newEntry
    new Directory(parentPath, name, newContents)
  }

  override def asDirectory: Directory = this

  override def getType: String = "Directory"

  override def asFile: File =
    throw new FileSystemException("A directory cannot be converted to a file!")

  def isRoot: Boolean = parentPath.isEmpty

  override def isDirectory: Boolean = true

  override def isFile: Boolean = false
}

object Directory {
  val SEPARATOR = "/"
  val ROOT_PATH = "/"

  def ROOT: Directory = Directory.empty("", "")
  def empty(parentPath: String, name: String) = {
    new Directory(parentPath, name, List())
  }
}
