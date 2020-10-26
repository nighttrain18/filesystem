package filesystem
import java.util.Scanner

import commands.{Command, Mkdir, Touch, Cd}
import files.Directory

object Filesystem extends App {
  val root = Directory.ROOT
  var state = State(root, root)
  val commandFlow: List[Command] = List(
    new Mkdir("folderA"),
    new Mkdir("folderB"),
    new Touch("file1"),
    new Cd("folderA"),
    new Mkdir("folderZ"),
    new Touch("file2"),
    new Cd("..")
  )
  state = commandFlow.foldLeft(state)((state, command) => command.apply(state))
  val scanner = new Scanner(System.in)
  while(true) {
    state.show
    val input = scanner.nextLine()
    state = Command
      .from(input)
      .apply(state)
  }
}
