package filesystem

import files.Directory

class State(
             val root: Directory,
             val workingDirectory: Directory,
             val output: String
           ) {
  def show: Unit = {
    if(!output.isEmpty) println(output)
    print(workingDirectory.path + State.SHELL_TOKEN)
  }

  def setMessage(message: String): State =
    State(root, workingDirectory, message)
}

object State {
  val SHELL_TOKEN = "$ "

  def apply(root: Directory, workingDirectory: Directory, output: String = "") =
    new State(root, workingDirectory, output)

}
