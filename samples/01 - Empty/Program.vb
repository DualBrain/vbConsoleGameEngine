Imports VbConsoleGameEngine

Module Program

  Sub Main()
    Dim game As New Empty
    game.ConstructConsole(160, 80, 10, 10)
    game.Start()
  End Sub

End Module

Class Empty
  Inherits ConsoleGameEngine

  Public Sub New()
    m_appName = "Empty"
  End Sub

  Public Overrides Function OnUserCreate() As Boolean

    Return True

  End Function

  Public Overrides Function OnUserUpdate(elapsedTime As Single) As Boolean

    Return True

  End Function

End Class
