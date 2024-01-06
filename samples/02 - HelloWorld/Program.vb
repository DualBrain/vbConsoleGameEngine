Imports VbConsoleGameEngine
Imports VbConsoleGameEngine.PixelType
Imports VbConsoleGameEngine.Color

Module Program

  Sub Main()
    Dim game As New HelloWorld
    game.ConstructConsole(160, 100, 4, 4)
    game.Start()
  End Sub

End Module

Class HelloWorld
  Inherits ConsoleGameEngine

  Private m_state As Integer = 0

  Private m_playerX As Single
  Private m_playerY As Single

  Public Sub New()
    m_appName = "Hello World"
  End Sub

  Public Overrides Function OnUserCreate() As Boolean

    m_playerX = 10
    m_playerY = 10

    Return True

  End Function

  Public Overrides Function OnUserUpdate(elapsedTime As Single) As Boolean

    If GetKey(VK_SPACE).Pressed Then
      m_state += 1
    End If

    Select Case m_state

      Case 0

        ' ------
        ' Random fill
        ' ------
        For x = 0 To ScreenWidth() - 1
          For y = 0 To ScreenHeight() - 1
            Draw(x, y, AscW("#"), Rand Mod 15)
          Next
        Next

      Case 1

        ' ------
        ' Color Swatch
        ' ------

        Fill(0, 0, ScreenWidth, ScreenHeight, AscW(" "), 0)
        For c = 0 To 15
          Fill(0, c * 6, 5, c * 6 + 5, Quarter, c)
          Fill(6, c * 6, 11, c * 6 + 5, Half, c)
          Fill(12, c * 6, 17, c * 6 + 5, ThreeQuarters, c)
          Fill(18, c * 6, 23, c * 6 + 5, Solid, c)
          Fill(24, c * 6, 29, c * 6 + 5, ThreeQuarters, c Or BgWhite)
          Fill(30, c * 6, 35, c * 6 + 5, Half, c Or BgWhite)
          Fill(36, c * 6, 41, c * 6 + 5, Quarter, c Or BgWhite)
        Next

      Case 2

        ' ------
        ' Simple character movement
        ' ------

        If m_keys(VK_LEFT).Held Then m_playerX -= 15 * elapsedTime
        If m_keys(VK_RIGHT).Held Then m_playerX += 15 * elapsedTime
        If m_keys(VK_UP).Held Then m_playerY -= 15 * elapsedTime
        If m_keys(VK_DOWN).Held Then m_playerY += 15 * elapsedTime

        Fill(0, 0, ScreenWidth, ScreenHeight, AscW(" "), 0)
        Fill(m_playerX, m_playerY, m_playerX + 5, m_playerY + 5)

      Case 3

        ' ------
        ' Mouse - https://youtu.be/tdqc9hZhHxM
        ' ------

        Cls()

        Draw(m_mousePosX - 1, m_mousePosY)
        Draw(m_mousePosX, m_mousePosY)
        Draw(m_mousePosX + 1, m_mousePosY)
        Draw(m_mousePosX, m_mousePosY - 1)
        Draw(m_mousePosX, m_mousePosY + 1)

        If m_mouse(0).Held Then
          Fill(20, 20, 50, 50)
        End If

      Case Else
        m_state = 0
    End Select

    Return True

  End Function

End Class