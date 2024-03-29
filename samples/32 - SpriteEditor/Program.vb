' Inspired by: "OneLoneCoder.com - Sprite Editor; Stop Crying about Paint ya big baby"
' https://github.com/OneLoneCoder/Javidx9/blob/master/ConsoleGameEngine/SmallerProjects/OneLoneCoder_SpriteEditor.cpp

Imports VbConsoleGameEngine
Imports VbConsoleGameEngine.PixelType
Imports VbConsoleGameEngine.Color

Module Program

  Sub Main()
    Dim game As New SpriteEditor
    game.ConstructConsole(160, 100, 8, 8)
    game.Start()
  End Sub

End Module

Class SpriteEditor
  Inherits ConsoleGameEngine

  Private m_posX As Integer = 0
  Private m_posY As Integer = 0
  Private m_offsetX As Integer = 0
  Private m_offsetY As Integer = 0
  Private m_zoom As Integer = 4
  Private m_currentGlyph As Integer = Solid
  Private m_currentColourFG As Integer = FgRed
  Private m_currentColourBG As Integer = FgBlack

  Private m_sprite As Sprite = Nothing
  Private m_currentSpriteFile As String

  Public Overrides Function OnUserCreate() As Boolean

    m_sprite = New Sprite(8, 32)
    'm_currentSpriteFile = "assets/fps_fireball1.spr"
    m_currentSpriteFile = "assets/car2.spr"

    Return True

  End Function

  Public Overrides Function OnUserUpdate(elapsedTime As Single) As Boolean

    ' Zooming 
    If m_keys(VK_PRIOR).Released Then m_zoom <<= 1
    If m_keys(VK_NEXT).Released Then m_zoom >>= 1

    If m_zoom > 32 Then m_zoom = 32
    If m_zoom < 2 Then m_zoom = 2

    ' Brushes
    If m_keys(VK_F1).Released Then m_currentGlyph = Solid
    If m_keys(VK_F2).Released Then m_currentGlyph = ThreeQuarters
    If m_keys(VK_F3).Released Then m_currentGlyph = Half
    If m_keys(VK_F4).Released Then m_currentGlyph = Quarter

    ' Colours
    For i = 0 To 7
      If m_keys(AscW("01234567".Chars(i))).Released Then
        If m_keys(VK_SHIFT).Held Then
          m_currentColourFG = i + 8
        Else
          m_currentColourFG = i
        End If
      End If
    Next

    If m_keys(VK_F7).Released Then m_currentColourBG -= 1
    If m_keys(VK_F8).Released Then m_currentColourBG += 1

    If m_currentColourBG < 0 Then m_currentColourBG = 15
    If m_currentColourBG > 15 Then m_currentColourBG = 0

    ' Cursing -)
    If m_keys(VK_SHIFT).Held Then
      If (m_keys(VK_UP).Released) Then m_offsetY += 1
      If (m_keys(VK_DOWN).Released) Then m_offsetY -= 1
      If (m_keys(VK_LEFT).Released) Then m_offsetX += 1
      If (m_keys(VK_RIGHT).Released) Then m_offsetX -= 1
    Else
      If (m_keys(VK_UP).Released) Then m_posY -= 1
      If (m_keys(VK_DOWN).Released) Then m_posY += 1
      If (m_keys(VK_LEFT).Released) Then m_posX -= 1
      If (m_keys(VK_RIGHT).Released) Then m_posX += 1
    End If

    If m_sprite IsNot Nothing Then

      If m_posX < 0 Then m_posX = 0
      If m_posX >= m_sprite.Width Then m_posX = m_sprite.Width - 1
      If m_posY < 0 Then m_posY = 0
      If m_posY >= m_sprite.Height Then m_posY = m_sprite.Height - 1

      If m_keys(VK_SPACE).Released Then
        m_sprite.SetGlyph(m_posX - 0, m_posY - 0, m_currentGlyph)
        m_sprite.SetColor(m_posX - 0, m_posY - 0, CType(m_currentColourFG Or (m_currentColourBG << 4), Color))
      End If

      If m_keys(VK_DELETE).Released Then
        m_sprite.SetGlyph(m_posX - 0, m_posY - 0, 32)
        m_sprite.SetColor(m_posX - 0, m_posY - 0, 0)
      End If

      If m_keys(VK_F9).Released Then
        m_sprite.Load(m_currentSpriteFile)
      End If

      If m_keys(VK_F10).Released Then
        m_sprite.Save(m_currentSpriteFile)
      End If

    End If

    ' Erase All
    Cls()

    ' Draw Menu
    DrawString(1, 1, "F1 = 100%  F2 = 75%  F3 = 50%  F4 = 25%    F9 = Load File  F10 = Save File")
    For i = 0 To 7
      DrawString(1 + 6 * i, 3, CStr(i) + " = ")
      If m_keys(VK_SHIFT).Held Then
        Draw(1 + 6 * i + 4, 3, Solid, i + 8)
      Else
        Draw(1 + 6 * i + 4, 3, Solid, i)
      End If
    Next

    DrawString(1, 5, "Current Brush = ")
    Draw(18, 5, m_currentGlyph, m_currentColourFG Or (m_currentColourBG << 4))

    ' Draw Canvas
    For x = 9 To 137
      Draw(x, 9)
      Draw(x, 74)
    Next

    For y = 9 To 74
      Draw(9, y)
      Draw(138, y)
    Next

    ' Draw Visible Sprite
    If m_sprite IsNot Nothing Then

      Dim nVisiblePixelsX = 128 \ m_zoom
      Dim nVisiblePixelsY = 64 \ m_zoom

      For x = 0 To nVisiblePixelsX - 1
        For y = 0 To nVisiblePixelsY - 1

          If (x - m_offsetX < m_sprite.Width AndAlso y - m_offsetY < m_sprite.Height AndAlso x - m_offsetX >= 0 AndAlso y - m_offsetY >= 0) Then

            ' Draw Sprite
            Fill(x * m_zoom + 10, y * m_zoom + 10, (x + 1) * m_zoom + 10, (y + 1) * m_zoom + 10, m_sprite.GetGlyph(x - m_offsetX, y - m_offsetY), m_sprite.GetColor(x - m_offsetX, y - m_offsetY))

            ' Draw Pixel Markers
            If m_sprite.GetGlyph(x - m_offsetX, y - m_offsetY) = 32 Then
              Draw(x * m_zoom + 10, y * m_zoom + 10, AscW("."c))
            End If

          End If

          If (x - m_offsetX = m_posX AndAlso y - m_offsetY = m_posY) Then
            Draw(x * m_zoom + 10, y * m_zoom + 10, AscW("O"c))
          End If

        Next
      Next

    End If

    ' Draw Actual Sprite
    For x = 0 To m_sprite.Width - 1
      For y = 0 To m_sprite.Height - 1
        Draw(x + 10, y + 80, m_sprite.GetGlyph(x, y), m_sprite.GetColor(x, y))
      Next
    Next

    Return True

  End Function

End Class