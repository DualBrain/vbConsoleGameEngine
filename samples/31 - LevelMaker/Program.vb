Imports VbConsoleGameEngine
Imports VbConsoleGameEngine.PixelType
Imports VbConsoleGameEngine.Color
Imports System.IO
Imports System.Text
Imports System.Runtime.InteropServices

Module Program

  Sub Main()
    Dim game As New LevelMaker
    game.ConstructConsole(400, 200, 4, 4)
    game.Start()
  End Sub

End Module

Friend Module Constants

  Friend Const TILE_WIDTH As Integer = 16
  Friend Const FONT_SPRITESHEET As String = "assets/javidx9_nesfont8x8.spr"
  Friend Const TILE_SPRITESHEET As String = "assets/loztheme.spr"
  Friend Const MAP_WIDTH As Integer = 10
  Friend Const MAP_HEIGHT As Integer = 10
  Friend Const DEFAULT_TILE As Integer = 14

  Friend Enum Tool
    TILES
    META
    EXPORT_IMPORT
    LAST
  End Enum

  Friend Enum Popup
    NONE
    NEW_MAP_SIZE
  End Enum

  Friend Class Popup_t

    Friend popup As Popup
    Friend menuActive As Boolean
    Friend newMapSize As NewMapSize_t

    Friend Structure NewMapSize_t
      Friend width As String
      Friend height As String
      Friend field As Integer ' 0 - width, 1 - height
    End Structure

    Friend Sub New()
      popup = Popup.NONE
      menuActive = False
      newMapSize = New NewMapSize_t()
    End Sub

  End Class

  ' embedded fill icon meta, should have a better way to store this...
  Friend fillSpriteData As String = "8 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 15 9608 0 0 0 0 0 0 0 0 12 9608 12 9608 12 9608 12 9608 15 9608 0 0 0 0 0 0 12 9608 15 9608 12 9608 12 9608 12 9608 15 9608 0 0 0 0 12 9608 0 9608 15 9608 12 9608 12 9608 12 9608 15 9608 0 0 12 9608 0 9608 0 9608 15 9608 12 9608 15 9608 0 0 0 0 12 9608 0 0 0 9608 0 9608 15 9608 0 9608 0 0 0 0 0 0 0 0 0 0 0 9608 0 9608 0 0 0 0"

End Module

Class LevelMaker
  Inherits ConsoleGameEngine

  'Private ReadOnly m_mapMoveX, m_mapMoveY As Single
  Private ReadOnly m_font As New SpriteSheet
  Private m_tiles As New SpriteSheet
  Private m_fillIcon As New Sprite
  Private m_tool As Tool = Tool.TILES
  Private ReadOnly m_level As New Level
  Private m_page As Integer = 0
  Private m_selectedSprite As Integer = 0
  Private m_pageCount As Integer = 1
  Private ReadOnly m_uiBase As Integer = 300
  Private m_tilesPerRow, m_tilesPerColumn, m_tilesPerPage, m_uiWidth As Integer
  Private m_worldOffsetX, m_worldOffsetY As Single
  Private m_grid As Boolean = False
  Private m_floodMode As Boolean = False
  Private m_pickedFirst, m_pickedSecond As Boolean
  Private m_startRec As (Integer, Integer)
  Private m_endRex As (Integer, Integer)
  Private m_file As String = ""
  Private m_spriteSheetFile As String = TILE_SPRITESHEET
  Private ReadOnly m_popup As New Popup_t()

  Public Sub New()
    m_appName = "Level Maker"
  End Sub

  Private Sub DrawStringFont(x As Integer, y As Integer, characters As String)
    ' will use ascii
    For Each c In characters
      Dim tileIndex = AscW(c) - AscW(" "c)
      If tileIndex >= m_font.GetTileCount() Then
        Continue For
      End If
      DrawSprite(x, y, m_font.Item(tileIndex))
      x += m_font.GetTileWidth()
    Next c
  End Sub

  Public Overrides Function OnUserCreate() As Boolean

    m_font.Load(FONT_SPRITESHEET, 8, 8)

    Dim values = fillSpriteData.Split(" "c)
    Dim index = 0
    Dim fillWidth, fillHeight As Integer
    fillWidth = Integer.Parse(values(index)) : index += 1
    fillHeight = Integer.Parse(values(index)) : index += 1
    m_fillIcon = New Sprite(fillWidth, fillHeight)
    For sy = 0 To m_fillIcon.Height - 1
      For sx = 0 To m_fillIcon.Width - 1
        Dim color = CType(Integer.Parse(values(index)), Color) : index += 1
        Dim glyph = Integer.Parse(values(index)) : index += 1
        m_fillIcon.SetColor(sx, sy, color)
        m_fillIcon.SetGlyph(sx, sy, glyph)
      Next
    Next

    m_level.Create(MAP_WIDTH, MAP_HEIGHT)
    For i = 0 To m_level.GetWidth() * m_level.GetHeight() - 1
      m_level.Item(i) = New Tile()
      m_level.Item(i).SetLevel(m_level)
      m_level.Item(i).SetSpriteId(DEFAULT_TILE)
    Next

    ' search for the default sprite sheet
    Dim spriteFile As New FileStream(TILE_SPRITESHEET, FileMode.Open, FileAccess.Read)
    If Not spriteFile.CanRead Then
      ImportSpriteSheet()
    Else
      m_level.LoadSpriteSheet(TILE_SPRITESHEET, TILE_WIDTH)
      m_tiles = m_level.GetSpriteSheet()
    End If
    spriteFile.Close()

    m_uiWidth = 400 - m_uiBase
    m_tilesPerRow = CInt(Fix(m_uiWidth / m_tiles.GetTileWidth()))
    m_tilesPerColumn = CInt(Fix((200 - 23) / m_tiles.GetTileHeight()))
    m_tilesPerPage = m_tilesPerColumn * m_tilesPerRow
    m_pageCount = CInt(Fix((m_tiles.GetTileCount() / m_tilesPerPage) + 1))

    Return True

  End Function

  Private m_moved As Boolean = False

  Public Overrides Function OnUserUpdate(elapsedTime As Single) As Boolean

    If m_moved Then
    End If

    Dim fTileX = m_mousePosX / 16.0F
    Dim fTileY = m_mousePosY / 16.0F
    ' simple
    Dim tileX = CInt(Math.Round(fTileX - m_worldOffsetX / 16))
    Dim tileY = CInt(Math.Round(fTileY - m_worldOffsetY / 16))

    Fill(0, 0, 300, 200, AscW(" "c), BgBlack Or FgBlack)

    Dim topTileX = -CInt(Fix(Math.Floor(m_worldOffsetX / 16.0F))) - 1
    Dim topTileY = -CInt(Fix(Math.Floor(m_worldOffsetY / 16.0F))) - 1

    ' draw map
    For y = topTileY To topTileY + CInt(Fix(Math.Ceiling(200.0 / TILE_WIDTH))) - 1
      For x = topTileX To topTileX + CInt(Fix(Math.Ceiling(300.0 / TILE_WIDTH))) - 1

        If x < 0 OrElse x >= m_level.GetWidth() OrElse y < 0 OrElse y >= m_level.GetHeight() Then
          Continue For
        End If

        Dim i = x + y * m_level.GetWidth()
        If i < 0 OrElse i >= m_level.GetWidth() * m_level.GetHeight() Then
          Continue For
        End If

        Dim screenX = CInt(Fix(x * TILE_WIDTH + m_worldOffsetX))
        Dim screenY = CInt(Fix(y * TILE_WIDTH + m_worldOffsetY))
        If screenX < -16 OrElse screenX >= 300 OrElse screenY < -16 OrElse screenY >= 200 Then
          Continue For
        End If

        If screenX < 0 Then
          screenX = 0
        End If

        If screenY < 0 Then
          screenY = 0
        End If

        DrawSprite(screenX, screenY, m_level(i).GetSprite())

        If m_grid Then
          DrawLine(screenX, screenY, screenX + TILE_WIDTH, screenY, Solid, BgBlack Or FgBlack)
          DrawLine(screenX, screenY, screenX, screenY + TILE_WIDTH, Solid, BgBlack Or FgBlack)
          DrawLine(screenX + TILE_WIDTH, screenY, screenX + TILE_WIDTH, screenY + TILE_WIDTH, Solid, BgBlack Or FgBlack)
          DrawLine(screenX, screenY + TILE_WIDTH, screenX + TILE_WIDTH, screenY + TILE_WIDTH, Solid, BgBlack Or FgBlack)
        End If

        If m_tool = Tool.META Then
          Dim offset = 0
          If m_level(i).IsSolid() Then
            Fill(screenX + offset, screenY + offset, screenX + 3 + offset, screenY + 3 + offset, Solid, BgBlack Or FgRed)
            offset += 3
          End If
        End If
      Next
    Next

    If m_popup.menuActive Then

      Select Case m_popup.popup
        Case Constants.Popup.NEW_MAP_SIZE

#Region "New Map Size Popup"

          ' New Map Size Popup
          Fill(110, 70, 290, 150, 32, FgDarkGray Or BgDarkGray)
          DrawStringFont(150, 75, "New Map Size")
          DrawStringFont(125, 90, "Width")
          DrawStringFont(210, 90, "Height")
          Fill(170, 130, 240, 140, 32, FgGray Or BgGray)
          DrawStringFont(180, 131, "Create")
          Select Case m_popup.newMapSize.field
            Case 0
              Fill(125, 100, 200, 108, 32, FgGray Or BgGray)
              Fill(210, 100, 280, 108, 32, FgBlack Or BgBlack)
              If IsFocused() Then
                If GetKey("0").Pressed OrElse GetKey(VK_NUMPAD0).Pressed Then
                  m_popup.newMapSize.width &= "0"c
                End If
                If GetKey("1").Pressed OrElse GetKey(VK_NUMPAD1).Pressed Then
                  m_popup.newMapSize.width &= "1"c
                End If
                If GetKey("2").Pressed OrElse GetKey(VK_NUMPAD2).Pressed Then
                  m_popup.newMapSize.width &= "2"c
                End If
                If GetKey("3").Pressed OrElse GetKey(VK_NUMPAD3).Pressed Then
                  m_popup.newMapSize.width &= "3"c
                End If
                If GetKey("4").Pressed OrElse GetKey(VK_NUMPAD4).Pressed Then
                  m_popup.newMapSize.width &= "4"c
                End If
                If GetKey("5").Pressed OrElse GetKey(VK_NUMPAD5).Pressed Then
                  m_popup.newMapSize.width &= "5"c
                End If
                If GetKey("6").Pressed OrElse GetKey(VK_NUMPAD6).Pressed Then
                  m_popup.newMapSize.width &= "6"c
                End If
                If GetKey("7").Pressed OrElse GetKey(VK_NUMPAD7).Pressed Then
                  m_popup.newMapSize.width &= "7"c
                End If
                If GetKey("8").Pressed OrElse GetKey(VK_NUMPAD8).Pressed Then
                  m_popup.newMapSize.width &= "8"c
                End If
                If GetKey("9").Pressed OrElse GetKey(VK_NUMPAD9).Pressed Then
                  m_popup.newMapSize.width &= "9"c
                End If
                If GetKey(VK_BACK).Pressed Then
                  If m_popup.newMapSize.width.Length <> 0 Then
                    m_popup.newMapSize.width = m_popup.newMapSize.width.Substring(0, m_popup.newMapSize.width.Length - 1)
                  End If
                End If
              End If
            Case 1
              Fill(125, 100, 200, 108, 32, FgBlack Or BgBlack)
              Fill(210, 100, 280, 108, 32, FgGray Or BgGray)
              If IsFocused() Then
                If GetKey(AscW("0"c)).Pressed OrElse GetKey(VK_NUMPAD0).Pressed Then
                  m_popup.newMapSize.height &= "0"
                End If
                If GetKey(AscW("1"c)).Pressed OrElse GetKey(VK_NUMPAD1).Pressed Then
                  m_popup.newMapSize.height &= "1"
                End If
                If GetKey(AscW("2"c)).Pressed OrElse GetKey(VK_NUMPAD2).Pressed Then
                  m_popup.newMapSize.height &= "2"
                End If
                If GetKey(AscW("3"c)).Pressed OrElse GetKey(VK_NUMPAD3).Pressed Then
                  m_popup.newMapSize.height &= "3"
                End If
                If GetKey(AscW("4"c)).Pressed OrElse GetKey(VK_NUMPAD4).Pressed Then
                  m_popup.newMapSize.height &= "4"
                End If
                If GetKey(AscW("5"c)).Pressed OrElse GetKey(VK_NUMPAD5).Pressed Then
                  m_popup.newMapSize.height &= "5"
                End If
                If GetKey(AscW("6"c)).Pressed OrElse GetKey(VK_NUMPAD6).Pressed Then
                  m_popup.newMapSize.height &= "6"
                End If
                If GetKey(AscW("7"c)).Pressed OrElse GetKey(VK_NUMPAD7).Pressed Then
                  m_popup.newMapSize.height &= "7"
                End If
                If GetKey(AscW("8"c)).Pressed OrElse GetKey(VK_NUMPAD8).Pressed Then
                  m_popup.newMapSize.height &= "8"
                End If
                If GetKey(AscW("9"c)).Pressed OrElse GetKey(VK_NUMPAD9).Pressed Then
                  m_popup.newMapSize.height &= "9"
                End If
                If GetKey(VK_BACK).Pressed Then
                  If m_popup.newMapSize.height.Length <> 0 Then
                    m_popup.newMapSize.height = m_popup.newMapSize.height.Substring(0, m_popup.newMapSize.height.Length - 1)
                  End If
                End If
              End If
            Case Else
              m_popup.newMapSize.field = 0
          End Select
          DrawStringFont(125, 100, m_popup.newMapSize.width)
          DrawStringFont(210, 100, m_popup.newMapSize.height)
          If m_mouse(0).Pressed Then
            If m_mousePosX > 125 AndAlso m_mousePosX < 200 AndAlso m_mousePosY > 100 AndAlso m_mousePosY < 108 Then
              m_popup.newMapSize.field = 0
            End If
            If m_mousePosX > 210 AndAlso m_mousePosX < 280 AndAlso m_mousePosY > 100 AndAlso m_mousePosY < 108 Then
              m_popup.newMapSize.field = 1
            End If
            If m_mousePosX > 170 AndAlso m_mousePosX < 240 AndAlso m_mousePosY > 130 AndAlso m_mousePosY < 140 Then
              ' Create the map
              m_popup.popup = Constants.Popup.NONE
              m_popup.menuActive = False
              Dim width = Integer.Parse(m_popup.newMapSize.width)
              Dim height = Integer.Parse(m_popup.newMapSize.height)
              If width <> 0 AndAlso height <> 0 Then
                m_popup.newMapSize.width = ""
                m_popup.newMapSize.height = ""
                m_level.Create(width, height)
                For i = 0 To m_level.GetWidth() * m_level.GetHeight() - 1
                  m_level(i).SetLevel(m_level)
                  m_level(i).SetSpriteId(DEFAULT_TILE)
                Next
                m_level.LoadSpriteSheet(m_spriteSheetFile, TILE_WIDTH)
                m_tiles = m_level.GetSpriteSheet()
                m_file = ""
              End If
            End If
          End If

#End Region

        Case Else
      End Select

    ElseIf IsFocused() Then

      ' fill the menu
      Fill(m_uiBase, 0, 400, 200, 32, BgGray Or FgBlack)

      If Not GetKey(VK_CONTROL).Held AndAlso ((m_pickedFirst AndAlso m_pickedSecond) OrElse (Not m_pickedFirst AndAlso Not m_pickedSecond)) Then
        If m_tool = Tool.TILES Then
          TilesTool(tileX, tileY)
        End If
        If m_tool = Tool.META Then
          MetaTool(tileX, tileY)
        End If
        If m_tool = Tool.EXPORT_IMPORT Then
          ExportAndImportTool()
        End If
      End If

      If tileX >= 0 AndAlso tileY >= 0 AndAlso tileX < m_level.GetWidth() AndAlso tileY < m_level.GetHeight() AndAlso m_mousePosX <= 300 Then
        ' draw coords
        Dim str As New StringBuilder("<")
        str.Append(tileX)
        str.Append(", ")
        str.Append(tileY)
        str.Append(">"c)
        DrawStringFont(0, 0, str.ToString())

        ' draw hovered tile rect
        If m_pickedFirst Then
          Dim firstRectTileX As Integer = m_startRec.Item1
          Dim firstRectTileY As Integer = m_startRec.Item2
          Dim secondRectTileX As Integer = tileX
          Dim secondRectTileY As Integer = tileY
          If m_pickedSecond Then
            secondRectTileX = m_endRex.Item1
            secondRectTileY = m_endRex.Item2
          End If

          Dim rectWidth As Integer = Math.Abs(firstRectTileX - secondRectTileX)
          Dim rectHeight As Integer = Math.Abs(firstRectTileY - secondRectTileY)
          rectHeight += 1
          rectWidth += 1

          DrawLine(firstRectTileX * 16 + m_worldOffsetX, firstRectTileY * 16 + m_worldOffsetY, firstRectTileX * 16 + 16 * rectWidth + m_worldOffsetX, firstRectTileY * 16 + m_worldOffsetY, Solid, BgBlack Or FgGray)
          DrawLine(firstRectTileX * 16 + m_worldOffsetX, firstRectTileY * 16 + m_worldOffsetY, firstRectTileX * 16 + m_worldOffsetX, firstRectTileY * 16 + 16 * rectHeight + m_worldOffsetY, Solid, BgBlack Or FgGray)
          DrawLine(firstRectTileX * 16 + 16 * rectWidth + m_worldOffsetX, firstRectTileY * 16 + m_worldOffsetY, firstRectTileX * 16 + 16 * rectWidth + m_worldOffsetX, firstRectTileY * 16 + 16 * rectHeight + m_worldOffsetY, Solid, BgBlack Or FgGray)
          DrawLine(firstRectTileX * 16 + m_worldOffsetX, firstRectTileY * 16 + 16 * rectHeight + m_worldOffsetY, firstRectTileX * 16 + 16 * rectWidth + m_worldOffsetX, firstRectTileY * 16 + 16 * rectHeight + m_worldOffsetY, Solid, BgBlack Or FgGray)
        Else
          DrawLine(tileX * 16 + m_worldOffsetX, tileY * 16 + m_worldOffsetY, tileX * 16 + 16 + m_worldOffsetX, tileY * 16 + m_worldOffsetY, Solid, BgBlack Or FgGray)
          DrawLine(tileX * 16 + m_worldOffsetX, tileY * 16 + m_worldOffsetY, tileX * 16 + m_worldOffsetX, tileY * 16 + 16 + m_worldOffsetY, Solid, BgBlack Or FgGray)
          DrawLine(tileX * 16 + 16 + m_worldOffsetX, tileY * 16 + m_worldOffsetY, tileX * 16 + 16 + m_worldOffsetX, tileY * 16 + 16 + m_worldOffsetY, Solid, BgBlack Or FgGray)
          DrawLine(tileX * 16 + m_worldOffsetX, tileY * 16 + 16 + m_worldOffsetY, tileX * 16 + 16 + m_worldOffsetX, tileY * 16 + 16 + m_worldOffsetY, Solid, BgBlack Or Color.FgGray)
        End If

        If GetKey(VK_CONTROL).Held AndAlso GetMouse(0).Pressed Then
          If m_pickedFirst Then
            If m_pickedSecond Then
              m_pickedSecond = False
              m_startRec = (tileX, tileY)
            Else
              m_pickedSecond = True
              m_endRex = (tileX, tileY)
            End If
          Else
            m_pickedFirst = True
            m_startRec = (tileX, tileY)
          End If
        End If

        If m_pickedFirst AndAlso Not m_pickedSecond AndAlso Not GetKey(VK_CONTROL).Held Then
          m_pickedFirst = False
        End If

      ElseIf m_pickedFirst AndAlso m_pickedSecond Then

        Dim firstRectTileX = m_startRec.Item1
        Dim firstRectTileY = m_startRec.Item2
        Dim secondRectTileX = m_endRex.Item1
        Dim secondRectTileY = m_endRex.Item2
        Dim rectWidth = Math.Abs(firstRectTileX - secondRectTileX)
        Dim rectHeight = Math.Abs(firstRectTileY - secondRectTileY)
        rectHeight += 1
        rectWidth += 1
        DrawLine(firstRectTileX * 16 + m_worldOffsetX, firstRectTileY * 16 + m_worldOffsetY, firstRectTileX * 16 + 16 * rectWidth + m_worldOffsetX, firstRectTileY * 16 + m_worldOffsetY, Solid, BgBlack Or FgGray)
        DrawLine(firstRectTileX * 16 + m_worldOffsetX, firstRectTileY * 16 + m_worldOffsetY, firstRectTileX * 16 + m_worldOffsetX, firstRectTileY * 16 + 16 * rectHeight + m_worldOffsetY, Solid, BgBlack Or FgGray)
        DrawLine(firstRectTileX * 16 + 16 * rectWidth + m_worldOffsetX, firstRectTileY * 16 + m_worldOffsetY, firstRectTileX * 16 + 16 * rectWidth + m_worldOffsetX, firstRectTileY * 16 + 16 * rectHeight + m_worldOffsetY, Solid, BgBlack Or FgGray)
        DrawLine(firstRectTileX * 16 + m_worldOffsetX, firstRectTileY * 16 + 16 * rectHeight + m_worldOffsetY, firstRectTileX * 16 + 16 * rectWidth + m_worldOffsetX, firstRectTileY * 16 + 16 * rectHeight + m_worldOffsetY, Solid, BgBlack Or FgGray)

      End If

      Dim iconOffset As Integer = 0
      If m_floodMode OrElse m_keys(VK_SHIFT).Held Then
        DrawSprite(2, 190, m_fillIcon)
        iconOffset += 10
      End If

#Region "Controls"

      ' world movement
#If SMOOTH_WORLD_MOVEMENT Then
      If GetKey(CChar("W")).bHeld Then
        moved = True
        worldOffsetY += 32 * fElapsedTime * If(GetKey(VK_SHIFT).bHeld, 2, 1)
      End If
      If GetKey(CChar("S")).bHeld Then
        moved = True
        worldOffsetY -= 32 * fElapsedTime * If(GetKey(VK_SHIFT).bHeld, 2, 1)
      End If
      If GetKey(CChar("A")).bHeld Then
        moved = True
        worldOffsetX += 32 * fElapsedTime * If(GetKey(VK_SHIFT).bHeld, 2, 1)
      End If
      If GetKey(CChar("D")).bHeld Then
        moved = True
        worldOffsetX -= 32 * fElapsedTime * If(GetKey(VK_SHIFT).bHeld, 2, 1)
      End If
#Else
      If GetKey(CChar("W")).Pressed OrElse (GetKey(VK_SHIFT).Held AndAlso GetKey(CChar("W")).Held) Then
        m_moved = True
        m_worldOffsetY += 16
      End If
      If (Not GetKey(0).Held AndAlso GetKey(CChar("S")).Pressed) OrElse (GetKey(VK_SHIFT).Held AndAlso GetKey(CChar("S")).Held) Then
        m_moved = True
        m_worldOffsetY -= 16
      End If
      If GetKey(CChar("A")).Pressed OrElse (GetKey(VK_SHIFT).Held AndAlso GetKey(CChar("A")).Held) Then
        m_moved = True
        m_worldOffsetX += 16
      End If
      If GetKey(CChar("D")).Pressed OrElse (GetKey(VK_SHIFT).Held AndAlso GetKey(CChar("D")).Held) Then
        m_moved = True
        m_worldOffsetX -= 16
      End If
#End If

      If (GetKey(VK_ESCAPE).Pressed) Then
        m_pickedFirst = False
        m_pickedSecond = False
      End If

      If (m_keys(VK_CONTROL).Held And m_keys(AscW("S"c)).Pressed) Then
        If (m_file.Length = 0) Then
          SaveLevel()
        Else
          m_level.Save(m_file)
        End If
      End If

      If (m_keys(VK_CONTROL).Held And m_keys(AscW("L"c)).Pressed) Then
        LoadLevel()
      End If

      If (m_keys(AscW("T"c)).Pressed) Then
        m_tool = CType(CType(m_tool, Integer) + 1, Tool)
        If (m_tool = Tool.LAST) Then
          m_tool = Tool.TILES
        End If
      End If

      If (m_keys(AscW("G"c)).Pressed) Then
        m_grid = Not m_grid
      End If

      If (m_keys(AscW("F"c)).Pressed) Then
        m_floodMode = Not m_floodMode
      End If

      If (m_keys(VK_LEFT).Pressed) Then
        If (m_pageCount <> 0) Then
          m_page -= 1
          If (m_page < 0) Then
            m_page = 0
          End If
        End If
      End If

      If (m_keys(VK_RIGHT).Pressed) Then
        If (m_pageCount <> 0) Then
          m_page += 1
          If (m_page >= m_pageCount) Then
            m_page = m_pageCount - 1
          End If
        End If
      End If

#End Region

    End If

    Return True

  End Function

#Region "Meta Tool"

  Private Enum MetaTools
    SolidBrush
  End Enum

  Private ReadOnly m_selectedMetaTool As MetaTools

  Sub MetaTool(tileX As Integer, tileY As Integer)

    Dim title = "TILE META"
    DrawStringFont(m_uiBase + 5, 5, title)

    Dim solidBrushText = ""
    If m_selectedMetaTool = MetaTools.SolidBrush Then
      solidBrushText &= " * "
    End If
    solidBrushText &= "SOLID"
    DrawStringFont(m_uiBase + 10, 18, solidBrushText)
    Fill(m_uiBase + 7, 19, m_uiBase + 7 + 5, 19 + 5, Solid, BgBlack Or FgRed)

    ' are we in the world editor
    If tileX >= 0 AndAlso tileY >= 0 AndAlso tileX < m_level.GetWidth() AndAlso tileY < m_level.GetHeight() AndAlso Not m_popup.menuActive Then
      Select Case m_selectedMetaTool
        Case MetaTools.SolidBrush
          ' change the tile
          If m_mouse(0).Held Then
            If m_floodMode OrElse m_keys(VK_SHIFT).Held Then
              FloorFillSolid(tileX, tileY, True)
            Else
              m_level(tileX + tileY * m_level.GetWidth()).SetSolid(True)
            End If
          ElseIf m_mouse(1).Held Then
            If m_floodMode OrElse m_keys(VK_SHIFT).Held Then
              FloorFillSolid(tileX, tileY, False)
            Else
              m_level(tileX + tileY * m_level.GetWidth()).SetSolid(False)
            End If
          End If
      End Select
    End If
  End Sub

#End Region

#Region "Import Export"

  Sub ExportAndImportTool()
    Dim title As String = "NEW"
    DrawStringFont(m_uiBase + 6, 5, title)
    DrawStringFont(m_uiBase + 1, 25, "IMPORT:")
    DrawStringFont(m_uiBase + 6, 35, "LEVEL")
    DrawStringFont(m_uiBase + 6, 45, "SPRITESHEET")
    DrawStringFont(m_uiBase + 1, 75, "EXPORT:")
    DrawStringFont(m_uiBase + 6, 85, "LEVEL")
    DrawStringFont(m_uiBase + 6, 95, "SPRITE")
    If m_popup.menuActive Then Return
    If m_mouse(0).Pressed Then
      ' new
      If m_mousePosX > m_uiBase + 6 AndAlso m_mousePosX < 400 AndAlso m_mousePosY > 5 AndAlso m_mousePosY < 5 + 8 Then
        m_popup.menuActive = True
        m_popup.popup = Constants.Popup.NEW_MAP_SIZE
      End If
      ' import level
      If m_mousePosX > m_uiBase + 6 AndAlso m_mousePosX < 400 AndAlso m_mousePosY > 25 AndAlso m_mousePosY < 25 + 8 Then
        LoadLevel()
      End If
      ' import spritesheet
      If m_mousePosX > m_uiBase + 6 AndAlso m_mousePosX < 400 AndAlso m_mousePosY > 35 AndAlso m_mousePosY < 35 + 8 Then
        ImportSpriteSheet()
      End If
      ' export level
      If m_mousePosX > m_uiBase + 6 AndAlso m_mousePosX < 400 AndAlso m_mousePosY > 75 AndAlso m_mousePosY < 75 + 8 Then
        SaveLevel()
      End If
      ' export level as sprite
      If m_mousePosX > m_uiBase + 6 AndAlso m_mousePosX < 400 AndAlso m_mousePosY > 85 AndAlso m_mousePosY < 85 + 8 Then
        ExportAsSprite()
      End If
    End If
  End Sub

#Region "Win32"

  '<DllImport("comdlg32.dll", SetLastError:=True, CharSet:=CharSet.Auto)>
  'Private Shared Function GetOpenFileName(<[In], Out> ofn As OpenFileName) As Boolean
  'End Function

  '<DllImport("comdlg32.dll", CharSet:=CharSet.Auto, SetLastError:=True)>
  'Private Shared Function GetSaveFileName(<[In], Out> ofn As OpenFileName) As Boolean
  'End Function

  Private Declare Function GetOpenFileName Lib "comdlg32.dll" Alias "GetOpenFileNameA" (ByRef ofn As OpenFileName) As Boolean
  Private Declare Function GetSaveFileName Lib "comdlg32.dll" Alias "GetSaveFileNameA" (ByRef ofn As OpenFileName) As Boolean

  '<StructLayout(LayoutKind.Sequential, CharSet:=CharSet.Auto)>
  'Public Structure OpenFileName
  '  Public lStructSize As Integer '= 0
  '  Public hwndOwner As IntPtr '= IntPtr.Zero
  '  Public hInstance As IntPtr '= IntPtr.Zero
  '  Public lpstrFilter As String '= Nothing
  '  Public lpstrCustomFilter As String '= Nothing
  '  Public nMaxCustFilter As Integer '= 0
  '  Public nFilterIndex As Integer ' = 0
  '  Public lpstrFile As String ' = Nothing
  '  Public nMaxFile As Integer '= 0
  '  Public lpstrFileTitle As String '= Nothing
  '  Public nMaxFileTitle As Integer '= 0
  '  Public lpstrInitialDir As String '= Nothing
  '  Public lpstrTitle As String '= Nothing
  '  Public flags As OpenFileNameFlags '= 0
  '  Public nFileOffset As Short '= 0
  '  Public nFileExtension As Short '= 0
  '  Public lpstrDefExt As String '= Nothing
  '  Public lCustData As IntPtr '= IntPtr.Zero
  '  Public lpfnHook As IntPtr '= IntPtr.Zero
  '  Public lpTemplateName As String '= Nothing
  '  Public pvReserved As IntPtr '= IntPtr.Zero
  '  Public dwReserved As Integer '= 0
  '  Public flagsEx As Integer '= 0
  'End Structure

  <StructLayout(LayoutKind.Sequential)> ', CharSet:=CharSet.Auto)>
  Private Structure OpenFileName
    Public StructSize As Integer
    Public HwndOwner As IntPtr
    Public Instance As IntPtr
    Public Filter As String
    Public CustomFilter As String
    Public MaxCustFilter As Integer
    Public FilterIndex As Integer
    Public File As String
    Public MaxFile As Integer
    Public FileTitle As String
    Public MaxFileTitle As Integer
    Public InitialDir As String
    Public Title As String
    Public Flags As Integer
    Public FileOffset As Short
    Public FileExtension As Short
    Public DefExt As String
    Public CustData As IntPtr
    Public Hook As IntPtr
    Public TemplateName As String
  End Structure

  <Flags>
  Public Enum OpenFileNameFlags As Integer
    FileMustExist = &H1000
  End Enum

#End Region

  Private Sub ImportSpriteSheet()
    Dim ofn = New OpenFileName With {.StructSize = Marshal.SizeOf(GetType(OpenFileName)),
                                     .Filter = "olcSprite (*.spr)" & Chr(0) & "*.spr" & Chr(0) & "Any File" & Chr(0) & "*.*" & Chr(0),
                                     .File = New String(Chr(0), MAX_PATH)}
    ofn.MaxFile = ofn.File.Length - 1
    ofn.Title = "Import Sprite Sheet"

    'ofn.hwndOwner = Nothing
    ofn.Flags = OpenFileNameFlags.FileMustExist
    If GetOpenFileName(ofn) Then
      m_spriteSheetFile = ofn.File
      m_level.LoadSpriteSheet(m_spriteSheetFile, 16)
      m_tiles = m_level.GetSpriteSheet()
    End If

    'Dim openFileDialog1 As New OpenFileDialog()
    'openFileDialog1.Filter = "olcSprite (*.spr)|*.spr|All Files (*.*)|*.*"
    'openFileDialog1.FilterIndex = 0
    'openFileDialog1.Multiselect = False

    'If openFileDialog1.ShowDialog() = DialogResult.OK Then
    '  Dim selectedFilePath As String = openFileDialog1.FileName
    '  ' Process the selected file path here

    '  spriteSheetFile = selectedFilePath
    '  level.LoadSpriteSheet(selectedFilePath, 16)
    '  tiles = level.GetSpriteSheet()

    'End If

  End Sub

  Private Sub LoadLevel()
    'Dim filename(MAX_PATH) As Char
    Dim ofn As New OpenFileName With {.StructSize = Marshal.SizeOf(GetType(OpenFileName)),
                                      .File = New String(Chr(0), MAX_PATH)}
    'Array.Clear(filename, 0, filename.Length)
    'ofn.hwndOwner = Nothing
    ofn.MaxFile = ofn.File.Length - 1 'MAX_PATH
    ofn.Flags = OpenFileNameFlags.FileMustExist
    ofn.Filter = "Level File (*.lvl)" & Chr(0) & "*.lvl" & Chr(0) & "Any File" & Chr(0) & "*.*" & Chr(0)
    ofn.Title = "Load Level"
    If GetOpenFileName(ofn) Then
      m_level.Load(ofn.File)
    End If

    'Dim openFileDialog1 As New OpenFileDialog()
    'openFileDialog1.Filter = "Level File (*.lvl)|*.lvl|All Files (*.*)|*.*"
    'openFileDialog1.FilterIndex = 0
    'openFileDialog1.Multiselect = False

    'If openFileDialog1.ShowDialog() = DialogResult.OK Then
    '  Dim selectedFilePath As String = openFileDialog1.FileName
    '   Process the selected file path here

    '  level.Load(selectedFilePath)

    'End If

  End Sub

  Private Sub SaveLevel()
    'Dim filename(MAX_PATH) As Char
    Dim ofn As New OpenFileName With {.StructSize = Marshal.SizeOf(GetType(OpenFileName)),
                                      .File = New String(Chr(0), MAX_PATH)}
    'Array.Clear(filename, 0, filename.Length)
    'ofn.hwndOwner = Nothing
    ofn.MaxFile = ofn.File.Length - 1 'MAX_PATH
    ofn.Filter = "Level File (.lvl)" & vbNullChar & ".lvl" & vbNullChar & "Any File" & vbNullChar & "*.*" & vbNullChar
    ofn.Title = "Save Level"
    If GetSaveFileName(ofn) Then
      Dim f As String = ofn.File 'filename
      If Not f.EndsWith(".lvl") Then
        f &= ".lvl"
      End If
      m_level.Save(f)
      m_file = f
    End If
  End Sub

  Private Sub ExportAsSprite()
    'Dim filename(MAX_PATH) As Char
    Dim ofn As New OpenFileName With {.StructSize = Marshal.SizeOf(GetType(OpenFileName)),
                                      .File = New String(Chr(0), MAX_PATH)}
    'Array.Clear(filename, 0, filename.Length)
    'ofn.hwndOwner = Nothing
    ofn.MaxFile = ofn.File.Length - 1 'MAX_PATH
    ofn.Filter = "olcSprite (*.spr)" & vbNullChar & "*.spr" & vbNullChar & "Any File" & vbNullChar & "*.*" & vbNullChar
    ofn.Title = "Export Level As Sprite"
    ofn.DefExt = "spr"
    If GetSaveFileName(ofn) Then
      Dim f As String = ofn.File 'filename
      If Not f.EndsWith(".spr") Then
        f &= ".spr"
      End If
      Dim exportedSprite As New Sprite(m_level.GetWidth() * TILE_WIDTH, m_level.GetHeight() * TILE_WIDTH)
      For y = 0 To m_level.GetHeight() - 1
        For x = 0 To m_level.GetWidth() - 1
          Dim sprite = m_level(x + y * m_level.GetWidth()).GetSprite()
          For sy = 0 To sprite.Height - 1
            For sx = 0 To sprite.Width - 1
              exportedSprite.SetColor(x * TILE_WIDTH + sx, y * TILE_WIDTH + sy, sprite.GetColor(sx, sy))
              exportedSprite.SetGlyph(x * TILE_WIDTH + sx, y * TILE_WIDTH + sy, sprite.GetGlyph(sx, sy))
            Next
          Next
        Next
      Next
      exportedSprite.Save(f)
    End If
  End Sub

#End Region

#Region "Tiles Tool"

  Private Sub TilesTool(tileX As Integer, tileY As Integer)

    ' draw page
    Dim pageText As New StringBuilder("TILES:")
    If m_pageCount = 0 Then
      pageText.Append(m_tiles.GetTileCount())
    Else
      pageText.Append((m_page + 1))
      pageText.Append("/"c)
      pageText.Append(m_pageCount)
    End If
    DrawStringFont(m_uiBase + 5, 5, pageText.ToString())

    ' draw sprites in menu
    Dim drawn = 0
    Dim toDraw = Math.Min(m_tilesPerPage, m_tiles.GetTileCount() - m_tilesPerPage * m_page)
    For row = 0 To m_tilesPerColumn - 1
      If drawn >= toDraw Then Exit For
      Dim y = 23 + row * m_tiles.GetTileHeight()
      For col = 0 To m_tilesPerRow - 1
        If drawn >= toDraw Then Exit For
        Dim x = m_uiBase + col * m_tiles.GetTileWidth()
        DrawSprite(x, y, m_tiles.Item((col + row * m_tilesPerRow) + m_tilesPerPage * m_page))
        drawn += 1
      Next
    Next

    ' draw selected sprite thing
    If m_selectedSprite >= m_tilesPerPage * m_page AndAlso m_selectedSprite < m_tilesPerPage * m_page + m_tilesPerPage Then
      Dim col = m_selectedSprite Mod m_tilesPerRow
      Dim row = CInt(Fix((m_selectedSprite - col) / m_tilesPerRow))
      row -= m_page * m_tilesPerColumn
      Dim y = 23 + row * m_tiles.GetTileHeight()
      Dim x = m_uiBase + col * m_tiles.GetTileWidth()
      DrawLine(x, y, x + 16, y, Solid, BgRed Or FgRed)
      DrawLine(x, y, x, y + 16, Solid, BgRed Or FgRed)
      DrawLine(x + 16, y, x + 16, y + 16, Solid, BgRed Or FgRed)
      DrawLine(x, y + 16, x + 16, y + 16, Solid, BgRed Or FgRed)
    End If

    ' are we in the selection menu
    If GetMouseX() >= m_uiBase AndAlso GetMouseX() < m_uiBase + m_tilesPerRow * m_tiles.GetTileWidth() AndAlso GetMouseY() > 28 AndAlso Not m_popup.menuActive Then
      Dim menuX = GetMouseX() - m_uiBase
      Dim menuY = GetMouseY() - 28
      Dim col = menuX \ m_tiles.GetTileWidth
      Dim row = menuY \ m_tiles.GetTileHeight()
      Dim index = (col + row * m_tilesPerRow) + m_page * m_tilesPerPage
      Dim y = 23 + row * m_tiles.GetTileHeight()
      Dim x = m_uiBase + col * m_tiles.GetTileWidth()
      DrawLine(x, y, x + 16, y, Solid, BgRed Or FgDarkRed)
      DrawLine(x, y, x, y + 16, Solid, BgRed Or FgDarkRed)
      DrawLine(x + 16, y, x + 16, y + 16, Solid, BgRed Or FgDarkRed)
      DrawLine(x, y + 16, x + 16, y + 16, Solid, BgRed Or Color.FgDarkRed)
      If m_mouse(0).Pressed Then
        m_selectedSprite = index
      End If
    End If

    ' are we in the world editor
    If tileX >= 0 AndAlso tileY >= 0 AndAlso tileX < m_level.GetWidth() AndAlso tileY < m_level.GetHeight() AndAlso Not m_popup.menuActive AndAlso GetMouseX() <= 300 Then
      ' do we have a selection rect, if so are we in it?
      If (Not m_pickedFirst AndAlso Not m_pickedSecond) OrElse (m_pickedFirst AndAlso m_pickedSecond AndAlso tileX >= m_startRec.Item1 AndAlso tileX < m_startRec.Item1 + m_endRex.Item1 - 1 AndAlso tileY >= m_startRec.Item2 AndAlso tileY < m_startRec.Item2 + m_endRex.Item2) Then
        ' change the tile
        If m_mouse(0).Held Then
          If m_floodMode OrElse m_keys(VK_SHIFT).Held Then
            FloodFillTile(tileX, tileY)
          Else
            m_level(tileX + tileY * m_level.GetWidth()).SetSpriteId(m_selectedSprite)
          End If
        ElseIf m_mouse(1).Pressed Then
          m_selectedSprite = m_level(tileX + tileY * m_level.GetWidth()).GetSpriteId()
        End If
      End If
      If GetKey(VK_BACK).Pressed AndAlso m_pickedFirst AndAlso m_pickedSecond Then
        For y = m_startRec.Item2 To m_endRex.Item2 + m_startRec.Item2 - 1
          For x = m_startRec.Item1 To m_endRex.Item1 + m_startRec.Item1 - 2
            Dim index = x + y * m_level.GetWidth()
            m_level(index).SetSpriteId(m_selectedSprite)
          Next
        Next
      End If
    End If

  End Sub

#End Region

#Region "Utils"

  Shared Function EndsWith(value As String, ending As String) As Boolean
    If ending.Length > value.Length Then Return False
    Return Enumerable.SequenceEqual(ending.Reverse(), value.Reverse().Take(ending.Length))
  End Function

  Dim fillTileOfType As Integer = DEFAULT_TILE
  Dim solidStart As Boolean = False

  Sub FloorFillSolid(x As Integer, y As Integer, fill As Boolean)
    fillTileOfType = m_level(x + y * m_level.GetWidth()).GetSpriteId()
    solidStart = fill
    Dim q As New Queue(Of (Integer, Integer))
    q.Enqueue((x, y))
    While q.Count <> 0
      Dim xy = q.Dequeue()
      m_level(xy.Item1 + xy.Item2 * m_level.GetWidth()).SetSolid(fill)
      If ShouldFillSolid(xy.Item1 + 1, xy.Item2) Then q.Enqueue((xy.Item1 + 1, xy.Item2))
      If ShouldFillSolid(xy.Item1 - 1, xy.Item2) Then q.Enqueue((xy.Item1 - 1, xy.Item2))
      If ShouldFillSolid(xy.Item1, xy.Item2 + 1) Then q.Enqueue((xy.Item1, xy.Item2 + 1))
      If ShouldFillSolid(xy.Item1, xy.Item2 - 1) Then q.Enqueue((xy.Item1, xy.Item2 - 1))
    End While
  End Sub

  Function ShouldFillSolid(x As Integer, y As Integer) As Boolean
    If x < 0 OrElse y < 0 OrElse x >= m_level.GetWidth() OrElse y >= m_level.GetHeight() Then Return False
    Return m_level(x + y * m_level.GetWidth()).GetSpriteId() = fillTileOfType AndAlso m_level(x + y * m_level.GetWidth()).IsSolid() <> solidStart
  End Function

  Private Sub FloodFillTile(x As Integer, y As Integer)

    fillTileOfType = m_level(x + y * m_level.GetWidth()).GetSpriteId()
    If fillTileOfType = m_selectedSprite Then
      Return
    End If

    Dim q As New Queue(Of (Integer, Integer))()
    q.Enqueue((x, y))

    While q.Count > 0
      Dim xy = q.Dequeue()
      m_level(xy.Item1 + xy.Item2 * m_level.GetWidth()).SetSpriteId(m_selectedSprite)
      If ShouldFillTile(xy.Item1 + 1, xy.Item2) Then
        q.Enqueue((xy.Item1 + 1, xy.Item2))
      End If
      If ShouldFillTile(xy.Item1 - 1, xy.Item2) Then
        q.Enqueue((xy.Item1 - 1, xy.Item2))
      End If
      If ShouldFillTile(xy.Item1, xy.Item2 + 1) Then
        q.Enqueue((xy.Item1, xy.Item2 + 1))
      End If
      If ShouldFillTile(xy.Item1, xy.Item2 - 1) Then
        q.Enqueue((xy.Item1, xy.Item2 - 1))
      End If
    End While

  End Sub

  Private Function ShouldFillTile(x As Integer, y As Integer) As Boolean
    If x < 0 OrElse y < 0 OrElse x >= m_level.GetWidth() OrElse y >= m_level.GetHeight() Then
      Return False
    End If
    Return m_level(x + y * m_level.GetWidth()).GetSpriteId() = fillTileOfType
  End Function

#End Region

End Class