Imports VbConsoleGameEngine

Friend Class Tile

  Private m_level As New Level
  Private m_spriteId As Integer = 0
  Private m_solid As Boolean = False

  Friend Sub New()
  End Sub

  Friend Sub New(spriteId As Integer)
    m_spriteId = spriteId
  End Sub

  Friend Sub SetSpriteId(spriteId As Integer)
    m_spriteId = spriteId
  End Sub

  Friend Function GetSpriteId() As Integer
    Return m_spriteId
  End Function

  Friend Sub SetLevel(level As Level)
    m_level = level
  End Sub

  Friend Function GetLevel() As Level
    Return m_level
  End Function

  Friend Sub SetSolid(ByVal solid As Boolean)
    Me.m_solid = solid
  End Sub

  Friend Function IsSolid() As Boolean
    Return m_solid
  End Function

  Friend Function GetSprite() As Sprite
    Dim tileCount = m_level.GetSpriteSheet().GetTileCount()
    Dim spr = m_level.GetSpriteSheet()
    If m_spriteId >= tileCount Then
      Return spr.Item(0)
    End If
    Return spr.Item(m_spriteId)
  End Function

  Public Shared Narrowing Operator CType(tile As Tile) As String
    Return $"{tile.m_spriteId} {tile.m_solid} "
  End Operator

  Public Shared Narrowing Operator CType(str As String) As Tile
    Dim parts() = str.Split(" ")
    Dim tile As New Tile With {.m_spriteId = Integer.Parse(parts(0)),
                               .m_solid = Boolean.Parse(parts(1))}
    Return tile
  End Operator

End Class

Friend Class Level

  Private ReadOnly m_spritesheet As New SpriteSheet()
  Private m_tiles() As Tile = Nothing
  Private m_mapWidth As Integer
  Private m_mapHeight As Integer

  Friend Sub New()
  End Sub

  Friend Sub New(mapWidth As Integer, mapHeight As Integer)
    m_mapWidth = mapWidth
    m_mapHeight = mapHeight
    ReDim m_tiles(mapWidth * mapHeight - 1)
    For i = 0 To mapWidth * mapHeight - 1
      m_tiles(i) = New Tile()
      m_tiles(i).SetLevel(Me)
    Next
  End Sub

  Friend Sub New(map As String)
    Load(map)
  End Sub

  Friend Sub New(map As String, spriteSheet As String, tileSize As Integer)
    Load(map)
    LoadSpriteSheet(spriteSheet, tileSize)
  End Sub

  Friend Sub LoadSpriteSheet(map As String, tileSize As Integer)
    m_spritesheet.Load(map, tileSize, tileSize)
  End Sub

  Friend Sub Create(mapWidth As Integer, mapHeight As Integer)
    m_mapWidth = mapWidth
    m_mapHeight = mapHeight
    'ReDim m_tiles(mapWidth * mapHeight - 1)
    m_tiles = New Tile(mapWidth * mapHeight - 1) {}
    'For index = 0 To m_tiles.Length - 1
    '  m_tiles(index) = New Tile
    'Next
  End Sub

  Friend Sub Load(mapFile As String)
    If mapFile <> "" AndAlso m_mapWidth > 0 Then
    End If
  End Sub

  Friend Sub Save(mapFile As String)
    If mapFile <> "" AndAlso m_mapWidth > 0 Then
    End If
  End Sub

  Default Friend Property Item(index As Integer) As Tile
    Get
      Return m_tiles(index)
    End Get
    Set(value As Tile)
      m_tiles(index) = value
    End Set
  End Property

  Friend Function GetWidth() As Integer
    Return m_mapWidth
  End Function

  Friend Function GetHeight() As Integer
    Return m_mapHeight
  End Function

  Friend ReadOnly Property GetSpriteSheet() As SpriteSheet
    Get
      Return m_spritesheet
    End Get
  End Property

End Class