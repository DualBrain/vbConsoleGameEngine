Imports VbConsoleGameEngine
Imports VbConsoleGameEngine.Color
Imports VbConsoleGameEngine.PixelType

Friend Class RPG_Engine
  Inherits ConsoleGameEngine

  Private m_currentMap As Map = Nothing
  Private m_player As Dynamic_Creature_Witty = Nothing
  Private m_dynamics As New List(Of Dynamic)()
  Private ReadOnly m_projectiles As New List(Of Dynamic)()
  Private ReadOnly m_script As New ScriptProcessor()

  Private ReadOnly m_listQuests As New List(Of Quest)()
  Private ReadOnly m_listItems As New List(Of Item)()

  Private m_cameraPosX As Single = 0.0F
  Private m_cameraPosY As Single = 0.0F

  Private m_font As Sprite = Nothing

  Private m_dialogToShow As New List(Of String)()
  Private m_showDialog As Boolean = False
  'Private ReadOnly m_dialogX As Single = 0.0F
  'Private ReadOnly m_dialogY As Single = 0.0F

  Private Enum GameMode
    Title
    LocalMap
    WorldMap
    Inventory
    Shop
  End Enum

  Private m_gameMode As GameMode = GameMode.LocalMap

  Private m_invSelectX As Integer = 0
  Private m_invSelectY As Integer = 0

  Public Sub New()
    m_appName = "Top Down Role Playing Game"
  End Sub

  Public Overrides Function OnUserCreate() As Boolean

    Command.g_engine = Me
    Map.g_script = m_script

    Quest.g_script = m_script
    Quest.g_engine = Me

    Dynamic.g_engine = Me

    Item.g_engine = Me

    RPG_Assets.Get().LoadSprites()
    RPG_Assets.Get().LoadMaps()
    RPG_Assets.Get().LoadItems()

    m_font = RPG_Assets.Get().GetSprite("font")

    m_listQuests.Insert(0, New Quest_MainQuest())

    m_player = New Dynamic_Creature_Witty()

    m_listItems.Add(RPG_Assets.Get().GetItem("Basic Sword"))

    ChangeMap("coder town", 5, 5)

    Return True

  End Function

  Public Overrides Function OnUserUpdate(elapsedTime As Single) As Boolean

    Select Case m_gameMode
        'Case GameMode.MODE_TITLE
            'Return UpdateTitleScreen(elapsedTime)
      Case GameMode.LocalMap
        Return UpdateLocalMap(elapsedTime)
      'Case GameMode.WorldMap
        '	Return UpdateWorldMap(elapsedTime)
      Case GameMode.Inventory
        Return UpdateInventory(elapsedTime)
        'Case GameMode.Shop
        'Return UpdateShop(elapsedTime)
    End Select

    Return True

  End Function

  Private Function UpdateLocalMap(elapsedTime As Single) As Boolean

    ' Update script
    m_script.ProcessCommands(elapsedTime)

    ' Erase and delete redundant projectiles	
    Dim i = m_projectiles.RemoveAll(Function(d) d.Redundant)

    If m_script.UserControlEnabled Then

      m_player.Vx = 0.0F
      m_player.Vy = 0.0F

      If Not m_showDialog AndAlso m_player.Controllable Then

        ' Handle Input
        If IsFocused() Then

          If GetKey(VK_UP).Held OrElse GetKey(AscW("W"c)).Held Then m_player.Vy = -4.0F
          If GetKey(VK_DOWN).Held OrElse GetKey(AscW("S"c)).Held Then m_player.Vy = 4.0F
          If GetKey(VK_LEFT).Held OrElse GetKey(AscW("A"c)).Held Then m_player.Vx = -4.0F
          If GetKey(VK_RIGHT).Held OrElse GetKey(AscW("D"c)).Held Then m_player.Vx = 4.0F
          If GetKey(&H5A).Released OrElse GetKey(AscW("I"c)).Released Then m_gameMode = GameMode.Inventory

          If GetKey(VK_SPACE).Released Then ' Interaction requested
            ' Grab a point from the direction the player is facing and check for interactions										
            Dim testX, testY As Single

            Select Case m_player.GetFacingDirection()
              Case 0 ' South
                testX = m_player.Px + 0.5F
                testY = m_player.Py + 1.5F
              Case 1 ' West
                testX = m_player.Px - 0.5F
                testY = m_player.Py + 0.5F
              Case 2 ' North
                testX = m_player.Px + 0.5F
                testY = m_player.Py - 0.5F
              Case 3 ' East
                testX = m_player.Px + 1.5F
                testY = m_player.Py + 0.5F
            End Select

            ' Check if test point has hit a dynamic object
            Dim hitSomething = False
            For Each dyns In m_dynamics

              If testX > dyns.Px AndAlso testX < (dyns.Px + 1.0F) AndAlso testY > dyns.Py AndAlso testY < (dyns.Py + 1.0F) Then

                If dyns.Friendly Then

                  hitSomething = True

                  ' Iterate through quest stack until something responds, the base quests should capture
                  ' interactions that are not specfied in other quests
                  For Each quest In m_listQuests
                    If quest.OnInteraction(m_dynamics, dyns, Quest.Nature.Talk) Then
                      Exit For
                    End If
                  Next

                  ' Some objects just do stuff when you interact with them
                  dyns.OnInteract(m_player)

                  ' Then check if it is map related
                  m_currentMap.OnInteraction(m_dynamics, dyns, Quest.Nature.Talk)

                Else
                  ' Interaction was with something not friendly - only enemies
                  ' are not friendly, so perfrom attack
                  m_player.PerformAttack()
                End If

              End If

            Next

            If Not hitSomething Then ' Default action is attack
              m_player.PerformAttack()
            End If

          End If
        End If
      End If
    Else
      ' Scripting system is in control
      If m_showDialog Then
        If GetKey(VK_SPACE).Released Then
          m_showDialog = False
          m_script.CompleteCommand()
        End If
      End If
    End If

    Dim workingWithProjectiles = False
    For Each source In {m_dynamics, m_projectiles}

      For Each o In source

        Dim newObjectPosX = o.Px + o.Vx * elapsedTime
        Dim newObjectPosY = o.Py + o.Vy * elapsedTime

        ' Collision
        Dim border = 0.1F
        Dim collisionWithMap = False

        If o.Vx <= 0 Then
          If m_currentMap.GetSolid(newObjectPosX + border, o.Py + border + 0.0F) OrElse m_currentMap.GetSolid(newObjectPosX + border, o.Py + (1.0F - border)) Then
            newObjectPosX = CInt(Fix(newObjectPosX)) + 1
            o.Vx = 0
            collisionWithMap = True
          End If
        Else
          If m_currentMap.GetSolid(newObjectPosX + (1.0F - border), o.Py + border + 0.0F) OrElse m_currentMap.GetSolid(newObjectPosX + (1.0F - border), o.Py + (1.0F - border)) Then
            newObjectPosX = CInt(Fix(newObjectPosX))
            o.Vx = 0
            collisionWithMap = True
          End If
        End If

        If o.Vy <= 0 Then
          If m_currentMap.GetSolid(newObjectPosX + border + 0.0F, newObjectPosY + border) OrElse m_currentMap.GetSolid(newObjectPosX + (1.0F - border), newObjectPosY + border) Then
            newObjectPosY = CInt(Fix(newObjectPosY)) + 1
            o.Vy = 0
            collisionWithMap = True
          End If
        Else
          If m_currentMap.GetSolid(newObjectPosX + border + 0.0F, newObjectPosY + (1.0F - border)) OrElse m_currentMap.GetSolid(newObjectPosX + (1.0F - border), newObjectPosY + (1.0F - border)) Then
            newObjectPosY = CInt(Fix(newObjectPosY))
            o.Vy = 0
            collisionWithMap = True
          End If
        End If

        If o.IsProjectile AndAlso collisionWithMap Then
          o.Redundant = True
        End If

        Dim dynamicObjectPosX = newObjectPosX
        Dim dynamicObjectPosY = newObjectPosY

        ' Object V Object collisions
        For Each dyn In m_dynamics
          If dyn IsNot o Then
            ' If the object is solid then the player must not overlap it
            If dyn.SolidVsDyn AndAlso o.SolidVsDyn Then
              ' Check if bounding rectangles overlap
              If dynamicObjectPosX < (dyn.Px + 1.0F) AndAlso (dynamicObjectPosX + 1.0F) > dyn.Px AndAlso o.Py < (dyn.Py + 1.0F) AndAlso (o.Py + 1.0F) > dyn.Py Then
                ' First Check Horizontally - Check Left
                If o.Vx <= 0 Then
                  dynamicObjectPosX = dyn.Px + 1.0F
                Else
                  dynamicObjectPosX = dyn.Px - 1.0F
                End If
              End If
              If dynamicObjectPosX < (dyn.Px + 1.0F) AndAlso (dynamicObjectPosX + 1.0F) > dyn.Px AndAlso dynamicObjectPosY < (dyn.Py + 1.0F) AndAlso (dynamicObjectPosY + 1.0F) > dyn.Py Then
                ' First Check Vertically - Check Left
                If o.Vy <= 0 Then
                  dynamicObjectPosY = dyn.Py + 1.0F
                Else
                  dynamicObjectPosY = dyn.Py - 1.0F
                End If
              End If

            Else
              If o Is m_dynamics(0) Then
                ' Object is player and can interact with things
                If dynamicObjectPosX < (dyn.Px + 1.0F) AndAlso (dynamicObjectPosX + 1.0F) > dyn.Px AndAlso o.Py < (dyn.Py + 1.0F) AndAlso (o.Py + 1.0F) > dyn.Py Then

                  ' First check if object is part of a quest
                  For Each quest In m_listQuests
                    If quest.OnInteraction(m_dynamics, dyn, Quest.Nature.Walk) Then
                      Exit For
                    End If
                  Next

                  ' Then check if it is map related
                  m_currentMap.OnInteraction(m_dynamics, dyn, Quest.Nature.Walk)

                  ' Finally just check the object
                  dyn.OnInteract(o)

                End If

              Else
                If workingWithProjectiles Then
                  If dynamicObjectPosX < (dyn.Px + 1.0F) AndAlso (dynamicObjectPosX + 1.0F) > dyn.Px AndAlso
                    dynamicObjectPosY < (dyn.Py + 1.0F) AndAlso (dynamicObjectPosY + 1.0F) > dyn.Py Then
                    If dyn.Friendly <> o.Friendly Then
                      ' We know object is a projectile, so dyn is something
                      ' opposite that it has overlapped with											
                      If dyn.IsAttackable Then
                        ' Dynamic object is a creature
                        Damage(DirectCast(o, Dynamic_Projectile), DirectCast(dyn, Dynamic_Creature))
                      End If
                    End If
                  End If
                End If
              End If
            End If
          End If
        Next

        o.Px = dynamicObjectPosX
        o.Py = dynamicObjectPosY

      Next

      workingWithProjectiles = True

    Next

    For Each source In {m_dynamics, m_projectiles}
      For Each dyn In source
        dyn.Update(elapsedTime, m_player)
      Next
    Next

    ' Remove quests that have been completed
    Dim discard = m_listQuests.RemoveAll(Function(q As Quest) q.Completed)

    m_cameraPosX = m_player.Px
    m_cameraPosY = m_player.Py

    ' Draw Level
    Dim tileWidth = 16
    Dim tileHeight = 16
    Dim visibleTilesX = ScreenWidth() \ tileWidth
    Dim visibleTilesY = ScreenHeight() \ tileHeight

    ' Calculate Top-Leftmost visible tile
    Dim offsetX = m_cameraPosX - CSng(visibleTilesX) / 2.0F
    Dim offsetY = m_cameraPosY - CSng(visibleTilesY) / 2.0F

    ' Clamp camera to game boundaries
    If offsetX < 0 Then offsetX = 0
    If offsetY < 0 Then offsetY = 0
    If offsetX > m_currentMap.Width - visibleTilesX Then offsetX = m_currentMap.Width - visibleTilesX
    If offsetY > m_currentMap.Height - visibleTilesY Then offsetY = m_currentMap.Height - visibleTilesY

    ' Get offsets for smooth movement
    Dim tileOffsetX = (offsetX - Fix(offsetX)) * tileWidth
    Dim tileOffsetY = (offsetY - Fix(offsetY)) * tileHeight

    ' Draw visible tile map
    For x = -1 To visibleTilesX + 1 Step 1
      For y = -1 To visibleTilesY + 1 Step 1
        Dim idx = m_currentMap.GetIndex(x + offsetX, y + offsetY)
        Dim sx = idx Mod 10
        Dim sy = idx \ 10
        DrawPartialSprite(x * tileWidth - tileOffsetX, y * tileHeight - tileOffsetY, m_currentMap.Sprite, sx * tileWidth, sy * tileHeight, tileWidth, tileHeight)
      Next
    Next

    ' Draw Object
    For Each source In {m_dynamics, m_projectiles}
      For Each dyns In source
        dyns.DrawSelf(Me, offsetX, offsetY)
      Next
    Next

    m_player.DrawSelf(Me, offsetX, offsetY)

    Dim health = $"HP: {m_player.Health}/{m_player.HealthMax}"
    DisplayDialog(health, 160, 10)

    ' Draw any dialog being displayed
    If m_showDialog Then
      DisplayDialog(m_dialogToShow, 20, 20)
    End If

    Return True

  End Function

  Friend Sub ShowDialog(lines As List(Of String))
    m_dialogToShow = lines
    m_showDialog = True
  End Sub

  Private Sub DisplayDialog(message As String, x As Integer, y As Integer)
    DisplayDialog(New List(Of String) From {message}, x, y)
  End Sub

  Private Sub DisplayDialog(text As List(Of String), x As Integer, y As Integer)

    Dim maxLineLength = 0
    Dim lines = text.Count

    For Each l In text
      If l.Length > maxLineLength Then maxLineLength = l.Length
    Next

    ' Draw Box
    Fill(x - 1, y - 1, x + maxLineLength * 8 + 1, y + lines * 8 + 1, Solid, FgDarkBlue)
    DrawLine(x - 2, y - 2, x - 2, y + lines * 8 + 1)
    DrawLine(x + maxLineLength * 8 + 1, y - 2, x + maxLineLength * 8 + 1, y + lines * 8 + 1)
    DrawLine(x - 2, y - 2, x + maxLineLength * 8 + 1, y - 2)
    DrawLine(x - 2, y + lines * 8 + 1, x + maxLineLength * 8 + 1, y + lines * 8 + 1)

    For l = 0 To text.Count - 1
      DrawBigText(text(l), x, y + l * 8)
    Next

  End Sub

  Private Sub DrawBigText(text As String, x As Integer, y As Integer)
    Dim i = 0
    For Each c In text
      Dim sx = ((AscW(c) - 32) Mod 16) * 8
      Dim sy = ((AscW(c) - 32) \ 16) * 8
      DrawPartialSprite(x + i * 8, y, m_font, sx, sy, 8, 8)
      i += 1
    Next
  End Sub

  Friend Sub ChangeMap(mapName As String, x As Single, y As Single)

    ' Destroy all dynamics
    m_dynamics.Clear()
    m_dynamics.Add(m_player)

    ' Set current map
    m_currentMap = RPG_Assets.Get().GetMap(mapName)

    ' Update player location
    m_player.Px = x
    m_player.Py = y

    ' Create new dynamics from map
    m_currentMap.PopulateDynamics(m_dynamics)

    ' Create new dynamics from quests
    For Each q In m_listQuests
      q.PopulateDynamics(m_dynamics, m_currentMap.Name)
    Next

  End Sub

  Friend Sub AddQuest(quest As Quest)
    m_listQuests.Insert(0, quest)
  End Sub

  Friend Function GiveItem(item As Item) As Boolean
    'm_script.AddCommand(new cCommand_ShowDialog({ "You have found a" , item->sName }));
    m_listItems.Add(item)
    Return True
  End Function

  Private Function TakeItem(item As Item) As Boolean
    If item IsNot Nothing Then
      m_listItems.Remove(item)
      Return True
    Else
      Return False
    End If
  End Function

  Friend Function HasItem(item As Item) As Boolean
    If item IsNot Nothing Then
      Return m_listItems.Contains(item)
    Else
      Return False
    End If
  End Function

  Private Function UpdateInventory(elapsedTime As Single) As Boolean

    If elapsedTime <> 0 Then
    End If

    'Fill(0, 0, ScreenWidth(), ScreenHeight(), &H2588)
    Cls()

    DrawBigText("INVENTORY", 4, 4)
    Dim i = 0
    Dim highlighted As Item = Nothing

    ' Draw Consumables
    For Each item In m_listItems

      Dim x = i Mod 4
      Dim y = i \ 4
      i += 1

      DrawPartialSprite(8 + x * 20, 20 + y * 20, item.Sprite, 0, 0, 16, 16)

      If m_invSelectX = x AndAlso m_invSelectY = y Then
        highlighted = item
      End If

    Next

    ' Draw selection reticule
    DrawLine(6 + (m_invSelectX) * 20, 18 + (m_invSelectY) * 20, 6 + (m_invSelectX + 1) * 20, 18 + (m_invSelectY) * 20)
    DrawLine(6 + (m_invSelectX) * 20, 18 + (m_invSelectY + 1) * 20, 6 + (m_invSelectX + 1) * 20, 18 + (m_invSelectY + 1) * 20)
    DrawLine(6 + (m_invSelectX) * 20, 18 + (m_invSelectY) * 20, 6 + (m_invSelectX) * 20, 18 + (m_invSelectY + 1) * 20)
    DrawLine(6 + (m_invSelectX + 1) * 20, 18 + (m_invSelectY) * 20, 6 + (m_invSelectX + 1) * 20, 18 + (m_invSelectY + 1) * 20)

    If GetKey(VK_LEFT).Released Then m_invSelectX -= 1
    If GetKey(VK_RIGHT).Released Then m_invSelectX += 1
    If GetKey(VK_UP).Released Then m_invSelectY -= 1
    If GetKey(VK_DOWN).Released Then m_invSelectY += 1

    If m_invSelectX < 0 Then m_invSelectX = 3
    If m_invSelectX >= 4 Then m_invSelectX = 0
    If m_invSelectY < 0 Then m_invSelectY = 3
    If m_invSelectY >= 4 Then m_invSelectY = 0

    If GetKey(AscW("Z"c)).Released Then
      m_gameMode = GameMode.LocalMap
    End If

    DrawBigText("SELECTED:", 8, 160)

    If highlighted IsNot Nothing Then

      DrawBigText("SELECTED:", 8, 160)
      DrawBigText(highlighted.Name, 8, 170)

      DrawBigText("DESCRIPTION:", 8, 190)
      DrawBigText(highlighted.Description, 8, 200)

      If Not highlighted.KeyItem Then
        DrawBigText("(Press SPACE to use)", 80, 160)
      End If

      If GetKey(VK_SPACE).Released Then
        ' Use selected item 
        If Not highlighted.KeyItem Then
          If highlighted.OnUse(m_player) Then
            ' Item has signalled it must be consumed, so remove it
            TakeItem(highlighted)
          End If
        Else
          ' ????
        End If
      End If

    End If

    DrawBigText("LOCATION:", 128, 8)
    DrawBigText(m_currentMap.Name, 128, 16)

    DrawBigText("HEALTH: " + m_player.Health.ToString(), 128, 32)
    DrawBigText("MAX HEALTH: " + m_player.HealthMax.ToString(), 128, 40)

    Return True

  End Function

  'Private Sub Attack(aggressor As Dynamic_Creature, weapon As Weapon)
  '  weapon.OnUse(aggressor)
  'End Sub

  Friend Sub AddProjectile(proj As Dynamic_Projectile)
    m_projectiles.Add(proj)
  End Sub

  Private Sub Damage(projectile As Dynamic_Projectile, victim As Dynamic_Creature)

    If victim IsNot Nothing Then

      ' Attack victim with damage
      victim.Health -= projectile.Damage

      ' Knock victim back
      Dim tx = victim.Px - projectile.Px
      Dim ty = victim.Py - projectile.Py
      Dim d = CSng(Math.Sqrt(tx * tx + ty * ty))

      If d < 1 Then d = 1.0F

      ' After a hit, they object experiences knock back, where it is temporarily
      ' under system control. This delivers two functions, the first being
      ' a visual indicator to the player that something has happened, and the second
      ' it stops the ability to spam attacks on a single creature
      victim.KnockBack(tx / d, ty / d, 0.2F)

      If victim IsNot m_player Then
        victim.OnInteract(m_player)
      Else
        ' We must ensure the player is never pushed out of bounds by the physics engine. This
        ' is a bit of a hack, but it allows knockbacks to occur providing there is an exit
        ' point for the player to be knocked back into. If the player is "mobbed" then they
        ' become trapped, and must fight their way out
        victim.SolidVsDyn = True
      End If

      If projectile.OneHit Then projectile.Redundant = True

    End If

  End Sub

End Class