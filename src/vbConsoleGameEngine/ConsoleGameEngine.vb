Imports System.Runtime.InteropServices
Imports System.Threading

Public Structure WaveFormatEx
  Public FormatTag As Short
  Public Channels As Short
  Public SamplesPerSec As Integer
  Public AvgBytesPerSec As Integer
  Public BlockAlign As Short
  Public BitsPerSample As Short
  Public Size As Short
End Structure

<StructLayout(LayoutKind.Sequential)>
Public Structure WaveHdr
  Public Data As IntPtr
  Public BufferLength As Integer
  Public BytesRecorded As Integer
  Public User As IntPtr
  Public Flags As Integer
  Public Loops As Integer
  Public [Next] As IntPtr
  Public Reserved As IntPtr
End Structure

<CodeAnalysis.SuppressMessage("Design", "CA1069:Enums values should not be duplicated", Justification:="<Pending>")>
Public Enum Color As Short
  FgBlack = &H0
  FgDarkBlue = &H1
  FgDarkGreen = &H2
  FgDarkCyan = &H3
  FgDarkRed = &H4
  FgDarkMagenta = &H5
  FgDarkYellow = &H6
  FgGray = &H7
  FgDarkGray = &H8
  FgBlue = &H9
  FgGreen = &HA
  FgCyan = &HB
  FgRed = &HC
  FgMagenta = &HD
  FgYellow = &HE
  FgWhite = &HF
  BgBlack = &H0
  BgDarkBlue = &H10
  BgDarkGreen = &H20
  BgDarkCyan = &H30
  BgDarkRed = &H40
  BgDarkMagenta = &H50
  BgDarkYellow = &H60
  BgGray = &H70
  BgDarkGray = &H80
  BgBlue = &H90
  BgGreen = &HA0
  BgCyan = &HB0
  BgRed = &HC0
  BgMagenta = &HD0
  BgYellow = &HE0
  BgWhite = &HF0
End Enum

Public Enum PixelType As Short
  Solid = &H2588
  ThreeQuarters = &H2593
  Half = &H2592
  Quarter = &H2591
End Enum

Public Class Sprite

  Public Sub New()
  End Sub

  Public Sub New(width As Integer, height As Integer)
    Create(width, height)
  End Sub

  Public Sub New(file As String)
    If Not Load(file) Then Create(8, 8)
  End Sub

  Public Property Width As Integer
  Public Property Height As Integer

  Private m_glyphs() As Integer
  Private m_colors() As Color

  Private Sub Create(w As Integer, h As Integer)
    Width = w
    Height = h
    ReDim m_glyphs(w * h)
    ReDim m_colors(w * h)
    For i = 0 To (w * h) - 1
      m_glyphs(i) = 32 '" "c
      m_colors(i) = Color.BgBlack
    Next
  End Sub

  Public Sub SetGlyph(x As Integer, y As Integer, color As Integer)
    If x < 0 OrElse x >= Width OrElse y < 0 OrElse y >= Height Then
      Return
    Else
      m_glyphs(y * Width + x) = color
    End If
  End Sub

  Public Sub SetColor(x As Integer, y As Integer, c As Color)
    If x < 0 OrElse x >= Width OrElse y < 0 OrElse y >= Height Then
      Return
    Else
      m_colors(y * Width + x) = c
    End If
  End Sub

  Public Function GetGlyph(x As Integer, y As Integer) As Integer
    If x < 0 OrElse x >= Width OrElse y < 0 OrElse y >= Height Then
      Return 32 '" "c
    Else
      Return m_glyphs(y * Width + x)
    End If
  End Function

  Public Function GetColor(x As Integer, y As Integer) As Color
    If x < 0 OrElse x >= Width OrElse y < 0 OrElse y >= Height Then
      Return Color.BgBlack
    Else
      Return m_colors(y * Width + x)
    End If
  End Function

  Public Function SampleGlyph(x As Single, y As Single) As Integer
    If Single.IsNaN(x) Then Return 32
    Dim sx = CInt(Fix(x * Width))
    Dim sy = CInt(Fix(y * Height - 1.0F))
    If sx < 0 OrElse sx >= Width OrElse sy < 0 OrElse sy >= Height Then
      Return AscW(" "c)
    Else
      Return m_glyphs(sy * Width + sx) 'AscW(m_glyphs(sy * Width + sx))
    End If
  End Function

  Public Function SampleColor(x As Single, y As Single) As Color
    If Single.IsNaN(x) Then Return Color.BgBlack
    Dim sx = CInt(Fix(x * Width))
    Dim sy = CInt(Fix(y * Height - 1.0F))
    If sx < 0 OrElse sx >= Width OrElse sy < 0 OrElse sy >= Height Then
      Return Color.BgBlack
    Else
      Return m_colors(sy * Width + sx)
    End If
  End Function

  Public Function Save(file As String) As Boolean

    Using f = IO.File.Create(file)

      If f Is Nothing Then Return False

      Using bw As New IO.BinaryWriter(f)
        bw.Write(Width)
        bw.Write(Height)
        For i = 0 To (Width * Height) - 1
          bw.Write(m_colors(i))
        Next
        For i = 0 To (Width * Height) - 1
          'bw.Write(CShort(AscW(m_glyphs(i))))
          bw.Write(m_glyphs(i))
        Next
      End Using

      f.Close()

    End Using

    Return True

  End Function

  Public Function Load(file As String) As Boolean

    ReDim m_glyphs(0)
    ReDim m_colors(0)
    Width = 0
    Height = 0

    Using f = IO.File.OpenRead(file)

      If f Is Nothing Then Return False

      Using br As New IO.BinaryReader(f)

        Width = br.ReadInt32()
        Height = br.ReadInt32()

        Create(Width, Height)

        For i = 0 To (Width * Height) - 1
          m_colors(i) = CType(br.ReadInt16(), Color)
        Next
        For i As Integer = 0 To (Width * Height) - 1
          m_glyphs(i) = AscW(BitConverter.ToChar(BitConverter.GetBytes(br.ReadInt16), 0))
        Next

      End Using

      f.Close()

    End Using

    Return True

  End Function

End Class

Public MustInherit Class ConsoleGameEngine

#Region "Win32"

  Public Const MAX_PATH As Integer = 260

  <StructLayout(LayoutKind.Sequential)>
  Private Structure KeyEventRecord
    Public KeyDown As Short
    Public RepeatCount As Short
    Public VirtualKeyCode As Short
    Public VirtualScanCode As Short
    Public [Char] As Char
    Public ControlKeyState As Integer
  End Structure

  <StructLayout(LayoutKind.Sequential)>
  Private Structure MouseEventRecord
    Public MousePosition As Coord
    Public ButtonState As Integer
    Public ControlKeyState As Integer
    Public EventFlags As Integer
  End Structure

  <StructLayout(LayoutKind.Sequential)>
  Private Structure WindowBufferSizeRecord
    Public Size As Coord
  End Structure

  <StructLayout(LayoutKind.Sequential)>
  Private Structure MenuEventRecord
    Public CommandId As Integer
  End Structure

  <StructLayout(LayoutKind.Sequential)>
  Private Structure FocusEventRecord
    Public SetFocus As Short
  End Structure

  <StructLayout(LayoutKind.Explicit)>
  Private Structure INPUT_RECORD
    <FieldOffset(0)> Public EventType As UShort
    <FieldOffset(4)> Public KeyEvent As KeyEventRecord
    <FieldOffset(4)> Public MouseEvent As MouseEventRecord
    <FieldOffset(4)> Public WindowBufferSizeEvent As WindowBufferSizeRecord
    <FieldOffset(4)> Public MenuEvent As MenuEventRecord
    <FieldOffset(4)> Public FocusEvent As FocusEventRecord
  End Structure

  <StructLayout(LayoutKind.Sequential)>
  Private Structure ConsoleScreenBufferInfo
    Public Size As Coord
    Public CursorPosition As Coord
    Public Attributes As Short
    Public Window As SmallRect
    Public MaximumWindowSize As Coord
  End Structure

  <StructLayout(LayoutKind.Sequential, CharSet:=CharSet.Unicode)>
  Private Structure ConsoleFontInfoEx
    Public Size As Integer
    Public Font As Integer
    Public FontSize As Coord
    Public FontFamily As Integer
    Public FontWeight As Integer
    <MarshalAs(UnmanagedType.ByValTStr, SizeConst:=LF_FACESIZE)>
    Public FaceName As String
  End Structure

  <StructLayout(LayoutKind.Sequential)>
  Private Structure SmallRect
    Public Left As Short
    Public Top As Short
    Public Right As Short
    Public Bottom As Short
    Public Sub New(left As Short, top As Short, right As Short, bottom As Short)
      Me.Left = left
      Me.Top = top
      Me.Right = right
      Me.Bottom = bottom
    End Sub
  End Structure

  <StructLayout(LayoutKind.Sequential)>
  Private Structure Coord
    Public X As Short
    Public Y As Short
    Public Sub New(x As Short, y As Short)
      Me.X = x
      Me.Y = y
    End Sub
  End Structure

  Private Structure CharInfo
    Public UnicodeChar As Char
    Public Attributes As Short
  End Structure

  <StructLayout(LayoutKind.Explicit, CharSet:=CharSet.Unicode)>
  Public Structure CharUnion
    <FieldOffset(0)> Public UnicodeChar As Char
    <FieldOffset(0)> Public AsciiChar As Byte
  End Structure

  <StructLayout(LayoutKind.Explicit, CharSet:=CharSet.Unicode)>
  Public Structure CharInfoUnion
    <FieldOffset(0)> Public CharUnion As CharUnion
    <FieldOffset(2)> Public Attributes As Short
  End Structure

  Private Declare Auto Function GetStdHandle Lib "kernel32.dll" (nStdHandle As Integer) As IntPtr
  Private Declare Function SetConsoleWindowInfo Lib "kernel32" (hConsoleOutput As IntPtr, bAbsolute As Boolean, ByRef lpConsoleWindow As SmallRect) As Boolean
  Private Declare Function SetConsoleScreenBufferSize Lib "kernel32" (hConsoleOutput As IntPtr, dwSize As Coord) As Boolean
  Private Declare Function SetConsoleActiveScreenBuffer Lib "kernel32" (hConsoleOutput As IntPtr) As Boolean
  'Private Declare Unicode Function SetCurrentConsoleFontEx Lib "kernel32" (hConsoleOutput As IntPtr, bMaximumWindow As Boolean, ByRef lpConsoleCurrentFontEx As CONSOLE_FONT_INFOEX) As Boolean
  <DllImport("kernel32.dll", SetLastError:=True, CharSet:=CharSet.Unicode)>
  Private Shared Function SetCurrentConsoleFontEx(hConsoleOutput As IntPtr, bMaximumWindow As Boolean, lpConsoleCurrentFontEx As ConsoleFontInfoEx) As Boolean
  End Function
  Private Declare Unicode Function GetConsoleScreenBufferInfo Lib "kernel32.dll" (hConsoleOutput As IntPtr, ByRef lpConsoleScreenBufferInfo As ConsoleScreenBufferInfo) As Boolean
  Private Declare Auto Function SetConsoleMode Lib "kernel32.dll" (hConsoleHandle As IntPtr, dwMode As Integer) As Boolean
  Private Delegate Function ConsoleCtrlHandlerRoutine(dwCtrlType As Integer) As Boolean
  Private Declare Auto Function SetConsoleCtrlHandler Lib "kernel32.dll" (handlerRoutine As ConsoleCtrlHandlerRoutine, add As Boolean) As Boolean
  Private Declare Function GetAsyncKeyState Lib "user32.dll" (virtualKeyCode As Integer) As Short
  Private Declare Function GetNumberOfConsoleInputEvents Lib "kernel32" (hConsoleInput As IntPtr, ByRef lpcNumberOfEvents As Integer) As Boolean
  Private Declare Function ReadConsoleInput Lib "kernel32" Alias "ReadConsoleInputW" (hConsoleInput As IntPtr, ByRef lpBuffer As INPUT_RECORD, nLength As Integer, ByRef lpNumberOfEventsRead As Integer) As Boolean
  Private Declare Function waveOutOpen Lib "winmm.dll" (ByRef hwo As IntPtr, uDeviceID As Integer, ByRef pwfx As WaveFormatEx, dwCallback As IntPtr, dwCallbackInstance As IntPtr, fdwOpen As Integer) As Integer
  Private Declare Function waveOutPrepareHeader Lib "winmm.dll" (hwo As IntPtr, ByRef pwh As WaveHdr, cbwh As Integer) As Integer
  Private Declare Function waveOutUnprepareHeader Lib "winmm.dll" (hwo As IntPtr, ByRef pwh As WaveHdr, cbwh As Integer) As Integer
  Private Declare Function waveOutWrite Lib "winmm.dll" (hwo As IntPtr, ByRef pwh As WaveHdr, cbwh As Integer) As Integer
  Private Declare Function GetLastError Lib "kernel32.dll" () As Integer

  <DllImport("kernel32.dll", SetLastError:=True)>
  Private Shared Function SetConsoleTitle(lpConsoleTitle As String) As Boolean
  End Function

  Private Declare Function WriteConsoleOutputCharacter Lib "kernel32.dll" Alias "WriteConsoleOutputCharacterA" (hConsoleOutput As IntPtr, lpCharacter As String, nLength As UInteger, dwWriteCoord As Coord, ByRef lpNumberOfCharsWritten As UInteger) As Boolean
  'Private Declare Function WriteConsoleOutput Lib "kernel32.dll" Alias "WriteConsoleOutputA" (ByVal hConsoleOutput As IntPtr, ByVal lpBuffer As CharInfo(), ByVal dwBufferSize As COORD, ByVal dwBufferCoord As COORD, ByRef lpWriteRegion As SMALL_RECT) As Boolean
  Private Declare Unicode Function WriteConsoleOutput Lib "kernel32.dll" Alias "WriteConsoleOutputW" (hConsoleOutput As IntPtr, lpBuffer As CharInfoUnion(), dwBufferSize As Coord, dwBufferCoord As Coord, ByRef lpWriteRegion As SmallRect) As Boolean

  Private Const WHDR_PREPARED As Integer = &H2
  Private Const WOM_DONE As Integer = &H3BD
  Private Const STD_INPUT_HANDLE As Integer = -10
  Private Const STD_OUTPUT_HANDLE As Integer = -11
  Private Const STD_ERROR_HANDLE As Integer = -12
  Private Const INVALID_HANDLE_VALUE As Integer = -1
  Private Const FF_DONTCARE As Integer = &H0
  Private Const FW_NORMAL As Short = &H400
  Private Const LF_FACESIZE As Integer = 32
  Private Const ENABLE_EXTENDED_FLAGS As Integer = &H80
  Private Const ENABLE_WINDOW_INPUT As Integer = &H20
  Private Const ENABLE_MOUSE_INPUT As Integer = &H10
  Private Const CTRL_CLOSE_EVENT As Integer = 2
  Private Const FOCUS_EVENT As Integer = &H10
  Private Const MOUSE_EVENT As Integer = &H2
  Private Const MOUSE_MOVED As Integer = &H1
  Private Const CALLBACK_FUNCTION As Integer = &H3
  Private Const S_OK As Integer = &H0

#End Region

#Region "VK"

  Public Const VK_BACK As Integer = &H8
  Public Const VK_TAB As Integer = &H9
  Public Const VK_SHIFT As Integer = &H10
  Public Const VK_CONTROL As Integer = &H11
  Public Const VK_ESCAPE As Integer = &H1B
  Public Const VK_SPACE As Integer = &H20
  Public Const VK_LEFT As Integer = &H25
  Public Const VK_RIGHT As Integer = &H27
  Public Const VK_UP As Integer = &H26
  Public Const VK_DOWN As Integer = &H28
  Public Const VK_PRIOR As Integer = &H21
  Public Const VK_NEXT As Integer = &H22
  Public Const VK_DELETE As Integer = &H2E
  Public Const VK_NUMPAD0 As Integer = &H60
  Public Const VK_NUMPAD1 As Integer = &H61
  Public Const VK_NUMPAD2 As Integer = &H62
  Public Const VK_NUMPAD3 As Integer = &H63
  Public Const VK_NUMPAD4 As Integer = &H64
  Public Const VK_NUMPAD5 As Integer = &H65
  Public Const VK_NUMPAD6 As Integer = &H66
  Public Const VK_NUMPAD7 As Integer = &H67
  Public Const VK_NUMPAD8 As Integer = &H68
  Public Const VK_NUMPAD9 As Integer = &H69
  Public Const VK_F1 As Integer = &H70
  Public Const VK_F2 As Integer = &H71
  Public Const VK_F3 As Integer = &H72
  Public Const VK_F4 As Integer = &H73
  Public Const VK_F5 As Integer = &H74
  Public Const VK_F6 As Integer = &H75
  Public Const VK_F7 As Integer = &H76
  Public Const VK_F8 As Integer = &H77
  Public Const VK_F9 As Integer = &H78
  Public Const VK_F10 As Integer = &H79

#End Region

  Public ReadOnly m_random As New Random

  Public Const RAND_MAX As Integer = 2147483647

  Public ReadOnly Property Rnd As Double
    Get
      Return m_random.NextDouble
    End Get
  End Property

  ' Provide for something *similar* to C++.
  Public ReadOnly Property Rand As Integer
    Get
      Return CInt(Fix(m_random.NextDouble * RAND_MAX))
    End Get
  End Property

  Private m_screenWidth As Integer
  Private m_screenHeight As Integer
  Private ReadOnly m_console As IntPtr
  Private ReadOnly m_consoleIn As IntPtr
  Private ReadOnly m_keyNewState(255) As Short
  Private ReadOnly m_keyOldState(255) As Short
  Private ReadOnly m_mouseOldState(4) As Boolean
  Private ReadOnly m_mouseNewState(4) As Boolean

  Public Structure KeyState
    Public Pressed As Boolean
    Public Released As Boolean
    Public Held As Boolean
  End Structure

  Public ReadOnly m_keys(255) As KeyState
  Public ReadOnly m_mouse(4) As KeyState

  Private ReadOnly m_originalConsole As IntPtr
  Public m_mousePosX As Integer
  Public m_mousePosY As Integer
  Private m_enableSound As Boolean
  Public m_appName As String
  Private m_rectWindow As SmallRect
  Public m_bufScreen As CharInfoUnion()
  Private m_consoleInFocus As Boolean = True

  Shared m_atomActive As Boolean = False
  Shared ReadOnly m_gameFinished As AutoResetEvent = New AutoResetEvent(False)
  Shared ReadOnly m_muxGame As Mutex = New Mutex()

  Public Sub New()

    m_screenWidth = 80
    m_screenHeight = 30

    m_console = GetStdHandle(STD_OUTPUT_HANDLE)
    m_consoleIn = GetStdHandle(STD_INPUT_HANDLE)

    Array.Clear(m_keyNewState, 0, 256)
    Array.Clear(m_keyOldState, 0, 256)
    Array.Clear(m_keys, 0, 256)
    m_mousePosX = 0
    m_mousePosY = 0

    m_enableSound = False

    m_appName = "Default"

  End Sub

  Public Function GetKey(keyId As String) As KeyState
    Return m_keys(AscW(keyId))
  End Function

  Public Function GetKey(keyId As Char) As KeyState
    Return m_keys(AscW(keyId))
  End Function

  Public Function GetKey(keyId As Integer) As KeyState
    Return m_keys(keyId)
  End Function

  Public Function GetMouseX() As Integer
    Return m_mousePosX
  End Function

  Public Function GetMouseY() As Integer
    Return m_mousePosY
  End Function

  Public Function GetMouse(mouseButtonId As Integer) As KeyState
    Return m_mouse(mouseButtonId)
  End Function

  Public Function IsFocused() As Boolean
    Return m_consoleInFocus
  End Function

  Public Sub EnableSound()
    m_enableSound = True
  End Sub

  Public Function ConstructConsole(width As Integer, height As Integer, fontWidth As Integer, fontHeight As Integer) As Integer

    If m_console = New IntPtr(INVALID_HANDLE_VALUE) Then
      Return GenError("Bad Handle")
    End If

    m_screenWidth = width
    m_screenHeight = height

    ' Update 13/09/2017 - It seems that the console behaves differently on some systems
    ' and I'm unsure why this is. It could be to do with windows default settings, or
    ' screen resolutions, or system languages. Unfortunately, MSDN does not offer much
    ' by way of useful information, and so the resulting sequence is the reult of experiment
    ' that seems to work in multiple cases.
    '
    ' The problem seems to be that the SetConsoleXXX functions are somewhat circular and 
    ' fail depending on the state of the current console properties, i.e. you can't set
    ' the buffer size until you set the screen size, but you can't change the screen size
    ' until the buffer size is correct. This coupled with a precise ordering of calls
    ' makes this procedure seem a little mystical :-P. Thanks to wowLinh for helping - Jx9

    ' Change console visual size to a minimum so ScreenBuffer can shrink
    ' below the actual visual size
    m_rectWindow = New SmallRect With {.Left = 0,
                                       .Top = 0,
                                       .Right = 1,
                                       .Bottom = 1}
    SetConsoleWindowInfo(m_console, True, m_rectWindow)

    ' Set the size of the screen buffer
    Dim coord As Coord
    coord.X = CShort(m_screenWidth)
    coord.Y = CShort(m_screenHeight)
    If Not SetConsoleScreenBufferSize(m_console, coord) Then
      Return GenError("SetConsoleScreenBufferSize")
    End If

    ' Assign screen buffer to the console
    If Not SetConsoleActiveScreenBuffer(m_console) Then
      Return GenError("SetConsoleActiveScreenBuffer")
    End If

    ' Set the font size now that the screen buffer has been assigned to the console
    Dim cfi As New ConsoleFontInfoEx
    'cfi.Size = Len(cfi)
    cfi.Size = Marshal.SizeOf(cfi)
    cfi.Font = 0
    cfi.FontSize.X = CShort(fontWidth)
    cfi.FontSize.Y = CShort(fontHeight)
    cfi.FontFamily = FF_DONTCARE
    cfi.FontWeight = FW_NORMAL

    ' Dim version As DWORD = GetVersion()
    ' Dim major As DWORD = CByte(LOBYTE(LOWORD(version)))
    ' Dim minor As DWORD = CByte(HIBYTE(LOWORD(version)))

    ' If (major > 6) OrElse ((major = 6) AndAlso (minor >= 2) AndAlso (minor < 4)) Then
    '     wcscpy_s(cfi.FaceName, L"Raster") ' Windows 8 :(
    ' Else
    '     wcscpy_s(cfi.FaceName, L"Lucida Console") ' Everything else :P
    ' End If

    ' wcscpy_s(cfi.FaceName, L"Liberation Mono")

    'wcscpy_s(cfi.FaceName, "Consolas")

    Dim faceName(32) As Char 'Allocate space for a 32-character buffer
    'Dim name = "Consolas"
    'name.CopyTo(faceName) 'Copy the string into the buffer
    'cfi.FaceName = faceName 'Assign the buffer to cfi.FaceName
    cfi.FaceName = "Consolas" '& ChrW(0)
    If Not SetCurrentConsoleFontEx(m_console, False, cfi) Then
      Return GenError("SetCurrentConsoleFontEx")
    End If

    ' Get screen buffer info and check the maximum allowed window size. Return
    ' error if exceeded, so user knows their dimensions/fontsize are too large
    Dim csbi As ConsoleScreenBufferInfo
    If Not GetConsoleScreenBufferInfo(m_console, csbi) Then
      Return GenError("GetConsoleScreenBufferInfo")
    End If
    If m_screenHeight > csbi.MaximumWindowSize.Y Then
      Return GenError("Screen Height / Font Height Too Big")
    End If
    If m_screenWidth > csbi.MaximumWindowSize.X Then
      Return GenError("Screen Width / Font Width Too Big")
    End If

    ' Set Physical Console Window Size
    m_rectWindow = New SmallRect(0, 0, CShort(m_screenWidth - 1), CShort(m_screenHeight - 1))
    If Not SetConsoleWindowInfo(m_console, True, m_rectWindow) Then
      Return GenError("SetConsoleWindowInfo")
    End If

    ' Set flags to allow mouse input		
    If Not SetConsoleMode(m_consoleIn, ENABLE_EXTENDED_FLAGS Or ENABLE_WINDOW_INPUT Or ENABLE_MOUSE_INPUT) Then
      Return GenError("SetConsoleMode")
    End If

    ' Allocate memory for screen buffer
    m_bufScreen = New CharInfoUnion(m_screenWidth * m_screenHeight - 1) {}
    Array.Clear(m_bufScreen, 0, m_screenWidth * m_screenHeight)

    SetConsoleCtrlHandler(AddressOf CloseHandler, True)

    Return 1

  End Function

  Private Function CloseHandler(evt As Integer) As Boolean

    ' Note that std::unique_lock has been replaced by Threading.Monitor.Enter, 
    ' which is a similar construct in .NET. Also, DWORD has been replaced by
    ' UInteger, since it is an unsigned 32-bit integer in both languages.

    ' Note this gets called in a seperate OS thread, so it must
    ' only exit when the game has finished cleaning up, Or else
    ' the process will be killed before OnUserDestroy() has finished
    If evt = CTRL_CLOSE_EVENT Then
      m_atomActive = False
      ' Wait for thread to be exited
      'Dim ul As New Threading.Monitor.Enter(m_muxGame)
      'm_gameFinished.Wait(ul)
    End If
    Return True
  End Function

  Private Shared Function GenError(msg As String) As Integer
    Dim lastError = GetLastError
    Dim ex As Exception = New System.ComponentModel.Win32Exception()
    Dim errorMessage As String = ex.Message
    Console.SetOut(New IO.StreamWriter(Console.OpenStandardOutput()))
    Console.WriteLine($"ERROR: {msg}{Environment.NewLine}" & vbTab & $"{errorMessage}")
    Return 1
  End Function

  Public Sub Draw(x As Double, y As Double, Optional c As Integer = &H2588S, Optional col As Integer = &HFS)
    Draw(CInt(x), CInt(y), c, col)
  End Sub

  Public Overridable Sub Draw(x As Integer, y As Integer, Optional c As Integer = &H2588S, Optional col As Integer = &HFS)
    If x >= 0 AndAlso x < m_screenWidth AndAlso y >= 0 AndAlso y < m_screenHeight Then
      m_bufScreen(y * m_screenWidth + x).CharUnion.UnicodeChar = ChrW(c)
      m_bufScreen(y * m_screenWidth + x).Attributes = CShort(col)
    End If
  End Sub

  Public Sub Cls()
    Fill(0, 0, ScreenWidth, ScreenHeight, AscW(" "), Color.FgBlack)
  End Sub

  Public Sub Fill(x1 As Single, y1 As Single, x2 As Single, y2 As Single, Optional c As Integer = &H2588S, Optional col As Integer = &HFS)
    Fill(CInt(Fix(x1)), CInt(Fix(y1)), CInt(Fix(x2)), CInt(Fix(y2)), c, col)
  End Sub

  Public Sub Fill(x1 As Integer, y1 As Integer, x2 As Integer, y2 As Integer, Optional c As Integer = &H2588S, Optional col As Integer = &HFS)
    Clip(x1, y1)
    Clip(x2, y2)
    For x = x1 To x2 - 1
      For y = y1 To y2 - 1
        Draw(x, y, c, col)
      Next
    Next
  End Sub

  Public Sub DrawString(x As Double, y As Double, c As String, Optional col As Short = &HFS)
    DrawString(CInt(x), CInt(y), c, col)
  End Sub

  Public Sub DrawString(x As Integer, y As Integer, c As String, Optional col As Short = &HFS)
    For i = 0 To c.Length - 1
      m_bufScreen(y * m_screenWidth + x + i).CharUnion.UnicodeChar = c(i)
      m_bufScreen(y * m_screenWidth + x + i).Attributes = col
    Next i
  End Sub

  Public Sub DrawStringAlpha(x As Integer, y As Integer, c As String, Optional col As Short = &HFS)
    For i As Integer = 0 To c.Length - 1
      If c(i) <> " "c Then
        m_bufScreen(y * m_screenWidth + x + i).CharUnion.UnicodeChar = c(i)
        m_bufScreen(y * m_screenWidth + x + i).Attributes = col
      End If
    Next i
  End Sub

  Public Sub Clip(ByRef x As Integer, ByRef y As Integer)
    If x < 0 Then x = 0
    If x >= m_screenWidth Then x = m_screenWidth
    If y < 0 Then y = 0
    If y >= m_screenHeight Then y = m_screenHeight
  End Sub

  Public Sub DrawLine(x1 As Double, y1 As Double, x2 As Double, y2 As Double, Optional c As Integer = &H2588, Optional col As Integer = &HF)
    DrawLine(CInt(x1), CInt(y1), CInt(x2), CInt(y2), c, col)
  End Sub

  Public Sub DrawLine(x1 As Integer, y1 As Integer, x2 As Integer, y2 As Integer, Optional c As Integer = &H2588, Optional col As Integer = &HF)
    Dim x, y, dx, dy, dx1, dy1, px, py, xe, ye, i As Integer
    dx = x2 - x1
    dy = y2 - y1
    dx1 = Math.Abs(dx)
    dy1 = Math.Abs(dy)
    px = 2 * dy1 - dx1
    py = 2 * dx1 - dy1
    If dy1 <= dx1 Then
      If dx >= 0 Then
        x = x1
        y = y1
        xe = x2
      Else
        x = x2
        y = y2
        xe = x1
      End If
      Draw(x, y, c, col)
      For i = 0 To (xe - x) - 1
        x += 1
        If px < 0 Then
          px += 2 * dy1
        Else
          If (dx < 0 AndAlso dy < 0) OrElse (dx > 0 AndAlso dy > 0) Then
            y += 1
          Else
            y -= 1
          End If
          px += 2 * (dy1 - dx1)
        End If
        Draw(x, y, c, col)
      Next
    Else
      If dy >= 0 Then
        x = x1
        y = y1
        ye = y2
      Else
        x = x2
        y = y2
        ye = y1
      End If
      Draw(x, y, c, col)
      For i = 0 To (ye - y) - 1
        y += 1
        If py <= 0 Then
          py += 2 * dx1
        Else
          If (dx < 0 AndAlso dy < 0) OrElse (dx > 0 AndAlso dy > 0) Then
            x += 1
          Else
            x -= 1
          End If
          py += 2 * (dx1 - dy1)
        End If
        Draw(x, y, c, col)
      Next
    End If
  End Sub

  Public Sub DrawTriangle(x1 As Single, y1 As Single, x2 As Single, y2 As Single, x3 As Single, y3 As Single, Optional c As Integer = &H2588, Optional col As Integer = &HF)
    DrawTriangle(CInt(Fix(x1)), CInt(Fix(y1)), CInt(Fix(x2)), CInt(Fix(y2)), CInt(Fix(x3)), CInt(Fix(y3)), c, col)
  End Sub

  Public Sub DrawTriangle(x1 As Integer, y1 As Integer, x2 As Integer, y2 As Integer, x3 As Integer, y3 As Integer, Optional c As Integer = &H2588, Optional col As Integer = &HF)
    If col = -1 Then
      DrawLine(x1, y1, x2, y2, c, Color.FgRed)
      DrawLine(x2, y2, x3, y3, c, Color.FgBlack)
      DrawLine(x3, y3, x1, y1, c, Color.FgBlue)
    Else
      DrawLine(x1, y1, x2, y2, c, col)
      DrawLine(x2, y2, x3, y3, c, col)
      DrawLine(x3, y3, x1, y1, c, col)
    End If
  End Sub

  Private Shared Sub Swap(ByRef x As Integer, ByRef y As Integer)
    Dim t = x
    x = y
    y = t
  End Sub

  Public Sub FillTriangle(x1 As Single, y1 As Single, x2 As Single, y2 As Single, x3 As Single, y3 As Single, Optional c As Integer = &H2588, Optional col As Integer = &HF)
    FillTriangle(CInt(Fix(x1)), CInt(Fix(y1)), CInt(Fix(x2)), CInt(Fix(y2)), CInt(Fix(x3)), CInt(Fix(y3)), c, col)
  End Sub

  ' https://www.avrfreaks.net/sites/default/files/triangles.c
  Public Sub FillTriangle(x1 As Integer, y1 As Integer, x2 As Integer, y2 As Integer, x3 As Integer, y3 As Integer, Optional c As Integer = &H2588, Optional col As Integer = &HF)

    ' Refactored to be a private shared function due to the byref need on the params.
    'Dim swap As Action(Of Integer, Integer) = Sub(ByRef x As Integer, ByRef y As Integer)
    '                                            Dim t As Integer = x
    '                                            x = y
    '                                            y = t
    '                                          End Sub

    Dim drawLine As Action(Of Integer, Integer, Integer) = Sub(sx As Integer, ex As Integer, ny As Integer)
                                                             For i = sx To ex
                                                               Draw(i, ny, c, col)
                                                             Next
                                                           End Sub

    Dim t1x, t2x, y, minx, maxx, t1xp, t2xp As Integer
    Dim changed1 = False
    Dim changed2 = False
    Dim signx1, signx2, dx1, dy1, dx2, dy2 As Integer
    Dim e1, e2 As Integer

    ' Sort vertices
    If y1 > y2 Then Swap(y1, y2) : Swap(x1, x2)
    If y1 > y3 Then Swap(y1, y3) : Swap(x1, x3)
    If y2 > y3 Then Swap(y2, y3) : Swap(x2, x3)

    t1x = x1 : t2x = x1 : y = y1 ' Starting points
    dx1 = x2 - x1
    If dx1 < 0 Then dx1 = -dx1 : signx1 = -1 Else signx1 = 1
    dy1 = y2 - y1

    dx2 = x3 - x1
    If dx2 < 0 Then dx2 = -dx2 : signx2 = -1 Else signx2 = 1
    dy2 = y3 - y1

    If dy1 > dx1 Then ' swap values
      Swap(dx1, dy1)
      changed1 = True
    End If
    If dy2 > dx2 Then ' swap values
      Swap(dy2, dx2)
      changed2 = True
    End If

    e2 = dx2 >> 1
    ' Flat top, just process the second half
    If y1 = y2 Then GoTo nextx
    e1 = dx1 >> 1

    For i = 0 To dx1 - 1
      t1xp = 0 : t2xp = 0
      If t1x < t2x Then minx = t1x : maxx = t2x Else minx = t2x : maxx = t1x
      ' process first line until y value is about to change
      While i < dx1
        'i += 1 <------------------- really don't understand why this was here????!!!???
        e1 += dy1
        While e1 >= dx1
          e1 -= dx1
          If changed1 Then t1xp = signx1 Else GoTo next1 ' t1x += signx1;
        End While
        If changed1 Then Exit While Else t1x += signx1
      End While
      ' Move line
next1:
      ' process second line until y value is about to change
      While True
        e2 += dy2
        While e2 >= dx2
          e2 -= dx2
          If changed2 Then t2xp = signx2 Else GoTo next2 ' t2x += signx2
        End While
        If changed2 Then Exit While Else t2x += signx2
      End While
next2:
      If minx > t1x Then minx = t1x
      If minx > t2x Then minx = t2x
      If maxx < t1x Then maxx = t1x
      If maxx < t2x Then maxx = t2x
      drawLine(minx, maxx, y) ' Draw line from min to max points found on the y; now increase y
      If Not changed1 Then t1x += signx1
      t1x += t1xp
      If Not changed2 Then t2x += signx2
      t2x += t2xp
      y += 1
      If y = y2 Then Exit For
    Next
nextx:
    ' Second half
    dx1 = x3 - x2
    If dx1 < 0 Then dx1 = -dx1 : signx1 = -1 Else signx1 = 1
    dy1 = y3 - y2
    t1x = x2

    If dy1 > dx1 Then   ' swap values
      Swap(dy1, dx1)
      changed1 = True
    Else
      changed1 = False
    End If

    e1 = dx1 >> 1

    For i = 0 To dx1 - 1
      t1xp = 0 : t2xp = 0
      If t1x < t2x Then minx = t1x : maxx = t2x Else minx = t2x : maxx = t1x
      ' process first line until y value is about to change
      While i < dx1
        e1 += dy1
        While e1 >= dx1
          e1 -= dx1
          If changed1 Then t1xp = signx1 : Exit While Else GoTo next3 ' t1x += signx1
        End While
        If changed1 Then Exit While Else t1x += signx1
        If i < dx1 Then i += 1
      End While
next3:
      ' process second line until y value is about to change
      While t2x <> x3
        e2 += dy2
        While e2 >= dx2
          e2 -= dx2
          If changed2 Then t2xp = signx2 Else GoTo next4
        End While
        If changed2 Then Exit While Else t2x += signx2
      End While
next4:
      If minx > t1x Then minx = t1x
      If minx > t2x Then minx = t2x
      If maxx < t1x Then maxx = t1x
      If maxx < t2x Then maxx = t2x
      drawLine(minx, maxx, y)
      If Not changed1 Then t1x += signx1
      t1x += t1xp
      If Not changed2 Then t2x += signx2
      t2x += t2xp
      y += 1
      If y > y3 Then
        Return
      End If
    Next

  End Sub

  Public Sub DrawCircle(xc As Single, yc As Single, r As Single, Optional c As Integer = &H2588, Optional col As Integer = &HF)
    DrawCircle(CInt(Fix(xc)), CInt(Fix(yc)), CInt(Fix(r)), c, col)
  End Sub

  Public Sub DrawCircle(xc As Integer, yc As Integer, r As Integer, Optional c As Integer = &H2588, Optional col As Integer = &HF)
    Dim x = 0
    Dim y = r
    Dim p = 3 - 2 * r
    If r = 0 Then Return
    While y >= x ' only formulate 1/8 of circle
      Draw(xc - x, yc - y, c, col) 'upper left left
      Draw(xc - y, yc - x, c, col) 'upper upper left
      Draw(xc + y, yc - x, c, col) 'upper upper right
      Draw(xc + x, yc - y, c, col) 'upper right right
      Draw(xc - x, yc + y, c, col) 'lower left left
      Draw(xc - y, yc + x, c, col) 'lower lower left
      Draw(xc + y, yc + x, c, col) 'lower lower right
      Draw(xc + x, yc + y, c, col) 'lower right right
      If p < 0 Then p += 4 * x + 6 : x += 1 Else p += 4 * (x - y) + 10 : x += 1 : y -= 1
    End While
  End Sub

  Sub FillCircle(xc As Double, yc As Double, r As Double, c As Integer, col As Integer)
    FillCircle(CInt(Fix(xc)), CInt(Fix(yc)), CInt(Fix(r)), c, col)
  End Sub

  Sub FillCircle(xc As Integer, yc As Integer, r As Integer, Optional c As Integer = &H2588, Optional col As Integer = &HF)

    Dim x = 0
    Dim y = r
    Dim p = 3 - 2 * r

    If r = 0 Then Return

    Dim drawLine = Sub(sx As Integer, ex As Integer, ny As Integer)
                     For i = sx To ex
                       Draw(i, ny, c, col)
                     Next
                   End Sub

    While y >= x
      ' Modified to draw scan-lines instead of edges
      drawLine(xc - x, xc + x, yc - y)
      drawLine(xc - y, xc + y, yc - x)
      drawLine(xc - x, xc + x, yc + y)
      drawLine(xc - y, xc + y, yc + x)
      If p < 0 Then
        p += 4 * x + 6
        x += 1
      Else
        p += 4 * (x - y) + 10
        x += 1
        y -= 1
      End If
    End While

  End Sub

  Sub DrawSprite(x As Double, y As Double, sprite As Sprite)
    DrawSprite(CInt(x), CInt(y), sprite)
  End Sub

  Sub DrawSprite(x As Integer, y As Integer, sprite As Sprite)
    If sprite Is Nothing Then
      Return
    End If
    For i As Integer = 0 To sprite.Width - 1
      For j As Integer = 0 To sprite.Height - 1
        If sprite.GetGlyph(i, j) <> 32 Then '" "c Then
          'Draw(x + i, y + j, AscW(sprite.GetGlyph(i, j)), sprite.GetColor(i, j))
          Draw(x + i, y + j, sprite.GetGlyph(i, j), sprite.GetColor(i, j))
        End If
      Next
    Next
  End Sub

  Public Sub DrawPartialSprite(x As Double, y As Double, sprite As Sprite, ox As Double, oy As Double, w As Double, h As Double)
    DrawPartialSprite(CInt(Fix(x)), CInt(Fix(y)), sprite, CInt(Fix(ox)), CInt(Fix(oy)), CInt(Fix(w)), CInt(Fix(h)))
  End Sub

  Public Sub DrawPartialSprite(x As Integer, y As Integer, sprite As Sprite, ox As Integer, oy As Integer, w As Integer, h As Integer)
    If sprite Is Nothing Then
      Return
    End If
    For i = 0 To w - 1
      For j = 0 To h - 1
        If sprite.GetGlyph(i + ox, j + oy) <> 32 Then '" "c Then
          'Draw(x + i, y + j, AscW(sprite.GetGlyph(i + ox, j + oy)), sprite.GetColor(i + ox, j + oy))
          Draw(x + i, y + j, sprite.GetGlyph(i + ox, j + oy), sprite.GetColor(i + ox, j + oy))
        End If
      Next
    Next
  End Sub

  Sub DrawWireFrameModel(vecModelCoordinates As List(Of (X As Single, Y As Single)), x As Single, y As Single, Optional r As Single = 0.0F, Optional s As Single = 1.0F, Optional col As Integer = Color.FgWhite, Optional c As Integer = PixelType.Solid)
    ' Tuple.Item1 = x coordinate
    ' Tuple.Item2 = y coordinate
    ' Create translated model vector of coordinate pairs
    Dim vecTransformedCoordinates As New List(Of (x As Single, y As Single))
    Dim verts As Integer = vecModelCoordinates.Count
    vecTransformedCoordinates.Capacity = verts

    ' Rotate
    For i As Integer = 0 To verts - 1
      Dim coordX As Single = vecModelCoordinates(i).X
      Dim coordY As Single = vecModelCoordinates(i).Y
      vecTransformedCoordinates.Add((coordX * CSng(Math.Cos(r)) - coordY * CSng(Math.Sin(r)), coordX * CSng(Math.Sin(r)) + coordY * CSng(Math.Cos(r))))
    Next

    ' Scale
    For i As Integer = 0 To verts - 1
      vecTransformedCoordinates(i) = (vecTransformedCoordinates(i).x * s, vecTransformedCoordinates(i).y * s)
    Next

    ' Translate
    For i As Integer = 0 To verts - 1
      vecTransformedCoordinates(i) = (vecTransformedCoordinates(i).x + x, vecTransformedCoordinates(i).y + y)
    Next

    ' Draw Closed Polygon
    For i As Integer = 0 To verts - 1
      Dim j As Integer = (i + 1) Mod verts
      DrawLine(CInt(vecTransformedCoordinates(i).x), CInt(vecTransformedCoordinates(i).y), CInt(vecTransformedCoordinates(j).x), CInt(vecTransformedCoordinates(j).y), c, col)
    Next

  End Sub

  Public Sub Start()
    ' Start the thread
    m_atomActive = True
    Dim t As New Threading.Thread(AddressOf GameThread)
    ' Wait for thread to be exited
    t.Start()
    't.Join()
  End Sub

  Public Function ScreenWidth() As Integer
    Return m_screenWidth
  End Function

  Public Function ScreenHeight() As Integer
    Return m_screenHeight
  End Function

  Private Sub GameThread()
    ' Create user resources as part of this thread
    If Not OnUserCreate() Then
      m_atomActive = False
    End If
    ' Check if sound system should be enabled
    If m_enableSound Then
      If Not CreateAudio() Then
        m_atomActive = False ' Failed to create audio system			
        m_enableSound = False
      End If
    End If

    Dim tp1 As DateTime = DateTime.Now
    Dim tp2 As DateTime = DateTime.Now

    While m_atomActive
      ' Run as fast as possible
      While m_atomActive
        ' Handle Timing
        tp2 = DateTime.Now
        Dim elapsedTime = tp2 - tp1
        tp1 = tp2
        Dim fElapsedTime = CSng(elapsedTime.TotalSeconds)

        ' Handle Keyboard Input
        For i = 0 To 255

          m_keyNewState(i) = GetAsyncKeyState(i)

          m_keys(i).Pressed = False
          m_keys(i).Released = False

          If m_keyNewState(i) <> m_keyOldState(i) Then
            If (m_keyNewState(i) And &H8000) <> 0 Then
              m_keys(i).Pressed = Not m_keys(i).Held
              m_keys(i).Held = True
            Else
              m_keys(i).Released = True
              m_keys(i).Held = False
            End If
          End If

          m_keyOldState(i) = m_keyNewState(i)

        Next

        ' Handle Mouse Input - Check for window events
        Dim inBuf(31) As INPUT_RECORD
        Dim events = 0
        GetNumberOfConsoleInputEvents(m_consoleIn, events)
        If events > 0 Then
          ReadConsoleInput(m_consoleIn, inBuf(0), events, events)
        End If

        ' Handle events - we only care about mouse clicks and movement for now
        For i = 0 To events - 1
          Select Case inBuf(i).EventType
            Case FOCUS_EVENT
              m_consoleInFocus = inBuf(i).FocusEvent.SetFocus <> 0
            Case MOUSE_EVENT
              Select Case inBuf(i).MouseEvent.EventFlags
                Case MOUSE_MOVED
                  m_mousePosX = inBuf(i).MouseEvent.MousePosition.X
                  m_mousePosY = inBuf(i).MouseEvent.MousePosition.Y
                Case 0
                  For m = 0 To 4
                    m_mouseNewState(m) = (inBuf(i).MouseEvent.ButtonState And (1 << m)) > 0
                  Next
                Case Else
                  ' Do nothing
              End Select
            Case Else
              ' Do nothing
          End Select
        Next

        For m = 0 To 4

          m_mouse(m).Pressed = False
          m_mouse(m).Released = False

          If m_mouseNewState(m) <> m_mouseOldState(m) Then
            If m_mouseNewState(m) Then
              m_mouse(m).Pressed = True
              m_mouse(m).Held = True
            Else
              m_mouse(m).Released = True
              m_mouse(m).Held = False
            End If
          End If

          m_mouseOldState(m) = m_mouseNewState(m)

        Next

        ' Handle Frame Update
        If Not OnUserUpdate(fElapsedTime) Then
          m_atomActive = False
        End If

        ' Update Title & Present Screen Buffer
        Dim s As String = String.Format("AddressOf.com - vbConsoleGameEngine - {0} - FPS: {1:0.00}", m_appName, 1.0F / fElapsedTime)
        SetConsoleTitle(s)
        WriteConsoleOutput(m_console, m_bufScreen, New Coord(CShort(m_screenWidth), CShort(m_screenHeight)), New Coord(0, 0), m_rectWindow)

        If m_enableSound Then
          ' Close and Clean up audio system
        End If

        'Allow the user to free resources if they have overridden the destroy function
        If OnUserDestroy() Then
          'Delete resources and exit if the user has permitted destroy
          Erase m_bufScreen
          SetConsoleActiveScreenBuffer(m_originalConsole)
          'm_cvGameFinished.notify_one()
        Else
          'User denied destroy for some reason, so continue running
          m_atomActive = True
        End If

      End While
    End While
  End Sub

  MustOverride Function OnUserCreate() As Boolean
  MustOverride Function OnUserUpdate(elapsedTime As Single) As Boolean

  Overridable Function OnUserDestroy() As Boolean
    Return False
  End Function

  Private Class AudioSample

    Sub New()
    End Sub

    Public Sub New(sWavFile As String)
      ' Load Wav file and convert to float format
      Dim f As System.IO.FileStream '= Nothing
      Try
        f = New System.IO.FileStream(sWavFile, System.IO.FileMode.Open, System.IO.FileAccess.Read)
      Catch ex As Exception
        Return
      End Try

      Dim dump(3) As Byte
      f.Read(dump, 0, 4) ' Read "RIFF"
      If System.Text.Encoding.ASCII.GetString(dump, 0, 4) <> "RIFF" Then Return
      f.Read(dump, 0, 4) ' Not Interested
      f.Read(dump, 0, 4) ' Read "WAVE"
      If System.Text.Encoding.ASCII.GetString(dump, 0, 4) <> "WAVE" Then Return

      ' Read Wave description chunk
      f.Read(dump, 0, 4) ' Read "fmt "
      f.Read(dump, 0, 4) ' Not Interested
      wavHeader = New WaveFormatEx() With {.FormatTag = 0}
      Dim length = Marshal.SizeOf(wavHeader) - 2 ' -2 because structure has 2 bytes to indicate its own size which are not in the wav file
      Dim headerBytes(length - 1) As Byte
      f.Read(headerBytes, 0, length)
      Dim headerPtr = Marshal.AllocHGlobal(length)
      Marshal.Copy(headerBytes, 0, headerPtr, length)
      Marshal.PtrToStructure(headerPtr, wavHeader)
      Marshal.FreeHGlobal(headerPtr)

      ' Just check if wave format is compatible with olcCGE
      If wavHeader.BitsPerSample <> 16 OrElse wavHeader.SamplesPerSec <> 44100 Then
        f.Close()
        Return
      End If

      ' Search for audio data chunk
      Dim nChunksize As Long = 0
      f.Read(dump, 0, 4) ' Read chunk header
      f.Read(BitConverter.GetBytes(nChunksize), 0, 4) ' Read chunk size
      While System.Text.Encoding.ASCII.GetString(dump, 0, 4) <> "data"
        ' Not audio data, so just skip it
        f.Seek(nChunksize, System.IO.SeekOrigin.Current)
        f.Read(dump, 0, 4)
        f.Read(BitConverter.GetBytes(nChunksize), 0, 4)
      End While

      ' Finally got to data, so read it all in and convert to float samples
      nSamples = nChunksize \ (wavHeader.Channels * (wavHeader.BitsPerSample >> 3))
      nChannels = wavHeader.Channels

      ' Create floating point buffer to hold audio sample
      fSample = New Single(CInt(nSamples * nChannels - 1)) {}

      ' Read in audio data and normalise
      For i = 0 To nSamples - 1
        For c = 0 To nChannels - 1
          Dim s = 0
          f.Read(BitConverter.GetBytes(s), 0, 2)
          fSample(CInt(i * nChannels + c)) = s \ Short.MaxValue
        Next
      Next

      ' All done, flag sound as valid
      f.Close()
      bSampleValid = True
    End Sub

    Public wavHeader As WaveFormatEx
    Public fSample As Single()
    Public nSamples As Long
    Public nChannels As Integer
    Public bSampleValid As Boolean

  End Class

  Private ReadOnly vecAudioSamples As New List(Of AudioSample)

  Structure sCurrentlyPlayingSample
    Public nAudioSampleID As Integer '= 0
    Public nSamplePosition As Long '= 0
    Public bFinished As Boolean '= False
    Public bLoop As Boolean '= False
  End Structure

  Private ReadOnly listActiveSamples As New List(Of sCurrentlyPlayingSample)

  Function LoadAudioSample(sWavFile As String) As UInteger
    If Not m_enableSound Then
      Return UInteger.MaxValue
    End If
    Dim a As New AudioSample(sWavFile)
    If a.bSampleValid Then
      vecAudioSamples.Add(a)
      Return CUInt(vecAudioSamples.Count)
    Else
      Return UInteger.MaxValue
    End If
  End Function

  ' Add sample 'id' to the mixers sounds to play list
  Sub PlaySample(id As Integer, Optional bLoop As Boolean = False)
    Dim a As sCurrentlyPlayingSample
    a.nAudioSampleID = id
    a.nSamplePosition = 0
    a.bFinished = False
    a.bLoop = bLoop
    listActiveSamples.Add(a)
  End Sub

  Shared Sub StopSample(id As Integer)

  End Sub

  Public Const WAVE_MAPPER As Integer = -1

  ' The audio system uses by default a specific wave format
  Public Function CreateAudio(Optional nSampleRate As Integer = 44100,
                              Optional nChannels As Integer = 1,
                              Optional nBlocks As Integer = 8,
                              Optional nBlockSamples As Integer = 512) As Boolean

    ' Initialise Sound Engine
    m_bAudioThreadActive = False
    m_nSampleRate = nSampleRate
    m_nChannels = nChannels
    m_nBlockCount = nBlocks
    m_nBlockSamples = nBlockSamples
    m_nBlockFree = m_nBlockCount
    m_nBlockCurrent = 0
    m_pBlockMemory = Nothing
    m_pWaveHeaders = Nothing

    ' Device is available
    Dim waveFormat As New WaveFormatEx With {
        .FormatTag = WAVE_FORMAT_PCM,
        .SamplesPerSec = m_nSampleRate,
        .BitsPerSample = CShort(8 * Marshal.SizeOf(Of Short)),
        .Channels = CShort(m_nChannels),
        .BlockAlign = CShort(waveFormat.BitsPerSample / 8 * waveFormat.Channels),
        .AvgBytesPerSec = waveFormat.SamplesPerSec * waveFormat.BlockAlign,
        .Size = 0
    }

    ' Open Device if valid
    'If waveOutOpen(m_hwDevice, WAVE_MAPPER, waveFormat, AddressOf waveOutProcWrap, Me, CALLBACK_FUNCTION) <> S_OK Then
    '  Return DestroyAudio()
    'End If

    ' Allocate Wave|Block Memory
    m_pBlockMemory = New Short(m_nBlockCount * m_nBlockSamples - 1) {}
    If m_pBlockMemory Is Nothing Then
      Return DestroyAudio()
    End If
    Array.Clear(m_pBlockMemory, 0, m_nBlockCount * m_nBlockSamples)

    m_pWaveHeaders = New WaveHdr(m_nBlockCount - 1) {}
    If m_pWaveHeaders Is Nothing Then
      Return DestroyAudio()
    End If
    Array.Clear(m_pWaveHeaders, 0, m_nBlockCount)

    ' Link headers to block memory
    For n As Integer = 0 To m_nBlockCount - 1
      m_pWaveHeaders(n).BufferLength = m_nBlockSamples * Marshal.SizeOf(Of Short)()
      'm_pWaveHeaders(n).lpData = Marshal.StringToHGlobalAnsi(CType(m_pBlockMemory + (n * m_nBlockSamples), String))
    Next

    m_bAudioThreadActive = True
    m_AudioThread = New System.Threading.Thread(AddressOf AudioThread)
    m_AudioThread.Start()

    ' Start the ball rolling with the sound delivery thread
    Dim lm As New Object()
    SyncLock lm
      Monitor.Pulse(m_muxBlockNotZero)
    End SyncLock

    Return True

  End Function

  Private Const WAVE_FORMAT_PCM As UShort = &H1

  ' Stop and clean up audio system
  Public Function DestroyAudio() As Boolean
    m_bAudioThreadActive = False
    Return False
  End Function

  ' Handler for soundcard request for more data
  Private Sub WaveOutProc(hWaveOut As IntPtr, uMsg As UInteger, dwParam1 As UInteger, dwParam2 As UInteger)
    If uMsg <> WOM_DONE Then Return
    m_nBlockFree += 1
    'Dim lm As New Threading.Mutex(m_muxBlockNotZero)
    'm_cvBlockNotZero.NotifyOne()
  End Sub

  ' Static wrapper for sound card handler
  Private Shared Sub WaveOutProcWrap(hWaveOut As IntPtr, uMsg As UInteger, dwInstance As UInteger, dwParam1 As UInteger, dwParam2 As UInteger)
    CType(dwInstance, ConsoleGameEngine).WaveOutProc(hWaveOut, uMsg, dwParam1, dwParam2)
  End Sub

  Public Shared Narrowing Operator CType(v As UInteger) As ConsoleGameEngine
    Throw New NotImplementedException()
  End Operator

  ' Audio thread. This loop responds to requests from the soundcard to fill 'blocks'
  ' with audio data. If no requests are available it goes dormant until the sound
  ' card is ready for more data. The block is fille by the "user" in some manner
  ' and then issued to the soundcard.
  Private Sub AudioThread()

    m_fGlobalTime = 0.0F
    Dim fTimeStep As Single = 1.0F / CSng(m_nSampleRate)

    ' Goofy hack to get maximum integer for a type at run-time
    Dim nMaxSample = CShort(Math.Pow(2, (2 * 8) - 1)) - 1S
    Dim fMaxSample = CSng(nMaxSample)
    Dim nPreviousSample As Short = 0

    While m_bAudioThreadActive
      ' Wait for block to become available
      If m_nBlockFree = 0 Then
        Dim lm As New Threading.Mutex()
        lm.WaitOne()

        While m_nBlockFree = 0 ' sometimes, Windows signals incorrectly
          m_cvBlockNotZero.WaitOne()
        End While
      End If

      ' Block is here, so use it
      m_nBlockFree -= 1

      ' Prepare block for processing
      If (m_pWaveHeaders(m_nBlockCurrent).Flags And WHDR_PREPARED) <> 0 Then
        waveOutUnprepareHeader(m_hwDevice, m_pWaveHeaders(m_nBlockCurrent), CInt(Marshal.SizeOf(m_pWaveHeaders(m_nBlockCurrent))))
      End If

      Dim nNewSample As Short = 0
      Dim nCurrentBlock As Integer = m_nBlockCurrent * m_nBlockSamples

      Dim clip = Function(fSample As Single, fMax As Single) As Single
                   If fSample >= 0.0F Then
                     Return Math.Min(fSample, fMax)
                   Else
                     Return Math.Max(fSample, -fMax)
                   End If
                 End Function

      For n = 0 To m_nBlockSamples - 1 Step m_nChannels
        ' User Process
        For c = 0 To m_nChannels - 1
          nNewSample = CShort(clip(GetMixerOutput(c, m_fGlobalTime, fTimeStep), 1.0F) * fMaxSample)
          m_pBlockMemory(nCurrentBlock + n + c) = nNewSample
          nPreviousSample = nNewSample
        Next

        m_fGlobalTime += fTimeStep
      Next

      ' Send block to sound device
      Dim junk = waveOutPrepareHeader(m_hwDevice, m_pWaveHeaders(m_nBlockCurrent), CInt(Marshal.SizeOf(m_pWaveHeaders(m_nBlockCurrent))))
      junk = waveOutWrite(m_hwDevice, m_pWaveHeaders(m_nBlockCurrent), CInt(Marshal.SizeOf(m_pWaveHeaders(m_nBlockCurrent))))
      m_nBlockCurrent += 1
      m_nBlockCurrent = m_nBlockCurrent Mod m_nBlockCount
    End While

  End Sub

  ' Overridden by user if they want to generate sound in real-time
  Public Overridable Function OnUserSoundSample(nChannel As Integer, fGlobalTime As Single, fTimeStep As Single) As Single
    Return 0.0F
  End Function

  ' Overriden by user if they want to manipulate the sound before it is played
  Public Overridable Function OnUserSoundFilter(nChannel As Integer, fGlobalTime As Single, fSample As Single) As Single
    Return fSample
  End Function

  ' The Sound Mixer - If the user wants to play many sounds simultaneously, and
  ' perhaps the same sound overlapping itself, then you need a mixer, which
  ' takes input from all sound sources for that audio frame. This mixer maintains
  ' a list of sound locations for all concurrently playing audio samples. Instead
  ' of duplicating audio data, we simply store the fact that a sound sample is in
  ' use and an offset into its sample data. As time progresses we update this offset
  ' until it is beyond the length of the sound sample it is attached to. At this
  ' point we remove the playing sound from the list.
  '
  ' Additionally, the users application may want to generate sound instead of just
  ' playing audio clips (think a synthesizer for example) in which case we also
  ' provide an "onUser..." event to allow the user to return a sound for that point
  ' in time.
  '
  ' Finally, before the sound is issued to the operating system for performing, the
  ' user gets one final chance to "filter" the sound, perhaps changing the volume
  ' or adding funky effects.
  Function GetMixerOutput(nChannel As Integer, fGlobalTime As Single, fTimeStep As Single) As Single
    ' Accumulate sample for this channel
    Dim fMixerSample As Single = 0.0F

    For Each s As sCurrentlyPlayingSample In listActiveSamples
      ' Calculate sample position
      s.nSamplePosition += CLng(vecAudioSamples(s.nAudioSampleID - 1).wavHeader.SamplesPerSec * fTimeStep)

      ' If sample position is valid add to the mix
      If s.nSamplePosition < vecAudioSamples(s.nAudioSampleID - 1).nSamples Then
        fMixerSample += vecAudioSamples(s.nAudioSampleID - 1).fSample(CInt(Fix((s.nSamplePosition * vecAudioSamples(s.nAudioSampleID - 1S).nChannels) + nChannel)))
      Else
        s.bFinished = True ' Else sound has completed
      End If
    Next

    ' If sounds have completed then remove them
    listActiveSamples.RemoveAll(Function(s As sCurrentlyPlayingSample) s.bFinished)

    ' The users application might be generating sound, so grab that if it exists
    fMixerSample += OnUserSoundSample(nChannel, fGlobalTime, fTimeStep)

    ' Return the sample via an optional user override to filter the sound
    Return OnUserSoundFilter(nChannel, fGlobalTime, fMixerSample)
  End Function

  Dim m_nSampleRate As Integer
  Dim m_nChannels As Integer
  Dim m_nBlockCount As Integer
  Dim m_nBlockSamples As Integer
  Dim m_nBlockCurrent As Integer

  Dim m_pBlockMemory As Short() = Nothing
  Dim m_pWaveHeaders As WaveHdr() = Nothing
  ReadOnly m_hwDevice As IntPtr = IntPtr.Zero

  Dim m_AudioThread As Thread
  Dim m_bAudioThreadActive As Boolean = False
  Dim m_nBlockFree As Integer = 0
  ReadOnly m_cvBlockNotZero As New Threading.ManualResetEvent(False)
  ReadOnly m_muxBlockNotZero As New Threading.Mutex()
  Dim m_fGlobalTime As Single

End Class