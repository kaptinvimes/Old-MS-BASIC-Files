'********** DATABASE.BAS

'Copyright (c) 1993 Ethan Winer
'Modifications (c) 1994 David. C. Ward

'$INCLUDE: 'dglib.bi'
COMMON SHARED row1%, col1%, row2%, col2%
COMMON SHARED m0%, m1%, m2%, m3%
COMMON SHARED Row%, Col%, lButton%, rButton%



' Declares
'DECLARE SUB MouseBorder (row1%, col1%, row2%, col2%)
'DECLARE SUB MouseDriver (m0%, m1%, m2%, m3%)
'DECLARE SUB MouseHide ()
'DECLARE SUB mouseinit ()
'DECLARE SUB MousePoll (Row%, Col%, lButton%, rButton%)
'DECLARE SUB mouseshow ()



DEFINT A-Z

DECLARE SUB ClearEOS (FG, BG)
DECLARE SUB Editor (text$, LeftCol, RightCol, KeyCode)
DECLARE SUB GetRecord (FileNum, RecNum, FileData$(), ScrnData$(), FldType())
DECLARE SUB OpenFile (FileName$, FileNum, FldType(), FldLength!(), FileData$())
DECLARE SUB PutRecord (FileNum, RecNum, FileData$(), ScrnData$(), FldType())

DECLARE FUNCTION FormatField$ (Value$, Places)
'mouseinit


'---- Set the screen colors based on the type of display installed, then read
'     the field definition and position information.
'
GOSUB SetColors

RESTORE ScreenData
READ NumFields

REDIM ScrnData$(1 TO NumFields) 'holds the field contents as they appear
                                'this is what is actually edited on screen

REDIM FileData$(1 TO NumFields) 'holds the field contents in "native" form
                                'ALWAYS use LSET to assign to this array
                                'normally you won't need to access this array

REDIM FldTitle$(1 TO NumFields) 'these are read from the DATA statements
REDIM Row(1 TO NumFields)       'the row this field goes on
REDIM Column(1 TO NumFields)    'the left-most column of this field
REDIM FldType(1 TO NumFields)   'the field type (see code in DATA items)

REDIM FldLength!(1 TO NumFields)'the on-screen length of the field -- the
                                '  whole portion holds the on-screen length,
                                '  the decimal portion holds the number of
                                '  decimal places (numeric fields only)

REDIM Protected(1 TO NumFields) 'true/false if field is protected from entry

FOR X = 1 TO NumFields          'read the actual field definitions
  READ FldTitle$(X), Row(X), Column(X), FldType(X), FldLength!(X), Protected(X)
NEXT

























'---- Display the data entry screen and allow entering/editing data.  If you
'     define field number 1 as protected, set CurField to 2 below so editing
'     instead will start at that field.
CLS

GOSUB DisplayScreen             'display the blank data entry screen
GOSUB DisplayPrompt
FileNum = FREEFILE
FileName$ = "TankData.dat"
RecNumber = 1

CALL OpenFile(FileName$, FileNum, FldType(), FldLength!(), FileData$())
CALL GetRecord(FileNum, RecNumber, FileData$(), ScrnData$(), FldType())

start:
GOSUB DisplayScreen
GOSUB DisplayPrompt
LOCATE 1, 1, 0: PRINT "Tank Number: "; RecNumber

CurField = 1                    'start editing at field number 1
GOSUB EditScreen                'enter/edit the data until Escape is pressed






CALL PutRecord(FileNum, RecNumber, FileData$(), ScrnData$(), FldType())

RecNumber = RecNumber + 1
CALL GetRecord(FileNum, RecNumber, FileData$(), ScrnData$(), FldType())

GOTO start




'<<<<<<<<<<<<<<<<<<<<< START OF SUBROUTINES SECTION <<<<<<<<<<<<<<<<<<<<<<

'---- See what kind of display is installed and set appropriate colors.
'     You can change these colors to whatever you want.
'
SetColors:
FG = 0                        'the foreground and background colors used
BG = 7                          '  for the field titles and prompt statements
EditFG = 10                     'the colors used for the data fields
EditBG = 0
LOCATE , , , 4, 7               'set the cursor size for a color display

DEF SEG = 0
IF PEEK(&H463) = &HB4 THEN      'it's a mono screen, use these colors instead
  FG = 7
  BG = 0
  EditFG = 0
  EditBG = 7
  LOCATE , , , 12, 13           'set the cursor size for a mono display
END IF
RETURN



'---- This routine displays the field names and contents.  The "- 2" below
'     adds space for the colon and an extra blank after the field name.
'
DisplayScreen:


'cLS
COLOR 0, 7
LOCATE 1, 1, 0
PRINT STRING$(2000, 32);
LOCATE 1, 1, 0

COLOR FG, BG


FOR X = 1 TO NumFields
  COLOR FG, BG
  LOCATE Row(X), Column(X) - LEN(FldTitle$(X)) - 2
  PRINT FldTitle$(X); ": ";
 
  'Format the field if decimal places were specified.
  Temp! = FldLength!(X)                 'see if there are any decimal places
  Places = (Temp! - INT(Temp!)) * 10
  IF Places THEN ScrnData$(X) = FormatField$(ScrnData$(X), Places)
 
  'Buffer$ pads the contents to the full on-screen field length if needed.
  Buffer$ = SPACE$(INT(FldLength!(X)))
  LSET Buffer$ = ScrnData$(X)
  COLOR EditFG, EditBG
  PRINT Buffer$;
NEXT
RETURN



'---- This routine handles full-screen editing of the data.
'
'
EditScreen:
COLOR EditFG, EditBG            'use the editing colors (inverse)
 
DO
  LeftCol = Column(CurField)    'calculate LeftCol/RightCol as Editor expects
  RightCol = LeftCol + INT(FldLength!(CurField)) - 1
  LOCATE Row(CurField), LeftCol 'locate for editing this field, then edit it
  CALL Editor(ScrnData$(CurField), LeftCol, RightCol, KeyCode)

  'Reformat the field automatically if decimal places were specified.

  Temp! = FldLength!(CurField)          'see if there are any decimal places
  Places = (Temp! - INT(Temp!)) * 10
  IF Places THEN                        'it is a formatted field
    Buffer$ = SPACE$(INT(FldLength!(CurField))) 'create a full-length buffer

    'Use RSET instead of LSET below to right-justify the number in the field.

    LSET Buffer$ = FormatField$(ScrnData$(CurField), Places)
    ScrnData$(CurField) = Buffer$       'copy the buffer back to the field
    LOCATE Row(CurField), LeftCol       'and then reprint the field
    PRINT ScrnData$(CurField)
  END IF





  

  IF CurField = 2 THEN
    Temp$ = UCASE$(ScrnData$(2))
    ScrnData$(2) = Temp$
    LOCATE Row(2), Column(2)
    PRINT Temp$;
    IF Temp$ <> "H" AND Temp$ <> "D" THEN
      GOTO Continue
    END IF
  END IF






  'Handle the key pressed to exit Editor, to allow tabbing between fields.
  SELECT CASE KeyCode           'handle based on what key they pressed

    CASE 9, -80, 32 TO 126  'Tab, Enter, Cursor Down, any character

      DO
        CurField = CurField + 1                     'go on to the next field
        IF CurField > NumFields THEN CurField = 1   'wrap around if necessary
        IF NOT Protected(CurField) THEN EXIT DO     'skip protected fields
      LOOP

    CASE -15, -72               'Shift-Tab, Up Arrow
      DO
        CurField = CurField - 1                     'go to the previous field
        IF CurField < 1 THEN CurField = NumFields   'wrap around if necessary
        IF NOT Protected(CurField) THEN EXIT DO     'skip protected fields
      LOOP

    CASE 13                     'Escape
      EXIT DO

    CASE 27          'add any other special key handling here

    CALL PutRecord(FileNum, RecNumber, FileData$(), ScrnData$(), FldType())
    CLS
    CLOSE #FileNum
    END


  END SELECT

Continue:
LOOP

COLOR FG, BG                    'back to the normal color scheme
RETURN



'---- This routine displays a prompt at the bottom of the screen explaining
'     what keys can be used.  Modify the PRINT statements to suit your own
'     program's requirements.  Note the semicolon at the end of the last
'     line.  This is needed whenever you print on lines 24 or 25 to prevent
'     the screen from scrolling up a line.
'
DisplayPrompt:
COLOR FG, BG
LOCATE 22, 1
CALL ClearEOS(FG, BG)
PRINT "  Use <Tab> or the <Down> Arrow key arrow key to advance to "
PRINT "  the next field,or use <Shift>+<Tab> or the <Up> Arrow key to go back "
PRINT "  one field. Press <Enter> to continue on to next tank or <Esc> to Exit.";
RETURN



'---- These DATA items define all of the fields on the screen.
'
ScreenData:

DATA 9                              : 'total number of fields on the screen
DATA "Product Name    ",5,22,1,20,0      : 'title, row, column, type, length, protect
DATA "Height/Dia (H/D)",7,22,1,1,0       : 'the field types are 1 = text, 2 = integer,
DATA "Contents        ",9,22,3,7,0      : '3 = long integer, 4 = single precision,
DATA "Scale           ",11,22,1,10,0     : '5 = double prec., 6 = Currency (PDS only)
DATA "Density",11,56,4,5,0
DATA "Range mm        ",13,22,3,5,0
DATA "Hi-Alarm mm     ",15,22,2,5,0     : 'the length is the field's size on screen
DATA "Lo-Alarm mm     ",17,22,2,5,0
DATA "Ullage   mm     ",19,22,2,5,0

STATIC SUB ClearEOS (FG, BG)    'clears from the cursor to bottom of screen

  Row = CSRLIN                  'see where the cursor is now
  Column = POS(0)

  ColorByte = FG + 16 * BG      'calculate the single-byte color value

  VideoSeg = &HB800             'assume a color monitor
  DEF SEG = 0
  IF PEEK(&H463) = &HB4 THEN    'it's a mono screen, use this segment
    VideoSeg = &HB000
  END IF
  DEF SEG = VideoSeg

  start = (Row - 1) * 160 + ((Column - 1) * 2)
  FOR X = start TO 3999 STEP 2  'poke video RAM from the cursor to the bottom
    POKE X, 32                  'use a CHR$(32) space for the character
    POKE X + 1, ColorByte       'and a color of white on black
  NEXT

END SUB

SUB Editor (text$, LeftCol, RightCol, KeyCode)

  'This is a general purpose text editor routine from "BASIC Techniques and
  'Utilities".  KeyCode is returned holding the last key pressed as a number.
  'Extended keys are returned as negative values (for example, F1 = -59).

  '----- Find the cursor's size.
  DEF SEG = 0
  IF PEEK(&H463) = &HB4 THEN
     CsrSize = 12               'mono uses 13 scan lines
  ELSE
     CsrSize = 7                'color uses 8
  END IF

  '----- Work with a temporary copy.
  Edit$ = SPACE$(RightCol - LeftCol + 1)
  LSET Edit$ = text$

  '----- See where to begin editing and print the string.
  TxtPos = POS(0) - LeftCol + 1
  IF TxtPos < 1 THEN TxtPos = 1
  IF TxtPos > LEN(Edit$) THEN TxtPos = LEN(Edit$)

  LOCATE , LeftCol
  PRINT Edit$;

  '----- This is the main loop for handling key presses.
  DO
     LOCATE , LeftCol + TxtPos - 1, 1

     DO
       Ky$ = INKEY$
     LOOP UNTIL LEN(Ky$)        'wait for a keypress

     IF LEN(Ky$) = 1 THEN       'create a key code
       KeyCode = ASC(Ky$)       'regular character key
     ELSE                       'extended key
       KeyCode = -ASC(RIGHT$(Ky$, 1))
     END IF

     '----- Branch according to the key pressed.
     SELECT CASE KeyCode

       '----- Backspace: decrement the pointer and the
       '      cursor, and ignore if in the first column.
       CASE 8
         TxtPos = TxtPos - 1
         LOCATE , LeftCol + TxtPos - 1, 0
         IF TxtPos > 0 THEN
           IF InsStatus THEN
             MID$(Edit$, TxtPos) = MID$(Edit$, TxtPos + 1) + " "
           ELSE
             MID$(Edit$, TxtPos) = " "
           END IF
           PRINT MID$(Edit$, TxtPos);
         END IF

       '----- Enter or Escape: this block is optional in
       '      case you want to handle these separately.
       CASE 13, 27
         EXIT DO                'exit the subprogram

       '----- Letter keys: turn off the cursor to hide
       '      the printing, handle Insert mode as needed.
       CASE 32 TO 254
         LOCATE , , 0
         IF InsStatus THEN      'expand the string
           MID$(Edit$, TxtPos) = Ky$ + MID$(Edit$, TxtPos)
           PRINT MID$(Edit$, TxtPos);
         ELSE                   'else insert character
           MID$(Edit$, TxtPos) = Ky$
           PRINT Ky$;
         END IF
         TxtPos = TxtPos + 1    'update position counter
         IF TxtPos > LEN(Edit$) THEN TxtPos = LEN(Edit$)

       '----- Left arrow: decrement the position counter.
       CASE -75
         TxtPos = TxtPos - 1

       '----- Right arrow: increment position counter.
       CASE -77
         TxtPos = TxtPos + 1
         IF TxtPos > LEN(Edit$) THEN TxtPos = LEN(Edit$)

       '----- Home: jump to the first character position.
       CASE -71
         TxtPos = 1

       '----- End: search for the last non-blank, and
       '      make that the current editing position.
       CASE -79
         FOR N = LEN(Edit$) TO 1 STEP -1
           IF MID$(Edit$, N, 1) <> " " THEN EXIT FOR
         NEXT
         TxtPos = N + 1
         IF TxtPos > LEN(Edit$) THEN TxtPos = LEN(Edit$)

       '----- Insert key: toggle the Insert state and
       '      adjust the cursor size.
       CASE -82
         InsStatus = NOT InsStatus
         IF InsStatus THEN
           LOCATE , , , CsrSize \ 2, CsrSize
         ELSE
           LOCATE , , , CsrSize - 1, CsrSize
         END IF

       '----- Delete: delete the current character and
       '      reprint what remains in the string.
       CASE -83
         MID$(Edit$, TxtPos) = MID$(Edit$, TxtPos + 1) + " "
         LOCATE , , 0
         PRINT MID$(Edit$, TxtPos);

       '---- All other keys: exit the subprogram
       CASE ELSE
         EXIT DO
     END SELECT

  '----- Loop until the cursor moves out of the field.
  LOOP UNTIL TxtPos < 1 OR TxtPos > LEN(Edit$)

  text$ = RTRIM$(Edit$)         'trim the text

END SUB

STATIC FUNCTION FormatField$ (Value$, Places)

  Mask$ = RIGHT$("###################." + STRING$(Places, "#"), 20)
  Value# = VAL(Value$)
 
  'UnREM and use the line below if you have VB/DOS.
  'FormatField$ = FORMAT$(Value#, Mask$)

  'UnREM and use the line below if you have QuickPak Professional.
  'FormatField$ = FUsing$(Value$, Mask$)

  'REM out the lines below if you are using either of the code lines above.
  FileNum = FREEFILE
  OPEN "\DEV\NUL" FOR RANDOM AS #FileNum LEN = 20
  FIELD #FileNum, 20 AS Fmt$
  LSET Fmt$ = ""
  PRINT #FileNum, USING Mask$; Value#;
  FormatField$ = LTRIM$(Fmt$)
  CLOSE #FileNum
 
END FUNCTION

STATIC SUB GetRecord (FileNum, RecNumber, FileData$(), ScrnData$(), FldType())

  GET #FileNum, RecNumber

  FOR X = 1 TO UBOUND(FldType)  'assign the file fields
    SELECT CASE FldType(X)      'again, based on the field types
      CASE 1                    'a string field, trim any trailing blanks
        ScrnData$(X) = RTRIM$(FileData$(X))
      CASE 2                    'integer field
        ScrnData$(X) = LTRIM$(STR$(CVI(FileData$(X))))
      CASE 3                    'long integer
        ScrnData$(X) = LTRIM$(STR$(CVL(FileData$(X))))
      CASE 4                    'single precision
        ScrnData$(X) = LTRIM$(STR$(CVS(FileData$(X))))
      CASE 5                    'double precision
        ScrnData$(X) = LTRIM$(STR$(CVD(FileData$(X))))
     'CASE 6                    'Currency (unREM if using PDS or VB/DOS)
     '  ScrnData$(X) = ltrim$(STR$(CVC(FileData$(X))))
    END SELECT
  NEXT
 
END SUB

STATIC SUB OpenFile (FileName$, FileNum, FldType(), FldLength!(), FileData$())

  RecLength = 0                 'clear the record length accumulator
  NumFields = UBOUND(FldType)   'see how many fields there are

  FOR X = 1 TO NumFields        'calculate the length of each record
    SELECT CASE FldType(X)      'based on the field types
      CASE 1                    'a string field, use its screen length
        RecLength = RecLength + FldLength!(X)
      CASE 2                    'integer fields use 2 bytes
        RecLength = RecLength + 2
      CASE 3, 4                 'long integers and single prec. need 4 bytes
        RecLength = RecLength + 4
      CASE 5, 6                 'double prec. and Currency take 8 bytes
        RecLength = RecLength + 8
    END SELECT
  NEXT

  OPEN FileName$ FOR RANDOM AS #FileNum LEN = RecLength
  LSET Pad$ = ""                'this is the trick to a generic FIELD routine
 
  FOR X = 1 TO NumFields        'field the file
    SELECT CASE FldType(X)      'again, based on the field types
      CASE 1                    'a string field, use its screen length
        FIELD #FileNum, Pad AS Dummy$, FldLength!(X) AS FileData$(X)
        Pad = Pad + FldLength!(X)
      CASE 2                    'integer fields use 2 bytes
        FIELD #FileNum, Pad AS Dummy$, 2 AS FileData$(X)
        Pad = Pad + 2
      CASE 3, 4                 'long integers and single prec. need 4 bytes
        FIELD #FileNum, Pad AS Dummy$, 4 AS FileData$(X)
        Pad = Pad + 4
      CASE 5, 6                 'double precision and Currency take 8 bytes
        FIELD #FileNum, Pad AS Dummy$, 8 AS FileData$(X)
        Pad = Pad + 8
    END SELECT
  NEXT

END SUB

STATIC SUB PutRecord (FileNum, RecNumber, FileData$(), ScrnData$(), FldType())

  FOR X = 1 TO UBOUND(FldType)  'assign the file fields
    SELECT CASE FldType(X)      'again, based on the field types
      CASE 1                    'a string field, assign directly (using LSET)
        LSET FileData$(X) = ScrnData$(X)
      CASE 2                    'integer field
        LSET FileData$(X) = MKI$(VAL(ScrnData$(X)))
      CASE 3                    'long integer
        LSET FileData$(X) = MKL$(VAL(ScrnData$(X)))
      CASE 4                    'single precision
        LSET FileData$(X) = MKS$(VAL(ScrnData$(X)))
      CASE 5                    'double precision
        LSET FileData$(X) = MKD$(VAL(ScrnData$(X)))
     'CASE 6                    'Currency (unREM if using PDS or VB/DOS)
     '  LSET FileData$(X) = MKC$(VAL(ScrnData$(X)))
    END SELECT
  NEXT

  PUT #FileNum, RecNumber

END SUB

