dim shared as string g_sDrive(3)
dim shared as long g_iDriveNum

g_sDrive(0)=exepath+"/DISK0/"
g_sDrive(1)=exepath+"/DISK1/"
g_sDrive(2)=exepath+"/DISK2/"
g_sDrive(3)=exepath+"/DISK3/"

#define LineInput ConLineInput
#define AdvanceLine ConAdvanceLine
#define AdvanceTab ConAdvanceTab
#define WriteString ConWriteString

function __pos() as long
  print !" \b";
  return pos()  
end function  

sub WriteInput(sLine as string)
  #ifdef __FB_WIN32__
    var iCount = len(sLine)
    #ifdef GfxMode
      'while len(inkey): wend
      for N as integer =0 to iCount-1
        SendMessage(g_appWnd,WM_CHAR,sLine[N],1)
      next N          
    #else
      var hInput = GetStdHandle(STD_INPUT_HANDLE)  
      dim as INPUT_RECORD arrInput(iCount-1)
      for N as integer =0 to iCount-1
        arrInput(N).EventType=KEY_EVENT
        with arrInput(N).Event.KeyEvent
          .bKeyDown = 1
          .wRepeatCount = 1
          .uChar.AsciiChar = sLine[N]
        end with
      next N
      'FlushConsoleInputBuffer(hInput)
      WriteConsoleInput(hInput,@arrInput(0),iCount,@iCount)
    #endif
  #else
    #error " implement me"
  #endif
end sub

function ConLineInput(sPreFill as string="",byref iBreak as long=0) as string
  if len(sPreFill) then WriteInput(sPreFill)
  dim as zstring*512 zTemp = any
  dim as string sLines = ""
  var iCol = __pos-1
  g_bInput = 1
  #ifdef GfxMode
    line input "",sLines
    iBreak=0 : g_bInput=0
  #else  
    if fgets ( zTemp , 511 , stdin ) then 
      var iLen = strlen(zTemp)
      if iLen>0 andalso zTemp[iLen-1]=10 then iLen -= 1
      if iLen>0 andalso zTemp[iLen-1]=13 then iLen -= 1
      zTemp[iLen]=0 : sLines = zTemp
      iBreak = 0 : g_bInput=0
    else
      iBreak = 1 : g_bInput=0
      return sLines
    end if  
  #endif
  'dim as string sLines
  'line input "",sLines
  var iWidth = LOWORD(width)
  var iNeed = iWidth-((len(sLines)+iCol) mod iWidth)
  locate csrlin()-1,(iWidth-iNeed)+1 : print space(iNeed);  
  return sLines
end function
#define ConAdvanceLine() ConAdvanceTab()
#if 0
sub ConAdvanceTab(iToTab as long=-1)
  var iPos = __pos  
  if iToTab<0 then
    if iPos=1 then exit sub
    iToTab=LOWORD(width)+1
  end if
  print space(iToTab-iPos);  
  var iN = __pos
  if csrlin()>=HIWORD(width) then
    print space(1+LOWORD(width)-iN);
    locate ,iN
  end if
end sub
#endif

sub ConAdvanceTab(iToTab as long=-1)
  var iPos = __pos()  
  if iToTab<0 then
    if iPos=1 then exit sub
    iToTab=LOWORD(width)+1
  end if
  print space(iToTab-iPos);
  var iN = __pos()
  if csrlin()>=HIWORD(width) then
    print space(1+LOWORD(width)-iN);
    'print HIWORD(width);iN
    locate HIWORD(width),iN: print !" \b";
  end if
end sub

sub ConWriteString( sString as string  )
  print sString;
end sub