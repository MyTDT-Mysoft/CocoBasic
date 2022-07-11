#include "crt.bi"
'#cmdline "-g -exx"
'#cmdline "-gen gcc -Wc -Ofast,-march=native,-mtune=native"

#include "fbgfx.bi"

#ifdef __FB_WIN32__
  #include "windows.bi"
  #if __FB_DEBUG__
    #include "MyTDT\Exceptions.bas"
    StartExceptions()
  #endif
#endif

'#define DbgParser
#define DbgErrTrigger
'#define UseMagic
'#define GfxMode

dim shared as any ptr g_AppWnd
dim shared as double g_StartTimer
dim shared as ulong g_MemoryUsage,g_DoBreak=0
dim shared as long g_MemLeft = 32767
dim shared as ubyte g_VirtMem(65535)
dim shared as byte g_bImmediateMode=1,g_bInput=0,g_bCleanUp
'#define Mem_fTimer g_timer

#include "Modules\Tables.bas"
#include "Modules\Strings.bas"
#include "Modules\Devices.bas"
#include "Modules\Edit.bas"
#include "Modules\Parser.bas"
#include "Modules\Vars.bas"
#include "Modules\Subs.bas"
#include "Modules\Funcs.bas"
#include "Modules\Init.bas"

randomize()
g_StartTimer = timer

dim as string sExpression

'width 40,30
#ifdef GfxMode
  #include "MyTDT\GfxResize.bas"
  Gfx.PreResize()
  screenres 80*8,25*14
  Gfx.Resize(80*8*1,25*14*1)
  if screenptr then
    width 80,25 
    palette 10,0,255,0     : palette 14,255,255,0
    palette  9,0,0,255     : palette 12,255,0,0
    palette 15,255,255,255 : palette 11,0,255,255
    palette 13,132,0,132   : palette 6,255,140,0
  end if
  screencontrol( fb.Get_Window_Handle , *cast(uinteger ptr,@g_AppWnd) )
#else
  #ifdef __FB_WIN32__
    g_AppWnd = GetConsoleWindow()
  #else
    #error " implement me!"
  #endif
#endif

color 0,10 : cls
WriteString( "COCO-2 BASIC v0.3 (" __DATE__ !")\n" )
WriteString( "Ready." !"\n" )

#ifdef __FB_WIN32__
  #ifndef GfxMode
    function MyHandlerRoutine( dwCtrlType as dword ) as WINBOOL
      if dwCtrlType = CTRL_BREAK_EVENT then
        g_DoBreak = 1 : return TRUE
      end if
      return FALSE
    end function
    SetConsoleCtrlHandler( @MyHandlerRoutine , TRUE )  
  #endif
#endif
'#else
  sub BreakThread( Dummy as any ptr )
    SetThreadPriority(GetCurrentThread,THREAD_PRIORITY_TIME_CRITICAL)
    do      
      'if multikey(fb.SC_ESCAPE) then         
      if GetForegroundWindow()=g_appWnd andalso (GetKeyState(VK_ESCAPE) shr 7) then
        g_DoBreak = 1
        #ifdef __FB_WIN32__
          'generateConsoleCtrlEvent( CTRL_BREAK_EVENT , 0 )
          if g_bInput then
            SendMessage(g_appWnd,WM_CHAR,13,1)
          end if
        #endif        
        'while multikey(fb.SC_ESCAPE)
        while (GetKeyState(VK_ESCAPE) shr 7)
          sleep 100,1
        wend
      end if
      sleep 100,1
      'printf(!"%1.3fkb   \r",g_MemoryUsage/1024)
    loop
  end sub
  ThreadCreate(@BreakThread,0)
'#endif

dim as ParserStruct tParser
do
  
  dim as string sExpression
  if g_pEditLine then
    dim as long iCancelled
    sExpression=EditLine(g_pEditLine,iCancelled)    
    g_pEditLine = 0
    if iCancelled then g_DoBreak = 0 : continue do
  else
    sExpression=LineInput()    
  end if
  g_pCurrentLine = NULL
  ConAdvanceTab(0)
  
  if sExpression=";" then sExpression="A=len(left$(string$(200,65),0))"
  
  dim as long uImmSize
  dim as zstring ptr pzStart = ToTokens(sExpression,uImmSize), pLast=pzStart-1  
  dim as ValStruct TgtValue , SrcValue , ShowValue
  dim as byte bFirstToken=1,bShowVar,bSilent=0

  with tParser
    .pzBytecode = pzStart-1 : g_OutputCount = 0
    .iError = 0 : g_DoBreak = 0
    .iValOff=0    : .iOpOff=0 : .iLastToken=0          
    .iOpenCount=0 : .iError=0 : bFirstToken = 1
    if .tCont.pRetLine=0 then .iScopeOff=0 : .iIfCount=0
  end with
  
  do
    CleanStrings(tParser)
    
    with tParser
      
      if g_DoBreak andalso pLast=0 then
        if __pos()>1 then AdvanceLine()
        if g_bImmediateMode then
          WriteString("BREAK")
        else
          WriteString(!"BREAK AT " & g_pCurrentLine->wNumber)
          .tCont.pRetLine = g_pCurrentLine
          .tCont.pRetCode = .pzBytecode
        end if
        AdvanceLine()          
        exit do
      end if      
      
      var iChar = iif(pLast=.pzBytecode,NextToken(),GetToken())      
      pLast=.pzByteCode
      #ifdef DbgParser
      ShowParser(tParser)
      #endif
      'print iChar , tParser.pzBytecode
      
      Select case as const iChar      
      case asc("0") to asc("9")               'line number
        if g_bImmediateMode andalso bFirstToken then        
          var iNum = ReadInt(tParser)
          if iNum > cMaxLineNumber then
            ShowErr(_SN_ERROR_)
            exit do
          end if
          'print "Line: ";iNum
          var uSz = uImmSize-(cuint(tParser.pzBytecode)-cuint(pzStart))          
          SetLine( iNum , uSz , tParser.pzBytecode )
          g_OutputCount += 1 : bSilent = 1
          .tCont.pRetLine = NULL
        else
          ShowErr(_SN_ERROR_)
        end if
        exit do
      case asc("A") to asc("Z"),ti_Let,ti_For 'Variable Set
        if (iChar = ti_Let orelse iChar=ti_For) andalso cuint(NextToken()-asc("A")) > (asc("Z")-asc("A")) then                  
          ShowErr(_SN_ERROR_)          
          exit do
        end if          
        if GetVariable( tParser , TgtValue , gvWrite ) <> otEqu then
          if g_OutputCount = 0 then
            ShowErr(_SN_ERROR_)
          end if
          exit do
        else          
          #ifdef DbgParser
          ShowParser(tParser)
          #endif
          NextToken() : tParser.iLastToken = 0 'asc("0")
          if ParseParameter( tParser , SrcValue , TgtValue.iType , pfIsSub, @tParser )=0 then exit do
          .pzBytecode -= 1
          SetVariable( TgtValue , SrcValue )          
          if iChar=ti_For then            
            if TgtValue.iType <> vtNumber then
              ShowErr(_TM_ERROR_)
              exit do
            else
              if __ForInit( tParser , TgtValue.pfNum , TgtValue.wName )=0 then exit do
            end if
          else            
            if bShowVar=0 then ShowValue=TgtValue:bShowVar=1
          end if
        end if
      case 255                                'setable functions
        select case NextToken()
        case tif_Timer 'timer=N
          NextToken() : tParser.iLastToken = 0
          if ParseParameter( tParser , SrcValue , vtNumber , pfIsSub, @tParser )=0 then exit do
          var iNum = int(SrcValue.fNumber)
          if cuint(iNum)>cMaxUnsigned then
            ShowErr(_FC_ERROR_)
            exit do
          else            
            g_StartTimer = timer-(iNum/60)
          end if                    
          if bShowVar=0 then ShowValue=SrcValue:bShowVar=1          
          .pzBytecode -= 1
        case tif_Mid   'mid$(...)="S"
        case else
          ShowErr(_SN_ERROR_)
          exit do
        end select
      case asc(":")                           'next instruction
        CleanStrings(tParser)
        'CleanStacks()
        .iValOff=0    : .iOpOff=0   : .iLastToken=0 : pLast=0        
        .iOpenCount=0 : .iError = 0 : bFirstToken = 1
        NextToken()
      case 0 , ti_Else                        'End Of Line
        CleanStrings(tParser)
        if g_bImmediateMode then
          if tParser.pzBytecode = pzStart then bSilent=1 : g_OutputCount += 1
          exit do          
        else
          g_pCurrentLine = g_pCurrentLine->pNext
          if g_pCurrentLine = NULL then exit do
          pzStart = cast(any ptr,g_pCurrentLine+1) : pLast=0
          .pzBytecode = pzStart          
          'same as ':'
          .iValOff=0    : .iOpOff=0 : .iLastToken=0 : .iIfCount=0
          .iOpenCount=0 : .iError=0 : bFirstToken = 1
        end if
      case else                               'instructions
        if iChar >= 128 then
          var pFunc = pSubProc(iChar)
          if pFunc = 0 then
            if g_uPri(iChar) orelse pzToken(iChar)=0 then          
              ShowErr(_SN_ERROR_)
            else              
              if iChar = ti_Rem then 
                CleanStrings(tParser)
                'if bFirstToken=0 then
                '  ShowErr( _SN_ERROR_ )
                '  CleanStacks() : exit do  
                'end if
                exit do
              end if
              ShowErrS("Not Implemented: '"+*pzToken(iChar)+"'")
            end if
            CleanStacks() : exit do  
          end if        
          .pzByteCode += 1 : .iLastToken = 0
          #ifdef DbgParser
          ShowParser(tParser)
          #endif
          if pFunc(tParser)=0 then            
            CleanStacks() : exit do            
          end if
          pLast=0
          
        else      
          print 
          ShowErr2( _SN_ERROR_ , iChar )
          exit do
        end if        
      end select
    end with      
    bFirstToken = 0
    
  loop 
  
  #ifdef DbgParser
  ShowParser(tParser)
  #endif
  
  'with tParser
  '  CleanStacks()
  'end with
  if bShowVar andalso g_bImmediateMode andalso g_OutputCount=0 then
    bShowVar=0 : ShowVariable(ShowValue)
    AdvanceLine()
  else    
    AdvanceLine()
    if bSilent=0 then WriteString("OK"):AdvanceLine()
    bSilent=1
  end if
  
  g_bImmediateMode = 1
  
loop

'print !"\nDone!"