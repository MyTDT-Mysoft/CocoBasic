#define Virt(_Name) Sub##_Name ( byref tParser as ParserStruct ) as long  
#define GetParameter( _Var , _Type ) ParseParameter( tParser , _Var , _Type , pfIsSub , @tParser )
#define GetOptParameter( _Var , _Type ) ParseParameter( tParser , _Var , _Type , pfIsSub or pfOptional , @tParser )

declare function Virt(_Run)

function __ForInit( byref tParser as ParserStruct , pfNumVar as numberT ptr , wName as ushort ) as long
  dim as ValStruct NumTo   = any
  dim as ValStruct NumStep = any : NumStep.fNumber = 1  
  if GetToken()<>ti_To then
    ShowErr(_SN_ERROR_)
    return 0
  end if  
  NextToken()
  if GetParameter( NumTo , vtNumber )=0 then return 0  
  if GetToken(-1)=ti_Step then
    if GetParameter( NumStep , vtNumber )=0 then return 0      
  end if  
  tParser.pzBytecode -= 1
  with tParser.tScopeStack(tParser.iScopeOff)
    .iType = stFor
    with .tFor
      .wName = wName
      .pfVar = pfNumVar
      .fEnd  = NumTo.fNumber
      .fStep = NumStep.fNumber  
      .pStartLine = g_pCurrentLine
      .pStartCode = tParser.pzBytecode
    end with
  end with  
  tParser.iScopeOff += 1    
  return 1
end function
function Virt(_For)     '128 FOR [placeholder]
  ShowErr(_SN_ERROR_) : return 0
end function
function __Goto( byref tParser as ParserStruct ) as long
  var iLine = ReadInt(tParser)
  var pLine = FindLine(iLine)
  if pLine =  NULL then ShowErr(_UL_ERROR_) : return 0
  tParser.iLastToken = 0 : 
  tParser.iIfCount=0 : g_pCurrentLine = pLine
  tParser.pzBytecode = cast(any ptr,g_pCurrentLine+1)
  tParser.tCont.pRetLine=0 : g_bImmediateMode = 0
  return 1
end function
function Virt(_Go)      '129 GOTO/GOSUB [N]  
  var iAct = GetToken()
  if g_bImmediateMode then
    tParser.tCont.pRetLine=0 
    if iAct=ti_to then return sub_Run( tParser )
  end if
  var iParm = NextToken()
  select case iAct 
  case ti_To,ti_Sub
    if iParm<asc("0") orelse iParm>asc("9") then
      ShowErr(_UL_ERROR_) : return 0
    end if
    var iLine = ReadInt(tParser)
    var pLine = FindLine(iLine)
    if pLine =  NULL then
      ShowErr(_UL_ERROR_) : return 0
    end if
    if iAct=ti_Sub then 'is gosub
      if tParser.iScopeOff > cMaxStack then
        ShowErr(_OM_ERROR_) : return 0
      end if
      with tParser.tScopeStack(tParser.iScopeOff)
        .iType = stReturn
        with .tRet          
          .pRetLine = g_pCurrentLine
          .pRetCode = tParser.pzBytecode+1
        end with
      end with
      tParser.iScopeOff += 1
    else
      tParser.iIfCount=0
    end if
    g_pCurrentLine = pLine
    tParser.pzBytecode = cast(any ptr,g_pCurrentLine+1)
  case else
    ShowErr(_SN_ERROR_) : return 0
  end select
  return 1
end function
function Virt(_Rem)     '130 REM [placeholder]
  ShowErr(_SN_ERROR_) : return 0
end function
function Virt(_Else)    '132 ELSE [placeholder]
  ShowErr(_SN_ERROR_) : return 0
end function
function Virt(_If)      '133 IF N THEN ... ELSE ...
  dim as ValStruct NumExp = any  
  if GetParameter( NumExp , vtNumber )=0 then return 0
  tParser.pzByteCode -= 1 : tParser.iIfCount += 1
  if NumExp.fNumber then 'true
    select case GetToken()
    case ti_Go   : NextToken(): return sub_Go( tParser )
    case ti_Then : 
      select case NextToken()
      case asc("0") to asc("9")
        return __Goto( tParser )
      end select
      return 1
    case else   
      ShowErr(_SN_ERROR_) : return 0
    end select
  else
    tParser.pzByteCode -= 1
    var iIfNum = tParser.iIfCount
    do
      'locate else if any  
      select case NextToken()    
      case 0               'found end of line
        return 1
      case ti_Else         'found else... is this the one?
        if iIfNum = tParser.iIfCount then           
          select case NextToken()
          case asc("0") to asc("9")
            return __Goto( tParser )
          end select
          return 1
        end if
        tParser.iIfCount -= 1
      case ti_If           'found another if
        tParser.iIfCount += 1    
      case asc("""")       'found quotes... look for end of it
        do 
          select case NextToken()
          case asc("""") : exit do  'found end quotes 
          case 0         : return 0 'found end of line
          end select
        loop      
      case ti_Rem,ti_Rem+1 'found end of line
        return 1
      end select    
    loop
  end if
end function
function Virt(_Print)   '135 PRINT [@,/TAB(/]
  var iPrevChar = 0
  tParser.pzByteCode -= 1 : g_OutputCount += 1
  do    
    var iChar = NextToken()
    select case as const iChar
    case asc("@") 'Position
      NextToken()
      dim as ValStruct NumPos = any
      if GetParameter(NumPos,vtNumber)<>asc(",") then return false      
      var iPos = cint(NumPos.fNumber)
      var iWH = width(), iWid=LOWORD(iWH)
      if cuint(iPos)>(iWid*HIWORD(iWH)) then
        ShowErr(_FC_ERROR_)
        return false
      end if    
      locate (iPos\iWid)+1,(iPos mod iWid)+1
      tParser.pzByteCode -= 1      
    case asc(",") 'Next TAB
      var iWid = LOWORD(width()), iPos=__pos()
      var iNextCol = (iPos or 15)+1      
      if iNextCol >= iWid then iNextCol = iWid+1
      print space(iNextCol-iPos);
    case asc(";") 'Right Next to it (default :P)
    case ti_Tab   'Advance to certain column
      NextToken() : tParser.iOpenCount += 1
      'tParser.bOpStack(tParser.iOpOff) = asc("(") : tParser.iOpOff += 1
      dim as ValStruct NumTabs = any
      if ParseParameter(tParser,NumTabs,vtNumber,pfIsSub or pfParenthesis or pfSemiColon,@tParser)=0 then return false
      var iTabs = int(NumTabs.fNumber)
      if cuint(iTabs)>cStringLimit then ShowErr(_FC_ERROR_) : return 0
      AdvanceTab(iTabs)
      tParser.pzByteCode -= 2      
    case 0,ti_Rem,ti_Rem+1,asc(":")
      if iPrevChar<>asc(",") and iPrevChar<>asc(";") then
        AdvanceTab(LOWORD(width))
      end if
      exit do
    case else     'other char suppose to be expression
      dim as ValStruct VarAny = any
      if ParseParameter(tParser,VarAny,vtString or vtNumber,pfIsSub or pfSemiColon,@tParser)=0 then return false
      with VarAny
        select case .iType
        case vtNumber    
          print .fNumber;" ";
        case vtString          
          print _StrText(.ptStr);
        end select
      end with      
      tParser.pzByteCode -= 2
    end select
    iPrevChar = iChar    
  loop  
  return true
end function
function Virt(_End)     '138 END
  return 0
end function
function Virt(_Next)    '139 NEXT
  if tParser.iScopeOff=0 orelse tParser.tScopeStack(tParser.iScopeOff-1).iType <> stFor then
    ShowErr(_NF_ERROR_) : return 0
  end if
  with tParser.tScopeStack(tParser.iScopeOff-1).tFor
    select case as const GetToken()
    case asc("A") to asc("Z") 'varname
      dim as ValStruct NumLine = any     
      if GetVariable(tParser,NumLine,gvName)=0 then
        ShowErr(_SN_ERROR_) : return 0
      end if
      if NumLine.iType <> vtNumber orelse .wName <> NumLine.wName then
        ShowErr(_NF_ERROR_) : return 0
      end if
    case 0,asc(":"),ti_Rem+1
      rem puts("no var")  
    case else
      ShowErr(_SN_ERROR_) : return 0
    end select
    
    *.pfVar += .fStep
    if (.fStep>0 andalso *.pfVar<=.fEnd) orelse (.fStep<0 andalso *.pfVar>=.fEnd) orelse (.fStep=0 andalso *.pfVar<>.fEnd) then
      g_pCurrentLine = .pStartLine : tParser.pzByteCode = .pStartCode
      tParser.iLastToken=0
    else
      tParser.iScopeOff -= 1
    end if
    
  end with
  return 1
end function
function __Run( byref tParser as ParserStruct ) as long  
  with tParser    
    if g_pCurrentLine = NULL then return 1    
    ClearAllVariables()
    .pzBytecode = cast(any ptr,g_pCurrentLine+1)
    .tCont.pRetLine=0 : g_bImmediateMode = 0
    'same as ':'
    .iValOff=0    : .iOpOff=0   : .iLastToken=0
    .iOpenCount=0 : .iError = 0 : .iIfCount=0
    return 1
  end with
end function
function Virt(_Run)     '142 RUN [N]
  dim as ValStruct NumLine = any : NumLine.fNumber = -65533
  var iResu = GetOptParameter( NumLine , vtNumber )
  if iResu=0 then return 0
  var iLine = int(NumLine.fNumber)
  if iLine <> -65533 then
    var pLine = FindLine(iLine)
    if pLine =  NULL then
      ShowErr(_UL_ERROR_)
      return 0
    end if
    g_pCurrentLine = pLine
  else
    g_pCurrentLine = g_pProgBegin
  end if  
  
  tParser.pzByteCode -= 1
  return __Run( tParser )  
  
end function  
function Virt(_Return)  '144 RETURN
  while tParser.iScopeOff
    tParser.iScopeOff -= 1
    if tParser.tScopeStack(tParser.iScopeOff).iType = stReturn then
      with tParser.tScopeStack(tParser.iScopeOff).tRet
        g_pCurrentLine = .pRetLine 
        tParser.pzByteCode = .pRetCode
        tParser.iLastToken=0
      end with
      return 1
    end if    
  wend
  ShowErr(_RG_ERROR_)
  return 0
end function
function Virt(_Stop)    '145 STOP
  if g_bImmediateMode then
    ShowErr(_ID_ERROR_)
  else
    printf(!"BREAK AT %i",g_pCurrentLine->wNumber)    
    tParser.tCont.pRetLine = g_pCurrentLine
    tParser.tCont.pRetCode = tParser.pzBytecode+1
  end if  
  return 0
end function
function Virt(_Cont)    '147 CONT
  with tParser.tCont  
    if .pRetLine = NULL then
      ShowErr(_CN_ERROR_) : return 0
    end if
    g_pCurrentLine     = .pRetLine
    tParser.pzBytecode = .pRetCode
    .pRetLine = NULL : g_bImmediateMode = 0
  end with
  return 1
end function
function Virt(_List)    '148 LIST[N-[N]]
  dim as integer iStart=-1,iEnd=65535
  select case GetToken()
  case asc("0") to asc("9") 'number
    iStart = ReadInt(tParser)
  case otMinus
    iStart=0
  case 0,asc(":")           'eol
  case else
    ShowErr(_SN_ERROR_)
  end select
  if iStart<>-1 then
    if GetToken()=otMinus then
      select case NextToken()
      case asc("0") to asc("9") 'number
        iEnd = ReadInt(tParser)
      case 0,asc(":")           'eol
      case else
        ShowErr(_SN_ERROR_)
      end select
    else
      iEnd = iStart
    end if
    select case GetToken()
    case 0,asc(":")
    case else
      print ,GetToken()
      ShowErr(_SN_ERROR_)
    end select
  end if  
  'print iStart,iEnd
  var pLine = g_pProgBegin
  while pLine
    if g_DoBreak then exit while
    with *pLine
      if .wNumber > iEnd then exit while
      if .wNumber >= iStart then
        DisplayLine( .wNumber , cast(ubyte ptr,pLine+1) )
        'WriteString( .wNumber & " " & Detokenize(cast(ubyte ptr,pLine+1)) & !"\n" )
      end if
      pLine = .pNext
    end with
  wend
  return 1
end function
function Virt(_Clear)   '149 CLEAR[N,N]
  dim as ValStruct NumTemp = any
  var iResu = GetOptParameter( NumTemp , vtNumber )
  if iResu=0 then return 0
  if iResu=asc(",") then
    iResu = GetOptParameter(  NumTemp , vtNumber )
    if iResu=0 then return 0
  end if  
  ClearAllVariables()
  tParser.pzByteCode -= 1
  return iResu
end function
function Virt(_New)     '150 NEW
  dim as ValStruct NumDummy = any 
  if GetOptParameter( NumDummy , vtNone )<>-2 then return 0
  ClearAllVariables()
  ClearLines()
  tParser.pzByteCode -= 1
  tParser.tCont.pRetLine = NULL  
  return 0
end function
function Virt(_Cls)     '158 CLS[N]
  dim as ValStruct NumColor = any : NumColor.fNumber = 1 
  var iResu = GetOptParameter( NumColor , vtNumber )
  if iResu=0 then return 0
  var iColor = int(NumColor.fNumber)
  if iColor < 0 then
    ShowErr(_FC_ERROR_) : return 0
  end if
  if iColor>8 then
    color 0,10: cls
    print "MYSOFT COCO-2 BASIC"
  else
    color 0,g_ColorTable(iColor)
    cls: color 0,10
  end if
  g_OutputCount = 0 : tParser.pzBytecode -= 1
  return iResu
end function
function Virt(_Sound)   '160 SOUND N,N         ('TODO: implement with sound')
  dim as ValStruct NumTemp = any
  if GetParameter( NumTemp , vtNumber )=0 then return 0  
  var iFreq = cint(int(NumTemp.fNumber))
  if iFreq<0 or iFreq>255 then 
    ShowErr(_FC_ERROR_) : return 0
  end if    
  if GetParameter( NumTemp , vtNumber )=0 then return 0
  var iDur = cint(int(NumTemp.fNumber))
  if iDur<0 or iDur>255 then
    ShowErr(_FC_ERROR_) : return 0
  end if
  if iFreq=0 then
    if iDur=0 then
      var dTime=timer-g_StartTimer
      dTime += fmod(dTime,1/60)
      while dTime > (timer-g_StartTimer)
        sleep 1,1
      wend
    else
      sleep iDur*66.67
    end if
  else
    sleep iDur*66.67,1
  end if
  tParser.pzByteCode -= 1 : return true
end function
function Virt(_Tab)     '164 FOR [placeholder]
  ShowErr(_SN_ERROR_) : return 0
end function
function Virt(_To)      '165 TO [placeholder]
  ShowErr(_SN_ERROR_) : return 0
end function
function Virt(_Sub)     '166 SUB [placeholder]
  ShowErr(_SN_ERROR_) : return 0
end function
function Virt(_Then)    '167 THEN [placeholder]
  ShowErr(_SN_ERROR_) : return 0
end function
function Virt(_Step)    '169 STEP [placeholder]
  ShowErr(_SN_ERROR_) : return 0
end function
function Virt(_Del)     '181 DEL [N-[N]]
  dim as integer iStart=-1,iEnd=65535
  select case GetToken()
  case asc("0") to asc("9") 'number
    iStart = ReadInt(tParser)
  case otMinus
    iStart=0
  case 0,asc(":")           'eol
  case else
    ShowErr(_SN_ERROR_)
  end select
  if iStart<>-1 then
    if GetToken()=otMinus then
      select case NextToken()
      case asc("0") to asc("9") 'number
        iEnd = ReadInt(tParser)
      case 0,asc(":")           'eol
      case else
        ShowErr(_SN_ERROR_)
      end select
    else
      iEnd = iStart
    end if
    select case GetToken()
    case 0,asc(":")
    case else
      print ,GetToken()
      ShowErr(_SN_ERROR_)
    end select
  end if  
  'print iStart,iEnd
  var pLine = g_pProgBegin
  while pLine
    with *pLine
      if .wNumber > iEnd then exit while
      if .wNumber >= iStart then 
        var pDel = pLine
        pLine = .pNext 
        DelLine(-1,pDel)        
        continue while
      end if
      pLine = .pNext
    end with
  wend
  tParser.tCont.pRetLine = NULL
  return 1
end function
function Virt(_Edit)    '182 EDIT N
  dim as ValStruct NumLine = any
  if GetParameter( NumLine , vtNumber )=0 then return 0  
  var iLine = int(NumLine.fNumber)
  if cuint(iLine) >= cMaxLineNumber then ShowErr(_FC_ERROR_) : return 0  
  g_pEditLine = FindLine( iLine )
  if g_pEditLine = NULL then ShowErr(_UL_ERROR_)
  return 0
end function
function Virt(_Let)     '186 FOR [placeholder]
  ShowErr(_SN_ERROR_) : return 0
end function
function Virt(_Renum)   '203 RENUM [B],[F],[S]
  dim as ValStruct NumBegin=any,NumFirst=any,NumStep=any
  NumBegin.fNumber=10 : NumFirst.fNumber=0 : NumStep.fNumber=10
  
  'all 3 parameters are optional
  if GetOptParameter(NumBegin,vtNumber)=0 then return 0
  if GetOptParameter(NumFirst,vtNumber)=0 then return 0
  if GetOptParameter(NumStep,vtNumber)=0 then return 0
  dim as long iBegin=int(NumBegin.fNumber)
  dim as long iFirst=int(NumFirst.fNumber)
  dim as long iStep=int(NumStep.fNumber)
  
  'no negatives or above line limit
  if cuint(iBegin)>cMaxLineNumber orelse cuint(iBegin)>cMaxLineNumber orelse cuint(iStep)>cMaxLineNumber then
    ShowErr(_FC_ERROR_)
    return 0
  end if
  
  dim as ushort wLines(cMaxLineNumber)=any
  memset( @wLines(0) , -1 , (cMaxLineNumber+1)*sizeof(uShort) )
    
  var pLine = g_pProgBegin, iLine = iBegin
  while pLine 'verify
    with *pLine
      wLines(.wNumber)=.wNumber
      if .wNumber >= iFirst then
        if cuint(iLine)>cMaxLineNumber then
          ShowErr(_FC_ERROR_) : return 0
        end if        
        wLines(.wNumber)=iLine : iLine += iStep
      elseif .wNumber > iBegin then
        ShowErr(_FC_ERROR_) : return 0      
      end if
      pLine = .pNext
    end with
  wend  
  pLine = g_pProgBegin
  while pLine 'replace
    pLine->wNumber = wLines(pLine->wNumber)
    'locate target numbers
    var pbCode = cast(ubyte ptr,pLine+1), iI=0,iO=0, iQuotes=0, iUpdate=0
    dim as ubyte bBuff(65535)=any    
    do 'the line have label to update?
      bBuff(iO) = pbCode[iI] : iO += 1
      select case pbCode[iI]
      case ti_Rem,ti_Rem+1: iQuotes = 2 'ignore rest of line
      case asc(""""): iQuotes xor= 1              
      case ti_Go,ti_Then,ti_Else,ti_Run
        if iQuotes then iI += 1: continue do
        'check for goto/gosub
        if pbCode[iI]=ti_go then
          do
            iI += 1 : if pbCode[iI]<>asc(" ") then exit do
            bBuff(iO)=pbCode[iI] : iO += 1
          loop
          if pbCode[iI]<>ti_To andalso pbCode[iI]<>ti_Sub then continue do
          bBuff(iO)=pbCode[iI] : iO += 1
        end if
        'locate next character
        do
          iI += 1 : if pbCode[iI]<>asc(" ") then exit do
          bBuff(iO)=pbCode[iI] : iO += 1
        loop        
        if pbCode[iI]<asc("0") orelse pbCode[iI]>asc("9") then continue do
        iUpdate = 1 'line need to be updated
        'if its a number then lets read it
        dim as long iNum = pbCode[iI]-asc("0"), iDig=1 'first digit
        do
          iI += 1 : var bChar = pbCode[iI]
          select case bChar
          case asc("0") to asc("9")
            iNum=iNum*10+(bChar-asc("0")) : iDig += 1
            if iDig>=cMaxLineDigits orelse iNum>cMaxLineNumber then exit do            
          case else : exit do
          end select
        loop
        if iNum>cMaxLineNumber orelse wLines(iNum)=cushort(-1) then          
          ShowErrX(_UL_ERROR_," " & iNum & " AT " & pLine->wNumber )
        else
          iNum = wLines(iNum)
        end if
        iO += sprintf(cast(zstring ptr,@bBuff(iO)),"%i",iNum)
        continue do
      case 0: exit do
      end select
      iI += 1
    loop    
    if iUpdate then pLine = SetLine( pLine->wNumber , iO , @bBuff(0) , pLine )
    pLine = pLine->pNext
  wend
  
  'cant continue, stop program
  tParser.tCont.pRetLine=0
  return 0
  
end function
function Virt(_Drive)   '207 DRIVE N 
  dim as ValStruct NumDrive = any
  if GetParameter( NumDrive , vtNumber )=0 then return 0  
  var iDrive = int(NumDrive.fNumber)
  if iDrive < 0 then ShowErr(_FC_ERROR_) : return 0  
  if iDrive > 3 then ShowErr(_DN_ERROR_) : return 0
  g_iDriveNum = iDrive
  tParser.pzBytecode -= 1
  return 1
end function
function Virt(_Dir)     '211 DIR [N,][$]
  dim as ValStruct StrFilter = any 
  dim as long iDrive = g_iDriveNum
  
  StrFilter.ptStr = 0 : StrFilter.iType = vtNone
  if GetOptParameter( StrFilter , vtString or vtNumber )=0 then return 0  
  
  if StrFilter.iType = vtNumber then
    iDrive = int(StrFilter.fNumber)
    if iDrive < 0 then ShowErr(_FC_ERROR_) : return 0    
    if iDrive > 3 then ShowErr(_DN_ERROR_) : return 0
    StrFilter.ptStr = 0
    if GetToken(-1) = asc(",") then
      if GetOptParameter( StrFilter , vtString )=0 then return 0  
    end if
  end if
  
  var sFilter = "*"
  if StrFilter.ptStr then
    sFilter = _StrText(StrFilter.ptStr)
    if instr(sFilter,"../") orelse instr(sFilter,"..\") then    
      ShowErr(_FC_ERROR_) : return 0
    end if
  end if
  var sFile = dir(g_sDrive(iDrive)+sFilter)
  while len(sFile)
    sFile = left$(sFile+string(15," "),16)
    WriteString(sFile)
    sFile = dir()
  wend    
  tParser.pzByteCode -= 1
  return true
end function
function Virt(_Load)    '211 LOAD $[,R]
  dim as ValStruct StrFile = any
  dim as byte bDoRun = 0
  dim as long iDrive = g_iDriveNum
  if GetParameter( StrFile , vtString )=0 then return 0
  if GetToken(-1)=asc(",") then
    if GetToken()=asc("R") orelse GetToken()=asc("r") then      
      bDoRun=1
    else
      ShowErr(_SN_ERROR_) : return 0
    end if
  end if
  tParser.pzByteCode -= 1
  
  var sFile = _StrText(StrFile.ptStr)
  var iPos = instr(sFile,":")
  if iPos then 
    var iTemp = cuint(sFile[iPos]-asc("0"))
    if iTemp >= cMaxDrives orelse sFile[iPos+1] then
      ShowErr(_FN_ERROR_) : return 0
    end if
    sFile = left(sFile,iPos-1): iDrive = iTemp
  end if
  if instr(sFile,"../") orelse instr(sFile,"..\") then    
    ShowErr(_FN_ERROR_) : return 0
  end if
  if instr(sfile,".")=0 then sFile += ".BAS"
  var f = freefile()
  if open(g_sDrive(iDrive)+sFile for binary access read as #f) then
    ShowErr(_NE_ERROR_) : return 0
  end if
  var iSz = lof(f)
  if iSz > 1024*1024 then ShowErr(_IO_ERROR_) : Close #f : return 0  
  dim as ubyte ptr pbFile = malloc(iSz+1)
  get #f,,*pbFile,iSz : close #f
  pbFile[iSz]=0
  ClearLines() 'clear previous file
  if pbFile[0] = 255 then 'binary
    var iPos = 1+2, iLen=0 'Sig SizeH SizeL
    var iNext = 0, iPrev = 0 , iLine=0  
    do
      iNext = pbFile[iPos]*256+pbFile[iPos+1]: iPos += 2 'NextH NextL BIG ENDIAN
      if iNext=0 then exit do
      iLine = pbFile[iPos]*256+pbFile[iPos+1]: iPos += 2 'LineH LineL BIG ENDIAN
      var iChk = strlen(pbFile+iPos)+1
      if iPrev=0 then iLen = strlen(pbFile+iPos)+1 else iLen=(iNext-iPrev)-4
      'print iPrev,iNext,iLen,iChk
      SetLine(iLine,iChk,pbFile+iPos)
      iPos += iLen : iPrev = iNext
    loop until iPos >= iSz
  else 'ascii
    dim as zstring ptr pzEol,pzStart = cast(zstring ptr,pbFile)
    do    
      pzEol = strpbrk(pzStart,@!"\13\10")
      if pzEol then 
        (*pzEol)[0]=0
        if (*pzEol)[1]=10 orelse (*pzEol)[1]=13 then 
          (*pzEol)[1]=0 : pzEol += 1
        end if
      end if      
      if pzEol <> pzStart then
        dim as long uImmSize        
        'printf(!"'%s'\n",pzStart)
        var pLine = ToTokens(*pzStart,uImmSize,32768)-1
        var pStart = pLine+1
        if cuint(__GetToken(pLine,true)-asc("0"))<10 then
          var iNum = ReadInt(*cptr(ParserStruct ptr,@pLine))
          if iNum > cMaxLineNumber then ShowErr(_SN_ERROR_) : free(pbFile) : return 0
          uImmSize -= cuint(pLine)-cuint(pStart)
          'print "Line: ";iNum,"Size: ";uImmSize
          SetLine( iNum , uImmSize , pLine )
        elseif *pLine then
          ShowErr(_DS_ERROR_) : free(pbFile) : return 0          
        end if
      end if
      pzStart=pzEol+1
    loop while pzEol    
  end if
  tParser.tCont.pRetLine = NULL  
  free(pbFile)
  if bDoRun then
    g_pCurrentLine = g_pProgBegin
    return __Run(tParser)
  end if
  return true
end function
function Virt(_Save)    '216 SAVE $[,A]
  dim as ValStruct StrFile = any
  dim as byte bDoAscii=0
  if GetParameter( StrFile , vtString )=0 then return 0  
  if GetToken(-1)=asc(",") then
    if GetToken()=asc("A") orelse GetToken()=asc("a") then      
      bDoAscii=1
    else
      ShowErr(_SN_ERROR_) : return 0
    end if
    NextToken()
  else
    tParser.pzByteCode -= 1
  end if  
  
  var sFile = _StrText(StrFile.ptStr)
  if instr(sFile,"../") orelse instr(sFile,"..\") then    
    ShowErr(_FC_ERROR_) : return 0
  end if
  if instr(sfile,".")=0 then sFile += ".BAS"
  
  dim as ubyte bBuffer(65535) = any  
  dim as long BuffPos = 0
  if bDoAscii then
    bBuffer(0) = &h0D : bBuffer(1) = &h0A : BuffPos = 2
    dim as ProgLineT pLine = g_pProgBegin
    while pLine
      with *pLine
        var sLine = .wNumber & " " & Detokenize(cast(ubyte ptr,pLine+1))
        BuffPos += sprintf(cast(zstring ptr,@bBuffer(BuffPos)),!"%s\r\n",sLine)
        pLine = .pnext
      end with
    wend
  else  
    bBuffer(0) = 255 : BuffPos = 3 '255+FileSize
    dim as ProgLineT pLine = g_pProgBegin
    while pLine
      with *pLine
        'next line
        var iNext = BuffPos+&h25FF+.wLen+3      
        bBuffer(BuffPos+0)=iNext shr 8
        bBuffer(BuffPos+1)=iNext and &hFF
        'line number
        bBuffer(BuffPos+2)=.wNumber shr 8
        bBuffer(BuffPos+3)=.wNumber and &hFF
        'print .wNumber,.wLen
        memcpy(@bBuffer(BuffPos+4),pLine+1,.wLen)
        BuffPos += 4+.wLen
        pLine = .pNext
      end with    
    wend
    'sleep : end
    'program finishes with 0 for next line
    bBuffer(BuffPos+0)=0
    bBuffer(BuffPos+1)=0  
    BuffPos += 2
    'set file size
    bBuffer(1) = (BuffPos-3) shr 8
    bBuffer(2) = (BuffPos-3) and &hFF
  end if
  
  var f = freefile()
  mkdir(g_sDrive(g_iDriveNum))
  if open(g_sDrive(g_iDriveNum)+sFile for binary access write as #f) then
    ShowErr(_WP_ERROR_)
    return 0
  end if  
  put #f,,bBuffer(0),BuffPos : close #f
  return true
end function
function Virt(_Width)   '226 WIDTH N,N
  dim as ValStruct NumTemp = any
  if GetParameter( NumTemp , vtNumber )=0 then return 0  
  var iWid = cint(int(NumTemp.fNumber))
  if iWid<>32 andalso iWid<>40 andalso iWid>80 then 
    ShowErr(_FC_ERROR_) : return 0
  end if    
  if GetParameter( NumTemp , vtNumber )=0 then return 0
  var iHei = cint(int(NumTemp.fNumber))
  if iHei<>16 andalso iHei<>25 then 
    ShowErr(_FC_ERROR_) : return 0
  end if
  width iWid,iHei
  color 0,10 : cls
  g_OutputCount = 0 : tParser.pzByteCode -= 1
  return true
end function
function Virt(_Help)    '253 HELP
  'dim sKey(128) as string, iKeyCnt
  for N as long = 128 to 255
    var pz = pzToken(N)
    if pz=NULL then continue for
    var sToken = left$(*pz+"       ",8)
    color iif(g_uPri(N) orelse pSubProc(N),0,4)
    WriteString(sTokeN)
  next N
  AdvanceLine()
  for N as long = 128 to 255
    var pz = pzFuncTok(N)
    if pz=NULL then continue for
    var sToken = left$(*pz+"       ",8)
    color iif(pFuncProc(N),0,4)
    WriteString(sTokeN)
  next N
  color 0
  return 1
end function
#undef Virt