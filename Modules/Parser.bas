declare function GetVariable( byref tParser as ParserStruct , byref tValue as ValStruct , Flags as GetVarFlags ) as long
declare sub ShowVariable( byref tValue as ValStruct , iNextLine as byte = TRUE )

sub ShowError( sPrev as string , iErr as long , sExtra as string )
  g_OutputCount += 1:AdvanceLine()
  if iErr=0 andalso len(sPrev) then WriteString(sPrev)
  if iErr then 
    if LOWORD(width)<40 then
      WriteString( *pzErrMin(iErr)+" ERROR" )
    else
      WriteString( *pzErrDesc(iErr) )
    end if
  end if
  if g_bImmediateMode=0 andalso g_pCurrentLine then
    WriteString( " AT " & g_pCurrentLine->wNumber )
  end if
  if iErr andalso len(sPrev) then WriteString(sPrev)
  if len(sExtra) then color 2 : WriteString(sExtra): color 0
  AdvanceLine()
end sub

#ifdef DbgErrTrigger
  #define __FILELINE__ " (" & mid(__FILE__,instrrev(__FILE__,"\")+1) & ":" & __LINE__ & ")" 
#else
  #define __FILELINE__ ""
#endif

#define ShowErr(_E) ShowError("",_E,__FILELINE__)
#define ShowErr2(_E,_B) ShowError("'"+chr(_B)+"' ",_E,__FILELINE__)
#define ShowErrX(_E,_S) ShowError(_S,_E,__FILELINE__)
#define ShowErrS(_S) ShowError(_S,0,__FILELINE__)

enum ParseFlags
  pfParenthesis = 1
  pfFirstParm   = 2  
  pfLastParm    = 4
  pfIsSub       = 8
  pfOptional    = 16
  pfSemiColon   = 32
end enum

function ToTokens( pzExpression as zstring ptr , byref uSize as long = 0 , uBuff as long = 0 ) as ubyte ptr  
  'lowercase conversion
  var pbProg = @ImmediateBuffer(uBuff), iQuotes=0
  if pzExpression = NULL then pbProg[0]=0 : return pbProg  
  for N as long = 0 to 65535
    do while iQuotes=0 andalso (*cptr(ulong ptr,pzExpression+N) and &hDFDFDF) = (cvl("REM ") and &hDFDFDF)      
      if N>0 then
        select case (*pzExpression)[N-1]
        case asc("A") to asc("Z"),asc("a") to asc("z")
          exit do        
        end select              
      end if
      (*pzExpression)[N+0] and= (not 32)
      (*pzExpression)[N+1] and= (not 32)
      (*pzExpression)[N+2] and= (not 32)
      exit for      
    loop
    do while iQuotes=0 andalso (*cptr(ulong ptr,pzExpression+N) and &hDFDFDFDF) = cvl("DATA")
      if N>0 then
        select case (*pzExpression)[N-1]
        case asc("A") to asc("Z"),asc("a") to asc("z")
          exit do        
        end select              
      end if
      (*pzExpression)[N+0] and= (not 32)
      (*pzExpression)[N+1] and= (not 32)
      (*pzExpression)[N+2] and= (not 32)
      (*pzExpression)[N+3] and= (not 32)
      N += 3
      do
        select case (*pzExpression)[N+1]
        case asc(""""): iQuotes xor= 1
        case 0: exit for
        case asc(":"): if iQuotes=0 then continue for
        end select           
        N += 1          
      loop
    loop
    select case as const (*pzExpression)[N]
    case 0
      exit for
    case asc("""")
      iQuotes xor= 1
    case asc("'")
      if iQuotes=0 then exit for
    case asc("a") to asc("z")
      if iQuotes=0 then (*pzExpression)[N] and= (not 32) 'UPPERCASE
    end select
  next N  
  
  dim as ubyte uChar = 0
  do
    uChar = (*pzExpression)[0]     
    select case uChar    
    case asc("A") to asc("Z")
      uChar and= (not 32)      
      for N as long = 128 to 255
        var pz = pzToken(N)
        if pz then
          var iSz=strlen(pz)
          if memcmp(pz,pzExpression,iSz)=0 then
            pbProg[0] = N 
            pbProg += 1 : pzExpression += iSz
            
            if N = ti_Rem orelse N = (ti_Rem+1) then
              do        
                pbProg[0] = (*pzExpression)[0] 
                pbProg += 1 : pzExpression += 1
              loop while pbProg[-1]
              exit do
            end if
            
            continue do
          end if
        end if
      next N
      for N as long = 128 to 255
        var pz = pzFuncTok(N)
        if pz then
          var iSz=strlen(pz)          
          if memcmp(pz,pzExpression,iSz)=0 then
            pbProg[0] = 255 : pbProg[1] = N
            pbProg += 2 : pzExpression += iSz
            continue do
          end if
        end if
      next N
      while (*pzExpression)[1] >= asc("A") and (*pzExpression)[1] <= asc("Z")
        *pbProg = uChar : pbProg += 1
        pzExpression += 1 : uChar = (*pzExpression)[0]
      wend        
    case asc("""")
      *pbProg = uChar : pbProg += 1
      do 
        pzExpression += 1
        uChar = (*pzExpression)[0]
        if uChar=asc("""") orelse uChar=0 then exit do        
        *pbProg = uChar : pbProg += 1
      loop      
    case asc("?")
      uChar = ti_Print
    case asc("-"),asc("+"),asc("*"),asc("/"),asc("<"),asc(">"),asc("="),asc("^"),asc("'")
      for N as long = 128 to 255
        var pz = pzToken(N)
        if pz andalso (*pz)[0]=uChar then
          
          if N = ti_Rem orelse N = (ti_Rem+1) then      
            *pbProg = N : pbProg += 1 : pzExpression += 1    
            do        
              pbProg[0] = (*pzExpression)[0] 
              pbProg += 1 : pzExpression += 1
            loop while pbProg[-1]
            exit do
          end if
          
          uChar = N : exit for
        end if
      next N
    end select
    *pbProg = uChar : pbProg += 1
    pzExpression += 1
  loop while pbProg[-1]
  pbProg[0]=&h3
  pbProg[1]=&h3
  pbProg[2]=&h3
  pbProg[3]=&h3
  uSize = cuint(pbProg)-cuint(@ImmediateBuffer(uBuff))
  return @ImmediateBuffer(uBuff)
end function
function Detokenize( pbBuffer as ubyte ptr ) as string
  dim as byte uQuotes=0
  dim as string sResult
  do
    var bChar = *pbBuffer
    select case bChar  
    case ti_Rem,ti_Rem+1 'comments
      sResult += *pzToken(cuint(bChar))+cptr(zstring ptr,pbBuffer)[1]
      exit do
    case 128 to 255      'opcodes
      dim as zstring ptr ptr pzTok = @pzToken(128)-128
      if bChar=255 then 
        pbBuffer += 1 : bChar= *pbBuffer
        pzTok = @pzFuncTok(128)-128
      end if
      sResult += *pzTok[cuint(bChar)]
      pbBuffer += 1 : continue do
    case 0               'end
      exit do
    case asc("""")       'strings
      do
        sResult += chr(bChar) : pbBuffer += 1
        bChar = *pbBuffer
        if bChar = 0 then exit do,do
        if bChar = asc("""") then exit do
      loop      
    end select  
    sResult += chr(bChar) : pbBuffer += 1
  loop
  return sResult
end function

sub DisplayLine( wLineNumber as ushort , pbBuffer as ubyte ptr )
  color 9 : WriteString( wLineNumber & " " )
  dim as byte uQuotes=0  
  do
    var bChar = *pbBuffer
    color 1
    select case bChar
    case asc("&")
      if pbBuffer[1] = asc("H") orelse pbBuffer[1] = asc("O") then
        color 3 : WriteString(chr(bChar)) 
        pbBuffer += 1 : bChar = *pbBuffer
        do
          WriteString( chr(bChar) ) : pbBuffer += 1
          bChar = *pbBuffer
          select case bChar
          case asc("0") to asc("9"),asc("A") to asc("F")
          case else: continue do,do
          end select          
        loop
      end if        
    case asc(","),asc(":"),asc("("),asc(")"),asc(";")
      color 9
    case asc("A") to asc("Z") 'variables
      color 5 : WriteString(chr(bChar)) 
      do 
        pbBuffer += 1 : bChar = *pbBuffer
        select case bChar
        case asc("A") to asc("Z"),asc("0") to asc("9")
        case else : exit do
        end select
        WriteString(chr(bChar)) 
      loop
      if bChar = asc("$") or bChar = asc("%") then pbBuffer += 1 : WriteString(chr(bChar))
      continue do      
      
    case ti_Rem,ti_Rem+1      'comments
      color 0,11
      WriteString( *pzToken(cuint(bChar))+cptr(zstring ptr,pbBuffer)[1] )
      color 0,10
      exit do
    case 128 to 255           'opcodes
      dim as zstring ptr ptr pzTok = @pzToken(128)-128
      dim as byte bIsFunction
      if bChar=255 then 
        pbBuffer += 1 : bChar= *pbBuffer
        pzTok = @pzFuncTok(128)-128
        bIsFunction = 1 : color 2
      else         
        color iif( g_bOpFlags(bChar).bIsChar , 9 , 0 )
      end if
      var pzVar = pzTok[cuint(bChar)]
      if pzVar = 0 then pzVar = @"???"
      WriteString( *pzVar ) 
      if bIsFunction=0 then
        select case bChar
        case ti_Data
          var iColor=0,iQuote=0
          do
            pbBuffer += 1 : bChar = *pbBuffer
            select case bChar
            case asc("""")
              iQuote xor= 1
              if iColor=0 then iColor=1:color 4          
            case asc("0") to asc("9")
              if iColor=0 then iColor=1:color 1
            case 0,asc(":"): continue do,do
            case asc(",")
              if iQuote=0 then color 0: iColor=0          
            case else
              if iColor=0 then iColor=1:color 6          
            end select
            WriteString( chr(bChar) )
          loop
        case ti_Save,ti_Load,ti_CSave,ti_CLoad,ti_QSave,ti_QLoad
          if pbBuffer[1]=asc("M") then
            pbBuffer += 1
            WriteString(chr(*pbBuffer))
          end if
        end select
      end if
      pbBuffer += 1 : continue do
    case 0                    'end
      color 0 : exit do
    case asc("""")            'strings
      color 4
      do
        WriteString( chr(bChar) ) : pbBuffer += 1
        bChar = *pbBuffer
        if bChar = 0 then continue do,do
        if bChar = asc("""") then exit do
      loop
    end select      
    WriteString(chr(bChar)) : pbBuffer += 1
  loop
  color 0,10 : AdvanceLine()
end sub
function EditLine( pLine as ProgLine ptr , byref iCancelled as long = 0 ) as string
  var sLine = pLine->wNumber & " "
  'DisplayLine( pLine->wNumber , cast(any ptr,pLine+1) )
  WriteString( sLine )
  return sLine + LineInput( Detokenize(cast(any ptr,pLine+1)) , iCancelled )
end function

function __GetToken( byref pzChar as zstring ptr , bAdvance as byte=0 ) as long  
  if bAdvance then pzChar += 1
  while (*pzChar)[0] = asc(" ")    
    pzChar += 1
  wend 
  var iChar = (*pzChar)[0]
  return iif(iChar=ti_Rem+1,0,iChar)
end function
#define GetToken(_P...) __GetToken(cptr(zstring ptr,tParser.pzByteCode _P))
#define NextToken() __GetToken(cptr(zstring ptr,tParser.pzByteCode),1)

function ReadInt( byref tParser as ParserStruct ) as long
  with tParser
    var iNum = GetToken()-asc("0")
    if cuint(iNum)>9 then return 0
    do
      var iChar = NextToken()
      select case iChar
      case asc("0") to asc("9")
        iNum = (iNum*10)+(iChar-asc("0"))
        if iNum >= cMaxLineNumber then exit do
      case else
        exit do
      end select
    loop
    return iNum
  end with
end function
function ReadHex( byref pzChar as zstring ptr ) as NumberT  
  dim as long iRead = any
  dim as long fResult = 0
  if *cptr(ushort ptr,pzChar) = asc(".") then pzChar += 1 : return 0
  if sscanf( pzChar , "%i%n",@fResult,@iRead )=0 then    
    pzChar += 1 : return 0  
  end if  
  pzChar += iRead
  return fResult
end function
function ReadNumber( byref pzChar as zstring ptr ) as NumberT
  dim as long iRead = any
  dim as single fResult = 0
  if *cptr(ushort ptr,pzChar) = asc(".") then pzChar += 1 : return 0
  if sscanf( pzChar , "%g%n",@fResult,@iRead )=0 then    
    pzChar += 1 : return 0  
  end if  
  pzChar += iRead
  return fResult
end function
function Evaluate2( byref lVal as ValStruct , iOper as long , rVal as ValStruct = type(vtNumber)) as long  
  if rVal.iType then
    if lVal.iType <> rVal.iType then
      ShowErr(_TM_ERROR_)
      return 0
    end if
  else
    if iOper=otMinus then iOper=otNeg
  end if
  select case lVal.iType
  case vtString
    select case iOper
    case otConcat
      var pTemp = ConcatStr( lVal.ptStr , rVal.ptStr )
      if pTemp then
        lVal.ptStr = pTemp
      else
        ShowErr(_LS_ERROR_)
        return 0
      end if
    case otDiff
      var iResu = strcmp(_strptr(lVal.ptStr),_strptr(rVal.ptStr))<>0
      DeleteStr(lVal.ptStr) : lVal.iType = vtNumber : lVal.fNumber = iResu
    case otLower      
      var iResu = strcmp(_strptr(lVal.ptStr),_strptr(rVal.ptStr))<0
      DeleteStr(lVal.ptStr) : lVal.iType = vtNumber : lVal.fNumber = iResu
    case otGreater
      var iResu = strcmp(_strptr(lVal.ptStr),_strptr(rVal.ptStr))>0
      DeleteStr(lVal.ptStr) : lVal.iType = vtNumber : lVal.fNumber = iResu
    case otEqu
      var iResu = strcmp(_strptr(lVal.ptStr),_strptr(rVal.ptStr))=0
      DeleteStr(lVal.ptStr) : lVal.iType = vtNumber : lVal.fNumber = iResu    
    case otLoEqu
      var iResu = strcmp(_strptr(lVal.ptStr),_strptr(rVal.ptStr))<=0
      DeleteStr(lVal.ptStr) : lVal.iType = vtNumber : lVal.fNumber = iResu    
    case otGtEqu
      var iResu = strcmp(_strptr(lVal.ptStr),_strptr(rVal.ptStr))>=0
      DeleteStr(lVal.ptStr) : lVal.iType = vtNumber : lVal.fNumber = iResu    
    case else
      ShowErr(_SN_ERROR_)      
      return 0
    end select
  case vtNumber
    select case as const iOper
    case otAnd
      lVal.fNumber and= rVal.fNumber
    case otOr
      lVal.fNumber or= rVal.fNumber
    case otPower
      lVal.fNumber ^= rVal.fNumber
    case otNeg      
      lVal.fNumber = -lVal.fNumber
    case otMinus
      lVal.fNumber -= rVal.fNumber
    case otPlus
      lVal.fNumber += rVal.fNumber
    case otMul
      lVal.fNumber *= rVal.fNumber
    case otDiv
      if rVal.fNumber=0 then
        ShowErr(_D0_ERROR_)
        return 0
      end if
      lVal.fNumber /= rVal.fNumber
    case otLower      
      lVal.fNumber = lVal.fNumber < rVal.fNumber
    case otGreater
      lVal.fNumber = lVal.fNumber > rVal.fNumber
    case otEqu
      lVal.fNumber = lVal.fNumber = rVal.fNumber
    case otDiff
      lVal.fNumber = lVal.fNumber <> rVal.fNumber
    case otLoEqu 
      lVal.fNumber = lVal.fNumber <= rVal.fNumber
    case otGtEqu
      lVal.fNumber = lVal.fNumber >= rVal.fNumber
    case else      
      ShowErr(_SN_ERROR_)    
      return 0
    end select  
  end select
  return 1
end function
sub CleanParser( byref tParser as ParserStruct )
  with tParser
    for N as long = .iValOff-1 to 0 step -1            
      if .tValStack(N).iType = vtString then DeleteStr(.tValStack(N).ptStr)
    next N
    .iValOff=-1 : .iOpOff = 0 
  end with  
end sub
sub __CleanStacks( byref ptVal as ValStruct ptr , byref iValOff as long , byref iOpOff as long )  
  for N as long = iValOff-1 to 0 step -1
    with ptVal[N]
      if .iType = vtString then
        DeleteStr(.ptStr)
      end if
    end with
  next N
  iValOff=-1 : iOpOff = 0 
end sub
'#define CleanStacks() __CleanStacks( @.tValStack(0) , .iValOff , .iOpOff )
#define CleanStacks() .iValOff=-1 : .iOpOff=0

sub CleanStrings( tParser as ParserStruct )
  for N as long = 1 to g_iStringCnt
    if g_ptString(N) then 
      DeleteStr( g_ptString(N) )
    end if
  next N
  g_iStringCnt=0
  #if 0
    with tParser
      for N as long = .iValOff-1 to 0 step -1
        with .tValStack(N)
          if .iType = vtString then
            DeleteStr(.ptStr)
          end if
        end with
      next N
    end with
  #endif
end sub

#ifdef DbgParser
sub ShowParser( tParser as ParserStruct )
  if g_bImmediateMode=0 then exit sub
  with tParser
    scope
      var iLine = csrlin() , iCol = __pos()
      locate 1,1 : print space(LOWORD(width())*2-1);      
      locate 1,1 :color 5      
      for N as integer = 0 to .iValOff-1
        ShowVariable( .tValStack(N) , FALSE ): print " ";
      next N
      locate 2,1
      for N as integer = 0 to .iOpOff-1
        if .bOpStack(N) >= 128 then
          print *pzToken(.bOpStack(N));" ";
        else
          print chr$(.bOpStack(N));" ";
        end if
      next N
      locate iLine,iCol : color 15
    end scope      
    var pbTemp = @ImmediateBuffer(0)-1
    do          
      pbTemp += 1
      if pbTemp = .pzByteCode then color 8
      if pbTemp[0] = 0 then exit do
      if pbTemp[0] = 255 then continue do
      if pbTemp[0] >= 128 then 
        var pzTok = iif(pbTemp[-1]=255,pzFuncTok(pbTemp[0]),pzToken(pbTemp[0]))
        if pzTok = 0 then pzTok = @"???"
        print *pzTok;
      else
        print chr(pbTemp[0]);
      end if
    loop         
    color 4 : print " (V=" & .iValOff & " O=" & .iOpOff & " {=" & .iOpenCount & " L='";
    if .iLastToken >= 128 then 
      var pzTok = iif(pbTemp[-1]=255,pzFuncTok(.iLastToken),pzToken(.iLastToken))
      if pzTok = 0 then pzTok = @"???"
      print *pzTok;
    else
      print chr(.iLastToken);
    end if    
    print !"')        \r";: color 0
    sleep' 500,1
  end with
end sub
#endif

function ParseExpression( byref tParser as ParserStruct , pzStart as zstring ptr , iFlags as ParseFlags = pfFirstParm ) as long  
  
  #macro OperatorEvaluate(_ex_...)
    if .iValOff<1 orelse .iOpOff<1 then
      ShowErr(_SN_ERROR_)
      CleanStacks() : _ex_
    else
      .iOpOff -= 1 
      'evaluate expression (TODO: unary?)
      var bOperator = .bOpStack(.iOpOff)
      if g_bOpFlags(bOperator).bUnary then    
        if .tValStack(.iValOff-1).iType = vtString then        
          ShowErr(_TM_ERROR_)        
          CleanStacks()
        else
          if Evaluate2( .tValStack(.iValOff-1) , bOperator ) = 0 then CleanStacks() : _ex_
        end if
      elseif .iValOff=1 then
        if g_uPri(bOperator) >= 20 then
          ShowErr(_SN_ERROR_)
          CleanStacks() : _ex_
        else
          if Evaluate2( .tValStack(.iValOff-1) , bOperator ) = 0 then CleanStacks() : _ex_
        end if
      else
        if .iValOff < 2 then
          ShowErr(_SN_ERROR_)
          CleanStacks() : _ex_
        else
          .iValOff -= 1 
          if Evaluate2( .tValStack(.iValOff-1) , .bOpStack(.iOpOff) , .tValStack(.iValOff) ) = 0 then 
            CleanStacks() : _ex_
          end if
        end if
      end if
    end if
  #endmacro
  
  with tParser
    
    're-init parser
    if pzStart = 0 then pzStart = @""
    .pzBytecode = cptr(ubyte ptr,pzStart)
    if (iFlags and pfFirstParm) then    
      .bOpStack(0)=0 : .bPriStack(0)=0
      .iError = 0 : .iOpOff = 0 : .iValOff=0
      .iOpenCount = 0 : .iLastToken = 0
    end if
    #define pzChar cptr(zstring ptr , .pzByteCode)
    
    'process expression
    do
      var bChar = clng(*.pzBytecode)
      #ifdef DbgParser
      ShowParser(tParser)
      #endif
      RestartParse:
      
      #macro ParameterIsolatePrint() 
        if .iValOff=(.iOpOff+1) andalso .iOpenCount=0 then 
          if g_uPri(.iLastToken) then
            ShowErr(_SN_ERROR_)
            CleanStacks()
          else      
            .iLastToken = 0
            while .iOpOff > -((iFlags and pfParenthesis)<>0)
              OperatorEvaluate(exit do)      
            wend
          end if
          exit do        
        end if
      #endmacro
      
      select case as const bChar        
      case asc("0") to asc("9"),asc(".") 'A number: push it onto the value stack.
        ParameterIsolatePrint()
        'TODO: Bug Compatibility: ignore spaces in between digits 10 2 4 = 1024
        .iLastToken = asc("0")
        .tValStack(.iValOff).bFlags  = 0
        .tValStack(.iValOff).iType   = vtNumber
        .tValStack(.iValOff).fNumber = ReadNumber(pzChar)                        
        .iValOff += 1 : pzChar -= 1
      case asc("&")
        var pzTemp = pzChar
        if .pzByteCode[1] = asc("O") then 
          ParameterIsolatePrint()
          dim as ushort wTemp = cvshort("00")
          swap wTemp , *cptr(ushort ptr,pzTemp)
          .iLastToken = asc("0")
          .tValStack(.iValOff).bFlags = 0
          .tValStack(.iValOff).iType   = vtNumber
          .tValStack(.iValOff).fNumber = ReadHex(pzChar)                        
          .iValOff += 1 : pzChar -= 1
          swap wTemp , *cptr(ushort ptr,pzTemp)          
        elseif .pzByteCode[1] = asc("H") then
          ParameterIsolatePrint()
          dim as ushort wTemp = cvshort("0X")
          swap wTemp , *cptr(ushort ptr,pzTemp)
          .iLastToken = asc("0")
          .tValStack(.iValOff).bFlags = 0
          .tValStack(.iValOff).iType   = vtNumber
          .tValStack(.iValOff).fNumber = ReadHex(pzChar)                        
          .iValOff += 1 : pzChar -= 1          
          swap wTemp , *cptr(ushort ptr,pzTemp)          
        else          
          ParameterIsolatePrint()
          .iLastToken = asc("0")
          .tValStack(.iValOff).bFlags = 0
          .tValStack(.iValOff).iType   = vtNumber
          .tValStack(.iValOff).fNumber = 0
          .iValOff += 1 ': pzChar -= 1
        end if
      case asc("""")
        ParameterIsolatePrint()
        .iLastToken = bChar : pzChar += 1
        var iLen = strcspn ( pzChar, """" )
        .tValStack(.iValOff).bFlags = 0
        .tValStack(.iValOff).iType = vtString        
        .tValStack(.iValOff).ptStr = AllocStr( iLen , pzChar )      
        pzChar += iLen
        if (*pzChar)[0] <> asc("""") then pzChar -= 1      
        .iValOff += 1
      case asc(" "),9                    'ignore spaces/tabs
      case asc("A") to asc("Z")          'A variable: get its value, and push onto the value stack.
        ParameterIsolatePrint()
        if GetVariable( tParser , .tValStack(.iValOff) , gvRead ) = 0 then
          ShowErr(_SN_ERROR_)
          exit do
        else   
          if .tValStack(.iValOff).iType = vtString then
            .tValStack(.iValOff).ptStr->bIsConst=1
          end if
          .iValOff += 1 : pzChar -= 1
          .iLastToken = asc("A")
        end if        
      case asc("(")                      'A left parenthesis: push it onto the operator stack.
        ParameterIsolatePrint()
        .iLastToken = bChar
        .bOpStack(.iOpOff) = bChar
        .bPriStack(.iOpOff) = g_uPri(bChar)
        .iOpOff += 1 : .iOpenCount += 1
      case asc(")")                      'A right parenthesis: While top of the operator stack is not left parenthesis
        if .iOpenCount < 1 then
          ShowErr(_SN_ERROR_)
          CleanStacks(): exit do
        end if
        .iLastToken = bChar : .iOpenCount -= 1    
        'While top of the operator stack is not left parenthesis
        do while .iOpOff>0
          if .bOpStack(.iOpOff-1) = asc("(") then .iOpOff -= 1 : exit do            
          OperatorEvaluate(exit do,do)            
          'stack gets modified with right value so just decrement count instead of push result
        loop   
        'parenthesis already popped of the stack
        
        if .iOpenCount=0 andalso (iFlags and pfParenthesis) then
          if g_uPri(.iLastToken) then
            ShowErr(_SN_ERROR_)
            CleanStacks()
          else      
            .iLastToken = 0          
            while .iOpOff > 0
              OperatorEvaluate(exit do)
            wend
          end if
          if (iFlags and pfIsSub) then .pzBytecode += 1 
          'after function
          exit do
        end if
        
      case 255                           'A Function call its procedure and get the value
        ParameterIsolatePrint()
        .pzByteCode += 1
        'is a valid function?
        var iFunc = .pzByteCode[0]
        var pFunc = iif(iFunc>=128,pFuncProc(iFunc),0)
        if pFunc = 0 then
          if iFunc < 128 orelse pzFuncTok(iFunc)=0 then          
            ShowErr(_SN_ERROR_)
          else
            ShowErrS("Not Implemented: '"+*pzFuncTok(iFunc)+"()'")
          end if
          CleanStacks() : exit do  
        end if
        
        .pzByteCode += 1
        'if the function requires parameters it requires a open parenthesis
        if g_bOpFlags( iFunc ).bFunc0=0 andalso GetToken()<>asc("(") then
          ShowErr(_SN_ERROR_)
          CleanStacks() : exit do
        end if
        'if the function callback succeeds one value is added otherwise an error happened
        if pFunc(tParser,.tValStack(.iValOff)) then
          .iLastToken = iif(.tValStack(.iValOff).iType=vtNumber,asc("0"),asc(""""))
          .iValOff += 1
        else          
          CleanStacks() : exit do  
        end if
        'if the function requires parameter it must finish with close parenthesis
        if g_bOpFlags( iFunc ).bFunc0=0 then
          if GetToken()<>asc(")") then
            ShowErr(_SN_ERROR_)
            CleanStacks() : exit do
          else
            .pzByteCode += 1
          end if
        end if
        continue do
      case asc(","),asc(";")
        if bChar = asc(";") andalso (iFlags and pfSemiColon)=0 then
          ShowErr2(_SN_ERROR_,bChar)
          CleanStacks(): exit do        
        end if
        if g_uPri(.iLastToken) then
          ShowErr(_SN_ERROR_)
          CleanStacks()
        else      
          .iLastToken = 0
          while .iOpOff > -((iFlags and pfParenthesis)<>0)
            OperatorEvaluate(exit do)      
          wend
        end if        
        exit do
      case ti_Else,ti_Rem+1,0, asc(":") 'end of line / statement / comment
        if g_uPri(.iLastToken) then
          ShowErr(_SN_ERROR_)
          CleanStacks()
        else      
          .iLastToken = 0          
          while .iOpOff > 0
            OperatorEvaluate(exit do)      
          wend
        end if
        exit do
      case else        
        if g_bOpFlags(bChar).bIsChar then
          ' While the operator stack is not empty, and the top thing on the
          ' operator stack has the same or greater precedence as thisOp,
          var nPriority = g_uPri(bChar)
          if .iLastToken=0 then
            if bChar=otMinus  then 
              bChar=otNeg: nPriority = g_uPri(bChar)          
            else
              .iLastToken = otNeg
              pzChar += 1 : continue do
            end if
          end if
          
          while .pzBytecode[1]=asc(" ")
            .pzBytecode += 1
          wend
          
          select case bChar
          case otMinus, otPlus          
            if pzChar <> pzStart then
              if .iLastToken=otPlus orelse .iLastToken=otMinus then            
                if bChar = otMinus then 
                  .bOpStack(.iOpOff-1) xor= (otMinus xor otPlus)            
                elseif .iValOff>0 andalso .tValStack(.iValOff-1).iType = vtString then
                  ShowErr(_TM_ERROR_)
                  CleanStacks()
                  exit do
                end if
                pzChar += 1 : continue do
              end if
            end if
          case otLower          
            if .pzBytecode[1] = otEqu     then bChar = otLoEqu : pzChar += 1
            if .pzBytecode[1] = otGreater then bChar = otDiff  : pzChar += 1
          case otGreater            
            if .pzBytecode[1] = otEqu     then bChar = otGtEqu : pzChar += 1
            if .pzBytecode[1] = otLower   then bChar = otDiff  : pzChar += 1
          case otEqu
            if .pzBytecode[1] = otLower   then bChar = otLoEqu : pzChar += 1
            if .pzBytecode[1] = otGreater then bChar = otGtEqu : pzChar += 1
          end select          
          
          if bChar=otMinus then
            if .iLastToken=asc("(") orelse g_uPri(.iLastToken) then
              bChar=otNeg: nPriority = g_uPri(bChar)          
            end if
          end if
          
          while .iValOff>0 andalso .iOpOff > 0 andalso .bPriStack(.iOpOff-1) >= nPriority
            OperatorEvaluate(exit do)
          wend
          .iLastToken = bChar
          '2 Push thisOp onto the operator stack.
          .bOpStack(.iOpOff) = bChar 
          .bPriStack(.iOpOff) = nPriority
          .iOpOff += 1
        else
          if (iFlags and pfIsSub) then
            bChar = asc(":")
            goto RestartParse
          end if
          ShowErr2(_SN_ERROR_,bChar)
          CleanStacks(): exit do
        end if
      end select   
      
      pzChar += 1
      
    loop while .iValOff >= 0
    
    #ifdef DbgParser
    ShowParser(tParser)
    #endif
    
    return .iError
  end with
  
end function

function ParseParameter( byref tParser as ParserStruct , byref tParameter as ValStruct , iWantTypes as long = vtNone , iFlags as long = pfFirstParm or pfLastParm , ptParser2 as ParserStruct ptr = 0 ) as long
  dim as ParserStruct tParserTemp = any
  dim as ParserStruct ptr ptParser = iif(ptParser2,ptParser2,@tParserTemp)
  if ptParser2=0 then memset(@tParserTemp,0,offsetof(ParserStruct,bOpStack(0)))
  with *ptParser
    
    var iVal = .iValOff , iOp = .iOpOff , pzStart = tParser.pzBytecode    
    
    if (iFlags and pfIsSub) then
      ParseExpression( *ptParser , tParser.pzBytecode , iFlags  )
    else
      ParseExpression( *ptParser , tParser.pzBytecode , pfParenthesis or iFlags )                
    end if
        
    if (iFlags and pfOptional) andalso .iValOff=iVal andalso .iOpOff=iOp then
      var iNext = GetToken()
      tParser.pzBytecode = ptParser->pzByteCode+1      
      if iNext = 0 then return -2      
      return iNext
    end if
    
    if .iValOff<>-1 andalso .pzBytecode <> pzStart then
      'TODO: can pfLastParm get deprecated? as it can be detected by the precense of a ")"?
      'but then that will need to postpode the error check for after the GetToken()
      'or change the nextToken to here?
      'if .iValOff<>1 orelse (iNext=asc(")") andalso .iOpOff>0) then
      
      if (.iValOff-iVal)<>1 orelse ((iFlags and pfLastParm) andalso .iOpOff>iOp) then
        ShowErr(_SN_ERROR_)
        CleanStacks() : return 0
      end if
    end if        
    
    tParser.pzBytecode = ptParser->pzByteCode    
    if (.iValOff-iVal)=1 then
      if iWantTypes andalso (.tValStack(.iValOff-1).iType and iWantTypes) then        
        var iNext = GetToken() ,  iCheck = (iNext=asc(")"))
        
        if (iFlags and pfSemiColon) then
          tParameter = .tValStack(0)
          if iNext <> asc(")") then 
            tParser.pzBytecode += 1
            if iNext <> asc(",") then iNext = asc(";")
          end if          
          .iValOff = iVal : .iOpOff = iOp
          return iNext          
        end if
        
        if (iFlags and pfIsSub) then iCheck=((iNext=0) or (iNext=asc(":")) or (iNext>=128))
        if (iFlags and pfSemiColon) then iCheck or= (iNext=asc(";"))
        if (iFlags and pfLastParm)=0 then
          var iCheck2 = (iNext=asc(","))          
          if (iFlags and pfFirstParm) then iCheck=iCheck2 else iCheck or= iCheck2
        end if          
        if iCheck=0 then
          ShowErr(_SN_ERROR_)
          CleanStacks() : return 0
        end if
        '(iFlags and pfLastParm)=0
        if iNext <> asc(")") then tParser.pzBytecode += 1' : .iValOff -= 1
        tParameter = .tValStack(0)
        if iNext = 0 then return -1
        
        'shouldnt be necessary?
        .iValOff = iVal : .iOpOff = iOp
        
        return iNext
      else
        if iWantTypes then 
          ShowErr(_TM_ERROR_)        
        else
          ShowErr(_SN_ERROR_)
        end if
      end if
    elseif .iValOff<>-1 then
      ShowErr(_SN_ERROR_)      
    end if    
    CleanStacks() : return 0
  end with
end function
