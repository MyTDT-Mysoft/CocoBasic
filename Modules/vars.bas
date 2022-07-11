const g_MaxVariables = 256
static shared as ValStruct g_tNumber(g_MaxVariables-1)
static shared as ValStruct g_tString(g_MaxVariables-1)
static shared as ValStruct g_tArrayNum(g_MaxVariables-1)
static shared as ValStruct g_tArrayStr(g_MaxVariables-1)

function GetVariable( byref tParser as ParserStruct , byref tValue as ValStruct , Flags as GetVarFlags ) as long
  with tParser
    dim as ushort wName = GetToken()
    'get remaining of variable name
    var iChar = NextToken()
    if (iChar >= asc("A") andalso iChar <= asc("Z")) orelse (iChar >= asc("0") andalso iChar <= asc("9")) then 
      wName += iChar*256
      do 'locate end of variable name
        iChar = NextToken()
        select case iChar
        case asc("A") to asc("Z")
        case asc("0") to asc("9")
        case else : exit do
        end select
      loop
    end if
    'get variable type
    var iType = vtNumber
    'case asc("%") 'TODO: integer
    dim as byte bIsArray=0
    select case iChar 'NextToken()
    case asc("$") 'string
      iType = vtString
      if NextToken()=asc("(") then bIsArray=1
    case asc("(") 'array
      bIsArray=1    
    end select
            
    if bIsArray=0 andalso (Flags and gvName) then 
      puts("name only")
      tValue.Wname = wName
      tValue.iType = iType      
      var iToken = GetToken()
      if iToken = 0 then iToken = -1
      return iToken
    end if
    
    if bIsArray then
      static as IntT iBound(3-1)={10,10,10}
      var iDim=3, pwBound = @iBound(0)
      var ptList = iif(iType=vtNumber,@g_tArrayNum(0),@g_tArrayStr(0))
      
      'array does exist?
      do
        if ptList->wName = wName then 
          iDim = ptList->ptArray->bDimLevel
          pwBound = ptList->ptArray->pwBounds
          exit do
        end if
        if ptList->iType = vtNone then exit do        
        ptList += 1
      loop
      
      'get array index (validate bounds as well)
      dim as ValStruct DimVal = any
      dim as integer iDimCnt = 0, iMul=1 , iIndex = 0      
      
      dim as ParserStruct tParser2 
      tParser2.pzByteCode = tParser.pzBytecode
      do
        with tParser2
          if iDimCnt >= iDim then iDimCnt += 1 : exit do        
          'print "<<"
          iChar = ParseParameter( tParser , DimVal , vtNumber , 0 , @tParser2 )          
          'print ">>";iChar;DimVal.fNumber
          if iChar = 0 then 
            CleanParser( tParser2 ) : CleanParser( tParser )
            return 0
          end if
          var iNum = int(DimVal.fNumber)
          if iNum<0 then
            ShowErr(_FC_ERROR_)
            return 0
          end if        
          if iNum>pwBound[iDimCnt] then 
            ShowErr(_BS_ERROR_)
            return 0
          end if        
          iIndex += iMul*iNum
          iMul *= (pwBound[iDimCnt]+1)
          iDimCnt += 1
        end with
      loop until iChar = asc(")")
            
      'print iIndex
      NextToken()      
      
      if pwBound = @iBound(0) andalso iDimCnt < iDim then iDim = iDimCnt
      if iDimCnt <> iDim then
        ShowErr(_BS_ERROR_)
        return 0
        'CleanStacks()
      end if      
      'if array was new... create it now (default size)
      'if ptList->iType = vtNone then
      if pwBound = @iBound(0) then
        var iSize = iif(iType=vtNumber,sizeof(numberT),sizeof(stringT))*iMul                
        ptList->ptArray = malloc(sizeof(ArrayStruct)+sizeof(IntT)*iDim+iSize)
        ptList->iType = iType : ptList->wName = wName
        ptList->bFlags = 0 : ptList->bArray = 1
        var pArr = ptList->ptArray
        pArr->bDimLevel=iDim
        pArr->pwBounds = cast(any ptr,pArr+1)
        pArr->pRaw = cast(any ptr,pArr+1)+sizeof(IntT)*iDim
        memcpy( pArr->pwBounds , pwBound , sizeof(IntT)*iDim )        
        if iType=vtNumber then
          memset( pArr->pwNumber , 0 , iSize )
        else
          for N as long = 0 to iMul-1
            pArr->ptStr[N] = @g_NullStr
          next N
        end if
      end if
      
      'return value/address
      var pArr = ptList->ptArray
      if (Flags and gvWrite) then 
        select case iType
        case vtNumber : tValue.pfNum  = pArr->pwNumber+iIndex
        case vtString : tValue.pptStr = pArr->ptStr+iIndex
        end select
      else
        select case iType
        case vtNumber : tValue.fNumber = pArr->pwNumber[iIndex]
        case vtString : tValue.ptStr   = pArr->ptStr[iIndex]
        end select
      end if
      
      'if IsByref then NextToken()
      
    else
      
      'process normally
      var ptList = iif(iType=vtNumber,@g_tNumber(0),@g_tString(0))
      do
        if ptList->wName = wName then exit do
        if ptList->iType = vtNone then
          ptList->iType = iType                    
          if iType=vtString then            
            ptList->ptStr = @g_NullStr
          else
            ptList->Raw = 0
          end if
          'ptList->fNumber = rnd
          ptList->wName = wName
          ptList[1].iType=vtNone
          ptList[1].wName=0
          exit do
        end if
        ptList += 1
      loop      
      
      if (Flags and gvWrite) then        
        tValue.pRaw = cast(any ptr ptr,@(ptList->Raw))
      else
        tValue.Raw = ptList->Raw        
      end if
      
    end if
    
    tValue.wName = wName
    tValue.bTemp = 1
    tValue.bByref = (Flags and gvWrite)<>0
    tValue.iType = iType    
    var iToken = GetToken()
    if iToken = 0 then iToken = -1
    return iToken
    
  end with
    
end function
sub ShowVariable( byref tValue as ValStruct , iNextLine as byte = TRUE )
  if g_bImmediateMode=0 then exit sub
  #ifdef DbgParser
    if iNextLine then AdvanceLine() : g_OutputCount += 1
  #else
    if iNextLine then g_OutputCount += 1
  #endif  
  color 1
  with tValue
    select case .iType
    case vtNumber    
      WriteString( str(iif(.bByref,*.pfNum,.fNumber)) )    
      'if iNextLine then print 'space(1+LOWORD(width)-pos());
    case vtString  
      var iConWid = LOWORD(width())
      dim as stringT pStr = iif(.bByref,*.pptStr,.ptStr)
      if (_StrLen(pStr)+2) >= iConWid then
        WriteString("'"+left(_StrText(pStr),iConWid-6)+"...'")
      else
        WriteString("'"+_StrText(pStr)+"'")
      end if
      'if iNextLine then print 'space(1+LOWORD(width)-pos());
    end select
  end with
  color 0
end sub
sub SetVariable( byref TgtValue as ValStruct , byref SrcValue as ValStruct )
  with TgtValue
    select case .iType
    case vtNumber    
      if .bByref then
        *.pfNum = SrcValue.fNumber
      else
        .fNumber = SrcValue.fNumber
      end if
    case vtString
      if .bByref then
        if *.pptStr<>SrcValue.ptStr then DeleteStr(*.pptStr,true)              
        if *.pptStr<>SrcValue.ptStr andalso _StrConst(SrcValue.ptStr) then
          *.pptStr = AllocStr( _StrLen(SrcValue.ptStr) , _StrPtr(SrcValue.ptStr) , true )
          DeleteStr(SrcValue.ptStr) 'redundant?
        else              
          SrcValue.ptStr->bIsConst=1
          *.pptStr = SrcValue.ptStr
        end if
      else
        if .ptStr<>SrcValue.ptStr then DeleteStr(.ptStr,true)              
        if .ptStr<>SrcValue.ptStr andalso _StrConst(SrcValue.ptStr) then
          .ptStr = AllocStr( _StrLen(SrcValue.ptStr) , _StrPtr(SrcValue.ptStr) , true )
          DeleteStr(SrcValue.ptStr) 'redundant?
        else              
          SrcValue.ptStr->bIsConst=1
          .ptStr = SrcValue.ptStr
        end if            
      end if
    end select
  end with
end sub

sub ClearAllVariables()
  #if 1
    g_tNumber(0).wName=0 : g_tNumber(0).iType=vtNone
  #else
    for N as long = 0 to g_MaxVariables-1
      with g_tNumber(N)
        if .iType=vtNone then .wName = 0 : exit for
        .wName=0 : .iType=vtNone
      end with
    next N
  #endif
  
  for N as long = 0 to g_MaxVariables-1
    with g_tString(N)
      if .iType=vtNone then .wName=0 : exit for
      DeleteStr(.ptStr,true) : .ptStr=0
      .wName=0 : .iType=vtNone
    end with    
  next N
  for N as long = 0 to g_MaxVariables-1
    with g_tArrayNum(N)
      if .iType=vtNone then .wName=0 : exit for
      free(.ptArray) : .ptArray=0
      .wName=0 : .iType=vtNone
    end with    
  next N
  for N as long = 0 to g_MaxVariables-1
    with g_tArrayStr(N)
      if .iType=vtNone then .wName=0 : exit for    
      if .ptArray then
        with *.ptArray
          var iElements = .pwBounds[0]
          for M as long = 1 to .bDimLevel-1
            iElements *= .pwBounds[M]
          next M
          for M as long = 0 to iElements-1
            DeleteStr(.ptStr[M],true)
          next M
        end with
      end if
      free(.ptArray) : .ptArray=0
      .wName=0 : .iType=vtNone
    end with
  next N
end sub
