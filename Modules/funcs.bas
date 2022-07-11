#define Virt(_Name) Func##_Name ( byref tParser as ParserStruct , byref tRetVal as ValStruct ) as long  
function Virt(_Sgn)   '(num) as num
  if ParseParameter( tParser , tRetVal , vtNumber ) then
    tRetVal.fNumber = sgn(tRetVal.fNumber)
    return 1
  end if
end function
function Virt(_Int)   '(num) as num
  if ParseParameter( tParser , tRetVal , vtNumber ) then
    tRetVal.fNumber = int(tRetVal.fNumber)
    return 1
  end if
end function
function Virt(_Abs)   '(num) as num
  if ParseParameter( tParser , tRetVal , vtNumber ) then
    tRetVal.fNumber = abs(tRetVal.fNumber)
    return 1
  end if
end function
function Virt(_Rnd)   '(num) as num
  if ParseParameter( tParser , tRetVal , vtNumber ) then
    if tRetVal.fNumber=0 then
      tRetVal.fNumber = rnd
    else
      tRetVal.fNumber = 1+int(rnd*int(tRetVal.fNumber))
    end if
    return 1
  end if
end function
function Virt(_Sin)   '(num) as num
  if ParseParameter( tParser , tRetVal , vtNumber ) then
    tRetVal.fNumber = sin(tRetVal.fNumber)    
    return 1
  end if
end function
function Virt(_Len)   '(str) as num
  dim as ValStruct TempStr = any
  if ParseParameter( tParser , TempStr , vtString ) then
    tRetVal.iType = vtNumber
    tRetVal.fNumber = _StrLen(TempStr.ptStr)
    DeleteStr( TempStr.ptStr )
    return 1
  end if
end function
function Virt(_Str)   '(num) as str
  dim as ValStruct TempNum  
  if ParseParameter( tParser , TempNum , vtNumber ) then
    dim as zstring*32 zTemp = any
    dim as zstring ptr pzTemp = @zTemp+1
    zTemp[0] = asc(" ")
    var iLen = sprintf(pzTemp,"%g",TempNum.fNumber)
    if TempNum.fNumber >= 0 then pzTemp -= 1: iLen += 1
    tRetVal.iType = vtString
    tRetVal.ptStr = AllocStr( iLen , pzTemp )
    return 1
  end if  
end function
function Virt(_Val)   '(str) as num TODO: block &B
  dim as ValStruct TempStr = any
  if ParseParameter( tParser , TempStr , vtString ) then
    var fbStr = _fbStrFromStr( TempStr.ptStr )    
    tRetVal.iType = vtNumber    
    tRetVal.fNumber = val( _fbStr(fbStr) )
    DeleteStr( TempStr.ptStr )
    return 1
  end if
end function
function Virt(_Asc)   '(str) as num
  dim as ValStruct TempStr = any
  if ParseParameter( tParser , TempStr , vtString ) then
    function = 1
    tRetVal.iType = vtNumber
    if _StrLen(TempStr.ptStr)=0 then
      ShowErr(_FC_ERROR_)
      function = 0
    else
      tRetVal.fNumber = _StrPtr(TempStr.ptStr)[0]
    end if
    DeleteStr( TempStr.ptStr )    
  end if
end function
function Virt(_Chr)   '(num) as str
  dim as ValStruct TempNum  
  if ParseParameter( tParser , TempNum , vtNumber ) then
    dim as long uChar = int(TempNum.fNumber)
    if uChar < 0 orelse uChar > 255 then
      ShowErr(_FC_ERROR_)
      return 0
    end if      
    tRetVal.iType = vtString
    dim as ubyte uChar2 = uChar
    tRetVal.ptStr = AllocStr( 1 , cptr(zstring ptr,@uChar2) )
    return 1
  end if  
end function
function Virt(_Left)  '(str,num) as str
  dim as ValStruct StrSrc=any,NumCnt=any
  dim as ParserStruct tParser2 : tParser2.pzBytecode = tParser.pzBytecode
  with tParser2
    if ParseParameter( tParser , StrSrc , vtString , pfFirstParm , @tParser2 )=0 then return 0        
    if ParseParameter( tParser , NumCnt , vtNumber , pfLastParm  , @tParser2 )=0 then return 0
    dim as long uCnt = int(NumCnt.fNumber)
    if uCnt < 0 then
      ShowErr(_FC_ERROR_)      
    else 'TODO: const optimization?
      if uCnt > _StrLen(StrSrc.ptStr) then uCnt = _StrLen(StrSrc.ptStr)
      tRetVal.iType = vtString      
      tRetVal.ptStr = AllocStr( uCnt , _StrPtr(StrSrc.ptStr) )
      function = 1
    end if
    CleanStacks()
    'DeleteStr( StrSrc.ptStr )
  end with
end function
function Virt(_Right) '(str,num) as str
  dim as ValStruct StrSrc=any,NumCnt=any
  dim as ParserStruct tParser2 : tParser2.pzBytecode = tParser.pzBytecode
  with tParser2    
    if ParseParameter( tParser , StrSrc , vtString , pfFirstParm , @tParser2 )=0 then return 0    
    if ParseParameter( tParser , NumCnt , vtNumber , pfLastParm  , @tParser2 )=0 then return 0    
    dim as long uCnt = int(NumCnt.fNumber)
    if uCnt < 0 then
      ShowErr(_FC_ERROR_)      
    else
      var uSz = _StrLen(StrSrc.ptStr)
      if uCnt > uSz then uCnt = uSz
      tRetVal.iType = vtString      
      tRetVal.ptStr = AllocStr( uCnt , _StrPtr(StrSrc.ptStr)+(uSz-uCnt) )
      function = 1
    end if
    CleanStacks()
    'DeleteStr( StrSrc.ptStr )
  end with
end function
function Virt(_Mid)   '(str,num[,num]) as str
  dim as ValStruct StrSrc=any,NumBegin=any,NumCnt=any
  dim as ParserStruct tParser2 : tParser2.pzBytecode = tParser.pzBytecode
  with tParser2    
    'get text (1st parameter)
    if ParseParameter( tParser , StrSrc   , vtString , pfFirstParm , @tParser2 )=0 then return 0    
    'get begin (2nd parameter)
    var iNext = ParseParameter( tParser , NumBegin , vtNumber , 0  , @tParser2 )
    if iNext=0 then return 0
    dim as long iBegin = int(NumBegin.fNumber)
    dim as long iCnt = cStringLimit
    'if theres a 3rd parameter...
    if iNext=asc(",") then 
      'get count (3rd parameter)
      if ParseParameter( tParser , NumCnt , vtNumber , pfLastParm  , @tParser2 )=0 then return 0
      iCnt = int(NumCnt.fNumber)      
    end if
    if iCnt<0 orelse iBegin <= 0 then
      ShowErr(_FC_ERROR_)      
    else
      iBegin -= 1
      var iSz  = _StrLen(StrSrc.ptStr)-iBegin
      if iSz <= 0 then iCnt = 0      
      if iCnt > iSz then iCnt = iSz
      tRetVal.iType = vtString      
      tRetVal.ptStr = AllocStr( iCnt , _StrPtr(StrSrc.ptStr)+iBegin )
      function = 1
    end if
    CleanStacks()
    'DeleteStr( StrSrc.ptStr )
  end with
end function
function Virt(_Inkey) '() as str
  dim as string sKey = inkey()
  tRetVal.iType = vtString    
  tRetVal.ptStr = AllocStr( len(sKey) , strptr(sKey) )
  return 1
end function
function Virt(_Atn)   '(num) as num
  if ParseParameter( tParser , tRetVal , vtNumber ) then
    tRetVal.fNumber = atn(tRetVal.fNumber)    
    return 1
  end if
end function
function Virt(_Cos)   '(num) as num
  if ParseParameter( tParser , tRetVal , vtNumber ) then
    tRetVal.fNumber = cos(tRetVal.fNumber)    
    return 1
  end if
end function
function Virt(_Tan)   '(num) as num
  if ParseParameter( tParser , tRetVal , vtNumber ) then
    tRetVal.fNumber = tan(tRetVal.fNumber)    
    return 1
  end if
end function
function Virt(_Exp)   '(num) as num
  if ParseParameter( tParser , tRetVal , vtNumber ) then
    tRetVal.fNumber = Exp(tRetVal.fNumber)    
    return 1
  end if
end function
function Virt(_Fix)   '(num) as num
  if ParseParameter( tParser , tRetVal , vtNumber ) then
    tRetVal.fNumber = Fix(tRetVal.fNumber)    
    return 1
  end if
end function
function Virt(_Log)   '(num) as num
  if ParseParameter( tParser , tRetVal , vtNumber ) then
    tRetVal.fNumber = log(tRetVal.fNumber)    
    return 1
  end if
end function
function Virt(_Pos)   '(num) as num
  if ParseParameter( tParser , tRetVal , vtNumber ) then
    if int(tRetVal.fNumber) then
      ShowErr(_NO_ERROR_)
      return 0
    end if
    tRetVal.fNumber = __pos()-1
    return 1
  end if
end function
function Virt(_Sqr)   '(num) as num
  if ParseParameter( tParser , tRetVal , vtNumber ) then
    tRetVal.fNumber = sqr(tRetVal.fNumber)    
    return 1
  end if
end function
function Virt(_Hex)   '(num) as str
  dim as ValStruct TempNum  
  if ParseParameter( tParser , TempNum , vtNumber ) then
    if TempNum.fNumber < 0 orelse fix(TempNum.fNumber) >= 65536 then
      ShowErr(_FC_ERROR_)
      return 0
    end if    
    dim as zstring*8 zTemp = any        
    var iLen = sprintf(zTemp,"%X",cint(int(TempNum.fNumber)))
    tRetVal.iType = vtString
    tRetVal.ptStr = AllocStr( iLen , zTemp )
    return 1
  end if  
end function
function Virt(_Instr) '([num,]str,str) as num
  dim as ValStruct StrSrc=any,StrChar=any
  dim as ParserStruct tParser2 : tParser2.pzBytecode = tParser.pzBytecode
  with tParser2    
    'get 1st parameter, is it string or start?
    if ParseParameter( tParser , StrSrc   , vtString or vtNumber , pfFirstParm , @tParser2 )=0 then return 0    
    dim as long iBegin = 1
    if StrSrc.iType = vtNumber then
      iBegin = int(StrSrc.fNumber)
      if iBegin < 1 then
        ShowErr(_FC_ERROR_)      
        return 0
      end if
      'if 1st parameter was integer then get the string now
      if ParseParameter( tParser , StrSrc   , vtString , 0 , @tParser2 )=0 then return 0
    end if    
    'get last string parameter
    if ParseParameter( tParser , StrChar , vtString , pfLastParm , @tParser2 )=0 then return 0        
    iBegin -= 1
    var iSz  = _StrLen(StrSrc.ptStr)-iBegin      
    tRetVal.iType = vtNumber
    if iSz <= 0 then 
      tRetVal.fNumber = 0            
    elseif _StrLen(StrChar.ptStr)=0 then
      tRetVal.fNumber = (iBegin+1)
    else
      var pStart = _StrPtr(StrSrc.ptStr)+iBegin
      var pFound = memchr( pStart , _StrPtr(StrChar.ptStr)[0] , iSz )
      tRetVal.fNumber = iif(pFound,iBegin+1+(cuint(pFound)-cuint(pStart)),0)
    end if
    function = 1
    CleanStacks()
  end with
end function
function Virt(_String)'(num,[str/num]) as str
  dim as ValStruct NumCnt=any,RepChr=any
  dim as ParserStruct tParser2 : tParser2.pzBytecode = tParser.pzBytecode
  with tParser2    
    if ParseParameter( tParser , NumCnt , vtNumber             , pfFirstParm , @tParser2 )=0 then return 0    
    dim as long uCnt = int(NumCnt.fNumber)
    if cuint(uCnt)>cStringLimit then
      ShowErr(_FC_ERROR_)  
      CleanStacks(): return 0
    end if
    if ParseParameter( tParser , RepChr , vtNumber or vtString , pfLastParm  , @tParser2 )=0 then return 0    
    dim as long uChar = 0
    if RepChr.iType = vtString then
      if _StrLen(RepChr.ptStr)=0 then 
        ShowErr(_FC_ERROR_)  
        CleanStacks(): return 0
      else
        uChar = _StrPtr(RepChr.ptStr)[0]
      end if
    else
      uChar = int(RepChr.fNumber)
      if cuint(uChar)>255 then
        ShowErr(_FC_ERROR_)  
        CleanStacks(): return 0
      end if
    end if
    tRetVal.iType = vtString      
    tRetVal.ptStr = AllocStr( uCnt , NULL )
    memset( _StrPtr(tRetVal.ptStr) , uChar , uCnt )
    CleanStacks()    
    return 1
  end with
end function
function Virt(_Timer) '() as num
  var dTime = timer
  while (dTime-g_StartTimer) >= (cMaxUnsigned/60)
    g_StartTimer += (cMaxUnsigned/60)
  wend
  tRetVal.iType   = vtNumber
  tRetVal.fNumber = fix((dTime-g_StartTimer)*60)
  return 1
end function
function Virt(_Mem)   '() as num
  tRetVal.iType   = vtNumber
  tRetVal.fNumber = g_MemLeft
  return 1
end function
#undef Virt



