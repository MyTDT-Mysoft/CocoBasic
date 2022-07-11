const cStringLimit = 255

type fbString
  pzData as zstring ptr
  iLen as integer
  iSize as integer
end type

#define _fbStrFromStr( _src ) type<fbString>( _StrPtr(_src) , _StrLen(_src) , _StrLen(_src) )
#define _fbStr( _src ) *cptr(string ptr,@_src)

#define _StrConst(_S) (_S)->bIsConst
#define _StrLen(_S) (_S)->bSize
#define _StrPtr(_S) (_S)->pzData
#define _StrText(_S) *cptr(zstring ptr,(_S)->pzData)

#ifndef StringStruct
  type StringStruct
    bMagic      as ubyte
    bIsConst :1 as byte       'is it a const? (so data isnt right after the struct)
    bSize       as ubyte      'length of the string
    bTemp       as ubyte      'its stored on a temp string slot? (which if not 0)
    pzData      as ubyte ptr  'pointer to the data (usually point to after the string)
  end type
#endif

static shared as StringStruct g_NullStr
g_NullStr.bIsConst=1
g_NullStr.bSize=0
#ifdef UseMagic
g_NullStr.bMagic=&hA5
#endif
g_NullStr.bTemp=0
g_NullStr.pzData = @g_NullStr.bSize

'#define DeleteStr(_parms...) _DeleteStr(" "& mid(__FILE__,instrrev(__FILE__,"\")+1) & ":" & __LINE__,_parms)
'sub _DeleteStr( sDebug as string , byref pTgt as stringT , bForce as byte = FALSE )
sub DeleteStr( byref pTgt2 as stringT , bForce as byte = FALSE )
  var pTgt = pTgt2
  if pTgt = NULL then exit sub
  if pTgt <> @g_NullStr then 
    #ifdef UseMagic
    if pTgt->bMagic <> &hA5 then
      printf("Invalid String: %p\n",pTgt)
      exit sub
    end if
    #endif
    if bForce orelse pTgt->bIsConst=0 then
      if pTgt->bTemp then        
        g_ptString(cint(pTgt->bTemp)) = 0
        pTgt->bTemp = 0
      end if      
      var iLen = sizeof(StringStruct)+1
      if cuint(pTgt->pzData) = cuint(pTgt+1) then iLen += strlen(cast(zstring ptr,pTgt+1))      
      memset(pTgt,&hCC,iLen) : g_MemoryUsage -= iLen
      'Printf(!"[%s]Del: '%s'\n",sDebug,pTgt->pzData)      
      #ifdef UseMagic
      pTgt->bMagic=0 
      #endif
      free(pTgt) : pTgt2 = 0
    else
      'puts("Skip freeing this!")
    end if
  end if
end sub
function AllocStr( iSize as long = -1 , pzData as zstring ptr = NULL , bConst as boolean = false ) as stringT
  if iSize < 0 then
    if pzData then iSize = strlen(pzData) else PUTS("!"):return NULL
  end if  
  if iSize > cStringLimit then PUTS("!"):return NULL
  if bConst andalso pzData=NULL then PUTS("!"):return NULL
  var iAlloc = sizeof(StringStruct)+iif(bConst,0,iSize+1)  
  dim as stringT ptResu = malloc(iAlloc)  
  g_MemoryUsage += iAlloc 
  g_bCleanUp=1
  if ptResu = NULL then PUTS("!"):return NULL    
  with *ptResu
    #ifdef UseMagic
    .bMagic = &hA5
    #endif
    if .bIsConst=0 then
      g_iStringCnt += 1 : g_ptString(g_iStringCnt) = ptResu
      .bTemp = g_iStringCnt
    else
      .bTemp = 0
    end if
    
    .bIsConst = bConst
    .bSize = iSize
    if bConst then
      .pzData = pzData
    else
      .pzData = cptr(ubyte ptr,ptResu+1)
      if pzData then memcpy(.pzData,pzData,iSize)
      .pzData[iSize]=0
    end if
  end with  
  return ptResu
end function
function ConcatStr( ptTgt as stringT , ptSrc as stringT ) as stringT  
  var iSzTgt = _StrLen(ptTgt) , iSize = iSzTgt+_StrLen(ptSrc), iMustKeep=0
  if iSize > cStringLimit then PUTS("?"):return 0
  dim as stringT ptResu=any
  
  g_MemoryUsage -= (iSzTgt+sizeof(StringStruct)+1)
  
  if ptTgt->bIsConst then
    ptResu = AllocStr(iSize+sizeof(StringStruct)+1)
    memcpy( ptResu+1 , ptTgt->pzData , ptTgt->bSize )    
  else    
    ptResu = realloc( ptTgt , iSize+sizeof(StringStruct)+1 )    
    iMustKeep=0 : g_bCleanUp=1
  end if
  if ptResu=NULL then PUTS("?"):return NULL
  
  g_MemoryUsage += iSize+sizeof(StringStruct)+1
  
  with *ptResu    
    if .bTemp then
      g_ptString(.bTemp) = ptResu
    elseif g_bCleanUp=0 then
      g_iStringCnt += 1 : g_ptString(g_iStringCnt) = ptResu
      .bTemp = g_iStringCnt      
    end if    
    .pzData = cptr(ubyte ptr,ptResu+1) : .bSize = iSize
    memcpy( .pzData+iSzTgt, _StrPtr(ptSrc) , _StrLen(ptSrc) )
    .pzData[iSize]=0
  end with
  'if ptSrc<>ptTgt orelse iMustKeep=0 then 
  DeleteStr(ptSrc)
  return ptResu
end function
