
dim shared as ProgLineT g_pProgBegin = NULL
dim shared as ProgLineT g_pCurrentLine = NULL
dim shared as ProgLineT g_pEditLine = NULL

sub ClearLines()
  var pLine = g_pProgBegin
  if pLine = NULL then exit sub
  do
    var pNext=pLine->pNext
    free(pLine)
    if pNext=NULL then exit do
    pLine=pNext
  loop
  g_pProgBegin = NULL
  g_pCurrentLine = NULL
end sub
function FindLine( wNumber as long ) as ProgLine ptr
  var pLine = g_pProgBegin
  if pLine=0 then return NULL
  while pLine
    if pLine->wNumber = wNumber then return pLine
    pLine = pLine->pNext
  wend
  return NULL
end function
function DelLine( wNumber as long , pLine as ProgLine ptr = null ) as byte
  if pLine = NULL then pLine = FindLine( wNumber )
  if pLine = NULL then return 0
  with *pLine
    if .pPrev then .pPrev->pNext = .pNext else g_pProgBegin = .pNext
    if .pNext then .pNext->pPrev = .pPrev
  end with
  free(pLine)
  return 1
end function  
function SetLine( wNumber as long , wLen as IntT , pBuffer as ubyte ptr , pLine as ProgLine ptr = null ) as ProgLine ptr
  if wLen<1 then return null
  if wLen<2 then DelLine( wNumber ) : return null
  
  'for N as long=0 to wLen-1
  '  print hex(pBuffer[N],2);
  'next N
  'print
  
  if pLine=0 then    
    'is the first line? yes, create it...
    pLine = g_pProgBegin
    if pLine=0 orelse pLine->wNumber > wNumber then
      'puts("first line...")
      pLine = malloc(sizeof(ProgLine)+wLen)
      with *pLine
        .pPrev   = NULL : .wNumber = wNumber : .wLen  = wLen
        if g_pProgBegin then
          .pNext = g_pProgBegin : .pNext->pPrev = pLine
        else
          .pNext = NULL 
        end if
        g_pProgBegin = pLine
      end with      
    end if
    
    'walk line by line to locate the line number    
    var pTemp = pLine
    do
      with *pLine
        if .wNumber = wNumber then
          if pTemp<>pLine then
            'puts("found existing line to replace")
          end if
          exit do
        end if
        'is the next line empty or above our designed line number
        if .pNext = NULL orelse .pNext->wNumber > wNumber then
          'puts("insert after...")
          dim pTemp as ProgLine ptr = malloc(sizeof(ProgLine)+wLen)          
          pTemp->pNext = .pNext
          with *pTemp
            .pPrev = pLine : .wNumber = wNumber : .wLen  = wLen
          end with
          if .pNext then .pNext->pPrev = pTemp
          .pNext = pTemp          
          pLine = pTemp : exit do
        end if
        pLine = .pNext
      end with
    loop
  end if
  
  'if length differs then reallocate
  if pLine->wLen <> wLen then
    dim pTemp as ProgLine ptr = realloc(pLine,sizeof(ProgLine)+wLen)
    if pTemp = NULL then
      puts("Failed to reallocate!")
      return pLine
    end if
    if pTemp <> pLine then
      with *pTemp
        if .pPrev then .pPrev->pNext = pTemp else g_pProgBegin = pTemp
        if .pNext then .pNext->pPrev = pTemp
      end with
      pLine = pTemp
    end if
    pLine->wLen = wLen
  end if
  pLine->wNumber = wNumber
  
  'set the line
  memcpy(pLine+1,pBuffer,wLen)
  return pLine
end function
  