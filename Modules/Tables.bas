const cMaxUnsigned = 65535
const cMaxLineNumber = 63999 , cMaxLineDigits = 5
const cMaxSigned = 32767
const cMaxStack = 255
const cTabSize = 16 'unused yet
const cMaxDrives = 4

#macro ForEachErrorNumber( _Act )
  _Act(NF, 0,  "NEXT WITHOUT FOR" )
  _Act(SN, 1,  "SYNTAX ERROR" )
  _Act(RG, 2,  "RETURN WITHOUT GOSUB" )
  _Act(OD, 3,  "OUT OF DATA" )
  _Act(FC, 4,  "ILLEGAL FUNCTION CALL" )
  _Act(OV, 5,  "OVERFLOW" )
  _Act(OM, 6,  "OUT OF MEMORY" )
  _Act(UL, 7,  "UNDEFINED LINE NUMBER" )
  _Act(BS, 8,  "BAD SUBSCRIPT" )
  _Act(DD, 9,  "REDIMENSIONED ARRAY" )
  _Act(/0, 10, "DIVISION BY ZERO" )
  _Act(ID, 11, "ILLEGAL DIRECT STATEMENT" )
  _Act(TM, 12, "TYPE MISMATCH" )
  _Act(OS, 13, "OUT OF STRING SPACE" )
  _Act(LS, 14, "STRING TOO LONG" )
  _Act(ST, 15, "STRING FORMULA TOO COMPLEX" )
  _Act(CN, 16, "CAN'T CONTINUE" )
  _Act(FD, 17, "BAD FILE DATA" )
  _Act(AO, 18, "FILE ALREADY OPEN" )
  _Act(DN, 19, "DEVICE NUMBER ERROR" )
  _Act(IO, 20, "I/O ERROR" )
  _Act(FM, 21, "BAD FILE MODE" )
  _Act(NO, 22, "FILE NOT OPEN" )
  _Act(IE, 23, "INPUT PAST END OF FILE" )
  _Act(DS, 24, "DIRECT STATEMENT IN FILE" )
  _Act(QQ, 25, "??")
  _Act(NE, 26, "FILE NOT FOUND" )  
  _Act(BR, 27, "Bad record number")
  _Act(OF, 28, "Disk full")
  _Act(OB, 29, "Out of buffer space")
  _Act(WP, 30, "Write-protected disk")
  _Act(FN, 31, "Bad filename")
  _Act(FS, 32, "Bad file structure")
  _Act(AE, 33, "File already exists")
  _Act(FO, 34, "Field overflow")
  _Act(SE, 35, "Set to non-fielded string")
  _Act(VF, 36, "Verification")
  _Act(ER, 37, "Write or input past end of record")
  _Act(ZZ, 38, "" )
#endmacro

#macro GenEnum( _Name , _Value , _Description )
  #undef #_Name  
  #if #_Name = "/0"
    _D0_ERROR_ = _Value
  #else        
    _##_Name##_ERROR_ = _Value
  #endif
#endmacro
enum ErrorNumber
  ForEachErrorNumber( GenEnum )
end enum
#undef GenEnum  

'Conflicting CRT symbols
#undef _Lower
'#undef _Greater
#undef _Hex
'should i change _Symbol to __Symbol?

'>>> TODO: Add DISK instructions/functions <<<

#macro ForEachToken( _Act )
'     Number    Name     Type    Function    
  _Act( 128	, "FOR"     , vtOther , _For    )
  _Act( 129	, "GO"      , vtOther , _Go     )
  _Act( 130	, "REM"     , vtOther , _Rem    )
  _Act( 131	, "'"       , vtOther , _Rem    )
  _Act( 132	, "ELSE"    , vtOther , _Else   )
  _Act( 133 , "IF"      , vtOther , _If     )
  _Act( 134	, "DATA"    , vtOther , _Data   )
  _Act( 135	, "?"       , vtOther , _Print  )
  _Act( 135	, "PRINT"   , vtOther , _Print  )
  _Act( 136	, "ON"      , vtOther , _On     )
  _Act( 137	, "INPUT"   , vtOther , _Input  )
  _Act( 138	, "END"     , vtOther , _End    )
  _Act( 139	, "NEXT"    , vtOther , _Next   )
  _Act( 140	, "DIM"     , vtOther , _Dim    )
  _Act( 141	, "READ"    , vtOther , _Read   )
  _Act( 142	, "RUN"     , vtOther , _Run    ) 
  _Act( 143	, "RESTORE" , vtOther , _Restore)
  _Act( 144	, "RETURN"  , vtOther , _Return )
  _Act( 145	, "STOP"    , vtOther , _Stop   )
  _Act( 146	, "POKE"    , vtSub   , _Poke   )
  _Act( 147	, "CONT"    , vtOther , _Cont   )
  _Act( 148	, "LIST"    , vtOther , _List   )
  _Act( 149	, "CLEAR"   , vtSub   , _Clear  )
  _Act( 150	, "NEW"     , vtOther , _New    )
  _Act( 151	, "CLOAD"   , vtOther , _CLoad  )
  _Act( 152	, "CSAVE"   , vtOther , _CSave  )
  _Act( 153	, "OPEN"    , vtOther , _Open   )
  _Act( 154	, "CLOSE"   , vtSub   , _Close  )
  _Act( 155	, "LLIST"   , vtOther , _LList  )
  _Act( 156	, "SET"     , vtSub   , _Set    )
  _Act( 157	, "RESET"   , vtSub   , _Reset  )
  _Act( 158	, "CLS"     , vtSub   , _Cls    )
  _Act( 159	, "MOTOR"   , vtOther , _Motor  )
  _Act( 160	, "SOUND"   , vtOther , _Sound  )
  _Act( 161	, "AUDIO"   , vtSub   , _Audio  )
  _Act( 162	, "EXEC"    , vtSub   , _Exec   )
  _Act( 163	, "SKIPF"   , vtOther , _SkipF  )
  _Act( 164	, "TAB("    , vtOther , _Tab    )
  _Act( 165	, "TO"      , vtOther , _To     ) 'Goto / To
  _Act( 166	, "SUB"     , vtOther , _Sub    ) 'Gosub
  _Act( 167	, "THEN"    , vtOther , _Then   )
  _Act( 168	, "NOT"     , vtOp    , _Not    )
  _Act( 169	, "STEP"    , vtOther , _Step   )
  _Act( 170	, "OFF"     , vtOther , _Off    )
  _Act( 171	, "+"       , vtOp    , _Plus   )
  _Act( 172	, "-"       , vtOp    , _Minus  )
  _Act( 173	, "*"       , vtOp    , _Mul    )
  _Act( 174	, "/"       , vtOp    , _Div    )
  _Act( 175	, "^"       , vtOp    , _Power  )
  _Act( 176	, "AND"     , vtOp    , _And    )
  _Act( 177	, "OR"      , vtOp    , _Or     )
  _Act( 178	, ">"       , vtOp    , _Greater)
  _Act( 179	, "="       , vtOp    , _Equal  )
  _Act( 180	, "<"       , vtOp    , _Lower  )
  _Act( 181	, "DEL"     , vtOther , _Del    )
  _Act( 182	, "EDIT"    , vtOther , _Edit   )
  _Act( 183	, "TRON"    , vtSub   , _TrOn   )
  _Act( 184	, "TROFF"   , vtSub   , _TrOff  )
  _Act( 185	, "DEF"     , vtOther , _Def    ) 'DEF FN
  _Act( 186	, "LET"     , vtOther , _Let    ) 'let 2nd token be first token xD
  _Act( 187	, "LINE"    , vtOther , _Line   )
  _Act( 188	, "PCLS"    , vtOther , _PCls   )
  _Act( 189	, "PSET"    , vtOther , _PSet   )
  _Act( 190	, "PRESET"  , vtOther , _PReset )
  _Act( 191	, "SCREEN"  , vtSub   , _Screen )
  _Act( 192	, "PCLEAR"  , vtSub   , _PClear )
  _Act( 193	, "COLOR"   , vtSub   , _Color  )
  _Act( 194	, "CIRCLE"  , vtOther , _Circle )
  _Act( 195	, "PAINT"   , vtOther , _Paint  )
  _Act( 196	, "GET"     , vtOther , _Get    )
  _Act( 197	, "PUT"     , vtOther , _Put    )
  _Act( 198	, "DRAW"    , vtSub   , _Draw   )
  _Act( 199	, "PCOPY"   , vtSub   , _PCopy  )
  _Act( 200	, "PMODE"   , vtSub   , _PMode  )
  _Act( 201	, "PLAY"    , vtSub   , _Play   )
  _Act( 202	, "DLOAD"   , vtSub   , _DLoad  )
  _Act( 203	, "RENUM"   , vtOther , _Renum  )
  _Act( 204	, "FN"      , vtOther , _Fn     ) 'DEF FN
  _Act( 205	, "USING"   , vtOther , __Using ) 'PRINT USING
  'Disk
  _Act( 206 , "DIR"     , vtSub   , _Dir    )
  _Act( 207 , "DRIVE"   , vtSub   , _Drive  )
  _Act( 208 , "FIELD"   , vtSub   , _Field  )
  _Act( 209 , "FILES"   , vtSub   , _Files  )
  _Act( 210 , "KILL"    , vtSub   , _Kill   )
  _Act( 211 , "LOAD"    , vtSub   , _Load   )
  _Act( 212 , "LSET"    , vtSub   , _LSet   )
  _Act( 213 , "MERGE"   , vtSub   , _Merge  )
  _Act( 214 , "RENAME"  , vtSub   , _Rename )
  _Act( 215 , "RSET"    , vtSub   , _RSet   )
  _Act( 216 , "SAVE"    , vtSub   , _Save   ) 
  _Act( 217 , "WRITE"   , vtSub   , _Write  )
  _Act( 218 , "VERIFY"  , vtSub   , _Verify )
  _Act( 219 , "UNLOAD"  , vtSub   , _Unload )
  _Act( 220 , "DSKINI"  , vtSub   , _DskIni )
  _Act( 221 , "BACKUP"  , vtSub   , _Backup )
  _Act( 222 , "COPY"    , vtSub   , _Copy   )
  _Act( 223 , "DSKI$"   , vtSub   , _DskI   )
  _Act( 224 , "DSKO$"   , vtSub   , _DskO   )
  _Act( 225 , "DOS"     , vtSub   , _Dos    )  
  'Super
  _Act( 226 , "WIDTH"   , vtSub   , _Width    )
  _Act( 227 , "PALETTE" , vtSub   , _Palette  )
  _Act( 228 , "HSCREEN" , vtSub   , _HScreen  )
  _Act( 229 , "LPOKE"   , vtSub   , _LPoke    )
  _Act( 230 , "HCLS"    , vtSub   , _HCls     )
  _Act( 231 , "HCOLOR"  , vtSub   , _HColor   )
  _Act( 232 , "HPAINT"  , vtSub   , _HPaint   )
  _Act( 233 , "HCIRCLE" , vtSub   , _HCircle  )
  _Act( 234 , "HLINE"   , vtSub   , _HLine    )
  _Act( 235 , "HGET"    , vtSub   , _HGet     )
  _Act( 236 , "HPUT"    , vtSub   , _HPut     )
  _Act( 237 , "HBUFF"   , vtSub   , _HBuff    )
  _Act( 238 , "HPRINT"  , vtSub   , _HPrint   )
  _Act( 239 , "ERR"     , vtOther , _EnableErr) 'ERR ON/OFF
  _Act( 240 , "BRK"     , vtOther , _EnableBrk) 'BRK ON/OFF
  _Act( 241 , "LOCATE"  , vtSub   , _Locate   )
  _Act( 242 , "HSTAT"   , vtSub   , _HStat    )
  _Act( 243 , "HSET"    , vtSub   , _HSet     )
  _Act( 244 , "HRESET"  , vtSub   , _HReset   )
  _Act( 245 , "HDRAW"   , vtSub   , _HDraw    )
  _Act( 246 , "CMP"     , vtSub   , _Composite)
  _ACT( 247 , "RGB"     , vtSub   , _RgbMode  )
  _ACT( 248 , "ATTR"    , vtSub   , _Attr     )
  'My Own
  _Act( 252 , "HELP"    , vtSub   , _Help     )
  _Act( 253 , "QSAVE"   , vtSub   , _QSave    )  
  _Act( 254 , "QLOAD"   , vtSub   , _QLoad    )  
  
#endmacro
#macro ForEachTokenFunc( _Act ) '255
'     Number    Name     Type    Function    
  _Act( 128 , "SGN"     , vtFunc  , _Sgn     ) 'OK
  _Act( 129 , "INT"     , vtFunc  , _Int     ) 'OK
  _Act( 130 , "ABS"     , vtFunc  , _Abs     ) 'OK
  _Act( 131 , "USR"     , vtFunc  , _Usr     ) 'TODO: DEF USR
  _Act( 132 , "RND"     , vtFunc  , _Rnd     ) 'OK
  _Act( 133 , "SIN"     , vtFunc  , _Sin     ) 'OK
  _Act( 134 , "PEEK"    , vtFunc  , _Peek    ) 'TODO: Memory handle
  _Act( 135 , "LEN"     , vtFunc  , _Len     ) 'OK
  _Act( 136 , "STR$"    , vtFunc  , _Str     ) 'OK
  _Act( 137 , "VAL"     , vtFunc  , _Val     ) 'OK
  _Act( 138 , "ASC"     , vtFunc  , _Asc     ) 'OK
  _Act( 139 , "CHR$"    , vtFunc  , _Chr     ) 'OK
  _Act( 140 , "EOF"     , vtFunc  , _Eof     ) 'TODO: File handling
  _Act( 141 , "JOYSTK"  , vtFunc  , _Joystk  ) 'TODO: Mouse or Joystick?
  _Act( 142 , "LEFT$"   , vtFunc  , _Left    ) 'OK
  _Act( 143 , "RIGHT$"  , vtFunc  , _Right   ) 'OK
  _Act( 144 , "MID$"    , vtFunc  , _Mid     ) 'OK
  _Act( 145 , "POINT"   , vtFunc  , _Point   ) 'TODO: SET
  _Act( 146 , "INKEY$"  , vtFunc0 , _Inkey   ) 'OK
  _Act( 147 , "MEM"     , vtFunc0 , _Mem     ) 'OK
  _Act( 148 , "ATN"     , vtFunc  , _Atn     ) 'OK
  _Act( 149 , "COS"     , vtFunc  , _Cos     ) 'OK
  _Act( 150 , "TAN"     , vtFunc  , _Tan     ) 'OK
  _Act( 151 , "EXP"     , vtFunc  , _Exp     ) 'OK
  _Act( 152 , "FIX"     , vtFunc  , _Fix     ) 'OK
  _Act( 153 , "LOG"     , vtFunc  , _Log     ) 'OK
  _Act( 154 , "POS"     , vtFunc  , _Pos     ) 'TODO: improve?
  _Act( 155 , "SQR"     , vtFunc  , _Sqr     ) 'OK
  _Act( 156 , "HEX$"    , vtFunc  , _Hex     ) 'OK
  _Act( 157 , "VARPTR"  , vtFunc  , _Varptr  ) 'TODO: Memory handle
  _Act( 158 , "INSTR"   , vtFunc  , _Instr   )
  _Act( 159 , "TIMER"   , vtFunc0 , _Timer   ) 'OK
  _Act( 160 , "PPOINT"  , vtFunc  , _PPoint  ) 'TODO: Gfx  
  _Act( 161 , "STRING$" , vtFunc  , _String  ) 'OK
  'Disk
  _Act( 162 , "CVN"     , vtFunc  , _Cvn     ) 
  _Act( 163 , "FREE"    , vtFunc  , _Free    )
  _Act( 164 , "LOC"     , vtFunc  , _Loc     )
  _Act( 165 , "LOF"     , vtFunc  , _Lof     )
  _Act( 166 , "MKN$"    , vtFunc  , _Mkn     )
  _Act( 167 , "FDB"     , vtFunc  , _Fdb     )
  'Super
  _Act( 168 , "LPEEK"   , vtFunc  , _LPeek   )
  _Act( 169 , "BUTTON"  , vtFunc  , _Button  )
  _Act( 170 , "HPOINT"  , vtFunc  , _HPoint  )
  _Act( 171 , "ERNO"    , vtFunc  , _ErrNum  )
  _ACt( 172 , "ERLIN"   , vtFunc  , _ErrLine )
#endmacro
#macro ForEachOperator( _Act )
'      Pri  EnumName    Char  Text  Unary  
  _Act(  2, otOr      , 177 , "OR" , 0 )
  _Act(  3, otAnd     , 176 , "AND", 0 )
  _Act(  4, otNot     , 168 , "NOT", 1 )
  _Act(  5, otLower   , 180 , "<"  , 0 )
  _Act(  5, otGreater , 178 , ">"  , 0 )
  _Act(  5, otEqu     , 179 , "="  , 0 )
  _Act(  5, otLoEqu   , 170 , ">=" , 0 )
  _Act(  5, otGtEqu   , 181 , "<=" , 0 )
  _Act(  5, otDiff    , 182 , "<>" , 0 )
  _Act( 10, otMinus   , 172 , "-"  , 0 )
  _Act( 10, otConcat  , 171 , "&"  , 0 )
  _Act( 10, otPlus    , 171 , "+"  , 0 )  
  _Act( 20, otDiv     , 174 , "/"  , 0 )
  _Act( 20, otMul     , 173 , "*"  , 0 )  
  _Act( 30, otNeg     , 169 , "-"  , 1 )
  _Act( 40, otPower   , 175 , "^"  , 0 )
#endmacro

#define DeclEnum( _priority , _enum , _char , _text , _unary ) _enum = _char
enum OperatorType
  ForEachOperator( DeclEnum )  
end enum
#undef DeclEnum
#macro DeclTokenEnum( _number , _name , _type , _Function )
  #if not defined(ti##_Function)    
    ti##_Function = _number
  #endif  
#endmacro
enum TokenId
  ForEachToken( DeclTokenEnum )
end enum
#undef DeclTokenEnum
#macro DeclTokenFuncEnum( _number , _name , _type , _Function )  
  #if not defined(tif##_Function)  
  tif##_Function = _number
  #endif    
#endmacro
enum TokenIdFunc
  ForEachTokenFunc( DeclTokenFuncEnum )
end enum
#undef DeclTokenFuncEnum

enum GetVarFlags
  gvName  = 0
  gvRead  = 1
  gvWrite = 2
end enum
  
enum ValType
  vtNone  
  vtNumber
  vtString  
  vtSub
  vtFunc
  vtFunc0
  vtOp
  vtOther
end enum
enum ScopeType
  stNone
  stFor  
  stReturn
end enum

type IntT as ushort
type numberT as single

type ProgLineT as ProgLine ptr
type ProgLine
  pPrev   as ProgLineT
  pNext   as ProgLineT
  wNumber as IntT
  wLen    as IntT  
end type

type stringT as StringStruct ptr
type StringStruct
  #ifdef UseMagic
  bMagic      as ubyte
  #endif
  bIsConst :1 as byte       'is it a const? (so data isnt right after the struct)
  bSize       as ubyte      'length of the string
  bTemp       as ubyte      'its stored on a temp string slot? (which if not 0)
  pzData      as ubyte ptr  'pointer to the data (usually point to after the string)
end type

type ArrayT as ArrayStruct ptr
type ArrayStruct      
  bDimLevel   as byte
  pwBounds    as IntT ptr  
  union    
    pwNumber as numberT ptr
    ptStr    as StringT ptr
    pRaw     as any ptr ptr
  end union
end type

type ValStruct
  wName    as ushort
  iType  :5 as ubyte 'ValType
  union
    bFlags as ubyte
    type
      bTemp  :1 as ubyte
      bByref :1 as ubyte
      bArray :1 as ubyte
    end type
  end union
  union
    fNumber as numberT
    ptStr   as stringT    
    Raw     as uinteger
    pptStr  as stringT ptr
    pfNum   as numberT ptr
    pRaw    as any ptr ptr
    ptArray as ArrayT
  end union
end type

type ForStruct
  wName as ushort  
  pfVar as NumberT ptr
  fEnd  as numberT
  fStep as numberT  
  pStartLine as ProgLineT
  pStartCode as ubyte ptr  
end type
type IfStruct
  iLevel as long
end type
type RetStruct
  pRetLine as ProgLineT
  pRetCode as ubyte ptr
end type  
type ScopeStruct
  iType as ubyte
  union
    tFor  as ForStruct
    tIf   as IfStruct
    tRet  as RetStruct
  end union
end type

type OperatorFlags
  bIsChar :1 as ubyte
  bUnary  :1 as ubyte  
  bFunc0  :1 as ubyte
end type
type ParserStruct  
  as ubyte ptr pzBytecode' = pzStart
  as long iOpenCount=0 , iLastToken=0
  as long iOpOff=0 , iValOff=0 , iScopeOff=0
  as long iError=0 , iIfCount=0  
  as RetStruct tCont
  as ubyte bOpStack(256)=any
  as ubyte bPriStack(256)=any  
  as ValStruct tValStack(256)=any
  as ScopeStruct tScopeStack(cMaxStack)=any  
end type

type TokenProc as function ( byref as ParserStruct ) as long
type TokenFunc as function ( byref as ParserStruct , byref tRetVal as ValStruct ) as long

static shared as ubyte g_ColorTable(...) = {0,10,14,9,12,15,11,5,6}

static shared as ubyte g_uPri(255)
static shared as OperatorFlags g_bOpFlags(255)
static shared as zstring*4 g_zOp(255)
static shared as zstring ptr pzToken(128 to 255), pzFuncTok(128 to 255)
static shared as zstring ptr pzErrMin(_ZZ_ERROR_) , pzErrDesc(_ZZ_ERROR_)
static shared as ubyte ImmediateBuffer(65535)
static shared pSubProc(255) as TokenProc , pFuncProc(255) as TokenFunc
static shared g_ptString(255) as stringT , g_iStringCnt as long
static shared as ubyte g_OutputCount=0
