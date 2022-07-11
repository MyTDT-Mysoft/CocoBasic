scope 'init
  
  #macro SetTokens( _num , _name , _type , _func ) 
    TokenArray( _num ) = @_name
    #ifdef Sub##_func
      pSubProc( _num ) = @Sub##_func
    #endif    
    #ifdef Func##_func
      #if _type=vtFunc0
        g_bOpFlags( _num ).bFunc0=1
      #endif
      pFuncProc( _num ) = @Func##_func
    #endif
  #endmacro
  #define TokenArray pzToken
  ForEachToken( SetTokens )
  #undef TokenArray
  #define TokenArray pzFuncTok
  ForEachTokenFunc( SetTokens )
  
  #macro SetPriority( _priority , _enum , _char , _text , _unary )
    #if len(_text)=1
      g_uPri( _char ) = _priority
    #endif
  #endmacro
  'ForEachOperator( SetPriority )
  #macro SetTables( _priority , _enum , _char , _text , _unary )    
    if *pzToken( _char ) = _text then g_bOpFlags( _char ).bIsChar = 1    
    #if _unary      
      g_bOpFlags( _char ).bUnary = 1
    #endif
    g_uPri( _char ) = _priority
    g_zOp( _char )  = _text
  #endmacro
  ForEachOperator( SetTables )
  
  #macro GenErrorTable( _Name , _Value , _Description )  
    #undef #_Name  
    #if #_Name = "/0"
      pzErrMin(_D0_ERROR_) = @#_Name
      pzErrDesc(_D0_ERROR_) = @_Description
    #else    
      pzErrMin(_##_Name##_ERROR_) = @#_Name
      pzErrDesc(_##_Name##_ERROR_) = @_Description
    #endif
  #endmacro
  ForEachErrorNumber( GenErrorTable )
  #undef GenErrorTable

  
end scope
