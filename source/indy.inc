{$IF CompilerVersion > 21}
  {$UNDEF INDY9}
  {$DEFINE INDY10}
{$ELSE}
  {$UNDEF INDY10}
  {$DEFINE INDY9}
{$IFEND}

{ By default it assumes Indy10 for DelphiXE and above 
  and INDY9 for lower versions, if you have different 
  INDY version installed that the default built in, 
  override the version putting a DEFINE below ... }

{$IFDEF MSWINDOWS}
  {$DEFINE WIN32}
  {$DEFINE ACTIVEX}
{$ENDIF}
