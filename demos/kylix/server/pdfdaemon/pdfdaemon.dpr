
{*******************************************************}
{                                                       }
{ XML-RPC Library for Delphi, Kylix and DWPL (DXmlRpc)  }
{ Demo project                                          }
{                                                       }
{ for Delphi 6, 7                                       }
{ Release 2.0.0                                         }
{ Copyright (c) 2001-2003 by Team-DelphiXml-Rpc         }
{ e-mail: team-dxmlrpc@dwp42.org                        }
{ www: http://sourceforge.net/projects/delphixml-rpc/   }
{                                                       }
{ The initial developer of the code is                  }
{   Clifford E. Baeseman, codepunk@codepunk.com         }
{                                                       }
{ This file may be distributed and/or modified under    }
{ the terms of the GNU Lesser General Public License    }
{ (LGPL) version 2.1 as published by the Free Software  }
{ Foundation and appearing in the included file         }
{ license.txt.                                          }
{                                                       }
{*******************************************************}
{
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/dxmlrpc/demos/kylix/server/pdfdaemon/pdfdaemon.dpr,v 1.1.1.1 2003-12-03 22:37:18 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
program PdfDaemon;

{$APPTYPE CONSOLE}

uses
{$IFDEF LINUX}
  Libc,
{$ENDIF}
  SysUtils,
  rpcserver in 'rpcserver.pas';

var
  { vars for daemonizing }
  bHup, bTerm: Boolean;
  fLog: Text;
  logname: string;
{$IFDEF LINUX}
  aOld, aTerm, aHup: PSigAction; //was pSigActionRec
  ps1: psigset;
  sSet: cardinal;
  pid: longint;
  secs: longint;
{$ENDIF}
  FServer: TXmlRpcServer;

{ handle SIGHUP & SIGTERM }
procedure DoSig(sig: longint); cdecl;
begin
{$IFDEF LINUX}
  case sig of
    SIGHUP: bHup := True;
    SIGTERM: bTerm := True;
  end;
{$ENDIF}
end;

{ open the log file }
procedure NewLog;
begin
  Assignfile(fLog, logname);
  Rewrite(fLog);
  Writeln(flog, 'Indy TCP DaemonLog created at ' + FormatDateTime('dd/mm/yyyy',
    Now));
  Closefile(fLog);
end;

begin
  logname := '/var/log/pdfdaemon.log';

  { set global daemon booleans }
  bHup := True; { to open log file }
  bTerm := False;
{$IFDEF LINUX}
  secs := 10;
  { block all signals except -HUP & -TERM }
  sSet := $FFFFBFFE;
  ps1 := @sSet;
  sigprocmask(sig_block, ps1, nil);
  { setup the signal handlers }
  new(aOld);
  new(aHup);
  new(aTerm);
  aTerm^.__sigaction_handler := @DoSig;
  aTerm^.sa_flags := 0;
  aTerm^.sa_restorer := nil;
  aHup^.__sigaction_handler := @DoSig;
  aHup^.sa_flags := 0;
  aHup^.sa_restorer := nil;
  SigAction(SIGTERM, aTerm, aOld);
  SigAction(SIGHUP, aHup, aOld);
  { daemonize }
  pid := Fork;
  case pid of
    0:
      begin { we are in the child }
        Close(input); { close standard in }
        AssignFile(output, '/dev/null');
        ReWrite(output);
        AssignFile(ErrOutPut, '/dev/null');
        ReWrite(ErrOutPut);
      end;
    -1: secs := 0; { forking error, so run as non-daemon }
  else
    Halt; { successful fork, so parent dies }
  end; //case
{$ENDIF}
  { Create any objects before you go into the processing loop}
  FServer := TXmlRpcServer.Create;

  { begin processing loop }
  repeat
    if bHup then
    begin
      NewLog;
      bHup := False;
    end;
    {----------------------}
    { Do your daemon stuff }
    Append(flog);
    Writeln(flog, 'pdfdaemon started at ' +
      PChar(FormatDateTime('mm/dd/yyy hh:mm:ss', Now)));
    Close(fLog);
    { the following output goes to the bit bucket }
    Writeln('pdfdaemon activated at ' +
      PChar(FormatDateTime('mm/dd/yyy hh:mm:ss', Now)));
    {----------------------}
    if (bTerm) then
    begin
      FServer.Free;
      BREAK
    end
    else
    begin
{$IFDEF LINUX}
      { wait a while }
      __sleep(secs * 1000);
{$ENDIF}
    end;
  until bTerm;
end.

