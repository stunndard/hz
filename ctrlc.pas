{*
 *  hz - AAC, AACplus, AACplusV2 Icecast source client
 *  Copyright (C) 2006 Roman Butusov <reaxis at mail dot ru>
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public
 *  License along with this library; if not, write to the Free
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *}

unit CtrlC;
{$MODE DELPHI}
{$H+}

interface

uses SysUtils {$IFDEF LINUX}, BaseUnix {$ENDIF};

procedure SetCtrlCHandler;
function IsAborted: boolean;
procedure AbortNow;

implementation

var
  Aborted: boolean;

{$IFDEF LINUX}
procedure CtrlHandler(sig: cint); cdecl;
begin
  //writeln('signal= ', sig);
  if sig = SIGINT then
    AbortNow;
end;

{$ELSE}
function SetConsoleCtrlHandler(HandlerRoutine: pointer; Add: LongBool): LongBool;
  stdcall; external 'kernel32.dll';

function CtrlHandler(CtrlType: DWORD): LongBool; stdcall;
const
  CTRL_C_EVENT = 0;
begin
  Result := False;
  if CtrlType = CTRL_C_EVENT then
  begin
    Aborted := True;
    Result := True;
  end;
end;
{$ENDIF}

procedure SetCtrlCHandler;
{$IFDEF LINUX}
var
  oa, na: SigActionRec;
{$ENDIF}
begin
{$IFDEF LINUX}
  na.sa_handler := SigActionHandler(@CtrlHandler);
  FillChar(na.sa_mask, sizeof(na.sa_mask), #0);
  na.sa_flags := SA_ONESHOT;
  na.sa_restorer := nil;
  fpSigAction(SIGINT, @na, @oa);
{$ELSE}
  SetConsoleCtrlHandler(@CtrlHandler, True);
{$ENDIF}
end;

function IsAborted: boolean;
begin
  Result := Aborted;
end;

procedure AbortNow;
begin
  Aborted := True;
end;

end.
