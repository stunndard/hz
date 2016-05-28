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

unit log;
{$MODE DELPHI}
{$H+}

interface

uses SysUtils, Classes,
     config;

const
  LOG_ERROR = -1;
  LOG_INFO  =  0;
  LOG_DEBUG =  1;

procedure hz_log(s: string; level: integer);
procedure hz_log_term(s: string; level: integer);
procedure hz_logln_term(s: string; level: integer);
procedure hz_log_both(s: string; level: integer);

implementation

{$IFDEF WINDOWS}
function CharToOem(SrsString, DstString: pchar): LongBool; stdcall;
  external 'user32.dll' name 'CharToOemA';
{$ENDIF}

procedure hz_log(s: string; level: integer);
var
  fs: TFileStream;
  time: string;
  lvl: string;
  ss: string;
begin
  if level > Opts.loglevel then exit;
  if not FileExists(Opts.logfile) then
    fs := TFileStream.Create(Opts.logfile, fmCreate)
  else begin
    fs := TFileStream.Create(Opts.logfile, fmOpenWrite or fmShareDenyWrite);
    fs.Seek(0, soFromEnd);
  end;
  DateTimeToString(time, '[dd/mm/yyyy hh:nn:ss]', Now);
  case level of
    -1: lvl := 'ERROR';
     0: lvl := 'INFO ';
     1: lvl := 'DEBUG';
  end;
  ss := time + ' ' + lvl + ' ' + s + crlf;
  fs.Write(ss[1], length(ss));
  fs.Free;
end;

procedure hz_log_term(s: string; level: integer);
var
  z: string;
begin
  if (level > Opts.loglevel) then exit;
  // write to terminal
{$IFDEF WINDOWS}
  z := space(Length(s));
  CharToOem(pchar(s), pchar(z));
{$ELSE}
  z := s;
{$ENDIF}
  write(#13 + space(79) + #13 + z);
end;

procedure hz_logln_term(s: string; level: integer);
var
  z: string;
begin
  if (level > Opts.loglevel) then exit;
  // write to terminal
{$IFDEF WINDOWS}
  z := space(Length(s));
  CharToOem(pchar(s), pchar(z));
{$ELSE}
  z := s;
{$ENDIF}
  writeln(#13 + space(79) + #13 + z);
end;

procedure hz_log_both(s: string; level: integer);
begin
  hz_logln_term(s, level);
  hz_log(s, level);
end;

end.
