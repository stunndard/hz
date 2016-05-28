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

unit cuesheet;
{$MODE DELPHI}
{$H+}

interface

uses SysUtils, Classes,
     config, log;

function cue_load(cuefile: string): boolean;
function cue_isupdate(time: dword): boolean;
function cue_get_tags(var Artist, Title: string): string;

implementation

type
  TCueEntry = record
    Title:    string;
    Artist:   string;
    Time:     dword;  // in milliseconds
  end;

var
  CueEntries: array of TCueEntry;
  idx: integer;

function get_value(s, arg: string): string;
begin
  s := Copy(s, Length(arg) + 2, Length(s));
  if s[1] = '"' then
    s := Copy(s, 2, Length(s) - 2);
  Result := s;
  //writeln(arg + ' = ' + s);
end;

function get_time(time: string): dword;
begin
  time := Copy(time, 1, length(time) - 3);
  Result := StrToInt(Copy(time, 1, Length(time) - 3)) * 60000 + StrToInt(Copy(time, Length(time) - 1, 2)) * 1000;
end;

function cue_get_tags(var Artist, Title: string): string;
begin
  Result := '';
  Artist := ''; Title := '';
  if idx > 0 then
  begin
    Artist := CueEntries[idx - 1].Artist;
    Title :=  CueEntries[idx - 1].Title;
    Result := Artist + ' - ' + Title;
  end;
end;

function cue_isupdate(time: dword): boolean;
begin
  //hz_log_both('cue_isupdate', LOG_DEBUG);
  //hz_log_both('idx= ' + IntToStr(idx) + ' len= ' + IntTostr(Length(CueEntries)), LOG_DEBUG);

  Result := False;

  if idx < Length(CueEntries) then
  begin
  if time >= CueEntries[idx].Time then
  begin
    //if idx < Length(CueEntries) then
    begin
      idx := idx + 1;
      Result := True;
    end;
  end;
  end;
  //hz_log_both('cue_isupdate2', LOG_DEBUG);

end;

function cue_load(cuefile: string): boolean;
var
  f: TextFile;
  s: string;
  //i: integer;
begin
  Result := False;
  idx := 0;
  Assign(f, cuefile);
  Reset(f);
  while not EOf(f) do
  begin
    ReadLn(f, s);
    s := Trim(s);
    //hz_logln_term('cue: ' + s, LOG_INFO);
    //readln;
    while (not EOF(f)) and (UpperCase(Copy(s, 1, 5)) = 'TRACK') do
    begin
      idx := idx + 1;
      SetLength(CueEntries, idx);
      Readln(f, s);
      s := Trim(s);
      while (*(not EOF(f)) and*) (UpperCase(Copy(s, 1, 5)) <> 'TRACK') do
      begin
        if UpperCase(Copy(s, 1, 5)) = 'TITLE' then
          CueEntries[idx - 1].Title := get_value(s, 'TITLE');
        if UpperCase(Copy(s, 1, 9)) = 'PERFORMER' then
          CueEntries[idx - 1].Artist := get_value(s, 'PERFORMER');
        if UpperCase(Copy(s, 1, 8)) = 'INDEX 01' then
          CueEntries[idx - 1].Time := get_time(get_value(s, 'INDEX 01'));

        if EOF(f) then break;

        Readln(f, s);
        s := Trim(s);
      end;
    end;
  end;
  Close(f);

  if idx > 0 then
    Result := True;

  idx := 0;

  (*
  for i := 0 to Length(CueEntries) - 1 do
  begin
    hz_logln_term(' Title' + IntToStr(i) + ' = ' + CueEntries[i].Title, LOG_INFO);
    hz_logln_term('Artist' + IntToStr(i) + ' = ' + CueEntries[i].Artist, LOG_INFO);
    hz_logln_term('  Time' + IntToStr(i) + ' = ' + IntToStr(CueEntries[i].Time) + ' ms', LOG_INFO);
  end;
  *)
end;

end.
