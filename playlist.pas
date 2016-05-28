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

unit playlist;
{$MODE DELPHI)
{$H+}

interface

uses Classes, SysUtils,
     config, script, log;

function load_playlist: integer;
function get_next_file: string;
//procedure unload_playlist;

var
  idx: integer = 0;
  np: string;

implementation

var
  pl: TStringList;

procedure save_idx;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(Opts.npfile, fmCreate);
  fs.WriteDWORD(idx);
  fs.Free;
end;

procedure load_idx;
var
  fs: TFileStream;
begin
  if not FileExists(Opts.npfile) then
  begin
    idx := 0;
    exit;
  end;
  fs := TFileStream.Create(Opts.npfile, fmOpenRead);
  idx := fs.ReadDWORD;
  fs.Free;
end;

function get_next_file: string;
{var
  s: string; }
begin
  if Opts.PlayListType = 'internal' then
  begin
    save_idx;
    // get_next_file := pl.Strings[idx];
    if idx > (pl.Count - 1) then idx := 0;
    np := pl.Strings[idx];
    load_playlist;
    if idx > (pl.Count - 1) then idx := 0;
    while (np = pl.Strings[idx]) do
    begin
      if Opts.PlayRandom = 0 then
      begin
        idx := idx + 1;
        if idx > (pl.Count - 1) then idx := 0;
      end
      else begin
        idx := Random(pl.Count);
      end;
    end;
    Result := pl.Strings[idx];
  end
  else begin
    Result := script_playlist_next;
    if Result = '' then
    begin
      hz_log_both('Error: hz_nextfile() in script returned an empty string', LOG_ERROR);
      hz_log_both('Check if hz was compiled with scripting support and your script returns correct data', LOG_ERROR);
    end
    else begin
      if not FileExists(Result) then
      begin
        hz_log_both('Error: hz_nextfile() returned a non-existing file name', LOG_ERROR);
        Result := '';
      end;
    end;
  end;
end;

function load_playlist: integer;
var
  i: integer;
begin
  Result := -1;
  if Opts.PlayListType = 'internal' then
  begin
    load_idx;
    if not FileExists(Opts.playlist) then exit;
    pl.Clear;
    pl.LoadFromFile(Opts.playlist);
    // check if all files in the playlist actually exist
    i := 0;
    while i < pl.Count do
    begin
      if not FileExists(pl.Strings[i]) then
      begin
        pl.Delete(i);
        continue;
      end;
      i := i + 1;
    end;
    if pl.Count < 1 then
    begin
      hz_log_both('Error: all files in the playlist do not exist', LOG_ERROR);
      exit;
    end;
    Result := 0;
  end
  else begin
    Result := 0; //script_playlist_init;
  end;
end;

initialization
  pl := TStringList.Create

finalization
  pl.Free;
end.
