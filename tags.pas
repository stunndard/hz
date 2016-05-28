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

unit tags;
{$MODE DELPHI}
{$H+}

interface

uses SysUtils, Classes,
     log;

function tag_get_id3v1(filename: string; var Artist, Title: string): string;
function tag_get_id3v2(filename: string; var Artist, Title: string): string;

implementation

function tag_get_id3v1(filename: string; var Artist, Title: string): string;
var
  fs: TFileStream;
  id3v1: array[0..127] of char;
  i: integer;
begin
  Result := '';
  if not FileExists(filename) then exit;
  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    if fs.Size < sizeof(id3v1) then exit;
    fs.Seek(-sizeof(id3v1), soFromEnd);
    if fs.Read(id3v1[0], sizeof(id3v1)) < sizeof(id3v1) then exit;
  finally
    fs.Free;
  end;

  // check for 'TAG' at the begin of the id3v1
  if (id3v1[0] <> 'T') or (id3v1[1] <> 'A') or (id3v1[2] <> 'G') then exit;

  Artist := '';
  Title  := '';

  for i := 1 to 30 do
  begin
    if id3v1[i + 2] <> #0 then
      Title := Title + id3v1[i + 2];
    if id3v1[i + 32] <> #0 then
      Artist := Artist + id3v1[i + 32];
  end;

  Title := Trim(Title);
  Artist := Trim(Artist);

  (*hz_log_term(#13'                                                   '#13, LOG_INFO);
  hz_log_both('ID3v1 tag found', LOG_DEBUG);
  hz_log_both('Artist : ' + Artist, LOG_INFO);
  hz_log_both('Title  : ' + Title, LOG_INFO);
    *)
  Result := Artist + ' - ' + Title;
end;


{
***
 This function is a quick hack. I'm too lazy to parse ID3V2 frames.
***
 }
function tag_get_id3v2(filename: string; var Artist, Title: string): string;
var
  fs: TFileStream;
  id3v2: array[0..49999] of char;
  //Artist, Title: string;
  size: integer;

function get_id3v2_frame(framename: string): string;
var
  i, j: integer;
  framesize: dword;
  frame: array[0..3] of char;
begin
  Result := '';

  for i := 0 to (size - 1 - Length(framename)) do
  begin
    for j := 0 to 3 do
    begin
      frame[j] := id3v2[i + j];
    end;

    if frame = framename then
    begin
      //writeln('frame ', framename);
      //framesize := (pdword(@id3v2[i + 4]))^;
      pbyte((@framesize))^     := byte(id3v2[i + 7]);
      pbyte((@framesize) + 1)^ := byte(id3v2[i + 6]);
      pbyte((@framesize) + 2)^ := byte(id3v2[i + 5]);
      pbyte((@framesize) + 3)^ := byte(id3v2[i + 4]);

      //writeln('framesize ', framesize);

      if id3v2[i + 10] = #0 then
      begin
        for j := 0 to (framesize - 2) do
          Result := Result + id3v2[i + 11 + j];
      end
      else
        ; // damn, unicode
      exit;
    end;
  end;

end;

begin
  Result := '';
  if not FileExists(filename) then exit;
  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    size := fs.Read(id3v2[0], sizeof(id3v2));
    // :)
    if size < 1024 then exit;
  finally
    fs.Free;
  end;

  // check for 'ID3' at the begin of the id3v2
  if (id3v2[0] <> 'I') or (id3v2[1] <> 'D') or (id3v2[2] <> '3') then exit;

  Artist := Trim(get_id3v2_frame('TPE1'));
  Title  := Trim(get_id3v2_frame('TIT2'));

  (*
  hz_log_both('ID3v2 tag found', LOG_DEBUG);
  hz_log_both('Artist : ' + Artist, LOG_INFO);
  hz_log_both('Title  : ' + Title, LOG_INFO);
  *)
  Result := Artist + ' - ' + Title;

end;

end.
