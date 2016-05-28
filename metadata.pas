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

unit metadata;
{$MODE DELPHI}
{$H+}

interface

uses {$IFDEF LINUX}BaseUnix,{$ENDIF}SysUtils, Classes, net,
     tags, cuesheet, timing, config, b64, script, log;

procedure metadata_update(filename: string); overload;
procedure metadata_update(time: dword); overload;
function metadata_cueload(cuename: string): boolean;

implementation

var
  _filename: string;

function URLEncode(const S: string): string;
var
  Idx: Integer; // loops thru characters in string
begin
  Result := '';
  for Idx := 1 to Length(S) do
  begin
    if S[Idx] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.'] then
        Result := Result + S[Idx]
    else
      Result := Result + '%' + IntToHex(Ord(S[Idx]), 2);
  end;
end;

procedure log_metadata_update(Artist, Title: string);
begin
  if Artist = '' then Artist := 'N/A';
  if Title  = '' then Title  := 'N/A';
  //
  hz_log_both('Artist : ' + Artist, LOG_INFO);
  hz_log_both('Title  : ' + Title, LOG_INFO);
end;

function format_metadata(Artist, Title: string): string;
begin
  Result := script_formatmetadata(Artist, Title);
  if Result <> '' then exit;
  if Artist <> '' then
    Result := Artist + ' - ' + Title
  else
    Result := Title;
  if Result = '' then
    Result := Opts.name;
end;

function get_tags(filename: string): string;
var
  Artist, Title: string;
begin
  hz_log_both('Getting tags...', LOG_DEBUG);
  tag_get_id3v1(filename, Artist, Title);
  if (Artist <> '') or (Title <> '') then
  begin
     log_metadata_update(Artist, Title);
     Result := format_metadata(Artist, Title);
     exit;
  end;
  tag_get_id3v2(filename, Artist, Title);
  if (Artist <> '') or (Title <> '') then
  begin
     log_metadata_update(Artist, Title);
     Result := format_metadata(Artist, Title);
     exit;
  end;

  log_metadata_update(Artist, Title);
  // if there are no tags, then use the station name as a tag
  Result := format_metadata(Artist, Title);
end;

function get_tags_cue: string;
var
  Artist, Title: string;
begin
  cue_get_tags(Artist, Title);
  if (Artist <> '') or (Title <> '') then
  begin
     log_metadata_update(Artist, Title);
     Result := format_metadata(Artist, Title);
     exit;
  end;

  log_metadata_update(Artist, Title);
  // if there are no tags, then use the station name as a tag
  Result := format_metadata(Artist, Title);
end;

procedure Execute(p: pointer);
var
  Sock: integer;
  headers: string;
  metadata: string;
begin
  //writeln('entering execute');

  // get metadata from id3v1 or id3v2 tags
  if _filename <> '' then
    metadata := get_tags(_filename)
  // or from cuesheet
  else
    metadata := get_tags_cue;
  //writeln('metadata = ', metadata);
  metadata := script_onmetadata(metadata);
  hz_log_both('Metadata set to: ' + metadata, LOG_INFO);
  metadata := URLEncode(metadata);

  // connect to server and send the metadata
  if not net_connect(Opts.host, Opts.port, sock) then exit;
  if Opts.Server = 'shoutcast' then
    headers := 'GET /admin.cgi?pass=' + UrlEncode(Opts.password) +
      '&mode=updinfo&song=' + metadata + ' HTTP/1.0' + crlf +
      'User-Agent: (Mozilla Compatible)' +
      crlf + crlf
  else
    headers := 'GET /admin/metadata?mode=updinfo&mount=/' + Opts.mount +
      '&song=' + metadata + ' HTTP/1.0' + crlf +
      'User-Agent: hz/' + version + crlf +
      'Authorization: Basic ' + EncodeBase64('source:' + Opts.password) +
      crlf + crlf;
  net_send(Sock, headers[1], length(headers));
  pause(100);
  net_close(Sock);
  //writeln('leaving execute');
end;

procedure metadata_update(filename: string); overload;
{$IFDEF LINUX}
var
  t: TPid;
{$ENDIF}
begin
  _filename := filename;
  //writeln('entering metadata_update');
{$IFDEF LINUX}
  t := FpFork;
  if t = 0 then // new "thread"
  begin
    Execute(nil);
    FpExit(0); // die child here
  end;
{$ELSE}
  BeginThread(@Execute);
{$ENDIF}
  //writeln('leaving metadata_update');
end;

procedure metadata_update(time: dword); overload;
{$IFDEF LINUX}
var
  t: TPid;
{$ENDIF}
begin
  //writeln('entering metadata_update');
  if cue_isupdate(time) then
  begin
    _filename := '';
{$IFDEF LINUX}
    t := FpFork;
    if t = 0 then // new "thread"
    begin
      Execute(nil);
      FpExit(0); // die child here
    end;
{$ELSE}
    BeginThread(@Execute);
{$ENDIF}
  end;
  //writeln('leaving metadata_update');
end;

function metadata_cueload(cuename: string): boolean;
begin
  Result := cue_load(cuename);
end;

end.
