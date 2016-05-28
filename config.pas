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

unit config;
{$MODE DELPHI}
{$H+}

interface

uses SysUtils, IniFiles;

const
  version = '0.61';
  crlf = #13#10;

type
  TOptions = record
    streamtype:     string;
    Server:         string;
    host:           string;
    port:           integer;
    mount:          string;
    connattempts:   integer;
    password:       string;
    buffersize:     integer;
    playlist:       string;
    PlaylistType:   string;
    npfile:         string;
    logfile:        string;
    scriptfile:     string;
    loglevel:       integer;
    PlayRandom:     integer;
    UpdateMetadata: integer;
    //
    name:           string;
    descr:          string;
    url:            string;
    genre:          string;
    IsPublic:       integer;
    IsDaemon:       integer;
  end;

procedure loadconfig(ConfigFile: string);

var
  Opts: TOptions;

implementation

procedure loadconfig(ConfigFile: string);
const
  s_server = 'server';
  s_stream = 'stream';
  s_playlist = 'playlist';
  s_misc =   'misc';
var
  cf: TIniFile;
begin
  cf := TIniFile.Create(ConfigFile);
  Opts.Server := LowerCase(cf.ReadString(s_server, 'server', 'icecast'));
  Opts.host := cf.ReadString(s_server, 'host', '127.0.0.1');
  Opts.port := cf.ReadInteger(s_server, 'port', 8000);
  Opts.mount := cf.ReadString(s_server, 'mount', 'hz');
  Opts.password := cf.ReadString(s_server, 'password', 'hackme');
  Opts.connattempts := cf.ReadInteger(s_server, 'connectionattempts', 5);

  //
  Opts.streamtype := cf.ReadString(s_stream, 'streamtype', 'aac');
  Opts.name := cf.ReadString(s_stream, 'name', 'hz radio station');
  Opts.descr := cf.ReadString(s_stream, 'description', 'check out our cranky tunes!');
  Opts.url := cf.ReadString(s_stream, 'url', 'http://radio.hz');
  Opts.genre := cf.ReadString(s_stream, 'genre', 'a vot hz');
  Opts.IsPublic := cf.ReadInteger(s_stream, 'public', 0);
  //
  Opts.PlaylistType := LowerCase(cf.ReadString(s_playlist, 'playlisttype', 'internal'));
  Opts.playlist := cf.ReadString(s_playlist, 'playlist', './playlist.txt');
  Opts.PlayRandom := cf.ReadInteger(s_playlist, 'playrandom', 0);
  //
  Opts.buffersize := round(cf.ReadFloat(s_misc, 'buffersize', 3) * 1000);
  Opts.UpdateMetadata := cf.ReadInteger(s_misc, 'updatemetadata', 1);

  Opts.scriptfile := cf.ReadString(s_misc, 'script', 'script.lua');
  Opts.npfile := cf.ReadString(s_misc, 'npfile', '/tmp/hznp.tmp');
  Opts.logfile := cf.ReadString(s_misc, 'logfile', '/tmp/hz.log');
  Opts.loglevel := cf.ReadInteger(s_misc, 'loglevel', 0);
  Opts.IsDaemon := cf.ReadInteger(s_misc, 'daemon', 0);

  //
  cf.Free;
end;

end.
