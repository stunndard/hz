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

{$INCLUDE CONFIG.INC}

unit script;
{$MODE DELPHI}
{$H+}

interface

uses SysUtils, Classes,
     config, log {$IFDEF USE_LUA} , luapas {$ENDIF};

function script_init: integer;
function script_playlist_init: integer;
procedure script_shutdown;
function script_playlist_next: string;
function script_formatmetadata(Artist, Title: string): string;
function script_onmetadata(metadata: string): string;

implementation

{$IFDEF USE_LUA}
var
  // lua vars
  L: Plua_State;
{$ENDIF}

function script_init: integer;
begin
  Result := 0;
{$IFDEF USE_LUA}
  Result := -1;
  if not FileExists(Opts.scriptfile) then
  begin
    hz_log_both('Script ' + Opts.scriptfile + ' doesn''t exist', LOG_ERROR);
    exit;
  end;

  hz_log_both('Initializing Lua', LOG_DEBUG);
  L := lua_open;
  luaL_openlibs(L);

  // set global lua vars
  lua_pushstring(L, pchar(Opts.Server));
  lua_setglobal(L, 'hzServerType');
  lua_pushstring(L, pchar(Opts.host));
  lua_setglobal(L, 'hzServerHost');
  lua_pushinteger(L, Opts.port);
  lua_setglobal(L, 'hzServerPort');
  lua_pushstring(L, pchar(Opts.mount));
  lua_setglobal(L, 'hzServerMount');
  lua_pushstring(L, pchar(Opts.password));
  lua_setglobal(L, 'hzServerPassword');

  lua_pushstring(L, pchar(Opts.name));
  lua_setglobal(L, 'hzStreamName');
  lua_pushstring(L, pchar(Opts.descr));
  lua_setglobal(L, 'hzStreamDescription');
  lua_pushstring(L, pchar(Opts.url));
  lua_setglobal(L, 'hzStreamURL');
  lua_pushstring(L, pchar(Opts.genre));
  lua_setglobal(L, 'hzStreamGenre');
  lua_pushinteger(L, Opts.IsPublic);
  lua_setglobal(L, 'hzStreamPublic');

  lua_pushstring(L, pchar(Opts.Playlist));
  lua_setglobal(L, 'hzPlaylist');
  lua_pushinteger(L, Opts.PlayRandom);
  lua_setglobal(L, 'hzPlaylistRandom');
  lua_pushinteger(L, Opts.UpdateMetadata);
  lua_setglobal(L, 'hzPlaylistUpdatemetadata');

  // execute script
  if luaL_dofile(L, pchar(Opts.scriptfile)) <> 0 then
  begin
    hz_log_both(lua_tostring(L, -1), LOG_ERROR);
    exit;
  end;
  Result := script_playlist_init;
{$ENDIF}
end;


function script_playlist_init: integer;
begin
  Result := 0;
{$IFDEF USE_LUA}
  Result := -1;
      hz_log_both('Calling hz_init() in lua', LOG_DEBUG);
      lua_getglobal(L, 'hz_init');
      // 0 arguments, 1 result
      if lua_pcall(L, 0, 1, 0) <> 0 then
      begin
        hz_log_both('Error calling function hz_init()', LOG_ERROR);
        hz_log_both(lua_tostring(L, -1), LOG_ERROR);
        //luaL_error(L, lua_tostring(L, -1));
        exit;
      end;
      if not lua_isnumber(L, -1) then
      begin
        hz_log_both('Function hz_init() should return a number', LOG_ERROR);
        exit;
      end;
      Result := lua_tointeger(L, -1);
      hz_log_both('hz_init()= ' + IntToStr(Result), LOG_DEBUG);
      lua_pop(L, 1);
{$ENDIF};
end;

procedure script_shutdown;
begin
{$IFDEF USE_LUA}
    hz_log_both('Calling hz_shutdown() in lua', LOG_DEBUG);
    lua_getglobal(L, 'hz_shutdown');
    // 0 arguments, 0 results
    if lua_pcall(L, 0, 0, 0) <> 0 then
    begin
      hz_log_both('Error calling function hz_shutdown()', LOG_ERROR);
      hz_log_both(lua_tostring(L, -1), LOG_ERROR);
      //luaL_error(L, lua_tostring(L, -1));
      //halt;
    end;
    //lua_pop(L, 1);
    hz_log_both('Shutting down Lua', LOG_DEBUG);
    lua_close(L);
{$ENDIF}
end;

function script_playlist_next: string;
begin
  Result := '';
{$IFDEF USE_LUA}
    hz_log_both('Calling hz_nextfile() in lua', LOG_DEBUG);
    lua_getglobal(L, 'hz_nextfile');
    // 0 arguments, 1 result
    if lua_pcall(L, 0, 1, 0) <> 0 then
    begin
      hz_log_both('Error calling function hz_nextfile()', LOG_ERROR);
      hz_log_both(lua_tostring(L, -1), LOG_ERROR);
      //luaL_error(L, lua_tostring(L, -1));
      halt;
    end;
    Result := lua_tostring(L, -1);
    hz_log_both('hz_nexftile()= ' + Result, LOG_DEBUG);
    lua_pop(L, 1);
{$ENDIF}
end;

function script_onmetadata(metadata: string): string;
begin
  Result := metadata;
{$IFDEF USE_LUA}
  hz_log_both('Calling hz_onmetadata() in lua', LOG_DEBUG);
  lua_getglobal(L, 'hz_onmetadata');
  // 1 argument, 1 result
  lua_pushstring(L, pchar(metadata));
  if lua_pcall(L, 1, 1, 0) <> 0 then
  begin
    hz_log_both('Error calling function hz_onmetadata()', LOG_ERROR);
    hz_log_both(lua_tostring(L, -1), LOG_ERROR);
    //luaL_error(L, lua_tostring(L, -1));
  end
  else begin
    Result := lua_tostring(L, -1);
    hz_log_both('hz_onmetadata()= ' + Result, LOG_DEBUG);
    lua_pop(L, 1);
  end;
{$ENDIF}
end;

function script_formatmetadata(Artist, Title: string): string;
begin
  Result := '';
{$IFDEF USE_LUA}
  hz_log_both('Calling hz_formatmetadata() in lua', LOG_DEBUG);
  lua_getglobal(L, 'hz_formatmetadata');
  // 2 arguments, 1 result
  lua_pushstring(L, pchar(Artist));
  lua_pushstring(L, pchar(Title));
  if lua_pcall(L, 2, 1, 0) <> 0 then
  begin
    hz_log_both('Error calling function hz_formatmetadata()', LOG_ERROR);
    hz_log_both(lua_tostring(L, -1), LOG_ERROR);
    //luaL_error(L, lua_tostring(L, -1));
  end
  else begin
    Result := lua_tostring(L, -1);
    hz_log_both('hz_formatmetadata()= ' + Result, LOG_DEBUG);
    lua_pop(L, 1);
  end;
{$ENDIF}
end;

end.
