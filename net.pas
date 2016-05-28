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


unit net;
{$MODE DELPHI}
{$H+}

interface

uses SysUtils, sockets, resolve;

function net_connect(host: string; port: word; var sock: integer): boolean;
function net_send(sock: integer; const buf; length: integer): boolean;
function net_recv(sock: integer): string;
procedure net_close(sock: integer);

implementation

function net_connect(host: string; port: word; var sock: integer): boolean;
var
  SAddr: TSockAddr;
  //psaddr: psockaddr;
  s: integer;
  R: THostResolver;
begin
  Result := False;
  s := fpSocket(AF_INET, SOCK_STREAM, 0);
  if s = -1 then
  begin
    writeln('error socket= ', SocketError);
    exit;
  end;
  SAddr.sin_family := AF_INET;
  SAddr.sin_port := htons(port);
  SAddr.sin_addr := StrToNetAddr(host);
  if SAddr.sin_addr.s_bytes[4] = 0 then
  begin
    R := THostResolver.Create(nil);
    try
      if not R.NameLookup(host) then
      begin
        writeln('cannot resolve ', host);
        exit;
      end;
      SAddr.sin_addr := {$IFDEF LINUX}R.HostAddress{$ELSE}R.NetHostAddress{$ENDIF};
    finally
      R.Free;
    end;
  end;
  if fpConnect(s, @SAddr, sizeof(SAddr)) = -1 then
  begin
    //writeln('error connect');
    net_close(s);
    exit;
  end;
  sock := s;
  Result := True;
end;

function net_send(sock: integer; const buf; length: integer): boolean;
var
  p: pchar;
begin
  p := pchar(buf);
  if fpSend(sock, @buf, length, 0) = -1 then
  begin
    //writeln('error send');
    Result := False;
  end
  else
    Result := True;
end;

function net_recv(sock: integer): string;
var
  buf: array[0..4095] of char;
  received: integer;
  s: string;
  i: integer;
begin
  Result := '';
  received := fpRecv(sock, @buf[0], sizeof(buf), 0);
  if received = -1 then
  begin
    //writeln('recv error');
    exit;
  end;
  SetLength(s, received);
  for i := 0 to received - 1 do
    s[1 + i] := buf[i];
  //writeln('recv: ', s);
  Result := s;
end;

procedure net_close(sock: integer);
begin
  fpShutdown(sock, 2);
  CloseSocket(sock);
end;

end.

