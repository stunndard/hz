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

unit timing;
{$MODE DELPHI)
{$H+}

interface

uses Classes, SysUtils,
     {$IFDEF LINUX} BaseUnix, {$ENDIF} config;

procedure pause(ms: integer);

implementation

procedure pause(ms: integer);
{$IFDEF LINUX}
var
  req, rem: TimeSpec;
{$ENDIF}
begin
{$IFDEF LINUX}
  if ms >= 1000 then
  begin
    req.tv_sec := ms div 1000;
    req.tv_nsec := 1000 * 1000 * (ms mod 1000);
  end
  else begin
    req.tv_sec := 0;
    req.tv_nsec := 1000 * 1000 * ms;
  end;
  fpNanoSleep(@req, @rem);
{$ELSE}
  Sleep(ms);
{$ENDIF}
end;

end.
