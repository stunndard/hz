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

unit aac;
{$MODE DELPHI}
{$H+}

interface

uses SysUtils, Classes,
     log;

function aac_GetFileInfo(filename: string; var br: real; var spf, sr, frames, ch: dword): integer;
function aac_GetFrames(fs: TFileStream; framestoread: dword; var sbuf: array of byte): dword;
function aac_SeekTo1stFrame(fs: TFileStream): integer;
function aac_IsValidFrameHeader(header: array of byte): boolean;

implementation

function aac_IsValidFrameHeader(header: array of byte): boolean;
var
  syncword: dword;
  profile: byte;
  sftable: array[0..15] of dword = (
    96000, 88200, 64000, 48000,
    44100, 32000, 24000, 22050,
    16000, 12000, 11025, 8000,
    7350,  0,     0,      0);
  sfindex: integer;
  frame_length: integer;
begin
  Result := False;

  // check for valid syncowrd
  syncword := (header[0] shl 4) or (header[1] shr 4);
  if syncword <> $0FFF then exit;

  // get and check the profile
  profile := (header[2] and $0C0) shr 6;
  if profile = 3 then exit;

  // get and check the 'sampling_frequency_index':
  sfindex := (header[2] and $03C) shr 2;
  if sftable[sfindex] = 0 then exit;

  frame_length := ((header[3] and $03) shl 11) or (header[4] shl 3)
    or ((header[5] and $0E0) shr 5);
  if (frame_length < 0) or (frame_length > 5000) then exit;

  //writeln('valid frame, len= ', frame_length);
  Result := True;
end;

function aac_SeekTo1stFrame(fs: TFileStream): integer;
var
  buf: array[0..49999] of byte;
  i, j, k: integer;
  aac_header: array[0..6] of byte;
begin
  // read 50000 bytes from the beginning and search for a
  // syncword (to skip ID3V2, for example)
  fs.Read(buf[0], sizeof(buf));
  j := -1;
  for i := 0 to sizeof(buf) - 1 do
  begin
    if (buf[i] = $0FF) and ((buf[i + 1] and $0F0) = $0F0) then
    begin
      if sizeof(buf) - i < 10 then break;
      for k := 0 to 6 do
        aac_header[k] := buf[i + k];
      if aac_IsValidFrameHeader(aac_header) then
      begin
        j := i;
        fs.Seek(j, soFromBeginning);
        break;
      end;
    end;
  end;
  Result := j;
end;


function aac_GetFrames(fs: TFileStream; FramesToRead: dword;
  var sbuf: array of byte): dword;
var
  headers: array[0..6] of byte;
  pos, BytesRead, numBytesToRead: integer;
  FramesRead: dword;
  FrameLength: integer;
  protection_absent: integer;
  i: integer;
  InSync: boolean = true;
begin
  pos := 0;
  FramesRead := 0;

  while(FramesRead < FramesToRead) do
  begin

      // begin by reading 7-byte fixed_variable_headers:
      BytesRead := fs.Read(headers[0], sizeof(headers));
      if BytesRead < sizeof(headers) then
      begin
        // the input file has ended
        //
        break;
      end;

      if not aac_IsValidFrameHeader(headers) then
      begin
        if InSync then
        begin
          hz_logln_term(#13'Bad AAC frame at offset ' +
            IntToStr(fs.Position - 7) + ', resyncing...                           ', LOG_DEBUG);
          hz_log('Bad AAC frame at offset ' +
            IntToStr(fs.Position - 7) + ', resyncing...', LOG_DEBUG);
        end;
        fs.Seek(-6, soFromCurrent);
        InSync := false;
        continue;
      end;

      //
      // from now on, frame is considered valid
      if not InSync then
      begin
        hz_logln_term('Resynced at offset ' + IntToStr(fs.Position - 7), LOG_DEBUG);
        hz_log('Resynced at offset ' + IntToStr(fs.Position - 7), LOG_DEBUG);
      end;
      InSync := true;

      // copy frame header to out buffer
      for i := 0 to 6 do
        sbuf[pos + i] := headers[i];
      pos := pos + 7;

      // extract important fields from aac headers:
      protection_absent := headers[1] and $01;
      FrameLength := ((headers[3] and $03) shl 11) or (headers[4] shl 3)
        or ((headers[5] and $0E0) shr 5);

      if FrameLength > sizeof(headers) then
        numBytesToRead := FrameLength - sizeof(headers)
      else
        numBytesToRead := 0;

      // if crc is present in this frame
      (*
      if protection_absent = 0 then
      begin
        fs.Seek(2, sofromCurrent);
        if numBytesToRead > 2 then
          numBytesToRead := numBytesToRead - 2
        else
          numBytesToRead := 0;
      end; *)

      // read raw frame data
      BytesRead := fs.Read(sbuf[pos], numBytesToRead);
      pos := pos + BytesRead;
      if BytesRead < numBytesToRead then
      begin
        // the input file has ended
        //
        break;
      end;
      FramesRead := FramesRead + 1;
  end; //while
  Result := pos;
end;


function aac_GetFileInfo(filename: string;
  var br: real; var spf, sr, frames, ch: dword): integer;
var
  fs: TFileStream;
  fixheader: array[0..3] of byte;
  headers: array[0..6] of byte;
  sftable: array[0..15] of dword = (
    96000, 88200, 64000, 48000,
    44100, 32000, 24000, 22050,
    16000, 12000, 11025, 8000,
    7350,  0,     0,      0);
  sfindex: integer;
  profile: byte;
  //syncword: dword;
  protection_absent: integer;
  //channel_configuration: integer;
  frame_length: integer;
  numBytesToRead: integer;
  frame: integer = 1;
  nsamples: dword;
  fsize: int64;
  playtime: real;
  //br: real;
  j: integer;
begin
  Result := -1;
  if not FileExists(filename) then exit;
  fs := TFileStream.Create(filename, fmOpenRead);
try
  hz_log_both('Checking ' + filename, LOG_DEBUG);

  j := aac_SeekTo1stFrame(fs);
  if j = -1 then
  begin
    hz_log_both('Couldn''t find AAC frame', LOG_ERROR);
    exit;
  end;
  hz_log_both('First frame found at offset: ' + IntToStr(j), LOG_DEBUG);

  // now having opened the input file, read the fixed header of the
  // first frame, to get the audio stream's parameters:
  if fs.Read(fixheader[0], sizeof(fixheader)) = sizeof(fixheader) then
  begin

    // check the 'syncword'
    if (fixheader[0] <> $0FF) and ((fixheader[1] and $0F0) <> $0F0) then
    begin
      hz_log_both('Bad "syncword" at frame # ' + IntToStr(frame), LOG_ERROR);
      exit;
    end;

    // get and check the profile
    profile := (fixheader[2] and $0C0) shr 6;
    if profile = 3 then
    begin
      hz_log_both('Bad (reserved) "profile":3 at frame # ' + IntToStr(frame), LOG_ERROR);
      exit;
    end;

    // get and check the 'sampling_frequency_index':
    sfindex := (fixheader[2] and $03C) shr 2;
    if sftable[sfindex] = 0 then
    begin
      hz_log_both('Bad "sampling_frequency_index" at frame # ' + IntToStr(frame), LOG_ERROR);
      exit;
    end;

    // get and check "channel configuration"
    ch := ((fixheader[2] and $01) shl 2) or
      ((fixheader[3] and $0C0) shr 6);

    fs.Seek(j, soFromBeginning);
    //////////

    while(true) do
    begin

      // begin by reading 7-byte fixed_variable_headers:
      if fs.Read(headers[0], sizeof(headers)) < sizeof(headers) then
      begin
        // the input file has ended
        //
        break;
      end;

      // extract important fields from var headers:
      protection_absent := headers[1] and $01;
      frame_length := ((headers[3] and $03) shl 11) or (headers[4] shl 3)
        or ((headers[5] and $0E0) shr 5);

      (*syncword := (headers[0] shl 4) or (headers[1] shr 4);
      //writeln('Read frame: frame ', frame,  ', syncword, ', syncword,
        //' protection absent ',
        //protection_absent, ' frame length ', frame_length);*)

      if not aac_IsValidFrameHeader(headers) then
      begin
        //writeln('Warning: Bad AAC frame at offset ', fs.Position-7);
        fs.Seek(-6, soFromCurrent);
        continue;
      end
      else
        frame := frame + 1;

      if frame_length > sizeof(headers) then
        numBytesToRead := frame_length - sizeof(headers)
      else
        numBytesToRead := 0;

      if protection_absent = 0 then
      begin
        fs.Seek(2, sofromCurrent);
        if numBytesToRead > 2 then
          numBytesToRead := numBytesToRead - 2
        else
          numBytesToRead := 0;
      end;

      //read or skip raw frame data
      fs.Seek(numBytesToRead, soFromCurrent);
    end;
  end;
  // close the file
  fsize := fs.Size;
finally
  fs.Free;
end;
  spf := 1024;
  sr := sftable[sfindex];
  frames := frame - 1;
  nsamples := 1024 * frames;
  playtime := nsamples / sr;
  // in bytes/sec
  br :=  fsize / playtime;
  //in kbits/sec
  br := br * 8 / 1000;

  hz_log_both('frames    : ' + IntToStr(frames), LOG_DEBUG);
  hz_log_both('samplerate: ' + IntToStr(sr) + ' Hz', LOG_DEBUG);
  hz_log_both('channels  : ' + IntToStr(ch), LOG_DEBUG);
  hz_log_both('playtime  : ' + IntToStr(Round(playtime)) + ' sec', LOG_DEBUG);
  hz_log_both('bitrate   : ' + IntToStr(Round(br)) + ' kbps (average)', LOG_DEBUG);
  Result := 0;
end;

end.

