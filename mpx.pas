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

unit mpx;
{$MODE DELPHI}
{$H+}

interface

uses SysUtils, Classes,
     log;

function mpx_GetFileInfo(filename: string; var br: real; var spf, sr, frames, ch: dword): integer;
function mpx_GetFrames(fs: TFileStream; framestoread: dword; var sbuf: array of byte): dword;
function mpx_SeekTo1stFrame(fs: TFileStream): integer;
function mpx_IsValidFrameHeader(headers: array of byte): boolean;

implementation

function mpx_IsValidFrameHeader(headers: array of byte): boolean;
var
  mpegver: byte;
  layer: byte;
  brindex, srindex: byte;
  frame_length: integer;
begin
  Result := False;

  // check the 'syncword'
  if (headers[0] <> $0FF) and ((headers[1] and $0E0) <> $0E0) then
  begin
    //hz_log_both('Bad "frame sync"' , LOG_ERROR);
    exit;
  end;

  // get and check the mpeg version
  mpegver := (headers[1] and $018) shr 3;
  if (mpegver = 1) or (mpegver > 3) then
  begin
  //    hz_log_both('Bad (reserved) mpeg version', LOG_ERROR);
      exit;
  end;

  // get and check mpeg layer
  layer := (headers[1] and $06) shr 1;
  if (layer = 0) or (layer > 3) then
  begin
   //hz_log_both('Bad (reserved) mpeg layer', LOG_ERROR);
    exit;
  end;

  // get and check bitreate index
  brindex := (headers[2] and $0F0) shr 4;
  if brindex > 15 then exit;

  // get and check the 'sampling_rate_index':
  srindex := (headers[2] and $0C) shr 2;
  if (srindex >= 3) then
  begin
    //hz_log_both('Bad "sampling_frequency_index"', LOG_ERROR);
    exit;
  end;

  //writeln('valid frame, len= ', frame_length);
  Result := True;
end;

function mpx_GetFrameSize(headers: array of byte): integer;
var
  mpegver: byte;
  layer: byte;
  brindex, srindex: byte;
  bitrate: dword;
  brtable: array[0..79] of dword = (
    0, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448, 0,
    0, 32, 48, 56, 64,  80,  96,  112, 128, 160, 192, 224, 256, 320, 384, 0,
    0, 32, 40, 48, 56,  64,  80,  96,  112, 128, 160, 192, 224, 256, 320, 0,
    0, 32, 48, 56, 64,  80,  96,  112, 128, 144, 160, 176, 192, 224, 256, 0,
    0, 8,  16, 24, 32,  40,  48,  56,  64,  80,  96,  112, 128, 144, 160, 0);
  srtable: array[0..11] of dword = (
    44100, 48000, 32000, 0,  // mpeg1
    22050, 24000, 16000, 0,  // mpeg2
    11025, 12000, 8000,  0); // mpeg2.5
  padding: integer;
  sr: dword;
begin
   Result := 0;
   // extract important fields from mpeg headers:
   // get and check the mpeg version
   mpegver := (headers[1] and $018) shr 3;
   if (mpegver = 1) or (mpegver > 3) then exit;

   // get and check mpeg layer
   layer := (headers[1] and $06) shr 1;
   if (layer = 0) or (layer > 3) then exit;

   brindex := (headers[2] and $0F0) shr 4;

   if (mpegver = 3) and (layer = 3) then // mpeg1, layer1
     bitrate := brtable[brindex];
   if (mpegver = 3) and (layer = 2) then // mpeg1, layer2
     bitrate := brtable[brindex + 16];
   if (mpegver = 3) and (layer = 1) then // mpeg1, layer3
     bitrate := brtable[brindex + 32];
   if ((mpegver = 2) or (mpegver = 0)) and (layer = 3) then // mpeg2, 2.5, layer1
     bitrate := brtable[brindex + 48];
   if ((mpegver = 2) or (mpegver = 0)) and ((layer = 2) or (layer = 1)) then //mpeg2, layer2 or layer3
     bitrate := brtable[brindex + 64];
   bitrate := bitrate * 1000;
   padding := (headers[2] and $02) shr 1;

   // get and check the 'sampling_rate_index':
   srindex := (headers[2] and $0C) shr 2;
   if (srindex >= 3) then exit;
   if mpegver = 3 then // mpeg1
     sr := srtable[srindex];
   if mpegver = 2 then // mpeg2
     sr := srtable[srindex + 4];
   if mpegver = 0 then // mpeg2.5
     sr := srtable[srindex + 8];

   //writeln('mpeg= ', mpegver, ' layer= ', layer, ' brindex= ', brindex, ' srindex= ', srindex);
   //writeln('br= ', bitrate, ' sr= ', sr, ' padding= ', padding);

   case mpegver of
     3: // mpeg1
     begin
       if layer = 3 then // layer1
         Result := (Trunc(12 * bitrate / sr) * 4) + (padding * 4);
       if (layer = 2) or (layer = 1) then // layer 2 & 3
         Result := Trunc(144 * bitrate / sr + padding);
     end;
     2, 0: //mpeg2 & mpeg2.5
     begin
       if layer = 3 then // layer1
         Result := (Trunc(12 * bitrate / sr) * 4) + (padding * 4);
       if layer = 2 then // layer2
         Result := Trunc(144 * bitrate / sr + padding);
       if layer = 1 then // layer3
         Result := Trunc(72 * bitrate / sr + padding);
     end;
   end;
end;

function mpx_SeekTo1stFrame(fs: TFileStream): integer;
var
  buf: array[0..49999] of byte;
  i, j, k: integer;
  mpx_header, mpx_header2: array[0..3] of byte;
  framelength: integer;
begin
  // read 50000 bytes from the beginning and search for a
  // syncword (to skip ID3V2, for example)
  fs.Read(buf[0], sizeof(buf));
  j := -1;
  for i := 0 to sizeof(buf) - 1 do
  begin

    if (buf[i] = $0FF) and ((buf[i + 1] and $0E0) = $0E0) then
    begin
      if sizeof(buf) - i < 10 then break;
      for k := 0 to 3 do
        mpx_header[k] := buf[i + k];
      //writeln('pos = ', i);
      if mpx_IsValidFrameHeader(mpx_header) then
      begin
        framelength := mpx_GetFrameSize(mpx_header);
        if framelength > 0 then
        begin
          if i + framelength + 4 > sizeof(buf) then break;
          for k := 0 to 3 do
            mpx_header2[k] := buf[i + framelength + k];
          if mpx_IsValidFrameHeader(mpx_header2) then
          begin
            j := i;
            fs.Seek(j, soFromBeginning);
            break;
          end;
        end;
      end;
    end;
  end;
  Result := j;
end;


function mpx_GetFrames(fs: TFileStream; FramesToRead: dword;
  var sbuf: array of byte): dword;
var
  headers, headers2: array[0..3] of byte;
  pos, BytesRead, numBytesToRead: integer;
  FramesRead: dword;
  FrameLength: integer;
  i: integer;
  InSync: boolean = true;
  zz: integer;
begin
  pos := 0;
  FramesRead := 0;

  while(FramesRead < FramesToRead) do
  begin
      //writeln('pos= ', fs.Position, ' framesread= ', FramesRead, '/', FramesToread);
      // begin by reading 4-byte fixed_variable_headers:
      BytesRead := fs.Read(headers[0], sizeof(headers));
      if BytesRead < sizeof(headers) then
      begin
        // the input file has ended
        //
        break;
      end;

      //writeln('r0');
      if not mpx_IsValidFrameHeader(headers) then
      begin
        if InSync then
        begin
          hz_logln_term(#13'Bad MPEG frame at offset ' +
            IntToStr(fs.Position - 4) + ', resyncing...                           ', LOG_DEBUG);
          hz_log('Bad MPEG frame at offset ' +
            IntToStr(fs.Position - 4) + ', resyncing...', LOG_DEBUG);
        end;
        fs.Seek(-3, soFromCurrent);
        InSync := false;
        //writeln('r1');
        continue;
      end;

      //writeln('enterin gfs');
      FrameLength := mpx_GetFrameSize(headers);
      //writeln('left gfs, fl= ', framelength);
      if (FrameLength = 0) or (FrameLength > 5000) then
      begin
        if InSync then
        begin
          hz_logln_term(#13'Bad MPEG frame at offset ' +
            IntToStr(fs.Position - 4) + ', resyncing...                           ', LOG_DEBUG);
          hz_log('Bad MPEG frame at offset ' +
            IntToStr(fs.Position - 4) + ', resyncing...', LOG_DEBUG);
        end;
        fs.Seek(-3, soFromCurrent);
        InSync := false;
        //writeln('r1');
        continue;
      end;

      if FrameLength > sizeof(headers) then
        numBytesToRead := FrameLength - sizeof(headers) // + crc
      else
        numBytesToRead := 0; // + crc;

      zz := fs.Position;
      BytesRead := fs.Seek(numBytesToRead, soFromCurrent) - zz;
      BytesRead := BytesRead + fs.Read(headers2[0], sizeof(headers2));
      //writeln('bytesread= ', BytesRead);
      fs.Seek(-BytesRead, soFromCurrent);
      //fs.Seek(-4, soFromCurrent);
      if not mpx_IsValidFrameHeader(headers2) then
      begin
        if InSync then
        begin
          hz_logln_term(#13'Bad MPEG frame at offset ' +
            IntToStr(fs.Position - 4 - numBytesToRead) + ', resyncing...                           ', LOG_DEBUG);
          hz_log('Bad MPEG frame at offset ' +
            IntToStr(fs.Position - 4 - numBytesToRead) + ', resyncing...', LOG_DEBUG);
        end;
        fs.Seek(-3, soFromCurrent);
        InSync := false;
        //writeln('r2');
        continue;
      end;

      //
      // from now on, frame is considered valid
      if not InSync then
      begin
        hz_logln_term('Resynced at offset ' + IntToStr(fs.Position - 4), LOG_DEBUG);
        hz_log('Resynced at offset ' + IntToStr(fs.Position - 4), LOG_DEBUG);
      end;
      InSync := true;

      // copy frame header to out buffer
      for i := 0 to 3 do
        sbuf[pos + i] := headers[i];
      pos := pos + 4;

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
  //writeln('getframes ok');
end;


function mpx_GetFileInfo(filename: string;
  var br: real; var spf, sr, frames, ch: dword): integer;
var
  fs: TFileStream;
  headers: array[0..3] of byte;
  frame: integer = 1;
  mpegver: byte;
  layer: byte;
  brindex, srindex: byte;
  bitrate: dword;
  brtable: array[0..79] of dword = (
    0, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448, 0,
    0, 32, 48, 56, 64,  80,  96,  112, 128, 160, 192, 224, 256, 320, 384, 0,
    0, 32, 40, 48, 56,  64,  80,  96,  112, 128, 160, 192, 224, 256, 320, 0,
    0, 32, 48, 56, 64,  80,  96,  112, 128, 144, 160, 176, 192, 224, 256, 0,
    0, 8,  16, 24, 32,  40,  48,  56,  64,  80,  96,  112, 128, 144, 160, 0);
  srtable: array[0..11] of dword = (
    44100, 48000, 32000, 0,  // mpeg1
    22050, 24000, 16000, 0,  // mpeg2
    11025, 12000, 8000,  0); // mpeg2.5
  j: integer;
  padding: integer;
  frame_length: integer;
  numBytesToRead: integer;
  fsize: int64;
  playtime: real;
  nsamples: dword;
  smpegver, slayer, sch: string;
begin
  Result := -1;
  if not FileExists(filename) then exit;
  fs := TFileStream.Create(filename, fmOpenRead);
try
  hz_log_both('Checking ' + filename, LOG_DEBUG);

  j := mpx_SeekTo1stFrame(fs);
  if j = -1 then
  begin
    hz_log_both('Couldn''t find MPEG frame', LOG_ERROR);
    exit;
  end;
  hz_log_both('First frame found at offset: ' + IntToStr(j), LOG_DEBUG);

  // now having opened the input file, read the headers of the
  // first frame, to get the audio stream's parameters:
  if fs.Read(headers[0], sizeof(headers)) = sizeof(headers) then
  begin

    // check the 'syncword'
    if (headers[0] <> $0FF) and ((headers[1] and $0E0) <> $0E0) then
    begin
      hz_log_both('Bad "frame sync" at frame # ' + IntToStr(frame), LOG_ERROR);
      exit;
    end;

    // get and check the mpeg version
    mpegver := (headers[1] and $018) shr 3;
    if mpegver = 1 then
    begin
      hz_log_both('Bad (reserved) mpeg version at frame # ' + IntToStr(frame), LOG_ERROR);
      exit;
    end;

    // get and check mpeg layer
    layer := (headers[1] and $06) shr 1;
    if layer = 0 then
    begin
      hz_log_both('Bad (reserved) mpeg layer at frame # ' + IntToStr(frame), LOG_ERROR);
      exit;
    end;

    // get and check the 'sampling_rate_index':
    srindex := (headers[2] and $0C) shr 2;
    if srtable[srindex] = 0 then
    begin
      hz_log_both('Bad "sampling_frequency_index" at frame # ' + IntToStr(frame), LOG_ERROR);
      exit;
    end;
    if mpegver = 3 then // mpeg1
      sr := srtable[srindex];
    if mpegver = 2 then // mpeg2
      sr := srtable[srindex + 4];
    if mpegver = 0 then // mpeg2.5
      sr := srtable[srindex + 8];

    // get and check "channel configuration"
    ch := (headers[3] and $0C0) shr 6;
    fs.Seek(j, soFromBeginning);
    //////////

    while(true) do
    begin

      // begin by reading 4byte headers:
      if fs.Read(headers[0], sizeof(headers)) < sizeof(headers) then
      begin
        // the input file has ended
        //
        break;
      end;

      // extract important fields from headers:
      // get and check bitrate
      brindex := (headers[2] and $0F0) shr 4;
      if (brindex = 0) or (brindex = $0F) then
      begin
        fs.Seek(-3, soFromCurrent);
        continue;
      end;

      if (mpegver = 3) and (layer = 3) then // mpeg1, layer1
        bitrate := brtable[brindex];
      if (mpegver = 3) and (layer = 2) then // mpeg1, layer2
        bitrate := brtable[brindex + 16];
      if (mpegver = 3) and (layer = 1) then // mpeg1, layer3
        bitrate := brtable[brindex + 32];
      if ((mpegver = 2) or (mpegver = 0)) and (layer = 3) then // mpeg2, 2.5, layer1
        bitrate := brtable[brindex + 48];
      if ((mpegver = 2) or (mpegver = 0)) and ((layer = 2) or (layer = 1)) then //mpeg2, layer2 or layer3
        bitrate := brtable[brindex + 64];

      bitrate := bitrate * 1000;
      padding := (headers[2] and $02) shr 1;

      frame_length := mpx_GetFrameSize(headers);

      if (not mpx_IsValidFrameHeader(headers)) or (frame_length = 0) or (frame_length > 5000) then
      begin
        //writeln('Warning: Bad MPEG frame at offset ', fs.Position - 4);
        fs.Seek(-3, soFromCurrent);
        continue;
      end
      else
        frame := frame + 1;

      if frame_length > sizeof(headers) then
        numBytesToRead := frame_length - sizeof(headers)
      else
        numBytesToRead := 0;

      //skip raw frame data
      fs.Seek(numBytesToRead, soFromCurrent);
    end;
  end;
  // close the file
  fsize := fs.Size;
finally
  fs.Free;
end;
  case mpegver of
    3: //mpeg1
    begin
      if (layer = 3) then // layer1
        spf := 384
      else
        spf := 1152; // layer2 & layer3
    end;
    2, 0: //mpeg2 & mpeg2.5
    begin
      if layer = 3 then // layer1
        spf := 384;
      if layer = 2 then
        spf := 1152    // layer2
      else
        spf := 576;   // layer 3
    end;
  end;
  frames := frame - 1;
  nsamples := spf * frames;
  playtime := nsamples / sr;
  // in bytes/sec
  br :=  fsize / playtime;
  //in kbits/sec
  br := br * 8 / 1000;

  case mpegver of
    3: smpegver := 'MPEG 1';
    2: smpegver := 'MPEG 2';
    0: smpegver := 'MPEG 2.5';
  end;
  case layer of
    3: slayer := 'Layer I';
    2: slayer := 'Layer II';
    1: slayer := 'Layer III';
  end;
  case ch of
    0: sch := 'Stereo';
    1: sch := 'Joint Stereo';
    2: sch := 'Dual channel';
    3: sch := 'Mono';
  end;
  if (ch = 0) or (ch = 1) or (ch = 2) then
    ch := 2
  else
    ch := 1;

  hz_log_both('format    : ' + smpegver + ' ' + slayer, LOG_DEBUG);
  hz_log_both('frames    : ' + IntToStr(frames), LOG_DEBUG);
  hz_log_both('samplerate: ' + IntToStr(sr) + ' Hz', LOG_DEBUG);
  hz_log_both('channels  : ' + IntToStr(ch) + ' (' + sch + ')', LOG_DEBUG);
  hz_log_both('playtime  : ' + IntToStr(Round(playtime)) + ' sec', LOG_DEBUG);
  hz_log_both('bitrate   : ' + IntToStr(Round(br)) + ' kbps (average)', LOG_DEBUG);
  Result := 0;
end;

end.

