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

program hz;
{$MODE DELPHI}
{$H+}

uses {$IFDEF LINUX}cthreads, BaseUnix, daemon,{$ENDIF} SysUtils, Classes, DateUtils,
     net, aac, mpx, playlist, timing, metadata, config, ctrlc, b64, script, log;

label reconnect;

type
  Tfmt_GetFileInfo = function(filename: string; var br: real; var spf, sr, frames, ch: dword): integer;
  Tfmt_SeekTo1stFrame = function(fs: TFileStream): integer;
  Tfmt_GetFrames = function(fs: TFileStream; framestoread: dword; var sbuf: array of byte): dword;

const
  bufsize = 100000;

var
  fmt_GetFileInfo: Tfmt_GetFileInfo;
  fmt_SeekTo1stFrame: Tfmt_SeekTo1stFrame;
  fmt_GetFrames: Tfmt_GetFrames;
  spf: dword;
  Sock: integer;
  fs: TFileStream;
  headers, response: string;
  buf: array of byte;
  res: integer;
  br: real;
  sr, frames, ch: dword;
  FramesToRead, FramesSent: dword;
  BytesToSend: dword;
  TimeBegin, SendBegin: TDateTime;
  TimeSent, TimeElapsed, SendLag, TimePause: dword;
  BufferSent: integer;
  TimeBetweenTracks: dword;
  filename, cuename: string;
  port: word;
  password: string;
  Connected: boolean;
  ConnectionAttemptsCount: integer = 0;
  cuesheet: boolean;
  contenttype: string;
  onplay: integer;
begin

  writeln('=====================================================================');
  writeln(' hz v' + version);
  writeln(' AAC/AACplus/AACplusV2 & MP1/MP2/MP3 Icecast/Shoutcast source client');
  writeln(' Copyright (C) 2006-2007 Roman Butusov <reaxis at mail dot ru>');
  writeln('=====================================================================');
  writeln;
  Flush(output);
  if not FileExists(ParamStr(1)) then
  begin
    writeln('Couldn''t find hz config file: ' + ParamStr(1));
    exit;
  end;
  loadconfig(ParamStr(1));
  hz_log('---------------------------', LOG_INFO);
  hz_log('hz v' + version + ' started', LOG_INFO);

{$IFDEF LINUX}
  if Opts.IsDaemon = 1 then
    Daemonize;
{$ENDIF}
  

  try
    // load script
    if script_init = -1 then exit;

    if load_playlist = -1 then
    begin
      hz_log_both('Error loading playlist: ' + Opts.playlist, LOG_ERROR);
      exit;
    end;

    if Opts.streamtype = 'aac' then
    begin
      fmt_GetFileInfo := aac_GetFileInfo;
      fmt_SeekTo1stFrame := aac_SeekTo1stFrame;
      fmt_GetFrames := aac_GetFrames;
      contenttype := 'audio/aacp';
    end
    else begin
      fmt_GetFileInfo := mpx_GetFileInfo;
      fmt_SeekTo1stFrame := mpx_SeekTo1stFrame;
      fmt_GetFrames := mpx_GetFrames;
      contenttype := 'audio/mpeg';
    end;

    SetCtrlCHandler;
    Randomize;
    SetLength(buf, bufsize);
    try
      Connected := false;
      while (true) do
      begin
        TimeBegin := Now;

        filename := get_next_file;
        if filename = '' then exit;
        res := fmt_GetFileInfo(filename, br, spf, sr, frames, ch);

reconnect:
        // if not connected yet, then do so
        while (not Connected) and (res = 0) do
        begin
          if Opts.ConnAttempts > 0 then
          begin
            ConnectionAttemptsCount := ConnectionAttemptsCount + 1;
            if ConnectionAttemptsCount > Opts.ConnAttempts then
            begin
              hz_log_both('Too many consecutive errors, exiting', LOG_ERROR);
              exit;
            end;
          end;

          // connect to icecast/shoutcast server
          if Opts.Server = 'shoutcast' then
            port := Opts.port + 1
          else
            port := Opts.port;
          hz_log_both('Connecting to ' + Opts.host + ':' + IntToStr(port) + '...',
                LOG_DEBUG);

          if not net_connect(Opts.host, port, sock) then
          begin
            hz_log_both('Couldn''t connect to ' + Opts.host + ':' + IntToStr(port), LOG_ERROR);
            continue;
          end;
          hz_log_both('Connected, sending headers...', LOG_DEBUG);

          // send headers to icecast/shoutcast server
          if Opts.Server = 'shoutcast' then
          begin
            password := Opts.password + crlf;
            net_send(sock, password[1], length(password));
            //Sock.Write(password[1], length(password));
            pause(1000);
            response := net_recv(sock);
            if Copy(response, 1, 3) <> 'OK2' then
            begin
              hz_log_both('Shoutcast server error: ' +
                Copy(response, 1, length(response) - 2), LOG_ERROR);
              net_close(sock);
              continue;
            end;
            headers := 'content-type:' + contenttype + crlf +
                'icy-name:' + Opts.name + crlf +
                'icy-genre:' + Opts.genre + crlf +
                'icy-url:' + Opts.url + crlf +
                'icy-pub:' + IntToStr(Opts.IsPublic) + crlf +
                'icy-br:' + IntToStr(Round(br)) +
                crlf + crlf;
          end
          else
            headers := 'SOURCE /' + Opts.mount + ' HTTP/1.0' + crlf +
               'Content-Type: ' + contenttype + crlf +
               'Authorization: Basic ' + EncodeBase64('source:' + Opts.password) + crlf +
               'User-Agent: hz/' + version + crlf +
               'ice-name: ' + Opts.name + crlf +
               'ice-public: ' + IntToStr(Opts.IsPublic) + crlf +
               'ice-url: ' + Opts.url + crlf +
               'ice-genre: ' + Opts.genre + crlf +
               'ice-description: ' + Opts.descr + crlf +
               'ice-audio-info: bitrate=' + IntToStr(Round(br)) +
               ';channels=' + IntToStr(ch) +
               ';samplerate=' + IntToStr(sr) +
               crlf + crlf;
          net_send(sock, headers[1], length(headers));
          //hz_log_both('Headers sent', LOG_DEBUG);
          if Opts.Server = 'icecast' then
          begin
            pause(1000);
            response := net_recv(sock);
            if Copy(response, 10, 3) <> '200' then
            begin
              hz_log_both('Icecast server error: ' + Copy(response, 1,
                pos(crlf, response)), LOG_ERROR);
              net_close(sock);
              continue;
            end;
          end;
          Connected := true; ConnectionAttemptsCount := 0;
          TimeBegin := Now;
        end;

        // if audio file is good, then play it
        if res = 0 then
        begin
          hz_log_both('Playing ' + filename, LOG_INFO);

          if Opts.UpdateMetadata = 1 then
          begin
            cuesheet := False;
            // if cuesheet exists, then take tags from there
            cuename := Copy(filename, 1,
              Length(filename) - Length(ExtractFileExt(filename))) + '.cue';
            if FileExists(cuename) then
            begin
              hz_log_both('Cuesheet: ' + cuename, LOG_INFO);
              if metadata_cueload(cuename) then
                cuesheet := True;
            end
            else
              metadata_update(filename);
          end;
          hz_logln_term('CTRL-C to stop', LOG_INFO);

          // open aac file and stop by the 1st frame position
          fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
          fmt_SeekTo1stFrame(fs);

          FramesSent := 0;
          TimeSent := 0;
          // get number of frames to read in 1 iteration
          FramesToRead := Round(sr / spf);

          while (FramesSent < frames) do
          begin
            // mark the time just before reading a chunk
            // of data and sending it over the socket
            SendBegin := Now;

            if cuesheet then
              metadata_update(TimeSent);

            BytesToSend := fmt_GetFrames(fs, FramesToRead, buf);
            //Sock.write(buf[0], BytesToSend);
            if not net_send(sock, buf[0], BytesToSend) then
            begin
              hz_logln_term('... Disconnected from server', LOG_ERROR);
              hz_log('Disconnected from server', LOG_ERROR);
              Connected := False;
              net_close(sock);
              fs.Free;
              goto reconnect;
            end;

            FramesSent := FramesSent + FramesToRead;

            // get on_play script result
            onplay := 0; //script_onplay(filename, FramesSent, frames);

            TimeElapsed := dword(MilliSecondsBetween(Now, TimeBegin));
            TimeSent := Round( ((FramesSent * spf) / sr) * 1000);

            if TimeElapsed > TimeSent then
              BufferSent := 0
            else
              BufferSent := TimeSent - TimeElapsed;

            // calculate the send lag
            SendLag := dword(MilliSecondsBetween(Now, SendBegin));

            if TimeElapsed > 1500 then
            hz_log_term('Frames: ' + IntToStr(FramesSent) + '/' +
              IntToStr(frames) + '  Time: ' +
              IntToStr(TimeElapsed div 1000) + '/' +
              IntToStr(TimeSent div 1000) + 's  Buffer: ' +
              IntToStr(BufferSent) + 'ms  Bps: ' + IntToStr(BytesToSend) + #13,// +
              //'SendLag: ' + IntToStr(SendLag) + 'ms    ',
              LOG_INFO);

            // regulate sending rate
            // ---------------------
            if BufferSent < (Opts.BufferSize - 100) then
              TimePause := 900 - SendLag
            else begin
              if BufferSent > Opts.BufferSize then
                TimePause := 1100 - SendLag
              else
                TimePause := 975 - SendLag;
            end;

            // check for user abort
            if IsAborted or (onplay = -2) then
            begin
              pause(200);
              hz_logln_term('Frames: ' + IntToStr(FramesSent) + '/' +
              IntToStr(frames) + '  Time: ' +
              IntToStr(TimeElapsed div 1000) + '/' +
              IntToStr(TimeSent div 1000) + 's  Buffer: ' +
              IntToStr(BufferSent) + 'ms  Bps: ' + IntToStr(BytesToSend),// +
              //'SendLag: ' + IntToStr(SendLag) + 'ms    ',
              LOG_INFO);

              hz_log_both('Aborted by user/SIGTERM', LOG_INFO);
              exit;
            end;

            if onplay = -1 then break;

            // sleep for required time
            pause(TimePause);
          end;

          // close the audio file
          fs.free;

          hz_logln_term(#13'Frames: ' + IntToStr(frames) + '  Time: ' +
              IntToStr(TimeElapsed div 1000) + '/' +
              IntToStr(TimeSent div 1000) + 's... Finished', LOG_INFO);

          // pause between tracks
          // --------------------
          if onplay <> -1 then
          begin
            TimeBetweenTracks := Round( ((frames * spf) / sr) * 1000) - dword(MilliSecondsBetween(Now, TimeBegin));
            hz_log_both('Pausing for ' + IntToStr(TimeBetweenTracks) + 'ms...', LOG_DEBUG);
            pause(TimeBetweenTracks);
          end;
        end;
        writeln;

      end; // while
    finally
      fs.free;
      net_close(sock);
      script_shutdown;
    end;
  finally
    hz_log_both('hz v' + version + ' terminated', LOG_INFO);
  end;
end.
