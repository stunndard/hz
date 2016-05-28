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

unit daemon;
{$MODE DELPHI}
{$H+}

interface

uses SysUtils, BaseUnix, ctrlc, log;

procedure Daemonize;

implementation

procedure TermHandler(sig: cint); cdecl;
begin
  //writeln('signal= ', sig);
  if sig = SIGTERM then
    AbortNow;
end;

procedure Daemonize;
var
  pid: TPid;
  oa: SigActionRec;
  rl: Trlimit;
  i: integer;

(*
function FpGetRLimit(resource : cInt; rlim : PRLimit) : cInt;
begin
  FpGetRLimit := do_syscall(syscall_nr_getrlimit,
    TSysParam(resource), TSysParam(rlim));
end;
*)
begin
  // fork
  pid := fpFork;
  if pid = -1 then
  begin
    hz_log_both('cant daemonize, fork() failed', LOG_ERROR);
    Halt;
  end;
  if pid > 0 then
  begin
    hz_log_both('Going to background, detaching from the terminal...', LOG_INFO);
    Flush(output);
    Flush(stderr);
    FpExit(0);
  end;

  //hz_log_both('pid= ' + IntToStr(pid), LOG_INFO);

  // we're in child process now
  sleep(100);
  
  (*
  rl.rlim_max := 0;
  if fpugetrlimit(RLIMIT_NOFILE, @rl) = -1 then
  begin
    hz_log_both('error calling getrlimit()', LOG_ERROR);
    FpExit(0);
  end;
  *)
  // heh, no easy way to call getrlimit from FP,
  // so this dirty hack is here
  for i := 0 to 100 do
    FpClose(i);

  // create a new SID for the child process
  if FpSetSid < 0 then
  begin
    hz_log('cant daemonize, setsid() failed', LOG_ERROR);
    FpExit(0);
  end;

  // fork again
  pid := fpfork;
  if pid = -1 then fpexit(0);
  if pid > 0 then fpexit(0);  
  
  // cancel certain signals
  FpSignal(SIGCHLD, SignalHandler(SIG_IGN));
  FpSignal(SIGTSTP, SignalHandler(SIG_IGN));
  FpSignal(SIGTTOU, SignalHandler(SIG_IGN));
  FpSignal(SIGTTIN, SignalHandler(SIG_IGN));
  FpSignal(SIGHUP,  SignalHandler(SIG_IGN));

  // set the SIGTERM handler
  oa.sa_handler := SigActionHandler(@TermHandler);
  FillChar(oa.sa_mask, sizeof(oa.sa_mask), #0);
  oa.sa_flags := 0;
  oa.sa_restorer := nil;
  fpSigAction(SIGTERM, @oa, nil);

  // change the file mode mask
  FpUmask(0);

  // change the current working directory
  if FpChDir('/') < 0 then
  begin
    hz_log('cant daemonize, chdir() failed', LOG_ERROR);
    Halt;
  end;

  hz_log('daemonized ok', LOG_INFO);
  
// redirect standard files to /dev/null
  close(input);
  assign(input, '/dev/null');
  rewrite(input);
  close(output);
  assign(output, '/dev/null');
  rewrite(output);
  close(stderr);
  assign(stderr, '/dev/null');
  rewrite(stderr);
  
end;

end.
