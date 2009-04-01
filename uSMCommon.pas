////////////////////////////////////////////////////////////////////////////////
// 
// The MIT License
// 
// Copyright (c) 2008 by Arcana Technologies Incorporated
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
// 
////////////////////////////////////////////////////////////////////////////////

unit uSMCommon;

interface

uses Windows, SysUtils, Classes;

type
  TRegistrationType = (rtEvaluation, rtLite, rtProfessional, rtEnterprise, rtResale);
  TRegType = TRegistrationType;

  ENoReportException = class(Exception);


procedure BreakupFileVersion(FileName: string; var Major, Minor, Release, Build : Cardinal);
procedure ExecuteAndWait(const Filename, Params : string);
function FindOpenPort : integer;

implementation

uses InTCPServer;

function FindOpenPort : integer;
var
  tcp : TInTCPServer;
begin
  tcp := TInTCPServer.Create(nil);
  try
    repeat
      Result := Random(30000)+30000;
      tcp.DefaultPort := Result;
      try
        tcp.Active := True;
        break;
      except
      end;
    until False;
  finally
    tcp.Free;
  end;
end;

procedure ExecuteAndWait(const Filename, Params : string);
var
  proc_info: TProcessInformation;
  startinfo: TStartupInfo;
  ExitCode: longword;
begin
  // Initialize the structures
  FillChar(proc_info, sizeof(TProcessInformation), 0);
  FillChar(startinfo, sizeof(TStartupInfo), 0);
  startinfo.cb := sizeof(TStartupInfo);

  // Attempts to create the process
  if CreateProcess(nil, PChar(Filename+' '+Params), nil, nil, false,
    NORMAL_PRIORITY_CLASS, nil, nil, startinfo, proc_info) <> False then
  begin
    try
      // The process has been successfully created
      // No let's wait till it ends...
      WaitForSingleObject(proc_info.hProcess, INFINITE);
      // Process has finished. Now we should close it.
      GetExitCodeProcess(proc_info.hProcess, ExitCode);  // Optional
    finally
      CloseHandle(proc_info.hThread);
      CloseHandle(proc_info.hProcess);
    end;
  end else
    raise Exception.Create('Error executing "'+Filename+'"');
end;

procedure BreakupFileVersion(FileName: string; var Major, Minor, Release, Build : Cardinal);
var
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
        begin
          Major := HiWord(FI.dwFileVersionMS);
          Minor := Lo(FI.dwFileVersionMS);
          Release := HiWord(FI.dwFileVersionLS);
          Build := Lo(FI.dwFileVersionLS);
        end;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

end.
 
