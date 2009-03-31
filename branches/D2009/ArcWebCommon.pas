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

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  unit WebCommon                                                            //
//    Copyright 2001, 2002 by Arcana Technologies Incorporated                //
//    Written By Jason Southwell                                              //
//                                                                            //
//  Description:                                                              //
//    This unit houses some common functions to assist with delphi web        //
//    development.                                                            //
//                                                                            //
//  Updates:                                                                  //
//    08/31/2001 - Released to Open Source.  Functions include DLLFilePath,   //
//                 MakeLinksGlobal, LogEvent.                                 //
//    10/16/2002 - Fixed Message Logging so that it will not include the      //
//                 EventID(0) message in front of your message. Also fixed    //
//                 a memory leak and added:                                   //
//                                                                            //
//                 SetLogApplication:                                         //
//                     Call this before logging to have logs show up with an  //
//                     application name.                                      //
//                 CleanupLogApplication:                                     //
//                     Call this if you wish to clean up registry entries     //
//                     created by SetLogApplication                           //
//                                                                            //
//  License:                                                                  //
//    This code is covered by the Mozilla Public License 1.1 (MPL 1.1)        //
//    Full text of this license can be found at                               //
//    http://www.opensource.org/licenses/mozilla1.1.html                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit ArcWebCommon;

interface

uses Windows, SysUtils, Classes;

{.$R webcommon.RES}

type
  TLogName = (lnApplication, lnSecurity, lnSystem);
  TMsgType = (lmtSuccess, lmtError, lmtWarning, lmtInformation, lmtAuditSuccess, lmtAuditFailure);

function  DLLFilePath : string;
function  DLLFileName : string;
function  MakeLinksGlobal(const HTMLText, BaseURL : string; Secure : boolean=False ) : string;
procedure LogEvent(LogName : TLogName; MsgType : TMsgType; Text : string; Category : integer=0; EventID : integer=0);
procedure SetLogApplication(AppName : string);
procedure CleanupLogApplication(AppName : string);

implementation

uses Registry;

var
  LogApplicationName : string;

const
  LogNames: array[0..2] of shortstring = ('Application','Security','System');

procedure CleanupLogApplication(AppName : string);
begin
  if AppName = '' then exit;
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if KeyExists('\System\CurrentControlSet\Services\EventLog\Application\'+AppName) then
      DeleteKey('\System\CurrentControlSet\Services\EventLog\Application\'+AppName);
  finally
    free;
  end;
end;

procedure SetLogApplication(AppName : string);
begin
  try
    with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey('\System\CurrentControlSet\Services\EventLog\Application\'+AppName,True);
      WriteString('EventMessageFile',DLLFileName);
    finally
      free;
    end;
    LogApplicationName := AppName;
  except
  end;
end;

procedure LogEvent(LogName : TLogName; MsgType : TMsgType; Text : string; Category : integer=0; EventID : integer=0);
var
  P: Pointer;
  S : string;
  FEventLog : THandle;
  FMsgType : Cardinal;
begin
  FMsgType := 0; // just to remove warning
  P := PChar(Text);
  case MsgType of
    lmtError: FMsgType := EVENTLOG_ERROR_TYPE;
    lmtWarning: FMsgType := EVENTLOG_WARNING_TYPE;
    lmtInformation: FMsgType := EVENTLOG_INFORMATION_TYPE;
    lmtAuditSuccess: FMsgType := EVENTLOG_AUDIT_SUCCESS;
    lmtAuditFailure: FMsgType := EVENTLOG_AUDIT_FAILURE;
    lmtSuccess: FMsgType := EVENTLOG_SUCCESS;
  end;
  if LogApplicationName='' then
    s := LogNames[Integer(LogName)]
  else
    s := LogApplicationName;

  FEventLog := RegisterEventSource(nil, PChar(S));
  try
    ReportEvent(FEventLog, FMsgType, Category, EventID, nil, 1, 0, @P, nil);
  finally
    if fEventLog <> Cardinal(-1) then
      DeRegisterEventSource(fEventLog);  //QF added
  end;
end;

function  DLLFileName : string;
begin
  SetLength(Result,MAX_PATH);
  GetModuleFileName(HInstance,PCHar(Result),MAX_PATH);
  SetLength(Result,StrLen(PChar(Result)));
end;

function DLLFilePath : string;
begin
  Result := ExtractFilePath(DLLFileName);
end;

function MakeLinksGlobal(const HTMLText, BaseURL : string; Secure : boolean=False ) : string;
const
  secProtocol   = 'https://';
  unsecProtocol = 'http://';
var
  Protocol, Base : string;
  iPos : integer;
begin
  Result := HTMLText;
  Base := BaseURL;
  if Base[length(Base)]<>'/' then
    Base := Base+'/';
  if Secure then
    Protocol := secProtocol
  else
    Protocol := unsecProtocol;

  while pos(' src="',lowercase(Result))>0 do
  begin
    iPos := pos(' src="',lowercase(Result))+6;
    if (Lowercase(Copy(Result,iPos,length(unsecProtocol))) <> unsecProtocol) and
       (Lowercase(Copy(Result,iPos,length(secProtocol))) <> secProtocol) then
    begin
      if Result[ iPos]='/' then
        Delete(Result,iPos,1);
      Insert(Protocol+Base,Result,iPos);
    end;
    Result := Copy(Result,1,pos(' src="',lowercase(Result))-1)+' src=~"'+Copy(Result,pos(' src="',lowercase(Result))+6,length(Result));
  end;

  while pos(' src=\"',lowercase(Result))>0 do
  begin
    iPos := pos(' src=\"',lowercase(Result))+7;
    if (Lowercase(Copy(Result,iPos,length(unsecProtocol))) <> unsecProtocol) and
       (Lowercase(Copy(Result,iPos,length(secProtocol))) <> secProtocol) then
    begin
      if Result[ iPos]='/' then
        Delete(Result,iPos,1);
      Insert(Protocol+Base,Result,iPos);
    end;
    Result := Copy(Result,1,pos(' src=\"',lowercase(Result))-1)+' src=\~"'+Copy(Result,pos(' src=\"',lowercase(Result))+7,length(Result));
  end;

  while pos(' href="',lowercase(Result))>0 do
  begin
    iPos := pos(' href="',lowercase(Result))+7;
    if (Lowercase(Copy(Result,iPos,length(unsecProtocol))) <> unsecProtocol) and
       (Lowercase(Copy(Result,iPos,length(secProtocol))) <> secProtocol) then
    begin
      if Result[ iPos]='/' then
        Delete(Result,iPos,1);
      Insert(Protocol+Base,Result,iPos);
    end;
    Result := Copy(Result,1,pos(' href="',lowercase(Result))-1)+' href=~"'+Copy(Result,pos(' href="',lowercase(Result))+7,length(Result));
  end;
  while pos('~',Result)>0 do
    Delete(Result,pos('~',Result),1);
end;

end.
