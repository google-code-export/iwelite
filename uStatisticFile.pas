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

unit uStatisticFile;

interface

{$I IntrawebVersion.inc}

uses
  SysUtils, Classes, HTTPApp, IWTypes, Windows
  {$IFDEF INTRAWEB120}, IW.HttpRequest {$ENDIF};

const
  ProLogFileCode = 'PROL';
  HitLogFileCode = 'HITL';

type
  TFileHeader = packed record
    FileCode : array[0..3] of char;
    DataVersion : SmallInt;
    TimeStamp : TDateTime;
  end;

  TProfilerRecord = packed record
    TimeStamp        : TDateTime;
    TickCount        : DWORD;
    OverallCPUUsage  : SmallInt;
    MemoryUsage      : Int64;
    ThreadCount      : Integer;
    SessionCount     : Int64;
    SessionCountPS   : Int64;
    HitCounter       : Int64;
    HitCounterPS     : Int64;
    ContentBytes     : Int64;
    ContentBytesPS   : Int64;
    VisitorsNew      : Integer;
    VisitorsReturn   : Integer;
    MemTotalPhysical : Int64;
    MemFreePhysical  : Int64;
    MemTotalVirtual  : Int64;
    MemFreeVirtual   : Int64;
    MemTotalPage     : Int64;
    MemFreePage      : Int64;
    MemLoad          : Byte;
    MemFree      : Int64;
    CPUCount         : integer;
  end;
  PProfilerRecord = ^TProfilerRecord;

  TIWRequestType = (rtForm, rtIntScript, rtFile, rtIntGraphic, rtCache, rtRoot);
  THitLogRecord = class(TComponent)
  private
    FNewVisitor: boolean;
    FNewSession: boolean;
    FHostName: string;
    FIP: string;
    FUserAgent: string;
    FUsername: string;
    FPathInfo: string;
    FScriptName: string;
    FFormName: string;
    FProtocol: string;
    FReferrer: string;
    FRequestType: TIWRequestType;
    {$IFDEF INTRAWEB120}
    FMethodType: THttpMethod;
    {$ELSE}
    FMethodType: TMethodType;
    {$ENDIF}
    FContentVersion: string;
    FContentType: string;
    FTerminated: boolean;
    FSecure: boolean;
    FAssigned: boolean;
    FContentLength: integer;
    FContentEncoding: string;
    FSessionID: string;
    FFormAction: string;
    {$IFNDEF INTRAWEB110}
    FClientType: TIWClientType;
    {$ENDIF}
    FStarted: TDateTime;
    FFinished: TDateTime;
    FFileRequest: boolean;
    FScreenMetrix: string;
  public
    property Assigned    : boolean        read FAssigned    write FAssigned;
  published
    property Started     : TDateTime      read FStarted     write FStarted;
    property Finished    : TDateTime      read FFinished    write FFinished;
    {$IFDEF INTRAWEB120}
    property MethodType  : THttpMethod    read FMethodType  write FMethodType;
    {$ELSE}
    property MethodType  : TMethodType    read FMethodType  write FMethodType;
    {$ENDIF}
    property Protocol    : string         read FProtocol    write FProtocol;
    property FormName    : string         read FFormName    write FFormName;
    property PathInfo    : string         read FPathInfo    write FPathInfo;
    property RequestType : TIWRequestType read FRequestType write FRequestType;
    property IP          : string         read FIP          write FIP;
    property HostName    : string         read FHostName    write FHostName;
    property Referrer    : string         read FReferrer    write FReferrer;
    property UserAgent   : string         read FUserAgent   write FUserAgent;
    property Username    : string         read FUsername    write FUsername ;
    property NewVisitor  : boolean        read FNewVisitor  write FNewVisitor;
    property NewSession  : boolean        read FNewSession  write FNewSession;
    property ScriptName  : string         read FScriptName  write FScriptName;
    property ContentEncoding : string     read FContentEncoding write FContentEncoding;
    property ContentType     : string     read FContentType     write FContentType;
    property ContentLength   : integer    read FContentLength   write FContentLength;
    property ContentVersion  : string     read FContentVersion  write FContentVersion;
    property SessionID : string           read FSessionID       write FSessionID;
    {$IFNDEF INTRAWEB110}
    property ClientType : TIWClientType   read FClientType       write FClientType;
    {$ENDIF}
    property FormAction : string          read FFormAction       write FFormAction;
    property Secure     : boolean         read FSecure           write FSecure;
    property FileRequest : boolean        read FFileRequest      write FFileRequest;
    property Terminated : boolean         read FTerminated       write FTerminated;
    property ScreenMetrix : string        read FScreenMetrix     write FScreenMetrix;
  end;

implementation

end.
