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

unit ArcIWServerTimer;

interface

uses
  SysUtils, Classes, ArcIWControlBase;

type
  TTimerThread = class(TThread)
  private
    FInterval : Integer;
    FOnTimer : TNotifyEvent;
    FOwner : TComponent;
  protected
    procedure DoTimerEvent;
    procedure Execute; override;
  public
    constructor Create(aOwner : TComponent; aInterval : integer; aOnTimer : TNotifyEvent); virtual;
  end;

  TArcIWServerTimer = class(TComponent)
  private
    FThread : TTimerThread;

    FInterval : Integer;
    FEnabled  : boolean;
    FOnTimer  : TNotifyEvent;
    procedure SetEnabled(const Value: boolean);
  protected
    procedure Loaded; override;
  public
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
    property Thread : TTimerThread read FThread;
  published
    property Interval : Integer      read FInterval write FInterval;
    property Enabled  : boolean      read FEnabled  write SetEnabled;
    property OnTimer  : TNotifyEvent read FOnTimer  write FOnTimer;
  end;

implementation

uses Windows;

{ TTimerThread }

constructor TTimerThread.Create(aOwner : TComponent; aInterval: integer; aOnTimer: TNotifyEvent);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FInterval := aInterval;
  FOnTimer := aOnTimer;
  FOwner := aOwner;
  Resume;
end;

procedure TTimerThread.DoTimerEvent;
begin
  if Assigned(FOnTimer) then
    FOnTimer(FOwner);
end;

procedure TTimerThread.Execute;
var
  Mark : Int64;
begin
  Mark := GetTickCount;
  while not Terminated do
  begin
    Sleep(1);
    if GetTickCount - Mark >= FInterval then
    begin
      DoTimerEvent;
      if Terminated then
        break;
      Mark := GetTickCount;
    end;
  end;
end;

{ TArcIWServerTimer }

constructor TArcIWServerTimer.Create(AOwner: TComponent);
begin
  inherited;
  FInterval := 1000;
end;

destructor TArcIWServerTimer.Destroy;
begin
  if FEnabled then
    Enabled := False;
  inherited;
end;

procedure TArcIWServerTimer.Loaded;
begin
  inherited;
  if FEnabled then
  begin
    FEnabled := False;
    Enabled := True;
  end;
end;

procedure TArcIWServerTimer.SetEnabled(const Value: boolean);
begin
  if FEnabled=Value then exit;
  FEnabled := Value;
  if (not (csDesigning in ComponentState)) and
     (not (csLoading in ComponentState)) then
  begin
    if FEnabled then
      FThread := TTimerThread.Create(Self, FInterval, FOnTimer)
    else
      FThread.Terminate;
  end;
end;

end.
