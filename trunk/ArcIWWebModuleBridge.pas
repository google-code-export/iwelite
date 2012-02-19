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

unit ArcIWWebModuleBridge;

interface

{$I IntrawebVersion.inc}

uses
  SysUtils, Classes, IWForm, IWApplication, IWServerControllerBase, IWBaseForm,
  HTTPApp, ArcIWIOPPooling, InGlobalProtocols, {$IFDEF INTRAWEB110} IWSystem, IWURLResponder {$ELSE} SWSystem {$ENDIF},
  Windows, ArcD5Fix, ArcIWInteropCommon
  {$IFDEF INTRAWEB120}, IW.HttpRequest, IW.HttpReply {$ENDIF};

{$I IntrawebVersion.inc}

type
  EArcIWBridgeSessionNotFound = class(Exception);

  TWebModuleClass = class of TWebModule;
  TGetWebModuleClassEvent = procedure(Sender : TObject; var WebModuleClass : TWebModuleClass) of object;

(*
  TWebSession = class(TAbstractWebSession)
  protected
    function GetSessionID: String;
    function GetTimeoutMinutes: Integer;
    function GetValue(const AName: String): Variant;
    procedure SetTimeoutMinutes(AValue: Integer);
    procedure SetValue(const AName: String; const AValue: Variant);
  public
    constructor Create(SessionID : string); virtual;
    procedure Terminate;
    procedure UpdateResponse(AResponse: TWebResponse);
  end;
*)

  TArcIWWebModuleBridge = class(TComponent)
  private
    FActive: boolean;
    FOnGetWebModuleClass: TGetWebModuleClassEvent;
    FBrowsingPaths: TD7StringList;
    FFileAliases: TD7StringList;
    {$IFDEF INTRAWEB110}
      {$IFDEF INTRAWEB120}
    function IWURLResponderEventRequest(AApplication: TIWApplication;
      ARequest: THttpRequest; AResponse: THttpReply): Boolean;
      {$ELSE}
    function IWURLResponderEventRequest(AApplication: TIWApplication;
      ARequest: TWebRequest; AResponse: TWebResponse): Boolean;
      {$ENDIF}
    function GetURLResponder: TIWURLResponderEvent;
    {$ELSE}
    _OnInvalidCommand : TOnInvalidCommandEvent;
    {$ENDIF}
    procedure SetActive(const Value: boolean);
    function GetPoolSize: integer;
    procedure SetPoolSize(const Value: integer);
    procedure SetBrowsingPaths(const Value: TD7StringList);
    procedure SetFileAliases(const Value: TD7StringList);
  protected
    FAppIDParam : string;
    FPool : TObjectPool;
    FWebModuleClass : TWebModuleClass;
    function AcquireWebModule : TWebModule; virtual;
    procedure ReleaseWebModule(wm : TWebModule); virtual;
    procedure Loaded; override;
    {$IFDEF INTRAWEB110}
      {$IFDEF INTRAWEB120}
    function InternalOnInvalidCommand(ARequest: THttpRequest; AResponse: THttpReply): Boolean; virtual;
      {$ELSE}
    function InternalOnInvalidCommand(ARequest: TWebRequest; AResponse: TWebResponse): Boolean; virtual;
      {$ENDIF}
    {$ELSE}
    procedure InternalOnInvalidCommand(ARequest: TWebRequest; AResponse: TWebResponse; AMsg: string); virtual;
    {$ENDIF}
    procedure InternalOnCreateWebModule(Sender : TObject; var AObject : TObject); virtual;
    procedure InternalOnDestroyWebModule(Sender : TObject; var AObject : TObject); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure HandlePathBrowse(ARequest : TWebRequest; AResponse : TWebResponse; APathIndex : integer); virtual;
    procedure HandleFileAliases(ARequest : TWebRequest; AResponse : TWebResponse; APathIndex : integer); virtual;

    {$IFDEF INTRAWEB110}
    property URLResponder: TIWURLResponderEvent read GetURLResponder;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure RegisterWebModuleClass(WebModuleClass : TWebModuleClass); virtual;
    function CurrentAppIDAsQueryParam(const QueryString : string='') : string; virtual;
    procedure CurrentAppIDAsCookie(Response : TWebResponse); virtual;
    function CurrentAppID : string; virtual;
    {$IFDEF INTRAWEB80}
    function LockSession(ARequest : TWebRequest) : TIWApplication; overload;
    function LockSession(const AppID : string) : TIWApplication; overload;
    function UnlockSession(ARequest : TWebRequest) : TIWApplication; overload;
    function UnlockSession(const AppID : string) : TIWApplication; overload;
    {$ELSE}
    function FindSession(ARequest : TWebRequest) : TIWApplication; overload;
    function FindSession(const AppID : string) : TIWApplication; overload;
    {$ENDIF}
  published
    property AppIDParam : string read FAppIDParam write FAppIDParam;
    property Active : boolean read FActive write SetActive;
    property PoolSize : integer read GetPoolSize write SetPoolSize;
    property OnGetWebModuleClass : TGetWebModuleClassEvent read FOnGetWebModuleClass write FOnGetWebModuleClass;
    //property BrowsingPaths : TStringList read FBrowsingPaths write SetBrowsingPaths;
    property FileAliases : TD7StringList read FFileAliases write SetFileAliases;
  end;

{$IFDEF VER130}
  TWebModuleHelper = class(TWebModule)
  protected
    function DispatchAction(Request: TWebRequest;
      Response: TWebResponse): Boolean;
  end;
{$ENDIF}

implementation

{$IFNDEF VER130}
uses WebCntxt;
{$ENDIF}

{ TWebModuleHelper }

{$IFDEF VER130}
function TWebModuleHelper.DispatchAction(Request: TWebRequest;
  Response: TWebResponse): Boolean;
begin

end;
{$ENDIF}

{ TArcIWWebModuleBridge }

function TArcIWWebModuleBridge.AcquireWebModule: TWebModule;
begin
  Result := TWebModule(FPool.Acquire);  
end;

constructor TArcIWWebModuleBridge.Create(AOwner: TComponent);
begin
  if not (AOwner is TIWServerControllerBase) then
    raise Exception.Create('TArcIWInteropController can only be used on an IntraWeb ServerController.');
  inherited;
  FPool := TObjectPool.Create;
  FPool.PoolSize := 20;
  FAppIDParam := '_AppID';
  FBrowsingPaths := TD7StringList.Create;
  FFileAliases := TD7StringList.Create;
end;

destructor TArcIWWebModuleBridge.Destroy;
begin
  FPool.Free;
  FBrowsingPaths.Free;
  FFileAliases.Free;
  inherited;
end;

function TArcIWWebModuleBridge.GetPoolSize: integer;
begin
  Result := FPool.PoolSize;
end;

procedure TArcIWWebModuleBridge.InternalOnCreateWebModule(Sender: TObject;
  var AObject: TObject);
begin
  AObject := FWebModuleClass.Create(nil);
end;

procedure TArcIWWebModuleBridge.InternalOnDestroyWebModule(Sender: TObject;
  var AObject: TObject);
begin
  AObject.Free;
end;

{$IFDEF INTRAWEB110}

  {$IFDEF INTRAWEB120}
function TArcIWWebModuleBridge.IWURLResponderEventRequest(AApplication: TIWApplication;
  ARequest: THttpRequest; AResponse: THttpReply): Boolean;
  {$ELSE}
function TArcIWWebModuleBridge.IWURLResponderEventRequest(AApplication: TIWApplication;
  ARequest: TWebRequest; AResponse: TWebResponse): Boolean;
  {$ENDIF}
begin
  Result:= InternalOnInvalidCommand(ARequest,AResponse);
end;

function TArcIWWebModuleBridge.GetURLResponder: TIWURLResponderEvent;
begin
  Result:= TIWServerControllerBase(Owner).UnhandledRequest as TIWURLResponderEvent;
end;

{$ENDIF}

{$IFDEF INTRAWEB110}
  {$IFDEF INTRAWEB120}
function TArcIWWebModuleBridge.InternalOnInvalidCommand(ARequest: THttpRequest; AResponse: THttpReply): Boolean;
  {$ELSE}
function TArcIWWebModuleBridge.InternalOnInvalidCommand(ARequest: TWebRequest; AResponse: TWebResponse): Boolean;
  {$ENDIF}
{$ELSE}
procedure TArcIWWebModuleBridge.InternalOnInvalidCommand(ARequest: TWebRequest; AResponse: TWebResponse;
  AMsg: string);
{$ENDIF}
var
  wm : TWebModule;
  idx : integer;
  //WebModuleList : TWebModuleList;
  {$IFNDEF VER130}
  WebContext : TWebContext;
  {$ENDIF}
  //Session : TWebSession;
  bHandled : boolean;
begin
  bHandled := False;
  if FActive then
  begin
    idx := FBrowsingPaths.IndexOfName(ARequest.PathInfo);
    if idx < 0 then
    begin
      idx := FFileAliases.IndexOfName(ARequest.PathInfo);
      if idx < 0 then
      begin
        wm := AcquireWebModule;
        try
          {$IFNDEF VER130} {TODO -oPlp -cConversion : Check this}
//          WebContext := TWebContext.Create(nil,ARequest,AResponse,nil);  // May need to implement session and webmodulelist here eventually.
          try
          {$ENDIF}
            {$IFDEF VER130}
            bHandled := TWebModuleHelper(wm).DispatchAction(ARequest,AResponse);
            {$ELSE} {TODO -oPlp -cConversion : Check this}
//            bHandled := (wm as IWebRequestHandler).HandleRequest(ARequest, AResponse);
            {$ENDIF}
          {$IFNDEF VER130}
          finally
//            WebContext.Free; {TODO -oPlp -cConversion : Check this}
          end;
          {$ENDIF}
        finally
          ReleaseWebModule(wm);
        end;
      end else
      begin
        HandleFileAliases(ARequest, AResponse, idx);
        bHandled := True;
      end;
    end else
    begin
      HandlePathBrowse(ARequest, AResponse, idx);
      bHandled := True;
    end;
  end  else
    AResponse.StatusCode := 404; {TODO -oPlp -cConversion : Check this}
  if not bHandled then
  begin
    {$IFNDEF INTRAWEB110}
    if Assigned(_OnInvalidCommand) then
      _OnInvalidCommand(ARequest, AResponse, AMsg);
    {$ENDIF}
  end;
  {$IFDEF INTRAWEB110}
  Result:= bHandled;
  {$ENDIF}
end;

procedure TArcIWWebModuleBridge.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    {$IFDEF INTRAWEB110}
    if Assigned(TIWServerControllerBase(Owner).UnhandledRequest) then
      raise Exception.Create('TArcIWWebModuleBridge cannot work when ServerController has UnhandledRequest assigned!');

     TIWServerControllerBase(Owner).UnhandledRequest:= TIWURLResponderEvent.Create(Self);
     URLResponder.OnRequest:= IWURLResponderEventRequest;
    {$ELSE}
    _OnInvalidCommand := TIWServerControllerBase(Owner).OnInvalidCommand;
    TIWServerControllerBase(Owner).OnInvalidCommand := InternalOnInvalidCommand;
    {$ENDIF}
    if Active then
      Start;
  end;
end;

procedure TArcIWWebModuleBridge.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
end;

procedure TArcIWWebModuleBridge.ReleaseWebModule(wm: TWebModule);
begin
  FPool.Release(wm);
end;

procedure TArcIWWebModuleBridge.SetActive(const Value: boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;

    if (csLoading in ComponentState) or (csDesigning in ComponentState) then
      exit;
      
    if FActive then
      Start
    else
      Stop;
  end;
end;

procedure TArcIWWebModuleBridge.SetPoolSize(const Value: integer);
begin
  FPool.PoolSize := Value;
end;

procedure TArcIWWebModuleBridge.Start;
begin
  if Assigned(FOnGetWebModuleClass) then
    FOnGetWebModuleClass(Self, FWebModuleClass);
  if FWebModuleClass = nil then
    raise Exception.Create('TArcIWWebModuleBridge must have a registered WebModule.');
  FPool.OnCreateObject := InternalOnCreateWebModule;
  FPool.OnDestroyObject := InternalOnDestroyWebModule;
  FPool.Start;
end;

procedure TArcIWWebModuleBridge.Stop;
begin
  FPool.Stop;
end;

procedure TArcIWWebModuleBridge.RegisterWebModuleClass(
  WebModuleClass: TWebModuleClass);
begin
  FWebModuleClass := WebModuleClass;
end;

function TArcIWWebModuleBridge.CurrentAppID: string;
var
  app : TIWApplication;
begin
  app := GGetWebApplicationThreadVar;
  if app = nil then
    raise Exception.Create('Cannot get appid from outside an IW application thread.');
  Result := app.AppID;
end;

function TArcIWWebModuleBridge.CurrentAppIDAsQueryParam(const QueryString : string=''): string;
begin
  Result := FAppIDParam+'='+CurrentAppID;
  if QueryString = '' then
    Result := '?'+Result
  else
    Result := QueryString+'&'+Result;
end;

procedure TArcIWWebModuleBridge.CurrentAppIDAsCookie(
  Response: TWebResponse);
begin
  with Response.Cookies.Add do
  begin
    // JS: W1058 Implicit string cast with potential data loss from 'string' to 'AnsiString'
    Name := FAppIDParam;
    // JS: W1058 Implicit string cast with potential data loss from 'string' to 'AnsiString'
    Value := CurrentAppId;
    Path := '/';
  end;
end;

{$IFDEF INTRAWEB80}
function TArcIWWebModuleBridge.UnlockSession(ARequest: TWebRequest): TIWApplication;
var
  sAppID : string;
begin
  sAppID := '';
  sAppID := ARequest.QueryFields.Values[FAppIDParam];
  if sAppID = '' then
    sAppID := ARequest.ContentFields.Values[FAppIDParam];
  if sAppID = '' then
    sAppID := ARequest.CookieFields.Values[FAppIDParam];
  if sAppID = '' then
    raise EArcIWBridgeSessionNotFound.Create('AppID not found in request.');
  Result := UnlockSession(sAppID);
end;

function TArcIWWebModuleBridge.UnlockSession(const AppID: string): TIWApplication;
var
  ls : TList;
  i : integer;
begin
  Result := nil;
  ls := GSessions.LockList;
  try
    for i := 0 to ls.Count-1 do
      if TIWApplication(ls[i]).AppID = AppID then
      begin
        Result := ls[i];
        break;
      end;
    {$IFDEF INTRAWEB80}
    if Result <> nil then
      Result.Unlock;
    {$ENDIF}
  finally
    GSessions.UnlockList;
  end;
  if Result = nil then
    raise EArcIWBridgeSessionNotFound.Create('Session not found for AppID: '+AppID+'. Session may have expired.');
end;

function TArcIWWebModuleBridge.LockSession(ARequest: TWebRequest): TIWApplication;
var
  sAppID : string;
begin
  sAppID := '';
  sAppID := ARequest.QueryFields.Values[FAppIDParam];
  if sAppID = '' then
    sAppID := ARequest.ContentFields.Values[FAppIDParam];
  if sAppID = '' then
    sAppID := ARequest.CookieFields.Values[FAppIDParam];
  if sAppID = '' then
    raise EArcIWBridgeSessionNotFound.Create('AppID not found in request.');
  Result := LockSession(sAppID);
end;

function TArcIWWebModuleBridge.LockSession(const AppID: string): TIWApplication;
var
  ls : TList;
  i : integer;
begin
  Result := nil;
  ls := GSessions.LockList;
  try
    for i := 0 to ls.Count-1 do
      if TIWApplication(ls[i]).AppID = AppID then
      begin
        Result := ls[i];
        break;
      end;
    if Result <> nil then
      Result.Lock;
  finally
    GSessions.UnlockList;
  end;
  if Result = nil then
    raise EArcIWBridgeSessionNotFound.Create('Session not found for AppID: '+AppID+'. Session may have expired.');
end;
{$ELSE}
function TArcIWWebModuleBridge.FindSession(ARequest: TWebRequest): TIWApplication;
var
  sAppID : string;
begin
  sAppID := '';
  sAppID := ARequest.QueryFields.Values[FAppIDParam];
  if sAppID = '' then
    sAppID := ARequest.ContentFields.Values[FAppIDParam];
  if sAppID = '' then
    sAppID := ARequest.CookieFields.Values[FAppIDParam];
  if sAppID = '' then
    raise EArcIWBridgeSessionNotFound.Create('AppID not found in request.');
  Result := FindSession(sAppID);
end;

function TArcIWWebModuleBridge.FindSession(const AppID: string): TIWApplication;
var
  ls : TList;
  i : integer;
begin
  Result := nil;
  ls := GSessions.LockList;
  try
    for i := 0 to ls.Count-1 do
      if TIWApplication(ls[i]).AppID = AppID then
      begin
        Result := ls[i];
        break;
      end;
  finally
    GSessions.UnlockList;
  end;
  if Result = nil then
    raise EArcIWBridgeSessionNotFound.Create('Session not found for AppID: '+AppID+'. Session may have expired.');
end;

{$ENDIF}

procedure TArcIWWebModuleBridge.SetBrowsingPaths(const Value: TD7StringList);
begin
  FBrowsingPaths.Assign(Value);
end;

procedure TArcIWWebModuleBridge.SetFileAliases(const Value: TD7StringList);
begin
  FFileAliases.Assign(Value);
end;

procedure TArcIWWebModuleBridge.HandlePathBrowse(ARequest: TWebRequest;
  AResponse: TWebResponse; APathIndex: integer);
  procedure ExtractTemplateFromResource;
  var
    rs : TResourceStream;
    fs : TFileStream;
  begin
    rs := TResourceStream.Create(HInstance, 'pathbrowsetemplate', RT_RCDATA);
    try
      fs := TFileStream.Create(gsAppPath+'PathBrowseTemplate.html',fmCreate or fmShareExclusive);
      try
        rs.Position := 0;
        fs.CopyFrom(rs,rs.Size);
      finally
        fs.Free;
      end;
    finally
      rs.Free;
    end;
  end;
  function BuildBrowseFromTemplate : string;
  begin

  end;
begin
  if not FileExists(gsAppPath+'PathBrowseTemplate.html') then
    ExtractTemplateFromResource;
  if FileExists(gsAppPath+'PathBrowseTemplate.html') then
  begin
    AResponse.Content := BuildBrowseFromTemplate;
  end else
    raise Exception.Create('No path browsing template file.');
end;

procedure TArcIWWebModuleBridge.HandleFileAliases(ARequest: TWebRequest;
  AResponse: TWebResponse; APathIndex: integer);
var
  fs : TFileStream;
  sFile : string;
begin
  sFile := FFileAliases.ValueFromIndex[APathIndex];

  fs := TFileStream.Create(sFile,fmShareDenyNone and fmOpenRead);
  try
    // JS: W1058 Implicit string cast with potential data loss from 'string' to 'AnsiString'
    AResponse.ContentType := GetMIMETypeFromFile(sFile);
    AResponse.CustomHeaders.Add('content-disposition: attachment;filename="'+ExtractFilename(sFile)+'"');
    AResponse.SendStream(fs);
  finally
    //fs.Free;  SendStream should free this filestream!
  end;
end;
(*
{ TWebSession }

constructor TWebSession.Create(SessionID: string);
begin
  FSessionID := SessionID;
end;

function TWebSession.GetSessionID: String;
begin
  result := FSessionID;
end;

function TWebSession.GetTimeoutMinutes: Integer;
begin
  result := 99999;
end;

function TWebSession.GetValue(const AName: String): Variant;
begin
  raise Exception.Create('WebSessionNotImplemented');
end;

procedure TWebSession.SetTimeoutMinutes(AValue: Integer);
begin

end;

procedure TWebSession.SetValue(const AName: String; const AValue: Variant);
begin

end;

procedure TWebSession.Terminate;
begin

end;

procedure TWebSession.UpdateResponse(AResponse: TWebResponse);
begin

end;
*)

end.
