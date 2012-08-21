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

unit ArcIWInteropController;

interface

{$I IntrawebVersion.inc}

uses
  SysUtils, Classes, SyncObjs, ActnList, IWForm, IWApplication, InURI,
  IWServerControllerBase, IWBaseForm, HTTPApp, ArcIWInteropCommon {$IFDEF INTRAWEB110} ,IWURLResponder {$ENDIF}
  {$IFDEF INTRAWEB120}, IW.Http.Request, IW.Http.Reply {$ENDIF};

type
  {$IFDEF INTRAWEB120}
  TRegisterEvent = procedure(ASender : TObject; Request : THttpRequest; var Allow : boolean) of object;
  {$ELSE}
  TRegisterEvent = procedure(ASender : TObject; Request : TWebRequest; var Allow : boolean) of object;
  {$ENDIF}
  TActionValueEvent = procedure(Sender : TObject; Action : TAction; Value : String) of object;

  TArcIWInteropController = class(TActionList)
  private
    _OnNewSession : TOnNewSessionEvent;
    FRegPath: string;
    FServerController : TIWServerControllerBase;
    FOnBeforeRegister: TRegisterEvent;
    FOnBeforeAction: TActionValueEvent;
    FOnAfterAction: TActionValueEvent;
    {$IFDEF INTRAWEB110}
      {$IFDEF INTRAWEB120}
      function IWURLResponderEventRequest(AApplication: TIWApplication; ARequest: THttpRequest; AResponse: THttpReply): Boolean;
      {$ELSE}
      function IWURLResponderEventRequest(AApplication: TIWApplication; ARequest: TWebRequest; AResponse: TWebResponse): Boolean;
      {$ENDIF}
    function GetURLResponder: TIWURLResponderEvent;
    {$ELSE}
    _OnInvalidCommand : TOnInvalidCommandEvent;
    _OnReentry : TOnReEntryEvent;
    {$ENDIF}
  protected
    procedure Loaded; override;
    procedure InternalOnNewSession(ASession: TIWApplication; var VMainForm: TIWBaseForm); virtual;
    {$IFDEF INTRAWEB110}
    property URLResponder: TIWURLResponderEvent read GetURLResponder;
    {$ELSE}
    procedure InternalOnInvalidCommand(ARequest: TWebRequest; AResponse: TWebResponse; AMsg: string); virtual;
    procedure InternalOnReEntry(ASession: TIWApplication);
    {$ENDIF}
    {$IFDEF INTRAWEB120}
    function ListActionNames(Request : THttpRequest) : string;
    {$ELSE}
    function ListActionNames(Request : TWebRequest) : string;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function RenderJavaScript(CompName : string) : string; virtual;
    procedure ClickActionByName(AName, AValue : string); virtual;
    function DefaultMoveToForm(WebApplication : TIWApplication; FormClass : TComponentClass) : TComponent;
  published
    property RegPath : string read FRegPath write FRegPath;
    property OnBeforeRegister : TRegisterEvent read FOnBeforeRegister write FOnBeforeRegister;
    property OnBeforeAction : TActionValueEvent read FOnBeforeAction write FOnBeforeAction;
    property OnAfterAction : TActionValueEvent read FOnAfterAction write FOnAfterAction;
  end;

implementation

uses {$IFNDEF VER130}StrUtils, {$ENDIF}ArcD5Fix;

{$IFDEF VER130}
function BoolToStr(B: Boolean): string;
const
  cSimpleBoolStrs: array [boolean] of String = ('0', '-1');
begin
  Result := cSimpleBoolStrs[B];
end;
{$ENDIF}

{ TArcIWInteropController }

procedure TArcIWInteropController.ClickActionByName(AName, AValue : string);
var
  i : integer;
begin
  for i := 0 to ActionCount-1 do
    if Actions[i].Name = AName then
    begin
      if TAction(Actions[i]).Enabled then
      begin
        if Assigned(FOnBeforeAction) then
          FOnBeforeAction(Self, TAction(Actions[i]), AValue);
        Actions[i].Execute;
        if Assigned(FOnAfterAction) then
          FOnAfterAction(Self, TAction(Actions[i]), AValue);
      end;
      break;
    end;
end;

constructor TArcIWInteropController.Create(AOwner: TComponent);
begin
  if not (AOwner is TIWServerControllerBase) then
    raise Exception.Create('TArcIWInteropController can only be used on an IntraWeb ServerController.');
  inherited;
  FRegPath := '/INTEROPREG';
  FServerController := TIWServerControllerBase(AOwner);
end;

function TArcIWInteropController.DefaultMoveToForm(
  WebApplication: TIWApplication; FormClass: TComponentClass): TComponent;
var
  bCreate : boolean;
begin
  Result := WebApplication.ActiveForm;
  bCreate := not Assigned(WebApplication.ActiveForm);
  if Assigned(WebApplication.ActiveForm) and (WebApplication.ActiveForm.ClassType <> FormClass) then
  begin
    TIWBaseForm(WebApplication.ActiveForm).Release;
    bCreate := True;
  end;
  if bCreate then
  begin
    Result := TIWBaseForm(FormClass.Create(WebApplication));
    TIWBaseForm(Result).Show;
  end;
end;

destructor TArcIWInteropController.Destroy;
begin
  inherited;
end;

{$IFDEF INTRAWEB110}

  {$IFDEF INTRAWEB120}
function TArcIWInteropController.IWURLResponderEventRequest(AApplication: TIWApplication; ARequest: THttpRequest;
  AResponse: THttpReply): Boolean;
  {$ELSE}
function TArcIWInteropController.IWURLResponderEventRequest(AApplication: TIWApplication; ARequest: TWebRequest;
  AResponse: TWebResponse): Boolean;
  {$ENDIF}
var
  s: string;
begin
  Result:= False;
  if ARequest.PathInfo = FRegPath then
  begin
    {$IFDEF INTRAWEB120}
    AResponse.WriteString('Interop Registration Set');
    s := ListActionNames(ARequest);
    AResponse.AddCookie('InteropReg',Ifthen(s='','""',s),'',0 {?});
    {$ELSE}
    AResponse.Content := 'Interop Registration Set';
    with AResponse.Cookies.Add do
    begin
      Name := 'InteropReg';
      s := ListActionNames(ARequest);
      Value := Ifthen(s='','""',s);
    end;
    {$ENDIF}
    Result:= True;
  end
  else
  begin
    raise Exception.CreateFmt('Invalid command: "%s"',[ARequest.PathInfo]);
  end;
end;

function TArcIWInteropController.GetURLResponder: TIWURLResponderEvent;
begin
  Result:= TIWServerControllerBase(Owner).UnhandledRequest as TIWURLResponderEvent;
end;

{$ELSE}

procedure TArcIWInteropController.InternalOnInvalidCommand(
  ARequest: TWebRequest; AResponse: TWebResponse; AMsg: string);
var
  s : string;
begin
  if ARequest.PathInfo = FRegPath then
  begin
    AResponse.Content := 'Interop Registration Set';
    with AResponse.Cookies.Add do
    begin
      Name := 'InteropReg';
      s := ListActionNames(ARequest);
      Value := Ifthen(s='','""',s);
    end;
  end else
  begin
    if Assigned(_OnInvalidCommand) then
      _OnInvalidCommand(ARequest, AResponse, AMsg)
    else
      raise Exception.Create(AMsg);
  end;
end;

procedure TArcIWInteropController.InternalOnReEntry(ASession: TIWApplication);
var
  sAction, sValue : string;
begin
  if Assigned(_OnReentry) then
    _OnReentry(ASession);

  if ASession <> nil then
  begin
    sAction := ASession.Request.QueryFields.Values['Action'];
    if sAction <> '' then
    begin
      sValue := ASession.Request.QueryFields.Values['Value'];
      ClickActionByName(sAction, sValue);
    end;
  end;
end;

{$ENDIF}

procedure TArcIWInteropController.InternalOnNewSession(
  ASession: TIWApplication; var VMainForm: TIWBaseForm);
var
  sAction, sValue : string;
begin
  if Assigned(_OnNewSession) then
    _OnNewSession(ASession, VMainForm);
  if VMainForm = nil then
  begin
    sAction := ASession.Request.QueryFields.Values['Action'];
    if sAction <> '' then
    begin
      sValue := TInURI.URLDecode(ASession.Request.QueryFields.Values['Value']);
      ClickActionByName(sAction, sValue);
      VMainForm := TIWBaseForm(ASession.ActiveForm);
    end;
  end;
end;

{$IFDEF INTRAWEB120}
function TArcIWInteropController.ListActionNames(Request : THttpRequest): string;
{$ELSE}
function TArcIWInteropController.ListActionNames(Request : TWebRequest): string;
{$ENDIF}
var
  i : integer;
  ai : TAction;
  bOK : boolean;
begin
  bOK := True;
  Result := '';

  if Assigned(FOnBeforeRegister) then
    FOnBeforeRegister(Self, Request, bOK);
  if bOK then
    for i := 0 to ActionCount-1 do
    begin
      ai := TAction(Actions[i]);
      if ai.Visible then
        Result := Result+IfThen(i>0,',"^','"^')+ai.Category+'^|^'+ai.Caption+'^|^'+ai.Name+'^|^'+IntToStr(ai.Tag)+'^|^'+BoolToStr(ai.Enabled)+'^"';
    end;
end;

procedure TArcIWInteropController.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    _OnNewSession := TIWServerControllerBase(Owner).OnNewSession;
    TIWServerControllerBase(Owner).OnNewSession := InternalOnNewSession;

   {$IFDEF INTRAWEB110}

   if Assigned(TIWServerControllerBase(Owner).UnhandledRequest) then
     raise Exception.Create('TArcIWInteropController cannot work when ServerController has UnhandledRequest assigned!');

    TIWServerControllerBase(Owner).UnhandledRequest:= TIWURLResponderEvent.Create(Self);
    URLResponder.OnRequest:= IWURLResponderEventRequest;

    {$ELSE}

    _OnInvalidCommand := TIWServerControllerBase(Owner).OnInvalidCommand;
    TIWServerControllerBase(Owner).OnInvalidCommand := InternalOnInvalidCommand;

    _OnReentry := TIWServerControllerBase(Owner).OnReEntry;
    TIWServerControllerBase(Owner).OnReEntry := InternalOnReEntry;

    {$ENDIF}
  end;
end;

function TArcIWInteropController.RenderJavaScript(CompName : string): string;
var
  i : integer;
begin
  Result := #13#10+
    'whichAppAmI="'+TIWServerControllerBase(Owner).AppName+'";'#13#10#13#10;

  for i := 0 to ActionCount-1 do
  begin
    Result := Result +
              'function Goto_'+Actions[i].Name+'(val) {'+#13#10+
              '  return SubmitClickConfirm("'+CompName+'","'+Actions[i].Name+'~"+val, false, "");'+#13#10+
              '}'#13#10#13#10;
  end;
end;

end.
