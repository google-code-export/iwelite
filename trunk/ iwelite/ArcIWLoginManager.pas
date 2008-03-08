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

unit ArcIWLoginManager;

interface

uses
  SysUtils, Classes, SyncObjs, IWApplication, IWAppForm, ArcIWControlBase,
  IWServerControllerBase, IWTypes;

type
  TArcIWAuthEvent = procedure(const AUserName, APassword: String;
      var PrimaryKey : Integer; var AValid: Boolean) of object;

  TArcIWLoginOptEvent = (loUserLogin, loGettingKeys);
  TArcIWLoginManager = class(TComponent)
  private
    FOldAuthRequest : TOnAuthRequest;
    CS : TCriticalSection;
    FUserKeys : TStringList;
    FOnAuthenticateUser: TArcIWAuthEvent;
    FOptimization: TArcIWLoginOptEvent;
    FAutoSynchronize: boolean;
    FLoginEveryPage: boolean;
    procedure _AuthRequestEvent(const AUserName: string; const APassword: string;
      var AValid: Boolean);
  protected
    procedure Loaded; override;
  public
    function Password(aWebApplication : TIWApplication) : string;
    function Username(aWebApplication : TIWApplication) : string;
    function UserKey(aWebApplication : TIWApplication) : Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnAuthenticateUser : TArcIWAuthEvent read FOnAuthenticateUser write FOnAuthenticateUser;
    property Optimization : TArcIWLoginOptEvent read FOptimization write FOptimization default loGettingKeys;
    property AutoSynchronize : boolean read FAutoSynchronize write FAutoSynchronize default true;
    property LoginEveryPage : boolean read FLoginEveryPage write FLoginEveryPage;
  end;

procedure Register;

implementation

uses InCoderMIME;

procedure Register;
begin
  RegisterComponents('IW ControlPack', [TArcIWLoginManager]);
end;

function MimeDecodeString(str : string) : string;
begin
  Result := TIdDecoderMIME.DecodeString(str)
end;

{ TArcIWLoginManager }

function TArcIWLoginManager.UserKey(aWebApplication : TIWApplication) : Integer;
var
  idx : integer;
  user : string;
begin
  Result := -1;

  user := Username(aWebApplication);
  CS.Enter;
  try
    idx := FUserKeys.IndexOf(user);
    if idx >= 0 then
      Result := Integer(FUserKeys.Objects[idx]);
  finally
    CS.Leave;
  end;
end;

constructor TArcIWLoginManager.Create(AOwner: TComponent);
begin
  if not (AOwner is TIWServerControllerBase) then
    raise Exception.Create('This component is only valid for use on IntraWeb ServerControllers');
  inherited Create(AOwner);
  FUserKeys := TStringList.Create;
  CS := TCriticalSection.Create;

  FAutoSynchronize := True;
  FOptimization := loGettingKeys;
end;

destructor TArcIWLoginManager.Destroy;
begin
  CS.Free;
  FUserKeys.Free;
  inherited;
end;

function TArcIWLoginManager.Username(aWebApplication : TIWApplication) : string;
var
  DecodedString : string;
  EncodedString : string;
begin
  if (not Assigned(aWebApplication)) or (not Assigned(aWebApplication.Request)) then
    raise Exception.Create('Session data has not yet been initialized');
  EncodedString := aWebApplication.Request.Authorization;
  EncodedString := Copy(EncodedString, 7, Length(EncodedString));
  DecodedString := MimeDecodeString(EncodedString);
  Result := Copy(DecodedString,1,Pos(':', DecodedString) -1);
end;

function TArcIWLoginManager.Password(aWebApplication : TIWApplication) : string;
var
  EncodedString : string;
begin
  if (not Assigned(aWebApplication)) or (not Assigned(aWebApplication.Request)) then
    raise Exception.Create('Session data has not yet been initialized');
  EncodedString := aWebApplication.Request.Authorization;
  EncodedString := Copy(EncodedString, 7, Length(EncodedString));
  Result := MimeDecodeString(EncodedString);
  Delete(Result,1,Pos(':', Result));
end;

procedure TArcIWLoginManager.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FOldAuthRequest := TIWServerControllerBase(Owner).OnAuthRequest;
    TIWServerControllerBase(Owner).OnAuthRequest := _AuthRequestEvent;

    if FOptimization = loGettingKeys then
      FUserKeys.Sorted := True;
  end;
end;

procedure TArcIWLoginManager._AuthRequestEvent(const AUserName,
  APassword: string; var AValid: Boolean);
var
  iKey : integer;
  bSync : boolean;
begin
  bSync := FAutoSynchronize;
  if bSync then
    CS.Enter;
  try
    if Assigned(FOldAuthRequest) then
      FOldAuthRequest(AUserName, APassword, AValid);

    if Assigned(FOnAuthenticateUser) then
    begin
      FOnAuthenticateUser(AUsername, APassword, iKey, AValid);
      FUserKeys.AddObject(AUsername, TObject(iKey));

      if not FLoginEveryPage then
        TIWServerControllerBase(Owner).AuthList.Add(AUserName+'='+APassword);
    end;
  finally
    if bSync then
      CS.Leave;
  end;
end;

end.
