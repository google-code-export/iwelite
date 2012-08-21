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

unit ArcIWPageFileUpload;

interface

{$I IntraWebVersion.inc}

uses
  {$IFDEF VSNET}System.ComponentModel, System.Drawing, {$ENDIF}
  {$IFDEF VSNET}
  IWNetClasses,
  {$ELSE}
  SysUtils, Classes,
  {$IFDEF Linux}
  QTypes, IWCLXComponent, IWCLXClasses,
  {$ELSE}
  IWVCLComponent, {$IFNDEF INTRAWEB120} IWVCLClasses, {$ENDIF}
  {$ENDIF}
  {$IFDEF Linux}QControls, {$ELSE}Controls, {$ENDIF}
  {$ENDIF}
  {$IFDEF Linux}QGraphics, {$ELSE}Graphics, {$ENDIF} ArcIWEliteResources,
  IWTypes, IWHTMLTag, IWControl, IWScriptEvents, IWRenderContext, IWBaseInterfaces, SyncObjs, IWServer,
  IWColor, IWCompButton, IWFileReference, IWGLobal, IWFont, IWForm, IWServerControllerBase, HTTPApp
  {$IFDEF INTRAWEB120}, IW.Http.Request, IW.Http.Reply {$ENDIF}, ArcCommon;

type
  TValidRID = class(TObject)
    RID : Int64;
    IP : string;
    Stamp : string;
  end;

  TArcFileInfo = class(TObject)
  private
    FRID : Int64;
    FFilename : string;
    FFileData: TMemoryStream;
    FContentType: string;
    FStamp: string;
  public
    property RID : Int64 read FRID;
    property Stamp : string read FStamp;
    property Filename : string read FFilename;
    property ContentType : string read FContentType;
    property FileData : TMemoryStream read FFileData;
    {$IFDEF INTRAWEB120}
    constructor Create(ARequest : THttpRequest); virtual;
    {$ELSE}
    constructor Create(ARequest : TWebRequest); virtual;
    {$ENDIF}
    destructor Destroy; override;
  end;

  TArcIWFileUploadCatcher = class(TComponent)
  private
    FFileList : TList;
    FFileListCS : TCriticalSection;
    FServerController: TIWServerControllerBase;

    procedure SetServerController(const Value: TIWServerControllerBase);
  protected
    _OnBeforeDispatch : TOnDispatch;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$IFDEF INTRAWEB120}
    procedure OnBeforeDispatch(Request: THttpRequest; Response: THttpReply); virtual;
    {$ELSE}
    procedure OnBeforeDispatch(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean); virtual;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function WaitAndSave(ValidRID : TValidRID; FilePath : string; Timeout : integer = 10) : string;
  published
    property ServerController : TIWServerControllerBase read FServerController write SetServerController;
  end;

  TArcUploadCompleteEvent = procedure(Sender : TObject; Filename : string) of object; 

  TArcIWPageFileUpload = class(TIWCustomControl, IIWInputControl, IIWMultiInputControl)
  private
    FCatcher : TArcIWFileUploadCatcher;
    FRID : TValidRID;
    FContextRID : Int64;
    FContextStamp : string;
    FFilePath: string;
    FIsReady : boolean;
    FUploadTimeout: integer;
    FOnUploadComplete: TArcUploadCompleteEvent;
  protected
    procedure Submit(const AValue: string); override;
  public
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    constructor Create(AOwner: TComponent); override;
    procedure GetInputControlNames(ANames: TStringList); override;
    function IsForThisControl(AName: string): Boolean; override;
    procedure SetValue(const AValue: string);
    procedure SetValueByName(const AName: string; const AValue: string);
    function SubmitJavascript(Button : string = ''; WaitTimeout : Integer = 12000) : string ;
  published
    property FilePath : string read FFilePath write FFilePath;
    property UploadTimeout : integer read FUploadTimeout write FUploadTimeout default 10;
    property OnUploadComplete : TArcUploadCompleteEvent read FOnUploadComplete write FOnUploadComplete;
  end;

  TArcIWPageFileUploadButton = class(TIWButton)
  private
    FPageFileUpload: TArcIWPageFileUpload;
    procedure SetPageFileUpload(const Value: TArcIWPageFileUpload);
  published
  published
    property PageFileUpload : TArcIWPageFileUpload read FPageFileUpload write SetPageFileUpload;
  end;

implementation



uses
  DateUtils, IWBaseHTMLControl;

var
  _CatcherRefCount : integer;
  _LastRID : Int64;
  _RIDCS : TCriticalSection;
  _RIDListCS : TCriticalSection;
  _RIDList : TList;

const
  I_CatcherWaitSleep : integer = 1000;
  S_FileUploadPath : string = '/_pageFileUpl'; // these two paths must be the same length to work in the current logic.
  S_FilePagePath   : string = '/_PageFileFrm';

function GenerateUploadFormHTML(HTMLName, Path, RID, Stamp, Width, Height : string) : string;
var
  CRLF : string;
begin
  if GServerController.DebugHTML then
    CRLF := #13#10;
  Result :=
    '<html>'+CRLF+
    '<script language="javascript">'+CRLF+
    'var RID = '''+RID+''';'+CRLF+
    'var Stamp = '''+Stamp+''';'+CRLF+
    'function DoOnLoad() {  '+CRLF+
    'parent.document.SubmitForm.'+HTMLName+'_RID.value = RID;'+CRLF+
    'parent.document.SubmitForm.'+HTMLName+'_STAMP.value = Stamp;'+CRLF+
    //'parent.document.SubmitForm['''+HTMLName+'_RID''].value = RID;'+CRLF+
    //'parent.document.SubmitForm['''+HTMLName+'_STAMP''].value = Stamp;'+CRLF+
    //'window.parent.document.'+HTMLName+'_IsReady.value = true;'+CRLF+
    'parent.document.SubmitForm.'+HTMLName+'_ISREADY.value = "true";'+CRLF+
    '}'+CRLF+
    '</script>'+CRLF+
    '<body margin=0 onLoad="DoOnLoad();">'+CRLF+
    '<form name="PageUpload" id="PageUpload" method="Post" enctype="multipart/form-data" action="'+Path+'">'+CRLF+
    '<input type="hidden" name="RID" value="'+RID+'">'+CRLF+
    '<input type="hidden" name="stamp" value="'+stamp+'">'+CRLF+
    '<input type="hidden" name="HTMLName" value="'+HTMLName+'">'+CRLF+
    '<input type="hidden" name="Width" value="'+Width+'">'+CRLF+
    '<input type="hidden" name="Height" value="'+Height+'">'+CRLF+
    '<input type="file" name="filedata" style="position:absolute;left:0px;top:0px;width:'+width+';height:'+height+'">'+CRLF+
    '</form></body></html>';
end;

function NextRID : Int64;
begin
  _RIDCS.Enter;
  try
    inc(_LastRID);
    Result := _LastRID;
  finally
    _RIDCS.Leave;
  end;
end;

{$IFDEF INTRAWEB120}
function AddValidRID(ARequest : THttpRequest) : TValidRID;
{$ELSE}
function AddValidRID(ARequest : TWebRequest) : TValidRID;
{$ENDIF}
begin
  Result := TValidRID.Create;
  _RIDListCS.Enter;
  try
    _RIDList.Add(Result);
  finally
    _RIDListCS.Leave;
  end;
  Result.RID := NextRID;
  Result.IP := ARequest.RemoteAddr;
  DateTimeToString(Result.Stamp,'yyyymmddhhnnsszzz',Now);
end;

function FindValidRID(RID : Int64; Stamp : string) : TValidRID;
var
  i: Integer;
begin
  Result := nil;
  _RIDListCS.Enter;
  try
    for i := _RIDList.Count - 1 downto 0 do
    begin
      if (TValidRID(_RIDList[i]).RID = RID) and
         (TValidRID(_RIDList[i]).Stamp = Stamp) then
      begin
        Result := TValidRID(_RIDList[i]);
        break;
      end;
    end;
  finally
    _RIDListCS.Leave;
  end;
end;

procedure ClearValidRIDs;
var
  i: Integer;
begin
  _RIDListCS.Enter;
  try
    for i := 0 to _RIDList.Count - 1 do
      TValidRID(_RIDList[i]).Free;
    _RIDList.Clear;
  finally
    _RIDListCS.Leave;
  end;
end;

function IsValidRID(RID : Int64; Stamp : string) : boolean;
var
  i: Integer;
  bFound : boolean;
begin
  Result := RID <= _LastRID;
  if Result then
  begin
    bFound := False;
    for i := 0 to _RIDList.Count - 1 do
    begin
      if (TValidRID(_RIDList[i]).RID = RID) and
         (TValidRID(_RIDList[i]).Stamp = Stamp) then
      begin
        bFound := True;
        break;
      end;
    end;
    Result := bFound;
  end;
end;

function FindCatcher : TArcIWFileUploadCatcher;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to GServerController.ComponentCount - 1 do
    if GServerController.Components[i] is TArcIWFileUploadCatcher then
    begin
      Result := TArcIWFileUploadCatcher(GServerController.Components[i]);
      break;
    end;
  if Result = nil then
  begin
    Result := TArcIWFileUploadCatcher.Create(GServerController);
  end;
end;

{ TArcIWPageFileUpload }

constructor TArcIWPageFileUpload.Create(AOwner: TComponent);
begin
  inherited;
  Width := 100;
  Height := 21;
  if not (csDesigning in ComponentState) then
    FCatcher := FindCatcher;
  FFilePath := 'Files\Upload\';
  FUploadTimeout := 10;
end;

procedure TArcIWPageFileUpload.GetInputControlNames(ANames: TStringList);
begin
  ANames.Add(HTMLName);
  ANames.Add(HTMLName+'_RID');
  ANames.Add(HTMLName+'_STAMP');
  ANames.Add(HTMLName+'_ISREADY');
end;

function TArcIWPageFileUpload.SubmitJavascript(Button : string = ''; WaitTimeout : Integer = 12000): string;
var
  CRLF: string;
begin
  if GServerController.DebugHTML then
    CRLF := #13#10;
  Result := '';
  //if Button <> '' then
  //  Result := 'IWTop().FindElem('''+HTMLName+''').disabled = true;';
  Result := Result+
    'document.SubmitForm.'+HTMLName+'_ISREADY.value = "false";' +CRLF+
    'ReadyChecker = document.SubmitForm.'+HTMLName+'_ISREADY;'+CRLF+
    'SubmitButton = "'+Button+'";'+CRLF+
    //'alert('+HTMLName+'.RID);'+
    HTMLName+'.document.PageUpload.submit();'+CRLF+
    'IntervalID = window.setInterval(''checkFileSubmit();'',500);'+CRLF+
    'return false;'+CRLF;
end;

function TArcIWPageFileUpload.IsForThisControl(AName: string): Boolean;
begin
  Result := (AName = HTMLName) or (AName = HTMLName+'_RID') or (AName = HTMLName+'_STAMP') or (AName = HTMLName+'_ISREADY');
end;

function TArcIWPageFileUpload.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
var
  tag : TIWHTMLTag;
  CRLF : string;
begin
  TIWPageContext40(AContext.PageContext).AddScriptFile('/js/ArcIWPageFileUpload.js');
  //TIWPageContext40(AContext.PageContext).AddToInitProc('var '+HTMLName+'_ISREADY = new BoolByRef();'+CRLF);
  if GServerController.DebugHTML then
    CRLF := #13#10;
    
  if FRID = nil then
    FRID := AddValidRID(AContext.WebApplication.Request);
  tag := TIWHTMLTag.CreateTag('iframe');
  tag.AddStringParam('name',HTMLName);
  tag.AddStringParam('id',HTMLName);
  tag.AddStringParam('src',AContext.WebApplication.AppURLBase+S_FilePagePath+'?RID='+IntToStr(FRID.RID)+'&stamp='+FRID.Stamp+'&width='+IntToStr(width)+'&height='+IntToStr(height)+'&HTMLName='+HTMLName);
  tag.AddIntegerParam('WIDTH', Width);
  tag.AddIntegerParam('HEIGHT', Height);
  tag.AddIntegerParam('FRAMEBORDER', StrToInt('0'));
  tag.AddIntegerParam('MARGINWIDTH', 0);
  tag.AddIntegerParam('MARGINHEIGHT', 0);
  tag.Add('SCROLLING=NO');
  tag.AddStringParam('onLoad','javascript:if (parentNode.style.visibility == ''hidden'') {document.SubmitForm.'+HTMLName+'_ISREADY.value = ''false''}}');

  Result := tag;
  //tag := TIWHTMLTag.CreateTag('script');
  //tag.AddStringParam('language','javascript');
  //tag.Contents.AddText('var '+HTMLName+'_IsReady = new BoolByRef();'+CRLF);

  //Result.Contents.AddTagAsObject(tag);
end;

procedure TArcIWPageFileUpload.SetValue(const AValue: string);
begin
end;

procedure TArcIWPageFileUpload.SetValueByName(const AName, AValue: string);
var
  rid : TValidRID;
  sFile: string;
begin
  if (not Visible) then
    exit;
  if AName = HTMLName+'_RID' then
    FContextRID := StrToIntDef(AValue,-1);
  if AName = HTMLName+'_STAMP' then
    FContextStamp := AValue;
  if AName = HTMLName+'_ISREADY' then
    FIsReady := StrToBool(AValue);

  if (FContextRID > 0) and (FContextStamp <> '') and FIsReady then
  begin
    rid := FindValidRID(FContextRID, FContextStamp);
    if rid <> nil then
    begin
      sFile := FindCatcher.WaitAndSave(rid,FFilePath,FUploadTimeout);
      if (sFile <>'') and (Assigned(FOnUploadComplete)) then
      begin
        FOnUploadComplete(Self,sFile);
      end;
      FContextRID := 0;
      FContextStamp := '';
    end;
  end;
end;

procedure TArcIWPageFileUpload.Submit(const AValue: string);
begin
  inherited;
  
end;

{ TArcIWFileUploadCatcher }

constructor TArcIWFileUploadCatcher.Create(AOwner: TComponent);
begin
  if not (AOwner is TIWServerControllerBase) then
    raise Exception.Create('Catcher must be created on a TIWServerController');
  if _CatcherRefCount > 0 then
    raise Exception.Create('Multiple Catchers can not be created for a single server controller.');

  inherited;
  ServerController := TIWServerControllerBase(AOwner);
  FFileListCS := TCriticalSection.Create;
  FFIleList := TList.Create;
end;

destructor TArcIWFileUploadCatcher.Destroy;
var
  i: Integer;
begin
  ServerController := nil;
  for i := 0 to FFileList.Count - 1 do
    FreeMem(FFileList[i]);
  FFileListCS.Free;
  FFileList.Free;
  inherited;
end;

procedure TArcIWFileUploadCatcher.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FServerController) then
  begin
    ServerController := nil;
  end;
end;

{$IFDEF INTRAWEB120}
procedure TArcIWFileUploadCatcher.OnBeforeDispatch(Request: THttpRequest; Response: THttpReply);
{$ELSE}
procedure TArcIWFileUploadCatcher.OnBeforeDispatch(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
{$ENDIF}
var
  iLen : integer;
  fi: TArcFileInfo;
  sURLBase: string;
  Content: string;
begin
  {$IFDEF INTRAWEB120}
  if (Request.HttpMethod = hmPost) then
  {$ELSE}
  if (Request.MethodType = mtPost) then
  {$ENDIF}
  begin
    iLen := Length(S_FileUploadPath);
    if (Copy(Request.PathInfo,iLen-iLen,iLen) = S_FileUploadPath) then // fixes check in the case of URLBase or Dll.
    begin
      fi := TArcFileInfo.Create(Request);
      FFileListCS.Enter;
      try
        FFileList.Add(fi);
      finally
        FFileListCS.Leave;
      end;
      {$IFNDEF INTRAWEB120}
      Handled := True;
      {$ENDIF}

      sURLBase := Copy(Request.PathInfo,1,Length(Request.PathInfo)-iLen);
      if (sURLBase = '') and (GserverController.URLBase <> '') then
        sURLBase := GserverController.URLBase;
      Content  :=
        GenerateUploadFormHTML(Request.ContentFields.Values['HTMLName'],
          sURLBase+S_FileUploadPath,
          Request.ContentFields.Values['RID'],
          Request.ContentFields.Values['stamp'],
          Request.ContentFields.Values['width'],
          Request.ContentFields.Values['height']);
       {$IFDEF INTRAWEB120}
       Response.WriteString(Content);
       {$ELSE}
       Response.Content:= Content;
       {$ENDIF}
    end;
  end;
  {$IFDEF INTRAWEB120}
  if (Request.HttpMethod = hmGet) then
  {$ELSE}
  if (Request.MethodType = mtGet) then
  {$ENDIF}
  begin
    iLen := Length(S_FilePagePath);
    if (Copy(Request.PathInfo,iLen-iLen,iLen) = S_FilePagePath) then // fixes check in the case of URLBase or Dll.
    begin
      sURLBase := Copy(Request.PathInfo,1,Length(Request.PathInfo)-iLen);
      if (sURLBase = '') and (GserverController.URLBase <> '') then
        sURLBase := GserverController.URLBase;

      Content :=
        GenerateUploadFormHTML(Request.QueryFields.Values['HTMLName'],
          sURLBase+S_FileUploadPath,
          Request.QueryFields.Values['RID'],
          Request.QueryFields.Values['stamp'],
          Request.QueryFields.Values['width'],
          Request.QueryFields.Values['height']);
       {$IFDEF INTRAWEB120}
       Response.WriteString(Content);
       {$ELSE}
       Response.Content:= Content;
       Handled := True;
       {$ENDIF}
    end;
  end;
end;

procedure TArcIWFileUploadCatcher.SetServerController(
  const Value: TIWServerControllerBase);
begin
  if Value = FServerController then
    exit;

  if (FServerController <> nil) then
  begin
    FServerController.RemoveFreeNotification(Self);
    dec(_CatcherRefCount);
    FServerController.OnBeforeDispatch := _OnBeforeDispatch;
  end;

  FServerController := Value;

  if (FServerController <> nil) then
  begin
    FServerController.FreeNotification(Self);
    inc(_CatcherRefCount);
    _OnBeforeDispatch := FServerController.OnBeforeDispatch;
    FServerController.OnBeforeDispatch := OnBeforeDispatch;
  end;
end;

function TArcIWFileUploadCatcher.WaitAndSave(ValidRID: TValidRID;
  FilePath: string; Timeout : integer = 10): string;
var
  i: Integer;
  fi : TArcFileInfo;
  dtStart : TDateTime;
begin
  Result := '';
  fi := nil;
  dtStart := Now;
  while fi = nil do
  begin
    FFileListCS.Enter;
    try
      for i := FFileList.Count - 1 downto 0 do
      begin
        if (TArcFileInfo(FFileList[i]).FRID = ValidRID.RID) and
           (TArcFileInfo(FFileList[i]).Stamp = ValidRID.Stamp) then
        begin
          fi := TArcFileInfo(FFileList[i]);
          break;
        end;
      end;
    finally
      FFileListCS.Leave;
    end;
    if fi = nil then
    begin
      Sleep(I_CatcherWaitSleep);
      if MinutesBetween(dtStart,Now) > Timeout then
        break;
    end;
  end;
  if fi <> nil then
  begin
    if fi.Filename <> '' then
    begin
      Result := FilePath+ExtractFileName(fi.Filename);
      fi.FFileData.SaveToFile(Result);
    end;
    
    FFileListCS.Enter;
    try
      i := FFileList.IndexOf(fi);
      if i >= 0 then
        FFileList.Delete(i);
    finally
      FFileListCS.Leave;
    end;
    fi.Free;
  end;
end;

{ TArcFileInfo }

{$IFDEF INTRAWEB120}
constructor TArcFileInfo.Create(ARequest : THttpRequest);
{$ELSE}
constructor TArcFileInfo.Create(ARequest : TWebRequest);
{$ENDIF}
var
  iRID : integer;
  sStamp : string;
begin
  iRID := StrToIntDef(ARequest.QueryFields.Values['RID'],-1);
  if iRID < 0 then
    iRID := StrToIntDef(ARequest.ContentFields.Values['RID'],-1);

  sStamp := ARequest.QueryFields.Values['Stamp'];
  if sStamp = '' then
    sStamp := ARequest.ContentFields.Values['Stamp'];

  if not IsValidRID(iRID, sStamp) then
    raise Exception.Create('Invalid RID');

  inherited Create;

  FFileData := TMemoryStream.Create;

  FRID := iRID;
  FStamp := sStamp;

  if ARequest.Files.Count > 0 then
  begin
    FFilename := ARequest.Files[0].FileName;
    FContentType := ARequest.Files[0].ContentType;
    {$IFDEF INTRAWEB120}
    ARequest.Files[0].SaveToStream(FFileData);
    {$ELSE}
    ARequest.Files[0].Stream.Position := 0;
    FFileData.CopyFrom(ARequest.Files[0].Stream,ARequest.Files[0].Stream.Size);
    {$ENDIF}
  end else
  begin
    FFilename := '';
    FContentType := '';
  end;
end;

destructor TArcFileInfo.Destroy;
begin
  FFileData.Free;
  inherited;
end;

{ TArcIWPageFileUploadButton }

procedure TArcIWPageFileUploadButton.SetPageFileUpload(const Value: TArcIWPageFileUpload);
begin
  if FPageFileUpload = Value then
    exit;

  if FPageFileUpload <> nil then
    FPageFileUpload.RemoveFreeNotification(Self);

  FPageFileUpload := Value;

  if FPageFileUpload <> nil then
  begin
    FPageFileUpload.FreeNotification(Self);
    ScriptEvents.Values['onClick'] := FPageFileUpload.SubmitJavascript(HTMLName);
  end else
    ScriptEvents.Clear;
end;

initialization
  _CatcherRefCount := 0;
  _LastRID := 0;
  _RIDCS := TCriticalSection.Create;
  _RIDListCS := TCriticalSection.Create;
  _RIDList := TList.Create;
  TIWServer.AddInternalFile('IW_JS_ArcIWPageFileUpload', '/js/ArcIWPageFileUpload.js');

finalization
  ClearValidRIDs;
  _RIDCS.Free;
  _RIDListCS.Free;
  _RIDList.Free

end.
