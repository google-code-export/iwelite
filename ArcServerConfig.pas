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

unit ArcServerConfig;

interface

{$I IntrawebVersion.inc}

uses
  SysUtils, Classes, IniFiles, IWServerControllerBase, ArcIWControlCommon, ArcIWControlBase;

type
  TArcININotifyEvent = procedure(Sender : TObject; Strings : TStrings) of object;
  TArcIWServerConfig = class(TComponent)
  private
    FTemplateFile: string;
    FOnBeforeProcess: TArcININotifyEvent;
    FOnAfterProcess: TArcININotifyEvent;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property TemplateFile : string read FTemplateFile write FTemplateFile;
    property OnBeforeProcess : TArcININotifyEvent read FOnBeforeProcess write FOnBeforeProcess;
    property OnAfterProcess : TArcININotifyEvent read FOnAfterProcess write FOnAfterProcess;
  end;

implementation

uses {$IFNDEF VER130}Variants,{$ENDIF} Graphics, TypInfo;

{ TArcIWServerConfig }

constructor TArcIWServerConfig.Create(AOwner: TComponent);
begin
  if not (AOwner is TIWServerControllerBase) then
    raise EComponentError.Create('TArcIWServerConfig requires an IW ServerController owner.');
  inherited;
end;

destructor TArcIWServerConfig.Destroy;
begin
  inherited;
end;

procedure TArcIWServerConfig.Loaded;
var
  sl : TStringList;
begin
  if not (csDesigning in ComponentState) then
  begin
    if FTemplateFile <> '' then exit;

    sl := TStringList.Create;
    with sl do
    try
      LoadFromFile(FTemplateFile);

      if Assigned(FOnBeforeProcess) then
        FOnBeforeProcess(Self, sl);

      if Values['AppName']<>'' then
        TIWServerControllerBase(Owner).AppName := Values['AppName'];
      if Values['BoundIP']<>'' then
        TIWServerControllerBase(Owner).BoundIP := Values['BoundIP'];
      if Values['Description']<>'' then
        TIWServerControllerBase(Owner).AppName := Values['Description'];
      if Values['ExceptionDisplayMode']<>'' then
        SetEnumProp(Owner,'ExceptionDisplayMode',Values['AppName']);
      {$IFNDEF INTRAWEB110}
      if Values['ExecCmd']<>'' then
        TIWServerControllerBase(Owner).ExecCmd := Values['ExecCmd'];
      {$ENDIF}
      if Values['HistoryEnabled']<>'' then
        TIWServerControllerBase(Owner).HistoryEnabled := CompareText(Values['HistoryEnabled'],'TRUE')=0;
      if Values['HTMLHeaders']<>'' then
        TIWServerControllerBase(Owner).HTMLHeaders.CommaText := Values['HTMLHeaders'];
      {$IFNDEF INTRAWEB110}
      if Values['InvalidCommandURL']<>'' then
        TIWServerControllerBase(Owner).InvalidCommandURL := Values['InvalidCommandURL'];
      {$ENDIF}
      if Values['Port']<>'' then
        TIWServerControllerBase(Owner).Port := StrToIntDef(Values['Port'],80);
      {$IFNDEF INTRAWEB110}
      if Values['RestrictIPs']<>'' then
        TIWServerControllerBase(Owner).RestrictIPs := CompareText(Values['RestrictIPs'],'TRUE')=0;
      {$ENDIF}
      if Values['SessionTimeout']<>'' then
        TIWServerControllerBase(Owner).SessionTimeout := StrToIntDef(Values['SessionTimeout'],10);
      if Values['SessionTrackingMethod']<>'' then
        SetEnumProp(Owner,'SessionTrackingMethod',Values['HTMLHeaders']);
      {$IFNDEF INTRAWEB110}
      if Values['ShowResyncWarning']<>'' then
        TIWServerControllerBase(Owner).ShowResyncWarning := CompareText(Values['ShowResyncWarning'],'TRUE')=0;
      {$ENDIF}
      {$IFNDEF INTRAWEB50}
      if Values['SSLCertificatePassword']<>'' then
        TIWServerControllerBase(Owner).SSLOptions.CertificatePassword := Values['SSLCertificatePassword'];
      if Values['SSLPort']<>'' then
        TIWServerControllerBase(Owner).SSLOptions.Port := StrToIntDef(Values['SSLPort'],0);
      if Values['NonSSLAction']<>'' then
        SetEnumProp(TIWServerControllerBase(Owner).SSLOptions,'NonSSLRequest', Values['NonSSLAction']);
      {$ELSE}
      if Values['SSLCertificatePassword']<>'' then
        TIWServerControllerBase(Owner).SSLCertificatePassword := Values['SSLCertificatePassword'];
      if Values['SSLPort']<>'' then
        TIWServerControllerBase(Owner).SSLPort := StrToIntDef(Values['SSLPort'],0);
      {$ENDIF}
      {$IFNDEF INTRAWEB110}
      if Values['StartCmd']<>'' then
        TIWServerControllerBase(Owner).StartCmd := Values['StartCmd'];
      {$ENDIF}
      if Values['SupportedBrowsers']<>'' then
        SetSetProp(Owner,'SupportedBrowsers',Values['SupportedBrowsers']);
      if Values['TemplateDir']<>'' then
        TIWServerControllerBase(Owner).TemplateDir := Values['TemplateDir'];
      {$IFNDEF INTRAWEB70}
      if Values['TimeoutURL']<>'' then
        TIWServerControllerBase(Owner).TimeoutURL := Values['TimeoutURL'];
      {$ELSE}
        {$IFNDEF INTRAWEB110}
        if Values['TimeoutURL']<>'' then
          TIWServerControllerBase(Owner).SessionTimeoutURL.URL := Values['TimeoutURL'];
        {$ENDIF}
      {$ENDIF}
      if Assigned(FOnAfterProcess) then
        FOnAfterProcess(Self, sl);

    finally
      free;
    end;
  end;
  inherited;
end;

end.
