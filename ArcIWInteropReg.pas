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

unit ArcIWInteropReg;

interface

uses
  Classes, SysUtils,ArcIWEliteResources,
  {$IFDEF Linux}{$ELSE}Windows,{$ENDIF}
  {$IFDEF Linux}QForms,{$ELSE}Forms,{$ENDIF}
  {$IFDEF Linux}QDialogs,{$ELSE}Dialogs,{$ENDIF}
  {$IFDEF Linux}QControls,{$ELSE}Controls,{$ENDIF}
  InGlobal, IWDsnWizard, ToolsApi;



{$I IWCompilerDefines.inc}

type
  TArcIWNotifierObject = class(TIWNotifierObject)
  private
    function GetIsBCB: boolean;
  public
    property IsBCB : boolean read GetIsBCB;
  end;

  TArcWebModuleCreator = class(TArcIWNotifierObject, IOTACreator, IOTAModuleCreator)
  private
    FOwner: IOTAModule;
  public
  // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
  // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
    constructor Create (AOwner : IOTAModule);
  end;

  TArcWebModuleFile = class(TArcIWNotifierObject, IOTAFile)
  public
    function GetSource: string;
    function GetAge: TDateTime;
  end;

  TArcWebModuleFileHeader = class(TArcIWNotifierObject, IOTAFile)
  public
    function GetSource: string;
    function GetAge: TDateTime;
  end;

  TArcWebModuleFormFile = class(TArcIWNotifierObject, IOTAFile)
  public
    function GetSource: string;
    function GetAge: TDateTime;
  end;

  TArcWebModuleWizard = class(TArcIWNotifierObject, IOTAWizard, IOTAProjectWizard, IOTARepositoryWizard, IUnknown)
  public
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;

  { IOTAProjectWizard }
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: {$IFDEF VCL6ORABOVE}Cardinal{$ELSE}HICON{$ENDIF};
    procedure Execute;
  end;

procedure Register;

var
  GIncludeSOAP : boolean;
  WizardServ: IOTAWizardServices;
  WebModuleWizard: Integer;

implementation

uses ArcIWInteropController, ArcIWInteropRenderer, ArcIWInteropReceiver,
  ArcIWWebModuleBridge;

procedure Register;
begin
  RegisterComponents( 'IWES NonVisual',
                      [TArcIWInteropController, TArcIWInteropRenderer,
                      TArcIWInteropReceiver, TArcIWWebModuleBridge]);
end;

{ TArcWebModuleWizard }

procedure TArcWebModuleWizard.Execute;
var
  Proj : IOTAProject;
  ModuleServices : IOTAModuleServices;
  sUnit, sClass, sFile : string;
begin
  GIncludeSOAP := MessageDlg('Would you like to turn your web module into a SOAP server?',mtConfirmation, [mbYes, mbNo],0) = mrYes;

  ModuleServices := (BorlandIDEServices as IOTAModuleServices);
  ModuleServices.GetNewModuleAndClassName('ArcIWWebModule', sUnit, sClass, sFile);
  Proj := GetCurrentProject;
  if Proj <> nil then
  begin
    ModuleServices.CreateModule(TArcWebModuleCreator.Create(Proj));
    {if GIsBCB then // Not sure what this was for???
    begin
      TIWApplicationWizard.SetupProject(LProj);
    end;}
  end;
end;

function TArcWebModuleWizard.GetAuthor: string;
begin
  Result := 'Arcana Technologies';
end;

function TArcWebModuleWizard.GetComment: string;
begin
  Result := 'Add WebModule to your IntraWeb Project';
end;

function TArcWebModuleWizard.GetIDString: string;
begin
  Result := 'Arcana.InteropWebModule';
end;

function TArcWebModuleWizard.GetName: string;
begin
  Result := 'New WebModule';
end;

function TArcWebModuleWizard.GetPage: string;
begin
  Result := 'IntraWeb';
end;

function TArcWebModuleWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

function TArcWebModuleWizard.GetGlyph: {$IFDEF VCL6ORABOVE}Cardinal{$ELSE}HICON{$ENDIF};
{$IFDEF Linux}
{Var
  ResInfo: Cardinal;}
{$ENDIF}
begin
  {$IFDEF Linux}
  // ResInfo := FindResource(hInstance, 'IW_FORM_XPM', RT_RCDATA);
  result := 0; //LoadResource(hInstance, ResInfo);
  {$ELSE}
  Result := LoadIcon(hInstance, 'IW_FORM_ICON');
  {$ENDIF}
end;

{ TArcWebModuleCreator }

constructor TArcWebModuleCreator.Create(AOwner: IOTAModule);
begin
  inherited Create;
  FOwner := AOWner;
end;

procedure TArcWebModuleCreator.FormCreated(
  const FormEditor: IOTAFormEditor);
begin

end;

function TArcWebModuleCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TArcWebModuleCreator.GetCreatorType: string;
begin
  Result := ToolsAPI.sForm;
end;

function TArcWebModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TArcWebModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TArcWebModuleCreator.GetFormName: string;
begin
  Result := '';
end;

function TArcWebModuleCreator.GetImplFileName: string;
begin
  Result := MakeFileName('WebModuleUnit', InGlobal.iif(IsBCB, 'cpp', 'pas'));
end;

function TArcWebModuleCreator.GetIntfFileName: string;
begin
  if IsBCB then begin
    Result := MakeFileName('WebModule', 'h');
  end else begin
    Result := '';
  end;
end;

function TArcWebModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TArcWebModuleCreator.GetOwner: IOTAModule;
begin
  Result := FOwner;
end;

function TArcWebModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TArcWebModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TArcWebModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TArcWebModuleCreator.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TArcWebModuleFormFile.Create;
end;

function TArcWebModuleCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TArcWebModuleFile.Create;
end;

function TArcWebModuleCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  if IsBCB then begin
    Result := TArcWebModuleFileHeader.Create;
  end else begin
  end;
end;

{ TArcWebModuleFile }

function TArcWebModuleFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TArcWebModuleFile.GetSource: string;
var
  LText: string;
  LResInstance: THandle;
  LHRes: HRSRC;
  LFormFile: String;
  LPlatform: String;
begin
  {$IFDEF Linux}
  LFormFile := '*.xfm';
  LPlatform := 'clx';
  {$ENDIF}
  {$IFNDEF Linux}
  LFormFile := '*.dfm';
  LPlatform := 'vcl';
  {$ENDIF}
  LResInstance := FindResourceHInstance(HInstance);
  {if IsBCB then
    LHRes := FindResource(LResInstance, 'ARCIW_WEBMODULE_FILE', RT_RCDATA);
  else}
  if GIncludeSOAP then
    LHRes := FindResource(LResInstance, 'ARCIW_SOAPMODULE_FILE', RT_RCDATA)
  else
    LHRes := FindResource(LResInstance, 'ARCIW_WEBMODULE_FILE', RT_RCDATA);

  LText := PChar(LockResource(LoadResource(LResInstance, LHRes)));
  SetLength(LText, SizeOfResource(LResInstance, LHRes));
  if IsBCB then begin
    Result := Format(LText, [LPlatform, LFormFile]);
  end else begin
    Result := Format(LText, [LFormFile]);
  end;
end;

{ TArcWebModuleFileHeader }

function TArcWebModuleFileHeader.GetAge: TDateTime;
begin
  Result := -1;
end;

function TArcWebModuleFileHeader.GetSource: string;
var
  LText: string;
  LResInstance: THandle;
  LHRes: HRSRC;
  LPlatform: String;
begin
  {$IFDEF Linux}
  LPlatform := 'Q';
  {$ENDIF}
  {$IFNDEF Linux}
  LPlatform := '';
  {$ENDIF}
  LResInstance := FindResourceHInstance(HInstance);
  if GIncludeSOAP then
    LHRes := FindResource(LResInstance, 'ARCIW_SOAPMODULE_HEADER', RT_RCDATA)
  else
    LHRes := FindResource(LResInstance, 'ARCIW_WEBMODULE_HEADER', RT_RCDATA);
  LText := PChar(LockResource(LoadResource(LResInstance, LHRes)));
  SetLength(LText, SizeOfResource(LResInstance, LHRes));
  Result := Format(LText, [LPlatform, LPlatform, LPlatform]);
  // ShowMessage(result);
end;

{ TArcWebModuleFormFile }

function TArcWebModuleFormFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TArcWebModuleFormFile.GetSource: string;
var
  LText: string;
  LResInstance: THandle;
  LHRes: HRSRC;
begin
  LResInstance := FindResourceHInstance(HInstance);
  if GIncludeSOAP then
    LHRes := FindResource(LResInstance, 'ARCIW_SOAPMODULE_FORM', RT_RCDATA)
  else
    LHRes := FindResource(LResInstance, 'ARCIW_WEBMODULE_FORM', RT_RCDATA);

  LText := PChar(LockResource(LoadResource(LResInstance, LHRes)));
  SetLength(LText, SizeOfResource(LResInstance, LHRes));
  Result := LText;
end;

{ TArcIWNotifierObject }

function TArcIWNotifierObject.GetIsBCB: boolean;
begin
  Result := (AnsiPos('BCB', ExtractFilename(Application.exename)) > 0);
end;

initialization
  BorlandIDEServices.QueryInterface(IOTAWizardServices, WizardServ);

  if WizardServ <> nil then 
    WebModuleWizard := WizardServ.AddWizard(TArcWebModuleWizard.Create);
  WizardServ := nil;

finalization
  // Cleanup Wizard registrations
  BorlandIDEServices.QueryInterface(IOTAWizardServices, WizardServ);
  if WizardServ <> nil then
    WizardServ.RemoveWizard(WebModuleWizard);
end.
