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

unit ArcIWLanguageEditors;

interface

{$I IntraWebVersion.inc}

uses Windows, Controls, Classes, SysUtils, ShellApi, Dialogs, ComCtrls, IWControl,
  IWForm, {$IFDEF VER130}Forms, DsgnIntf,{$ELSE}DesignIntf, DesignEditors,{$ENDIF} Menus
  {$IFDEF INTRAWEB60}, IWBaseControlInterface {$ENDIF}
  {$IFDEF INTRAWEB70}, IWBaseInterfaces {$ENDIF}
  , ArcIWStringContainer;

type
  TLanguageComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

  TControlTranslationsEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetValue: String; override;
  end;

  TTranslationTableEditor = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetValue: String; override;
  end;

  TLanguageStringEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: String; override;
    procedure SetValue(const Value: String); override;
  end;

procedure Register;

implementation

uses ArcIWLanguages, ArcIWLanguageTypes, ArcIWTranslationTableEditor,
  ArcIWTranslationEditor, ArcIWSelectLanguage, ArcIWAutoTranslate, TypInfo,
  ArcIWTranslationProgress, Forms;

procedure Register;
begin
  RegisterComponentEditor(TArcIWTranslator, TLanguageComponentEditor);
  RegisterPropertyEditor(TypeInfo(TTranslationTable),TArcIWTranslator,'Languages',TTranslationTableEditor);
  RegisterPropertyEditor(TypeInfo(TControlTranslations),TArcIWTranslator,'Translations',TControlTranslationsEditor);
  RegisterPropertyEditor(TypeInfo(TArcIWLanguageString),TArcIWTranslator,'',TLanguageStringEditor);
end;

{$IFDEF VER130}
function Supports(const Instance: TObject; const IID: TGUID): Boolean; overload;
var
  LUnknown: IUnknown;
  Intf : IUnknown;
begin
  Result := (Instance <> nil) and
            ((Instance.GetInterface(IUnknown, LUnknown) and Supports(LUnknown, IID, Intf)) or
             Instance.GetInterface(IID, Intf));
end;

function _GetPropList5(TypeInfo: PTypeInfo; out PropList: PPropList): Integer;
begin
  Result := GetTypeData(TypeInfo)^.PropCount;
  if Result > 0 then
  begin
    GetMem(PropList, Result * SizeOf(Pointer));
    GetPropInfos(TypeInfo, PropList);
  end;
end;

function GetPropList5(AObject: TObject; out PropList: PPropList): Integer;
begin
  Result := _GetPropList5(PTypeInfo(AObject.ClassInfo), PropList);
end;
{$ENDIF}

procedure DoEditTranslations(EditComponent : TArcIWTranslator; Dsn : IDesigner);
var
  frm : TfrmTranslation;
begin
  frm := TfrmTranslation.Create(nil);
  try
    {$IFNDEF VER130}
    frm.FDesignerForm := TIWForm(Dsn.Root);
    {$ELSE}
    frm.FDesignerForm := TIWForm(Designer.GetRoot);
    {$ENDIF}
    frm.FEditComponent := EditComponent;
    frm.Translations.Assign(frm.FEditComponent.Translations);

    frm.RefreshListBox;

    if frm.ShowModal=mrOK then
    begin
      EditComponent.Translations.Assign(frm.Translations);
      Dsn.Modified;
      //Designer.Modified;// not sure if this is a suitable replacement.
      //Modified;
    end;
  finally
    frm.free;
  end;
end;

procedure DoEditLanguageGroups(EditComponent : TArcIWTranslator; Dsn : IDesigner);
var
  i,j, k : integer;
  ti : TTreeNode;
begin
  with TfrmTranslationTable.Create(nil) do
  try
    for i := 0 to EditComponent.Languages.Count-1 do
    begin
      ti := tvGroups.Items.Add(nil,EditComponent.Languages[i].Language);
      for j := 0 to EditComponent.Languages[i].HTTPLanguageCodes.Count-1 do
      begin
        k := 0;
        while k < lbCodes.Items.Count do
        begin
          if Copy(lbCodes.Items[k],1,2)= EditComponent.Languages[i].HTTPLanguageCodes[j] then
          begin
            tvGroups.Items.AddChild(ti,lbCodes.Items[k]);
            lbCodes.Items.Delete(k);
          end else
            inc(k);            
        end;
      end;
    end;
    if ShowModal=mrOK then
    begin
      EditComponent.Languages.Clear;
      for i := 0 to tvGroups.Items.Count-1 do
      begin
        if tvGroups.Items[i].Level = 0 then
        begin
          with TTransTableItem(EditComponent.Languages.Add) do
          begin
            Language := tvGroups.Items[i].Text;
            for k := 0 to tvGroups.Items[i].Count -1 do
              HTTPLanguageCodes.Add(Copy(tvGroups.Items[i].Item[k].Text,1,2));
          end;
        end;
      end;
      Dsn.Modified;
    end;
  finally
    free;
  end;
end;

procedure DoLanguagePreview(Component : TArcIWTranslator);
var
  i : integer;
begin
  with TfrmSelectLang.Create(nil) do
  try
    for i := 0 to Component.Languages.Count-1 do
      lbLangs.Items.Add(Component.Languages[i].Language);

    if ShowModal=mrOK then
    begin
      if lbLangs.ItemIndex >=0 then
      begin
        for i := 0 to Component.Translations.Count -1 do
          Prop_SetPropertyValue(TIWForm(Component.Owner).FindComponent(Component.Translations[i].ControlName), Component.Translations[i].PropName, Component.Translations[i].Translations.TextByLanguage(lbLangs.Items[lbLangs.ItemIndex]));
      end;
      TIWForm(Component.Owner).Invalidate;
    end;
  finally
    free;
  end;
end;

{ TLanguageComponentEditor }

procedure TLanguageComponentEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0: DoEditTranslations(TArcIWTranslator(Component),Self.Designer);
    1: DoEditLanguageGroups(TArcIWTranslator(Component), Self.Designer);
    2: ;
    3: DoLanguagePreview(TArcIWTranslator(Component));
    4: ;
    5: ShellExecute(0, 'open', 'http://www.arcanatech.com/','','',SW_SHOW);
  end;
end;

function TLanguageComponentEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := 'Edit Translations';
    1: Result := 'Edit Language Groups';
    2: Result := '-';
    3: Result := 'Language Preview';
    4: Result := '-';
    5: Result := 'Visit arcanatech.com';
  end;
end;

function TLanguageComponentEditor.GetVerbCount: Integer;
begin
  result := 6;
end;

{ TControlTranslationsEditor }

procedure TControlTranslationsEditor.Edit;
begin
  inherited;
  DoEditTranslations(TArcIWTranslator(Self.GetComponent(Self.PropCount-1)),Self.Designer);
end;

function TControlTranslationsEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paFullWidthName];
end;

function TControlTranslationsEditor.GetValue: String;
begin
  Result := 'Translations';
end;

{ TTranslationTableEditor }

procedure TTranslationTableEditor.Edit;
begin
  inherited;
  DoEditLanguageGroups(TArcIWTranslator(Self.GetComponent(Self.PropCount-1)),Self.Designer);
end;

function TTranslationTableEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paFullWidthName];
end;

function TTranslationTableEditor.GetValue: String;
begin
  Result := 'Translation Table';
end;

{ TLanguageStringEditor }

function TLanguageStringEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paAutoUpdate]
end;

function TLanguageStringEditor.GetValue: String;
begin
  Result := Self.GetStrValue;
end;

procedure TLanguageStringEditor.GetValues(Proc: TGetStrProc);
var
  i : integer;
  EditComponent : TArcIWTranslator;
begin
  EditComponent := TArcIWTranslator(Self.GetComponent(Self.PropCount-1));
  for i := 0 to EditComponent.Languages.Count-1 do
  begin
    proc(EditComponent.Languages[i].Language);
  end;
end;

procedure TLanguageStringEditor.SetValue(const Value: String);
begin
  SetStrValue(Value);
end;

end.
