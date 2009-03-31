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

unit ArcIWTranslationEditor;

interface

uses
  Windows, Messages, {$IFNDEF VER130}Variants, DesignIntf, DesignEditors, {$ELSE}
  DsgnIntf,{$ENDIF} Classes, SysUtils, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ArcIWLanguages, ArcIWLanguageTypes, IWControl, IWForm,
  Buttons, ArcIWTranslationEditorInputForm, IWFileReference;

type
  TfrmTranslation = class(TForm)
    Panel7: TPanel;
    Panel8: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    lbControls: TListBox;
    Panel3: TPanel;
    pnlEdit: TPanel;
    tvLangs: TTreeView;
    pnlProperties: TPanel;
    cbProperties: TComboBox;
    sbAddEdit: TSpeedButton;
    sbRemove: TSpeedButton;
    Panel6: TPanel;
    sbControlAdd: TSpeedButton;
    sbControlDelete: TSpeedButton;
    sbExport: TSpeedButton;
    sbImport: TSpeedButton;
    sbSubmitTranslation: TSpeedButton;
    sbTranslate: TSpeedButton;
    Splitter: TSplitter;
    Label1: TLabel;
    Label3: TLabel;
    procedure lbControlsClick(Sender: TObject);
    procedure cbPropertiesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AddEditItemClick(Sender: TObject);
    procedure RefreshTreeView;
    procedure RefreshListBox;
    procedure RemoveItemClick(Sender: TObject);
    procedure tvLangsChange(Sender: TObject; Node: TTreeNode);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure pnlPropertiesResize(Sender: TObject);
    procedure sbControlAddClick(Sender: TObject);
    procedure sbControlDeleteClick(Sender: TObject);
  private
    CurrentTrans : TLanguageItem;
  public
    FEditComponent : TArcIWTranslator;
    FDesignerForm : TIWForm;
    Translations : TControlTranslations;
  end;

var
  frmTranslation: TfrmTranslation;

implementation

uses TypInfo, Math, ArcIWComponentSelectorForm;

{$R *.dfm}

{$IFDEF VER130}
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

procedure TfrmTranslation.lbControlsClick(Sender: TObject);
  procedure AddClassProperties(obj : TObject; pi : PPropInfo; list : TStrings; Prefix : string);
    procedure WriteStringProps(obj : TObject);
    begin
      list.Add(Prefix+pi.Name);
    end;
    procedure WriteCollection(obj : TObject;PropName : string);
    var
      i, y, iCnt : integer;
      item : TCollectionItem;
      ppl : PPropList;
      sPrefix : string;
    begin
      for y := 0 to TCollection(obj).Count-1 do
      begin
        item := TCollection(obj).Items[y];
        sPrefix := Prefix+PropName+'['+IntToStr(y)+'].';
        {$IFNDEF VER130}
        iCnt := GetPropList(item,ppl);
        {$ELSE}
        iCnt := GetPropList5(item,ppl);
        {$ENDIF}
        for i := 0 to iCnt-1 do
        begin
          if ppl[i].PropType^^.Kind in [tkString, tkLString, tkWString] then
          begin
            if not Prop_AvoidName(ppl[i].Name) then
              list.Add(sPrefix+ppl[i].Name);
          end else
          begin
            if Prop_AcceptedClassType(obj, ppl[i]) then
               begin
                  if not Prop_AvoidClassName(ppl[i].Name) then
                  begin
                    AddClassProperties(item, ppl[i], list, sPrefix)
                  end;
               end ;
          end;
        end;
      end;
    end;
    procedure WritePersistent(obj : TObject;PropName : string);
    var
      i, iCnt : integer;
      ppl : PPropList;
      sPrefix : string;
    begin
      sPrefix := Prefix+PropName+'.';

      {$IFNDEF VER130}
      iCnt := GetPropList(obj,ppl);
      {$ELSE}
      iCnt := GetPropList5(obj,ppl);
      {$ENDIF}
      for i := 0 to iCnt-1 do
      begin
        if ppl[i].PropType^^.Kind in [tkString, tkLString, tkWString] then
        begin
          if not Prop_AvoidName(ppl[i].Name) then
            list.Add(sPrefix+ppl[i].Name);
        end else
        begin
          if Prop_AcceptedClassType(obj, ppl[i]) then
          begin
             if not Prop_AvoidClassName(ppl[i].Name) then
               AddClassProperties(obj, ppl[i], list, sPrefix)
          end;
        end;
      end;
    end;
  var
    o : TObject;
  begin
    o := GetObjectProp(obj,pi.Name);
    if o is TStrings then
      WriteStringProps(o)
    else if o is TCollection then
      WriteCollection(o,pi.Name)
    else if o is TPersistent then
      WritePersistent(o,pi.Name); // Persistent must be handled last.
  end;
var
  ppl : PPropList;
  i, iCount : integer;
  TheComponent : TControl;
begin
  if lbControls.ItemIndex >= 0 then
  begin
    cbProperties.Clear;
    TheComponent := nil;
    if lbControls.Items[lbControls.ItemIndex] = FDesignerForm.Name then
      TheComponent := FDesignerForm
    else
      for i := 0 to FDesignerForm.ComponentCount-1 do
      begin
        if FDesignerForm is TControl then
        begin
          if FDesignerForm.Components[i].Name = lbControls.Items[lbControls.ItemIndex] then
          begin
            TheComponent := TControl(FDesignerForm.Components[i]);
            break;
          end;
        end;
      end;
    if Assigned(TheComponent) then
    begin

      {$IFNDEF VER130}
      iCount := GetPropList(TheComponent,ppl);
      {$ELSE}
      iCount := GetPropList5(TheComponent,ppl);
      {$ENDIF}
      for i := 0 to iCount-1 do
      begin
        if ppl[i].PropType^^.Kind in [tkString, tkLString, tkWString] then
        begin
          if not Prop_AvoidName(ppl[i].Name) then
            cbProperties.Items.Add(ppl[i].Name);
        end else
        begin
          if Prop_AcceptedClassType(TheComponent, ppl[i]) then
          begin
            if not Prop_AvoidClassName(ppl[i].Name) then
              AddClassProperties(TheComponent, ppl[i], cbProperties.Items,'')
          end ;//else showmessage(ppl[i].PropType^^.Name);
        end;
      end;
    end;
    cbProperties.ItemIndex := 0;
    sbControlDelete.Enabled := True;
  end else
  begin
    sbControlDelete.Enabled := False;
  end;
  pnlEdit.Visible := lbControls.ItemIndex >= 0;
  cbPropertiesChange(cbProperties);
end;

procedure TfrmTranslation.cbPropertiesChange(Sender: TObject);
var
  i, j : integer;
  Trans : TLanguageItem;
begin
  Trans := nil;
  RefreshTreeView;
  for i := 0 to Translations.Count-1 do
    if (Translations[i].ControlName = lbControls.Items[lbControls.ItemIndex]) and
       (Translations[i].PropName = cbProperties.Text) then
    begin
      Trans := Translations[i];
      break;
    end;
  if Assigned(Trans) then
  begin
    CurrentTrans := Trans;
    for i := 0 to Trans.Translations.Count-1 do
    begin
      for j := 0 to tvLangs.Items.Count-1 do
      begin
        if (tvLangs.Items[j].Level = 0) and (tvLangs.Items[j].Text = Trans.Translations[i].Language) then
        begin
          tvLangs.Items[j].DeleteChildren;
          tvLangs.Items.AddChild(tvLangs.Items[j],Trans.Translations[i].Text);
          break;
        end;
      end;
    end;
  end else
    CurrentTrans := nil;
  for i := 0 to tvLangs.Items.Count-1 do
    tvLangs.Items[i].Expand(True);
end;

procedure TfrmTranslation.FormCreate(Sender: TObject);
begin
  Translations := TControlTranslations.Create(nil);
end;

procedure TfrmTranslation.FormDestroy(Sender: TObject);
begin
  Translations.Free;
end;

procedure TfrmTranslation.AddEditItemClick(Sender: TObject);
  function GetText(Lang, Text : string) : string;
  begin
    with TfrmEnterText.Create(nil) do
    try
      Caption := Lang+' Translation';
      mText.Lines.Text := Text;
      ShowModal;
      Result := mText.Lines.Text;
    finally
      free;
    end;
  end;
var
  s : string;
  i, idx : integer;
begin
  if not Assigned(tvLangs.Selected) then exit;
  if tvLangs.Selected.Level > 0 then
    tvLangs.Selected := tvLangs.Selected.Parent;
  if tvLangs.Selected.Count > 0 then
    s := tvLangs.Selected.Item[0].Text
  else
    s := '';
  s := GetText(tvLangs.Selected.Text, s); 
  if s <> '' then
  begin
    if not Assigned(CurrentTrans) then
    begin
      CurrentTrans := TLanguageItem(Translations.Add);
      if lbControls.Items[lbControls.ItemIndex] = FDesignerForm.Name then
        CurrentTrans.ControlName := FDesignerForm.Name
      else
        for i := 0 to FDesignerForm.ComponentCount -1 do
          if FDesignerForm.Components[i].Name = lbControls.Items[lbControls.ItemIndex] then
          begin
            CurrentTrans.ControlName := FDesignerForm.Components[i].Name;
            break;
          end;
      CurrentTrans.PropName := cbProperties.Text;
    end;
    idx := CurrentTrans.Translations.IndexOf(tvLangs.Selected.Text);
    if idx > 0 then
      CurrentTrans.Translations.Delete(idx);
    with TTransLanguageItem(CurrentTrans.Translations.Add) do
    begin
      Language := tvLangs.Selected.Text;
      Text := s;
    end;
  end;
  cbPropertiesChange(cbProperties);
end;

procedure TfrmTranslation.RefreshTreeView;
var
  i : integer;
begin
  tvLangs.Items.Clear;
  for i := 0 to FEditComponent.Languages.Count-1 do
    tvLangs.Items.Add(nil,FEditComponent.Languages[i].Language);
  tvLangs.AlphaSort{$IFNDEF VER130}(True){$ENDIF};
end;

procedure TfrmTranslation.RemoveItemClick(Sender: TObject);
var
  idx : integer;
begin
  if not Assigned(tvLangs.Selected) then exit;
  if (tvLangs.Selected.Level = 0) then
    if tvLangs.Selected.Count > 0 then
      tvLangs.Selected := tvLangs.Selected.Item[0]
    else
      exit;
  idx := CurrentTrans.Translations.IndexOf(tvLangs.Selected.Parent.Text);
  if idx >=0 then
    CurrentTrans.Translations.Delete(idx);
  tvLangs.Selected.Delete;
end;

procedure TfrmTranslation.tvLangsChange(Sender: TObject; Node: TTreeNode);
begin
  sbRemove.Enabled := Assigned(tvLangs.Selected);
  sbAddEdit.Enabled := Assigned(tvLangs.Selected);
end;

procedure TfrmTranslation.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift=[]) and (Key=VK_DELETE) then
  begin
    RemoveItemClick(nil);
  end;
end;

procedure TfrmTranslation.pnlPropertiesResize(Sender: TObject);
begin
  cbProperties.Width := pnlProperties.Width - 87;
end;

procedure TfrmTranslation.sbControlAddClick(Sender: TObject);
var
  frm : TfrmSelectComponent;
  i : integer;
  sSel : string;
begin
  frm := TfrmSelectComponent.Create(nil);
  try
    if not Translations.HasControl(FDesignerForm.Name) then
      frm.lbComponents.Items.AddObject(FDesignerForm.Name,FDesignerForm);
    for i := 0 to FDesignerForm.ComponentCount-1 do
    begin
      if not Translations.HasControl(FDesignerForm.Components[i].Name) then
        frm.lbComponents.Items.AddObject(FDesignerForm.Components[i].Name,FDesignerForm.Components[i]);
    end;

    for i := 0 to tvLangs.Items.Count-1 do
      frm.cbLanguage.Items.Add(tvLangs.Items[i].Text);
    frm.cbLanguage.ItemIndex := 0;

    frm.lbComponents.Sorted := True;

    if frm.ShowModal = mrOK then
    begin
      for i := 0 to frm.lbComponents.Items.Count-1 do
      begin
        if frm.lbComponents.Checked[i] then
        begin
          if sSel = '' then
            sSel := frm.lbComponents.Items[i];
          Translations.BuildComponent(FDesignerForm.FindComponent(frm.lbComponents.Items[i]),frm.cbLanguage.Items[frm.cbLanguage.ItemIndex]);
        end;
      end;
      RefreshListBox;
      if sSel <> '' then
      begin
        lbControls.ItemIndex := lbControls.Items.IndexOf(sSel);
        lbControlsClick(lbControls);
      end;
    end;
  finally
    frm.Free;
  end;
end;

procedure TfrmTranslation.RefreshListBox;
var
  sSel : string;
  i : integer;
begin
  if lbControls.ItemIndex >= 0 then
    sSel := lbControls.Items[lbControls.ItemIndex];
  lbControls.Clear;
  for i := 0 to Translations.Count -1 do
  begin
    if lbControls.Items.IndexOf(Translations[i].ControlName) < 0 then
      lbControls.Items.Add(Translations[i].ControlName);
  end;
  if sSel <> '' then
  begin
    i := lbControls.Items.IndexOf(sSel);
    if i >= 0 then
      lbControls.ItemIndex := i
    else
      lbControls.ItemIndex := 0;
  end else
    lbControls.ItemIndex := 0;
  lbControlsClick(lbControls);
end;

procedure TfrmTranslation.sbControlDeleteClick(Sender: TObject);
var
  li : TLanguageItem;
begin
  if lbControls.ItemIndex >= 0 then
  begin
    repeat
      li := Translations.ItemByName(lbControls.Items[lbControls.ItemIndex]);
      if li <> nil then
        li.Free;
    until li = nil;
    RefreshListBox;
  end;
end;

end.
