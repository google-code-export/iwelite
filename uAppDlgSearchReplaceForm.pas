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

unit uAppDlgSearchReplaceForm;
{PUBDIST}
                               
interface

{$I IntrawebVersion.inc}

uses
  IWAppForm, IWApplication, IWTypes, IWCompButton, IWControl, IWCompRectangle, IWColor,
  Classes, Controls, Forms, IWContainer, IWRegion, IWCompEdit, IWCompLabel,
  ArcIWDlgSearchReplace, SysUtils, IWExtCtrls, IWHTMLTag, IWCompCheckbox,
  ArcIWDlgBase{$IFDEF INTRAWEB51}, IWBaseControl, IWVCLBaseControl,
  IWVCLBaseContainer, IWBaseHTMLControl, IWHTMLContainer{$ENDIF};

type
  TCompHelper = class(TArcIWDlgSearchReplace)
  end;

  TfrmDlgSearchReplace = class(TIWAppForm)
    rgnParent: TIWRegion;
    rctCaption: TIWRectangle;
    IWRegion2: TIWRegion;
    IWRegion5: TIWRegion;
    rgnText: TIWRegion;
    rgnButtons: TIWRegion;
    btnCancel: TIWButton;
    btnReplaceAll: TIWButton;
    IWRegion7: TIWRegion;
    IWRegion3: TIWRegion;
    IWRegion4: TIWRegion;
    edtFind: TIWEdit;
    edtReplace: TIWEdit;
    chkCaseSensitive: TIWCheckBox;
    IWRegion1: TIWRegion;
    IWRegion6: TIWRegion;
    IWRegion8: TIWRegion;
    lblReplace: TIWLabel;
    IWRegion9: TIWRegion;
    IWRegion10: TIWRegion;
    lblFind: TIWLabel;
    procedure ReplaceClick(Sender: TObject);
    procedure edtFindHTMLTag(ASender: TObject; ATag: TIWHTMLTag);
    procedure CancelClick(Sender: TObject);
  public
    DlgComponent : TArcIWDlgSearchReplace;
    Text : string;
    constructor Create(AOwner:TComponent);override;
  end;

implementation

uses {$IFDEF CLR} ArcFastStringsDOTNET {$ELSE} ArcFastStrings {$ENDIF},
  IWContainerBorderOptions, Graphics, IWGridCommon;


procedure TfrmDlgSearchReplace.ReplaceClick(Sender: TObject);
begin
  Text := FastReplace(Text,edtFind.Text,edtReplace.Text,chkCaseSensitive.Checked);  
  TCompHelper(DlgComponent).DoReplaceEvent(Text);
  Release;
end;

constructor TfrmDlgSearchReplace.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);

  Name := 'frmDlgSearchReplace';
  Left := 0;
  Top := 0;
  Width := 270;
  Height := 145;
  ConnectionMode := cmAny;
  AllowPageAccess := True;
  BrowserSecurityCheck := True;
  Background.Fixed := False;
  HandleTabs := False;
  LeftToRight := True;
  LockUntilLoaded := True;
  LockOnSubmit := True;
  ShowHint := True;
  XPTheme := True;

  rgnParent := TIWRegion.Create(Self);
  rctCaption := TIWRectangle.Create(Self);
  IWRegion2 := TIWRegion.Create(Self);
  IWRegion5 := TIWRegion.Create(Self);
  rgnText := TIWRegion.Create(Self);
  rgnButtons := TIWRegion.Create(Self);
  btnCancel := TIWButton.Create(Self);
  btnReplaceAll := TIWButton.Create(Self);
  IWRegion7 := TIWRegion.Create(Self);
  edtFind := TIWEdit.Create(Self);
  edtReplace := TIWEdit.Create(Self);
  chkCaseSensitive := TIWCheckBox.Create(Self);
  IWRegion1 := TIWRegion.Create(Self);
  IWRegion6 := TIWRegion.Create(Self);
  IWRegion8 := TIWRegion.Create(Self);
  lblReplace := TIWLabel.Create(Self);
  IWRegion9 := TIWRegion.Create(Self);
  IWRegion10 := TIWRegion.Create(Self);
  lblFind := TIWLabel.Create(Self);
  IWRegion3 := TIWRegion.Create(Self);
  IWRegion4 := TIWRegion.Create(Self);

  with rgnParent do
  begin
    Name := 'rgnParent';
    Parent := self;
    Left := 0;
    Top := 0;
    Width := 270;
    Height := 145;
    Cursor := crAuto;
    HorzScrollBar.Visible := False;
    VertScrollBar.Visible := False;
    TabOrder := 0;
    Anchors := [];
    BorderOptions.NumericWidth := 1;
    BorderOptions.BorderWidth := cbwNumeric;
    BorderOptions.Style := cbsNone;
    BorderOptions.Color := clNone;
    Color := clNone;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := -1;
    Splitter := False;
  end;
  with rctCaption do
  begin
    Name := 'rctCaption';
    Parent := rgnParent;
    Left := 0;
    Top := 0;
    Width := 270;
    Height := 21;
    Cursor := crAuto;
    Align := alTop;
    IW50Hint := False;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := 0;
    RenderSize := True;
    Text := 'Search / Replace';
    Font.Color := clWebWHITE;
    Font.Size := 10;
    Font.Style := [fsBold];
    BorderOptions.Color := clNone;
    BorderOptions.Width := 0;
    FriendlyName := 'rctCaption';
    Color := clWebNAVY;
    Alignment := taCenter;
    VAlign := vaMiddle;
  end;
  with IWRegion2 do
  begin
    Name := 'IWRegion2';
    Parent := rgnParent;
    Left := 0;
    Top := 21;
    Width := 270;
    Height := 4;
    Cursor := crAuto;
    HorzScrollBar.Visible := False;
    VertScrollBar.Visible := False;
    TabOrder := 0;
    Align := alTop;
    BorderOptions.NumericWidth := 1;
    BorderOptions.BorderWidth := cbwNumeric;
    BorderOptions.Style := cbsNone;
    BorderOptions.Color := clNone;
    Color := clNone;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := -1;
    Splitter := False;
  end;
  with IWRegion5 do
  begin
    Name := 'IWRegion5';
    Parent := rgnParent;
    Left := 268;
    Top := 25;
    Width := 2;
    Height := 118;
    Cursor := crAuto;
    HorzScrollBar.Visible := False;
    VertScrollBar.Visible := False;
    TabOrder := 1;
    Align := alRight;
    BorderOptions.NumericWidth := 1;
    BorderOptions.BorderWidth := cbwNumeric;
    BorderOptions.Style := cbsNone;
    BorderOptions.Color := clNone;
    Color := clNone;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := -1;
    Splitter := False;
  end;
  with rgnText do
  begin
    Name := 'rgnText';
    Parent := rgnParent;
    Left := 4;
    Top := 25;
    Width := 264;
    Height := 118;
    Cursor := crAuto;
    HorzScrollBar.Visible := False;
    VertScrollBar.Visible := False;
    TabOrder := 2;
    Align := alClient;
    BorderOptions.NumericWidth := 1;
    BorderOptions.BorderWidth := cbwNumeric;
    BorderOptions.Style := cbsNone;
    BorderOptions.Color := clNone;
    Color := clNone;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := -1;
    Splitter := False;
  end;
  with rgnButtons do
  begin
    Name := 'rgnButtons';
    Parent := rgnText;
    Left := 0;
    Top := 93;
    Width := 264;
    Height := 25;
    Cursor := crAuto;
    HorzScrollBar.Visible := False;
    VertScrollBar.Visible := False;
    TabOrder := 0;
    Align := alBottom;
    BorderOptions.NumericWidth := 1;
    BorderOptions.BorderWidth := cbwNumeric;
    BorderOptions.Style := cbsNone;
    BorderOptions.Color := clNone;
    Color := clNone;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := -1;
    Splitter := False;
  end;
  with btnCancel do
  begin
    Name := 'btnCancel';
    Parent := rgnButtons;
    Left := 144;
    Top := 2;
    Width := 75;
    Height := 20;
    Cursor := crAuto;
    IW50Hint := False;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := 0;
    RenderSize := True;
    Caption := 'Cancel';
    DoSubmitValidation := False;
    Color := clBtnFace;
    Font.Color := clNone;
    Font.Size := 10;
    Font.Style := [];
    FriendlyName := 'btnCancel';
    TabOrder := 4;
    OnClick := CancelClick;
  end;
  with btnReplaceAll do
  begin
    Name := 'btnReplaceAll';
    Parent := rgnButtons;
    Left := 52;
    Top := 2;
    Width := 89;
    Height := 20;
    Cursor := crAuto;
    IW50Hint := False;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := 0;
    RenderSize := True;
    Caption := 'Replace All';
    DoSubmitValidation := True;
    Color := clBtnFace;
    Font.Color := clNone;
    Font.Size := 10;
    Font.Style := [];
    FriendlyName := 'btnReplaceAll';
    TabOrder := 3;
    OnClick := ReplaceClick;
  end;
  with IWRegion7 do
  begin
    Name := 'IWRegion7';
    Parent := rgnText;
    Left := 0;
    Top := 0;
    Width := 264;
    Height := 93;
    Cursor := crAuto;
    HorzScrollBar.Visible := False;
    VertScrollBar.Visible := False;
    TabOrder := 1;
    Align := alClient;
    BorderOptions.NumericWidth := 1;
    BorderOptions.BorderWidth := cbwNumeric;
    BorderOptions.Style := cbsNone;
    BorderOptions.Color := clNone;
    Color := clNone;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := -1;
    Splitter := False;
  end;
  with edtFind do
  begin
    Name := 'edtFind';
    Parent := IWRegion7;
    Left := 98;
    Top := 13;
    Width := 151;
    Height := 21;
    Cursor := crAuto;
    OnHTMLTag := edtFindHTMLTag;
    IW50Hint := False;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := 0;
    RenderSize := True;
    Alignment := taLeftJustify;
    BGColor := clNone;
    FocusColor := clNone;
    DoSubmitValidation := True;
    Editable := True;
    NonEditableAsLabel := True;
    Font.Color := clNone;
    Font.Size := 10;
    Font.Style := [];
    FriendlyName := 'Find';
    MaxLength := 0;
    ReadOnly := False;
    Required := False;
    TabOrder := 0;
    PasswordPrompt := False;
  end;
  with edtReplace do
  begin
    Name := 'edtReplace';
    Parent := IWRegion7;
    Left := 98;
    Top := 37;
    Width := 151;
    Height := 21;
    Cursor := crAuto;
    OnHTMLTag := edtFindHTMLTag;
    IW50Hint := False;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := 0;
    RenderSize := True;
    Alignment := taLeftJustify;
    BGColor := clNone;
    FocusColor := clNone;
    DoSubmitValidation := True;
    Editable := True;
    NonEditableAsLabel := True;
    Font.Color := clNone;
    Font.Size := 10;
    Font.Style := [];
    FriendlyName := 'Replace';
    MaxLength := 0;
    ReadOnly := False;
    Required := False;
    TabOrder := 1;
    PasswordPrompt := False;
  end;
  with chkCaseSensitive do
  begin
    Name := 'chkCaseSensitive';
    Parent := IWRegion7;
    Left := 98;
    Top := 62;
    Width := 121;
    Height := 21;
    Cursor := crAuto;
    IW50Hint := False;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := 0;
    RenderSize := False;
    Caption := 'Case Sensitive';
    Editable := True;
    Font.Color := clNone;
    Font.Size := 10;
    Font.Style := [];
    DoSubmitValidation := True;
    Style := stNormal;
    TabOrder := 2;
    Checked := False;
    FriendlyName := 'chkCaseSensitive';
  end;
  with IWRegion1 do
  begin
    Name := 'IWRegion1';
    Parent := IWRegion7;
    Left := 0;
    Top := 0;
    Width := 97;
    Height := 93;
    Cursor := crAuto;
    HorzScrollBar.Visible := False;
    VertScrollBar.Visible := False;
    TabOrder := 0;
    Align := alLeft;
    BorderOptions.NumericWidth := 1;
    BorderOptions.BorderWidth := cbwNumeric;
    BorderOptions.Style := cbsNone;
    BorderOptions.Color := clNone;
    Color := clNone;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := -1;
    Splitter := False;
  end;
  with IWRegion6 do
  begin
    Name := 'IWRegion6';
    Parent := IWRegion1;
    Left := 0;
    Top := 34;
    Width := 97;
    Height := 6;
    Cursor := crAuto;
    HorzScrollBar.Visible := False;
    VertScrollBar.Visible := False;
    TabOrder := 0;
    Align := alTop;
    BorderOptions.NumericWidth := 1;
    BorderOptions.BorderWidth := cbwNumeric;
    BorderOptions.Style := cbsNone;
    BorderOptions.Color := clNone;
    Color := clNone;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := -1;
    Splitter := False;
  end;
  with IWRegion8 do
  begin
    Name := 'IWRegion8';
    Parent := IWRegion1;
    Left := 0;
    Top := 40;
    Width := 97;
    Height := 18;
    Cursor := crAuto;
    HorzScrollBar.Visible := False;
    VertScrollBar.Visible := False;
    TabOrder := 1;
    Align := alTop;
    BorderOptions.NumericWidth := 1;
    BorderOptions.BorderWidth := cbwNumeric;
    BorderOptions.Style := cbsNone;
    BorderOptions.Color := clNone;
    Color := clNone;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := -1;
    Splitter := False;
  end;
  with lblReplace do
  begin
    Name := 'lblReplace';
    Parent := IWRegion8;
    Left := 8;
    Top := 0;
    Width := 89;
    Height := 18;
    Cursor := crAuto;
    Align := alRight;
    IW50Hint := False;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := 0;
    RenderSize := False;
    Alignment := taLeftJustify;
    BGColor := clNone;
    Font.Color := clNone;
    Font.Size := 10;
    Font.Style := [];
    NoWrap := False;
    ConvertSpaces := False;
    FriendlyName := 'lblReplace';
    Caption := 'Replace Text:';
    RawText := False;
  end;
  with IWRegion9 do
  begin
    Name := 'IWRegion9';
    Parent := IWRegion1;
    Left := 0;
    Top := 0;
    Width := 97;
    Height := 16;
    Cursor := crAuto;
    HorzScrollBar.Visible := False;
    VertScrollBar.Visible := False;
    TabOrder := 2;
    Align := alTop;
    BorderOptions.NumericWidth := 1;
    BorderOptions.BorderWidth := cbwNumeric;
    BorderOptions.Style := cbsNone;
    BorderOptions.Color := clNone;
    Color := clNone;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := -1;
    Splitter := False;
  end;
  with IWRegion10 do
  begin
    Name := 'IWRegion10';
    Parent := IWRegion1;
    Left := 0;
    Top := 16;
    Width := 97;
    Height := 18;
    Cursor := crAuto;
    HorzScrollBar.Visible := False;
    VertScrollBar.Visible := False;
    TabOrder := 3;
    Align := alTop;
    BorderOptions.NumericWidth := 1;
    BorderOptions.BorderWidth := cbwNumeric;
    BorderOptions.Style := cbsNone;
    BorderOptions.Color := clNone;
    Color := clNone;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := -1;
    Splitter := False;
  end;
  with lblFind do
  begin
    Name := 'lblFind';
    Parent := IWRegion10;
    Left := 32;
    Top := 0;
    Width := 65;
    Height := 18;
    Cursor := crAuto;
    Align := alRight;
    IW50Hint := False;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := 0;
    RenderSize := False;
    Alignment := taLeftJustify;
    BGColor := clNone;
    Font.Color := clNone;
    Font.Size := 10;
    Font.Style := [];
    NoWrap := False;
    ConvertSpaces := False;
    FriendlyName := 'lblFind';
    Caption := 'Find Text:';
    RawText := False;
  end;
  with IWRegion3 do
  begin
    Name := 'IWRegion3';
    Parent := rgnParent;
    Left := 0;
    Top := 143;
    Width := 270;
    Height := 2;
    Cursor := crAuto;
    HorzScrollBar.Visible := False;
    VertScrollBar.Visible := False;
    TabOrder := 3;
    Align := alBottom;
    BorderOptions.NumericWidth := 1;
    BorderOptions.BorderWidth := cbwNumeric;
    BorderOptions.Style := cbsNone;
    BorderOptions.Color := clNone;
    Color := clNone;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := -1;
    Splitter := False;
  end;
  with IWRegion4 do
  begin
    Name := 'IWRegion4';
    Parent := rgnParent;
    Left := 0;
    Top := 25;
    Width := 4;
    Height := 118;
    Cursor := crAuto;
    HorzScrollBar.Visible := False;
    VertScrollBar.Visible := False;
    TabOrder := 4;
    Align := alLeft;
    BorderOptions.NumericWidth := 1;
    BorderOptions.BorderWidth := cbwNumeric;
    BorderOptions.Style := cbsNone;
    BorderOptions.Color := clNone;
    Color := clNone;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := -1;
    Splitter := False;
  end;
end;

procedure TfrmDlgSearchReplace.edtFindHTMLTag(ASender: TObject;
  ATag: TIWHTMLTag);
begin
  ATag.AddStringParam('Style','Width: '+IntToStr(TIWButton(ASender).Width));
end;

procedure TfrmDlgSearchReplace.CancelClick(Sender: TObject);
begin
  TCompHelper(DlgComponent).DoCancelEvent;
  Release;
end;

end.
