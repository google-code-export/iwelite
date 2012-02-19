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

unit uAppDlgScrollableForm;
{PUBDIST}

interface
{$I IntrawebVersion.inc}                       

uses
  IWAppForm, IWApplication, IWTypes, Forms, ArcIWDlgMessageBox, ArcIWDlgScrollableMessageBox, Classes, IWColor,
  Controls, IWContainer, IWRegion, IWControl, IWCompRectangle, IWCompText,
  IWCompLabel, IWCompButton, ArcIWDlgBase{$IFNDEF INTRAWEB50}, IWBaseControl,
  IWCompMemo{$ENDIF} {$IFDEF INTRAWEB60},IWVCLBaseControl, IWVCLBaseContainer,
  IWBaseHTMLControl, IWHTMLContainer{$ENDIF}
  {$IFDEF INTRAWEB120}, IWCompExtCtrls {$ELSE}, IWExtCtrls {$ENDIF};

type
  TCompHelper = class(TArcIWDlgScrollableMessageBox)
  end;

  TfrmAppDialogScroll = class(TIWAppForm)
    rgnParent: TIWRegion;
    rctCaption: TIWRectangle;
    IWRegion2: TIWRegion;
    IWRegion5: TIWRegion;
    rgnText: TIWRegion;
    rgnIcon: TIWRegion;
    imgIcon: TIWImageFile;
    IWRegion3: TIWRegion;
    IWRegion4: TIWRegion;
    rgnButtons: TIWRegion;
    IWRegion6: TIWRegion;
    IWRegion7: TIWRegion;
    IWRegion9: TIWRegion;
    IWRegion10: TIWRegion;
    IWRegion11: TIWRegion;
    IWRegion12: TIWRegion;
    btn1: TIWButton;
    btn2: TIWButton;
    btn4: TIWButton;
    btn3: TIWButton;
    mMessage: TIWMemo;
    procedure btn1Click(Sender: TObject);
  public
    DlgComponent : TArcIWDlgScrollableMessageBox;
    constructor Create(AOwner:TComponent);override;
  end;

implementation

uses
  IWContainerBorderOptions, Graphics
  {$IFDEF INTRAWEB120}, IWCompGridCommon {$ELSE}, IWGridCommon {$ENDIF};

procedure TfrmAppDialogScroll.btn1Click(Sender: TObject);
begin
  DlgComponent.ModalResult := ArcIWDlgMessageBox.mrNone;
  if TIWButton(Sender).Caption = TCompHelper(DlgComponent).FElementText.BtnTxtOk then
    DlgComponent.ModalResult := ArcIWDlgMessageBox.mrOK;
  if TIWButton(Sender).Caption = TCompHelper(DlgComponent).FElementText.BtnTxtCancel then
    DlgComponent.ModalResult := ArcIWDlgMessageBox.mrCancel;
  if TIWButton(Sender).Caption = TCompHelper(DlgComponent).FElementText.BtnTxtAbort then
    DlgComponent.ModalResult := ArcIWDlgMessageBox.mrAbort;
  if TIWButton(Sender).Caption = TCompHelper(DlgComponent).FElementText.BtnTxtRetry then
    DlgComponent.ModalResult := ArcIWDlgMessageBox.mrRetry;
  if TIWButton(Sender).Caption = TCompHelper(DlgComponent).FElementText.BtnTxtIgnore then
    DlgComponent.ModalResult := ArcIWDlgMessageBox.mrIgnore;
  if TIWButton(Sender).Caption = TCompHelper(DlgComponent).FElementText.BtnTxtYes then
    DlgComponent.ModalResult := ArcIWDlgMessageBox.mrYes;
  if TIWButton(Sender).Caption = TCompHelper(DlgComponent).FElementText.BtnTxtNo then
    DlgComponent.ModalResult := ArcIWDlgMessageBox.mrNo;
  if TIWButton(Sender).Caption = TCompHelper(DlgComponent).FElementText.BtnTxtAll then
    DlgComponent.ModalResult := ArcIWDlgMessageBox.mrAll;
  if TIWButton(Sender).Caption = TCompHelper(DlgComponent).FElementText.BtnTxtNoToAll then
    DlgComponent.ModalResult := ArcIWDlgMessageBox.mrNoToAll;
  if TIWButton(Sender).Caption = TCompHelper(DlgComponent).FElementText.BtnTxtYesToAll then
    DlgComponent.ModalResult := ArcIWDlgMessageBox.mrYesToAll;
  TCompHelper(DlgComponent).DoResultNotify;
  Release;
end;

constructor TfrmAppDialogScroll.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);

  Name := 'frmAppDialogScroll';
  Left := 0;
  Top := 0;
  Width := 317;
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
  rgnIcon := TIWRegion.Create(Self);
  imgIcon := TIWImageFile.Create(Self);
  IWRegion12 := TIWRegion.Create(Self);
  rgnButtons := TIWRegion.Create(Self);
  btn1 := TIWButton.Create(Self);
  btn2 := TIWButton.Create(Self);
  btn4 := TIWButton.Create(Self);
  btn3 := TIWButton.Create(Self);
  IWRegion6 := TIWRegion.Create(Self);
  IWRegion7 := TIWRegion.Create(Self);
  mMessage := TIWMemo.Create(Self);
  IWRegion10 := TIWRegion.Create(Self);
  IWRegion11 := TIWRegion.Create(Self);
  IWRegion9 := TIWRegion.Create(Self);
  IWRegion3 := TIWRegion.Create(Self);
  IWRegion4 := TIWRegion.Create(Self);

  with rgnParent do
  begin
    Name := 'rgnParent';
    Parent := self;
    Left := 0;
    Top := 0;
    Width := 317;
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
    Width := 317;
    Height := 21;
    Cursor := crAuto;
    Align := alTop;
    IW50Hint := False;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := 0;
    RenderSize := True;
    Text := 'Warning...';
    Font.Color := clWebBLACK;
    Font.Size := 10;
    Font.Style := [fsBold];
    BorderOptions.Color := clNone;
    BorderOptions.Width := 0;
    FriendlyName := 'rctCaption';
    Color := clNone;
    Alignment := taCenter;
    VAlign := vaMiddle;
  end;
  with IWRegion2 do
  begin
    Name := 'IWRegion2';
    Parent := rgnParent;
    Left := 0;
    Top := 21;
    Width := 317;
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
    Left := 315;
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
    Width := 311;
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
  with rgnIcon do
  begin
    Name := 'rgnIcon';
    Parent := rgnText;
    Left := 5;
    Top := 0;
    Width := 40;
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
  with imgIcon do
  begin
    Name := 'imgIcon';
    Parent := rgnIcon;
    Left := 0;
    Top := 5;
    Width := 40;
    Height := 40;
    Cursor := crAuto;
    Align := alTop;
    IW50Hint := False;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := 0;
    RenderSize := True;
    AutoSize := False;
    BorderOptions.Color := clNone;
    BorderOptions.Width := 0;
    DoSubmitValidation := True;
    TabOrder := -1;
    UseSize := True;
    Cacheable := True;
    FriendlyName := 'imgIcon';
  end;
  with IWRegion12 do
  begin
    Name := 'IWRegion12';
    Parent := rgnIcon;
    Left := 0;
    Top := 0;
    Width := 40;
    Height := 5;
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
  with rgnButtons do
  begin
    Name := 'rgnButtons';
    Parent := rgnText;
    Left := 0;
    Top := 93;
    Width := 311;
    Height := 25;
    Cursor := crAuto;
    HorzScrollBar.Visible := False;
    VertScrollBar.Visible := False;
    TabOrder := 1;
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
  with btn1 do
  begin
    Name := 'btn1';
    Parent := rgnButtons;
    Left := 232;
    Top := 2;
    Width := 75;
    Height := 20;
    Cursor := crAuto;
    Visible := False;
    IW50Hint := False;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := 0;
    RenderSize := True;
    Caption := 'btn1';
    DoSubmitValidation := True;
    Color := clBtnFace;
    Font.Color := clNone;
    Font.Size := 10;
    Font.Style := [];
    FriendlyName := 'btn1';
    TabOrder := 3;
    OnClick := btn1Click;
  end;
  with btn2 do
  begin
    Name := 'btn2';
    Parent := rgnButtons;
    Left := 154;
    Top := 2;
    Width := 75;
    Height := 20;
    Cursor := crAuto;
    Visible := False;
    IW50Hint := False;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := 0;
    RenderSize := True;
    Caption := 'ArcIWOperaButton1';
    DoSubmitValidation := True;
    Color := clBtnFace;
    Font.Color := clNone;
    Font.Size := 10;
    Font.Style := [];
    FriendlyName := 'btn2';
    TabOrder := 2;
    OnClick := btn1Click;
  end;
  with btn4 do
  begin
    Name := 'btn4';
    Parent := rgnButtons;
    Left := 2;
    Top := 2;
    Width := 75;
    Height := 20;
    Cursor := crAuto;
    Visible := False;
    IW50Hint := False;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := 0;
    RenderSize := True;
    Caption := 'ArcIWOperaButton1';
    DoSubmitValidation := True;
    Color := clBtnFace;
    Font.Color := clNone;
    Font.Size := 10;
    Font.Style := [];
    FriendlyName := 'btn4';
    TabOrder := 0;
    OnClick := btn1Click;
  end;
  with btn3 do
  begin
    Name := 'btn3';
    Parent := rgnButtons;
    Left := 78;
    Top := 2;
    Width := 75;
    Height := 20;
    Cursor := crAuto;
    Visible := False;
    IW50Hint := False;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := 0;
    RenderSize := True;
    Caption := 'ArcIWOperaButton1';
    DoSubmitValidation := True;
    Color := clBtnFace;
    Font.Color := clNone;
    Font.Size := 10;
    Font.Style := [];
    FriendlyName := 'btn3';
    TabOrder := 1;
    OnClick := btn1Click;
  end;
  with IWRegion6 do
  begin
    Name := 'IWRegion6';
    Parent := rgnText;
    Left := 45;
    Top := 0;
    Width := 5;
    Height := 93;
    Cursor := crAuto;
    HorzScrollBar.Visible := False;
    VertScrollBar.Visible := False;
    TabOrder := 2;
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
  with IWRegion7 do
  begin
    Name := 'IWRegion7';
    Parent := rgnText;
    Left := 50;
    Top := 0;
    Width := 261;
    Height := 93;
    Cursor := crAuto;
    HorzScrollBar.Visible := False;
    VertScrollBar.Visible := False;
    TabOrder := 3;
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
  with mMessage do
  begin
    Name := 'mMessage';
    Parent := IWRegion7;
    Left := 0;
    Top := 0;
    Width := 242;
    Height := 88;
    Cursor := crAuto;
    Align := alClient;
    IW50Hint := False;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := 0;
    RenderSize := True;
    BGColor := clWebWHITE;
    Editable := True;
    Font.Color := clNone;
    Font.Size := 10;
    Font.Style := [];
    InvisibleBorder := False;
    HorizScrollBar := False;
    VertScrollBar := True;
    Required := False;
    TabOrder := 4;
    FriendlyName := 'IWMemo1';
  end;
  with IWRegion10 do
  begin
    Name := 'IWRegion10';
    Parent := IWRegion7;
    Left := 242;
    Top := 0;
    Width := 19;
    Height := 88;
    Cursor := crAuto;
    HorzScrollBar.Visible := False;
    VertScrollBar.Visible := False;
    TabOrder := 0;
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
  with IWRegion11 do
  begin
    Name := 'IWRegion11';
    Parent := IWRegion7;
    Left := 0;
    Top := 88;
    Width := 261;
    Height := 5;
    Cursor := crAuto;
    HorzScrollBar.Visible := False;
    VertScrollBar.Visible := False;
    TabOrder := 1;
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
  with IWRegion9 do
  begin
    Name := 'IWRegion9';
    Parent := rgnText;
    Left := 0;
    Top := 0;
    Width := 5;
    Height := 93;
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
  with IWRegion3 do
  begin
    Name := 'IWRegion3';
    Parent := rgnParent;
    Left := 0;
    Top := 143;
    Width := 317;
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

end.
