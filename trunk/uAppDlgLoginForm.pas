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

unit uAppDlgLoginForm;
{PUBDIST}                                                                                      

interface

{$I IntrawebVersion.inc}

uses
  IWAppForm, IWApplication, IWTypes, IWCompButton, IWControl, IWCompRectangle, IWColor,
  Classes, Controls, Forms, IWContainer, IWRegion, IWCompEdit, IWCompLabel,
  ArcIWDlgLogin, SysUtils
  {$IFDEF INTRAWEB120}, IWCompExtCtrls {$ELSE}, IWExtCtrls {$ENDIF}, IWHTMLTag
  {$IFDEF INTRAWEB51}, IWBaseControl, IWVCLBaseControl, IWVCLBaseContainer, IWBaseHTMLControl, IWHTMLContainer {$ENDIF};

type
  TCompHelper = class(TArcIWDlgLogin)
  end;

  TfrmDlgLogin = class(TIWAppForm)
    rgnParent: TIWRegion;
    rctCaption: TIWRectangle;
    IWRegion2: TIWRegion;
    IWRegion5: TIWRegion;
    rgnText: TIWRegion;
    rgnButtons: TIWRegion;
    btn1: TIWButton;
    btn2: TIWButton;
    btn3: TIWButton;
    IWRegion7: TIWRegion;
    IWRegion3: TIWRegion;
    IWRegion4: TIWRegion;
    edtUsername: TIWEdit;
    edtPassword: TIWEdit;
    IWRegion1: TIWRegion;
    IWRegion6: TIWRegion;
    IWRegion8: TIWRegion;
    IWRegion9: TIWRegion;
    IWRegion10: TIWRegion;
    lblUsername: TIWLabel;
    lblPassword: TIWLabel;
    procedure Click(Sender: TObject); reintroduce;
    procedure edtUsernameHTMLTag(ASender: TObject; ATag: TIWHTMLTag);
  public
    DlgComponent : TArcIWDlgLogin;
    constructor Create(AOwner:TComponent);override;
  end;

implementation

uses
  IWContainerBorderOptions, Graphics
  {$IFDEF INTRAWEB120}, IWCompGridCommon {$ELSE}, IWGridCommon {$ENDIF};

procedure TfrmDlgLogin.Click(Sender: TObject);
begin
  if TIWButton(Sender).Caption='Login' then
    TCompHelper(DlgComponent).DoLoginEvent(edtUsername.Text,edtPassword.Text)
  else
    if TIWButton(Sender).Caption='Register' then
      TCompHelper(DlgComponent).DoRegisterEvent
  else
      TCompHelper(DlgComponent).DoCancelEvent;
  Release;
end;

constructor TfrmDlgLogin.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);

  Name := 'frmDlgLogin';
  Left := 0;
  Top := 0;
  Width := 269;
  Height := 129;
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
  btn1 := TIWButton.Create(Self);
  btn2 := TIWButton.Create(Self);
  btn3 := TIWButton.Create(Self);
  IWRegion7 := TIWRegion.Create(Self);
  edtUsername := TIWEdit.Create(Self);
  edtPassword := TIWEdit.Create(Self);
  IWRegion1 := TIWRegion.Create(Self);
  IWRegion6 := TIWRegion.Create(Self);
  IWRegion8 := TIWRegion.Create(Self);
  lblPassword := TIWLabel.Create(Self);
  IWRegion9 := TIWRegion.Create(Self);
  IWRegion10 := TIWRegion.Create(Self);
  lblUsername := TIWLabel.Create(Self);
  IWRegion3 := TIWRegion.Create(Self);
  IWRegion4 := TIWRegion.Create(Self);


  with rgnParent do
  begin
    Name := 'rgnParent';
    Parent := self;
    Left := 0;
    Top := 0;
    Width := 269;
    Height := 129;
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
    Width := 269;
    Height := 21;
    Cursor := crAuto;
    Align := alTop;
    IW50Hint := False;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := 0;
    RenderSize := True;
    Text := 'Login';
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
    Width := 269;
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
    Left := 267;
    Top := 25;
    Width := 2;
    Height := 102;
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
    Width := 263;
    Height := 102;
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
    Top := 77;
    Width := 263;
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
  with btn1 do
  begin
    Name := 'btn1';
    Parent := rgnButtons;
    Left := 184;
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
    FriendlyName := 'btn1';
    TabOrder := 4;
    OnClick := Click;
  end;
  with btn2 do
  begin
    Name := 'btn2';
    Parent := rgnButtons;
    Left := 106;
    Top := 2;
    Width := 75;
    Height := 20;
    Cursor := crAuto;
    IW50Hint := False;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := 0;
    RenderSize := True;
    Caption := 'Login';
    DoSubmitValidation := True;
    Color := clBtnFace;
    Font.Color := clNone;
    Font.Size := 10;
    Font.Style := [];
    FriendlyName := 'btn2';
    TabOrder := 3;
    OnClick := Click;
  end;
  with btn3 do
  begin
    Name := 'btn3';
    Parent := rgnButtons;
    Left := 28;
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
    Caption := 'Login';
    DoSubmitValidation := True;
    Color := clBtnFace;
    Font.Color := clNone;
    Font.Size := 10;
    Font.Style := [];
    FriendlyName := 'btn3';
    TabOrder := 2;
    OnClick := Click;
  end;
  with IWRegion7 do
  begin
    Name := 'IWRegion7';
    Parent := rgnText;
    Left := 0;
    Top := 0;
    Width := 263;
    Height := 77;
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
  with edtUsername do
  begin
    Name := 'edtUsername';
    Parent := IWRegion7;
    Left := 90;
    Top := 13;
    Width := 151;
    Height := 21;
    Cursor := crAuto;
    OnHTMLTag := edtUsernameHTMLTag;
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
    FriendlyName := 'Username';
    MaxLength := 0;
    ReadOnly := False;
    Required := True;
    TabOrder := 0;
    PasswordPrompt := False;
  end;
  with edtPassword do
  begin
    Name := 'edtPassword';
    Parent := IWRegion7;
    Left := 90;
    Top := 37;
    Width := 151;
    Height := 21;
    Cursor := crAuto;
    OnHTMLTag := edtUsernameHTMLTag;
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
    FriendlyName := 'Password';
    MaxLength := 0;
    ReadOnly := False;
    Required := True;
    TabOrder := 1;
    PasswordPrompt := True;
  end;
  with IWRegion1 do
  begin
    Name := 'IWRegion1';
    Parent := IWRegion7;
    Left := 0;
    Top := 0;
    Width := 89;
    Height := 77;
    Cursor := crAuto;
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
    Width := 89;
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
    Width := 89;
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
  with lblPassword do
  begin
    Name := 'lblPassword';
    Parent := IWRegion8;
    Left := 20;
    Top := 0;
    Width := 69;
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
    FriendlyName := 'lblPassword';
    Caption := 'Password:';
    RawText := False;
  end;
  with IWRegion9 do
  begin
    Name := 'IWRegion9';
    Parent := IWRegion1;
    Left := 0;
    Top := 0;
    Width := 89;
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
    Width := 89;
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
  with lblUsername do
  begin
    Name := 'lblUsername';
    Parent := IWRegion10;
    Left := 16;
    Top := 0;
    Width := 73;
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
    FriendlyName := 'lblUsername';
    Caption := 'Username:';
    RawText := False;
  end;
  with IWRegion3 do
  begin
    Name := 'IWRegion3';
    Parent := rgnParent;
    Left := 0;
    Top := 127;
    Width := 269;
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
    Height := 102;
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

procedure TfrmDlgLogin.edtUsernameHTMLTag(ASender: TObject;
  ATag: TIWHTMLTag);
begin
  ATag.AddStringParam('Style','Width: '+IntToStr(TIWButton(ASender).Width));
end;

end.
