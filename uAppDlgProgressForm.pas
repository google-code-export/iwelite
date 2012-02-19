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

unit uAppDlgProgressForm;
{PUBDIST}                                                                                      

interface

{$I IntrawebVersion.inc}

uses
  IWAppForm, IWApplication, IWTypes, IWCompButton, IWControl, IWCompRectangle, IWColor,
  Classes, Controls, Forms, IWContainer, IWRegion, IWCompEdit, IWCompLabel,
  ArcIWDlgProgress, SysUtils, IWHTMLTag, ArcIWDlgBase
  {$IFDEF INTRAWEB51}, IWBaseControl, IWVCLBaseControl, IWVCLBaseContainer,
  IWBaseComponent, IWBaseHTMLComponent, IWBaseHTMLControl, IWHTMLContainer{$ENDIF}
  {$IFDEF INTRAWEB120}, IWCompExtCtrls {$ELSE}, IWExtCtrls {$ENDIF};

type
  TCompHelper = class(TArcIWDlgProgress)
  end;

  TfrmDlgProgress = class(TIWAppForm)
    rgnParent: TIWRegion;
    rctCaption: TIWRectangle;
    IWRegion2: TIWRegion;
    IWRegion5: TIWRegion;
    rgnText: TIWRegion;
    rgnButtons: TIWRegion;
    btnCancel: TIWButton;
    IWRegion7: TIWRegion;
    IWRegion3: TIWRegion;
    IWRegion4: TIWRegion;
    imgClock: TIWImageFile;
    tmrTest: TIWTimer;
    procedure OnClick(Sender: TObject);
    procedure tmrTestTimer(Sender: TObject);
  public
    DlgComponent : TArcIWDlgProgress;
    constructor Create(AOwner:TComponent);override;
  end;

implementation

uses
  IWContainerBorderOptions, Graphics
  {$IFDEF INTRAWEB120}, IWCompGridCommon {$ELSE}, IWGridCommon {$ENDIF};

constructor TfrmDlgProgress.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);

  Name := 'frmDlgProgress';
  Left := 0;
  Top := 0;
  Width := 177;
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
  UpdateMode := umPartial;

  rgnParent := TIWRegion.Create(Self);
  rctCaption := TIWRectangle.Create(Self);
  IWRegion2 := TIWRegion.Create(Self);
  IWRegion5 := TIWRegion.Create(Self);
  rgnText := TIWRegion.Create(Self);
  rgnButtons := TIWRegion.Create(Self);
  btnCancel := TIWButton.Create(Self);
  IWRegion7 := TIWRegion.Create(Self);
  imgClock := TIWImageFile.Create(Self);
  IWRegion3 := TIWRegion.Create(Self);
  IWRegion4 := TIWRegion.Create(Self);
  tmrTest:= TIWTimer.Create(Self);

  with rgnParent do
  begin
    Name := 'rgnParent';
    Parent := self;
    Left := 0;
    Top := 0;
    Width := 177;
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
    Width := 177;
    Height := 21;
    Cursor := crAuto;
    Align := alTop;
    IW50Hint := False;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := 0;
    RenderSize := True;
    Text := 'Please Wait...';
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
    Width := 177;
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
    Left := 175;
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
    Width := 171;
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
    Width := 171;
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
    Left := 95;
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
    Caption := 'Cancel';
    DoSubmitValidation := False;
    Color := clBtnFace;
    Font.Color := clNone;
    Font.Size := 10;
    Font.Style := [];
    FriendlyName := 'btnCancel';
    TabOrder := 0;
    OnClick := OnClick;
  end;
  with IWRegion7 do
  begin
    Name := 'IWRegion7';
    Parent := rgnText;
    Left := 0;
    Top := 0;
    Width := 171;
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
  with imgClock do
  begin
    Name := 'imgClock';
    Parent := IWRegion7;
    Left := 69;
    Top := 26;
    Width := 40;
    Height := 40;
    Cursor := crAuto;
    Anchors := [];
    IW50Hint := False;
    ParentShowHint := False;
    ShowHint := True;
    ZIndex := 0;
    RenderSize := False;
    BorderOptions.Color := clNone;
    BorderOptions.Width := 0;
    DoSubmitValidation := True;
    TabOrder := -1;
    UseSize := False;
    Cacheable := True;
    FriendlyName := 'imgClock';
  end;
  with IWRegion3 do
  begin
    Name := 'IWRegion3';
    Parent := rgnParent;
    Left := 0;
    Top := 143;
    Width := 177;
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
  with tmrTest do
  begin
    Enabled := True;
    Interval := 0;
    OnTimer := tmrTestTimer;
    Left := 12;
    Top := 31;
    Name := 'tmrTest';
  end;
end;

procedure TfrmDlgProgress.OnClick(Sender: TObject);
begin
  TCompHelper(DlgComponent).DoCancelEvent;
  DlgComponent.DialogForm := nil;
  Release;
end;

procedure TfrmDlgProgress.tmrTestTimer(Sender: TObject);
begin
  if TCompHelper(DlgComponent).DoTestEvent then
  begin
    DlgComponent.DialogForm := nil;
    Release;
  end;
end;

end.
