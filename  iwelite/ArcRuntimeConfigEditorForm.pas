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

unit ArcRuntimeConfigEditorForm;

interface                                    

uses
  Windows, Messages, SysUtils, {$IFNDEF VER130}Variants, {$ENDIF}Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TfrmExtractOptions = class(TForm)
  public
    Label1: TLabel;
    edtINI: TEdit;
    chkChanged: TCheckBox;
    chkVisual: TCheckBox;
    chkNonvisual: TCheckBox;
    btnExtract: TButton;
    btnCancel: TButton;
    constructor Create(AOwner:TComponent);override;
  end;

implementation

constructor TfrmExtractOptions.Create(AOwner:TComponent);
begin
  inherited CreateNew(AOwner);
  
  Name := 'frmExtractOptions';
  Left := 198;
  Top := 109;
  BorderIcons := [];
  BorderStyle := bsDialog;
  Caption := 'Extract Properties';
  ClientHeight := 150;
  ClientWidth := 204;
  Color := clBtnFace;
  Font.Charset := DEFAULT_CHARSET;
  Font.Color := clWindowText;
  Font.Height := -11;
  Font.Name := 'MS Sans Serif';
  Font.Style := [];
  OldCreateOrder := False;
  Position := poScreenCenter;
  PixelsPerInch := 96;

  Label1 := TLabel.Create(self);
  edtINI := TEdit.Create(self);
  chkChanged := TCheckBox.Create(self);
  chkVisual := TCheckBox.Create(self);
  chkNonvisual := TCheckBox.Create(self);
  btnExtract := TButton.Create(self);
  btnCancel := TButton.Create(self);
  //

  with Label1 do
  begin
    Name := 'Label1';
    Parent := self;
    Left := 16;
    Top := 12;
    Width := 62;
    Height := 13;
    Caption := 'INI Filename:';
  end;
  with edtINI do
  begin
    Name := 'edtINI';
    Parent := self;
    Left := 16;
    Top := 28;
    Width := 173;
    Height := 21;
    TabOrder := 0;
  end;
  with chkChanged do
  begin
    Name := 'chkChanged';
    Parent := self;
    Left := 16;
    Top := 56;
    Width := 173;
    Height := 17;
    Caption := 'Extract Minimal Properties';
    TabOrder := 1;
  end;
  with chkVisual do
  begin
    Name := 'chkVisual';
    Parent := self;
    Left := 16;
    Top := 72;
    Width := 173;
    Height := 17;
    Caption := 'Extract Visual Controls';
    Checked := True;
    State := cbChecked;
    TabOrder := 2;
  end;
  with chkNonvisual do
  begin
    Name := 'chkNonvisual';
    Parent := self;
    Left := 16;
    Top := 88;
    Width := 173;
    Height := 17;
    Caption := 'Extract Nonvisual Components';
    Checked := True;
    State := cbChecked;
    TabOrder := 3;
  end;
  with btnExtract do
  begin
    Name := 'btnExtract';
    Parent := self;
    Left := 23;
    Top := 116;
    Width := 75;
    Height := 25;
    Caption := 'Extract';
    Default := True;
    ModalResult := 1;
    TabOrder := 4;
  end;
  with btnCancel do
  begin
    Name := 'btnCancel';
    Parent := self;
    Left := 107;
    Top := 116;
    Width := 75;
    Height := 25;
    Cancel := True;
    Caption := 'Cancel';
    ModalResult := 2;
    TabOrder := 5;
  end;  
end;

end.

