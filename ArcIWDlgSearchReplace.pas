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

unit ArcIWDlgSearchReplace;

interface

uses
  SysUtils, Classes, IWAppForm, IWCompButton, IWFont, IWTypes, Controls,
  Graphics, IWForm, ArcIWDlgBase, ArcCommon;

type
  TArcTextNotifyEvent = procedure(Sender : TObject; Text : string) of object;

  TSRElementText = class(TPersistent)
  private
    FLblTxtFind: string;
    FLblTxtReplace: string;
    FBtnTxtReplace: string;
    FBtnTxtCancel: string;
    FLblTxtCase: string;
  public
  published
    property BtnTxtReplace : string read FBtnTxtReplace write FBtnTxtReplace;
    property BtnTxtCancel  : string read FBtnTxtCancel write FBtnTxtCancel;
    property LblTxtFind    : string read FLblTxtFind write FLblTxtFind;
    property LblTxtReplace : string read FLblTxtReplace write FLblTxtReplace;
    property LblTxtCase    : string read FLblTxtCase write FLblTxtCase;
  end;

  TArcIWDlgSearchReplace = class(TComponent)
  private
    FWidth: integer;
    FHeight: integer;
    FCaption: string;
    FOnReplace: TArcTextNotifyEvent;
    FCaptionBkColor: TColor;
    FWindowColor: TColor;
    FTextBkColor: TColor;
    FTextFont: TIWFont;
    FCaptionFont: TIWFont;
    FOnCancel: TNotifyEvent;
    FBackgroundColor: TColor;
    FBackground: TIWBackground;
  protected
    FElementText: TSRElementText;
    procedure DoReplaceEvent(Text : string);
    procedure DoCancelEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(aText : string); overload;
  published
    property Background : TIWBackground read FBackground write FBackground;
    property BackgroundColor : TColor read FBackgroundColor write FBackgroundColor;
    property Width : integer read FWidth write FWidth;
    property Height : integer read FHeight write FHeight;
    property TextBkColor : TColor read FTextBkColor write FTextBkColor;
    property TextFont : TIWFont read FTextFont write FTextFont;
    property CaptionBkColor : TColor read FCaptionBkColor write FCaptionBkColor;
    property CaptionFont : TIWFont read FCaptionFont write FCaptionFont;
    property Caption : string read FCaption write FCaption;
    property OnReplace : TArcTextNotifyEvent read FOnReplace write FOnReplace;
    property OnCancel : TNotifyEvent read FOnCancel write FOnCancel;
    property WindowColor : TColor read FWindowColor write FWindowColor;
    property ElementText : TSRElementText read FElementText write FElementText;
  end;

implementation

uses uAppDlgSearchReplaceForm;

{ TArcIWDlgSearchReplace }

constructor TArcIWDlgSearchReplace.Create(AOwner: TComponent);
begin
  inherited;
  if Assigned(AOwner) and (not (AOwner is TIWAppForm)) then
    raise Exception.Create('This component may only be used on an IW Application Form.');
  FBackground := TIWBackground.Create;
  FElementText := TSRElementText.Create;
  FElementText.FLblTxtFind := 'Find Text:';
  FElementText.FLblTxtReplace := 'Replace Text:';
  FElementText.FLblTxtCase    := 'Case Sensitive';
  FElementText.FBtnTxtReplace := 'Replace';
  FElementText.FBtnTxtCancel   := 'Cancel';

  FCaption := 'Search / Replace';
  FCaptionFont := TIWFont.Create;
  FTextFont := TIWFont.Create;
  FCaptionBkColor := clNavy;
  FCaptionFont.Color := clWhite;
  FCaptionFont.Style := [fsBold];
  FTextBkColor := clSilver;
  FWindowColor := clBlack;
  FWidth := 270;
  FHeight := 145;
  FBackgroundColor := clWhite;
end;

destructor TArcIWDlgSearchReplace.Destroy;
begin
  FBackground.Free;
  FCaptionFont.Free;
  FTextFont.Free;
  inherited;
end;

procedure TArcIWDlgSearchReplace.DoCancelEvent;
begin
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

procedure TArcIWDlgSearchReplace.DoReplaceEvent(Text: string);
begin
  if Assigned(OnReplace) then
    OnReplace(Self, Text);
end;

procedure TArcIWDlgSearchReplace.Execute(aText : string);
var
  frm : TfrmDlgSearchReplace;
  iBtn, x : integer;
begin
  frm := TfrmDlgSearchReplace.Create(TIWAppForm(Owner).WebApplication);
  with frm do
  begin
    DlgComponent := Self;

    Text := aText;
    rgnParent.Width := FWidth;
    rgnParent.Height := FHeight;
    rgnParent.Left := Width-FWidth div 2;
    rgnParent.Top := Height-FHeight div 2;
    frm.Width := FWidth;
    frm.Height := FHeight;
    rgnParent.Left := 0;
    rgnParent.Top := 0;

    rctCaption.Text := FCaption;
    rctCaption.Color := FCaptionBkColor;
    rctCaption.Font.Assign(FCaptionFont);
    rgnParent.Color := FWindowColor;
    rgnText.Color   := FTextBkColor;
    lblFind.Font.Assign(FTextFont);
    lblReplace.Font.Assign(FTextFont);
    chkCaseSensitive.Font.Assign(FTextFont);
    lblFind.Caption    := FElementText.FLblTxtFind;
    lblReplace.Caption := FElementText.FLblTxtReplace;
    chkCaseSensitive.Caption := FElementText.FLblTxtCase;
    btnCancel.Caption := FElementText.FBtnTxtCancel;
    btnReplaceAll.Caption := FElementText.FBtnTxtReplace;

    if BrowserIsOpera(TIWAppForm(Self.Owner).WebApplication.Browser) then
    begin
      btnReplaceAll.Top := btnReplaceAll.Top+rgnParent.Top+rgnText.Top+rgnButtons.Top;
      btnCancel.Top := btnCancel.Top+rgnParent.Top+rgnText.Top+rgnButtons.Top;
      edtFind.Top := edtFind.Top+rgnParent.Top+rgnText.Top;
      edtReplace.Top := edtReplace.Top+rgnParent.Top+rgnText.Top;
      btnReplaceAll.Left := btnReplaceAll.Left+rgnParent.Left+rgnText.Left+rgnButtons.Left;
      btnCancel.Left := btnCancel.Left+rgnParent.Left+rgnText.Left+rgnButtons.Left;
      edtFind.Left := edtFind.Left+rgnParent.Left+rgnText.Left;
      edtReplace.Left := edtReplace.Left+rgnParent.Left+rgnText.Left;
    end;

    BackgroundColor := clWhite;
    Background.Fixed := FBackground.Fixed;
    Background.Filename := FBackground.Filename;
    Background.URL      := FBackground.URL;
    BackgroundColor     := FBackgroundColor;
    Show;
  end;
end;

end.

