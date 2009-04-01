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

unit ArcIWDlgScrollableMessageBox;

interface

uses
  SysUtils, Classes, IWAppForm, IWCompButton, IWFont, IWTypes, Controls,
  Graphics, IWForm, ArcIWDlgBase, ArcIWDlgMessageBox;

{$I IntraWebVersion.inc}

type
  TArcIWDlgScrollableMessageBox = class(TComponent)
  private
    FHeight: integer;
    FWidth: integer;
    FText: string;
    FCaption: string;
    FModalResult: TArcModalResult;
    FOnResult: TArcModalResultEvent;
    FButtons: TArcMsgDlgBtns;
    FDlgType: TArcMsgDlgType;
    FCaptionBkColor: TColor;
    FTextBkColor: TColor;
    FCaptionFont: TIWFont;
    FTextFont: TIWFont;
    FWindowColor: TColor;
    FCustomImageURL: string;
    FBackgroundColor: TColor;
    FBackground: TIWBackground;
    procedure SetDlgType(const Value: TArcMsgDlgType);
    { Private declarations }
  protected
    FElementText: TMBElementText;
    procedure Loaded; override;
    procedure DoResultNotify;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Execute; overload;
    property ModalResult : TArcModalResult read FModalResult write FModalResult;
  published
    property Background : TIWBackground read FBackground write FBackground;
    property BackgroundColor : TColor read FBackgroundColor write FBackgroundColor;
    property Width : integer read FWidth write FWidth;
    property Height : integer read FHeight write FHeight;
    property TextBkColor : TColor read FTextBkColor write FTextBkColor;
    property TextFont : TIWFont read FTextFont write FTextFont;
    property CaptionBkColor : TColor read FCaptionBkColor write FCaptionBkColor;
    property CaptionFont : TIWFont read FCaptionFont write FCaptionFont;
    property Text : string read FText write FText;
    property Caption : string read FCaption write FCaption;
    property Buttons : TArcMsgDlgBtns read FButtons write FButtons;
    property DlgType : TArcMsgDlgType read FDlgType write SetDlgType;
    property OnResult : TArcModalResultEvent read FOnResult write FOnResult;
    property WindowColor : TColor read FWindowColor write FWindowColor;
    property CustomImageURL : string read FCustomImageURL write FCustomImageURL;
    property ElementText : TMBElementText read FElementText write FElementText;
  end;

implementation

uses uAppDlgScrollableForm, IWApplication;

{ TArcIWDlgScrollableMessageBox }

constructor TArcIWDlgScrollableMessageBox.Create(AOwner: TComponent);
begin
  inherited;
  if Assigned(AOwner) and (not (AOwner is TIWAppForm)) then
    raise Exception.Create('This component may only be used on an IW Application Form.');
  FBackground := TIWBackground.Create;
  FElementText := TMBElementText.Create;
  FElementText.BtnTxtNo := 'No';
  FElementText.BtnTxtOK := 'Ok';
  FElementText.BtnTxtAbort := 'Abort';
  FElementText.BtnTxtYesToAll := 'Yes To All';
  FElementText.BtnTxtRetry := 'Retry';
  FElementText.BtnTxtAll := 'All';
  FElementText.BtnTxtCancel := 'Cancel';
  FElementText.BtnTxtYes := 'Yes';
  FElementText.BtnTxtNoToAll := 'No To All';
  FElementText.BtnTxtIgnore := 'Ignore';

  FCaptionFont := TIWFont.Create;
  FTextFont := TIWFont.Create;
  FCaptionBkColor := clNavy;
  FCaptionFont.Color := clWhite;
  FCaptionFont.Style := [fsBold];
  FTextBkColor := clSilver;
  FWindowColor := clBlack;
  FWidth := 350;
  FHeight := 145;
  DlgType := mtCustom;
  FButtons := [mbOK, mbCancel];
  FBackgroundColor := clWhite;
end;

destructor TArcIWDlgScrollableMessageBox.Destroy;
begin
  FBackground.Free;
  FElementText.Free;
  FCaptionFont.Free;
  FTextFont.Free;
  inherited;
end;

procedure TArcIWDlgScrollableMessageBox.DoResultNotify;
begin
  if Assigned(FOnResult) then
    FOnResult(Self,FModalResult);
end;

procedure TArcIWDlgScrollableMessageBox.Execute;
var
  frm : TfrmAppDialogScroll;
  iBtn, x : integer;
begin
  frm := TfrmAppDialogScroll.Create(TIWAppForm(Owner).WebApplication);
  with frm do
  begin
    DlgComponent := Self;

    rgnParent.Width := FWidth;
    rgnParent.Height := FHeight;
    frm.Width := FWidth;
    frm.Height := FHeight;
    rgnParent.Left := 0;
    rgnParent.Top := 0;

    rctCaption.Text := FCaption;
    rctCaption.Color := FCaptionBkColor;
    rctCaption.Font.Assign(FCaptionFont);
    mMessage.Lines.Text := FText;
    rgnParent.Color := FWindowColor;
    rgnText.Color   := FTextBkColor;
    mMessage.Font.Assign(FTextFont);
    case FDlgType of
      {$IFDEF INTRAWEB50}
      mtWarning:       imgIcon.ImageFile.URL := WebApplication.URLBase + '/Files/ArcDlgWarning.gif';
      mtError:         imgIcon.ImageFile.URL := WebApplication.URLBase + '/Files/ArcDlgError.gif';
      mtInformation:   imgIcon.ImageFile.URL := WebApplication.URLBase + '/Files/ArcDlgInformation.gif';
      mtConfirmation:  imgIcon.ImageFile.URL := WebApplication.URLBase + '/Files/ArcDlgConfirmation.gif';
      {$ELSE}
      mtWarning:       imgIcon.ImageFile.URL := WebApplication.AppURLBase + '/Files/ArcDlgWarning.gif';
      mtError:         imgIcon.ImageFile.URL := WebApplication.AppURLBase + '/Files/ArcDlgError.gif';
      mtInformation:   imgIcon.ImageFile.URL := WebApplication.AppURLBase + '/Files/ArcDlgInformation.gif';
      mtConfirmation:  imgIcon.ImageFile.URL := WebApplication.AppURLBase + '/Files/ArcDlgConfirmation.gif';
      {$ENDIF}
      mtCustom:        if FCustomImageURL<>'' then
                         imgIcon.ImageFile.URL := FCustomImageURL
                       else
                         rgnIcon.Visible       := False;
      else;
    end;

    iBtn := 1;
    if mbCancel in FButtons then
    begin
      case iBtn of
        1: begin btn1.Caption := FElementText.BtnTxtCancel; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.BtnTxtCancel; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.BtnTxtCancel; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.BtnTxtCancel; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if mbNo in FButtons then
    begin
      case iBtn of
        1: begin btn1.Caption := FElementText.BtnTxtNo; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.BtnTxtNo; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.BtnTxtNo; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.BtnTxtNo; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if mbNoToAll in FButtons then
    begin
      case iBtn of
        1: begin btn1.Caption := FElementText.BtnTxtNoToAll; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.BtnTxtNoToAll; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.BtnTxtNoToAll; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.BtnTxtNoToAll; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if mbAll in FButtons then
    begin
      case iBtn of
        1: begin btn1.Caption := FElementText.BtnTxtAll; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.BtnTxtAll; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.BtnTxtAll; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.BtnTxtAll; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if mbYesToAll in FButtons then
    begin
      case iBtn of
        1: begin btn1.Caption := FElementText.BtnTxtYesToAll; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.BtnTxtYesToAll; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.BtnTxtYesToAll; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.BtnTxtYesToAll; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if mbYes in FButtons then
    begin
      case iBtn of
        1: begin btn1.Caption := FElementText.BtnTxtYes; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.BtnTxtYes; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.BtnTxtYes; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.BtnTxtYes; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if mbOK in FButtons then
    begin
      case iBtn of
        1: begin btn1.Caption := FElementText.BtnTxtOk; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.BtnTxtOk; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.BtnTxtOk; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.BtnTxtOk; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if mbIgnore in FButtons then
    begin
      case iBtn of
        1: begin btn1.Caption := FElementText.BtnTxtIgnore; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.BtnTxtIgnore; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.BtnTxtIgnore; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.BtnTxtIgnore; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if mbRetry in FButtons then
    begin
      case iBtn of
        1: begin btn1.Caption := FElementText.BtnTxtRetry; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.BtnTxtRetry; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.BtnTxtRetry; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.BtnTxtRetry; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if mbAbort in FButtons then
    begin
      case iBtn of
        1: begin btn1.Caption := FElementText.BtnTxtAbort; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.BtnTxtAbort; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.BtnTxtAbort; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.BtnTxtAbort; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if (TIWAppForm(Self.Owner).WebApplication.Browser = brOpera) then
    begin
      btn1.Top := btn1.Top+rgnParent.Top+rgnText.Top+rgnButtons.Top;
      btn2.Top := btn2.Top+rgnParent.Top+rgnText.Top+rgnButtons.Top;
      btn3.Top := btn3.Top+rgnParent.Top+rgnText.Top+rgnButtons.Top;
      btn4.Top := btn4.Top+rgnParent.Top+rgnText.Top+rgnButtons.Top;
      btn1.Left := btn1.Left+rgnParent.Left+rgnText.Left+rgnButtons.Left;
      btn2.Left := btn2.Left+rgnParent.Left+rgnText.Left+rgnButtons.Left;
      btn3.Left := btn3.Left+rgnParent.Left+rgnText.Left+rgnButtons.Left;
      btn4.Left := btn4.Left+rgnParent.Left+rgnText.Left+rgnButtons.Left;
    end;

    BackgroundColor := clWhite;
    Background.Fixed := FBackground.Fixed;
    Background.Filename := FBackground.Filename;
    Background.URL      := FBackground.URL;
    BackgroundColor     := FBackgroundColor;
    Show;
  end;
end;

procedure TArcIWDlgScrollableMessageBox.Loaded;
begin
  inherited;
end;

procedure TArcIWDlgScrollableMessageBox.SetDlgType(const Value: TArcMsgDlgType);
var
  bSetCaption : boolean;
begin
  if FCaption = '' then
    bSetCaption := True
  else
    case FDlgType of
      mtWarning:       bSetCaption := FCaption = 'Warning...';
      mtError:         bSetCaption := FCaption = 'Error...';
      mtInformation:   bSetCaption := FCaption = 'Information...';
      mtConfirmation:  bSetCaption := FCaption = 'Confirmation...';
    end;

  FDlgType := Value;

  if bSetCaption then
    case FDlgType of
      mtWarning:       FCaption := 'Warning...';
      mtError:         FCaption := 'Error...';
      mtInformation:   FCaption := 'Information...';
      mtConfirmation:  FCaption := 'Confirmation...';
    end;
end;

end.
