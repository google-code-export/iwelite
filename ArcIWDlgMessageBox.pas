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

unit ArcIWDlgMessageBox;

interface

uses
  SysUtils, Classes, IWAppForm, IWCompButton, IWFont, IWTypes, Controls,
  Graphics, IWForm, ArcIWDlgBase, ArcCommon;

{$I IntraWebVersion.inc}

type
  TArcModalResult = (mrNone, mrOK, mrCancel, mrAbort, mrRetry, mrIgnore, mrYes, mrNo, mrAll, mrNoToAll, mrYesToAll);
  TArcMsgDlgType = (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);
  TArcMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
    mbAll, mbNoToAll, mbYesToAll, mbHelp);
  TArcMsgDlgBtns = set of TArcMsgDlgBtn;

  TArcModalResultEvent = procedure(Sender : TObject; ModalResult : TArcModalResult) of object;

  TMBElementText = class(TPersistent)
  private
    FBtnTxtNo: string;
    FBtnTxtOK: string;
    FBtnTxtAbort: string;
    FBtnTxtYesToAll: string;
    FBtnTxtRetry: string;
    FBtnTxtAll: string;
    FBtnTxtCancel: string;
    FBtnTxtYes: string;
    FBtnTxtNoToAll: string;
    FBtnTxtIgnore: string;
  public
  published
    property BtnTxtOK : string read FBtnTxtOK write FBtnTxtOK;
    property BtnTxtCancel : string read FBtnTxtCancel write FBtnTxtCancel;
    property BtnTxtAbort : string read FBtnTxtAbort write FBtnTxtAbort;
    property BtnTxtRetry : string read FBtnTxtRetry write FBtnTxtRetry;
    property BtnTxtIgnore : string read FBtnTxtIgnore write FBtnTxtIgnore;
    property BtnTxtYes : string read FBtnTxtYes write FBtnTxtYes;
    property BtnTxtNo : string read FBtnTxtNo write FBtnTxtNo;
    property BtnTxtAll : string read FBtnTxtAll write FBtnTxtAll;
    property BtnTxtNoToAll : string read FBtnTxtNoToAll write FBtnTxtNoToAll;
    property BtnTxtYesToAll : string read FBtnTxtYesToAll write FBtnTxtYesToAll;
  end;

  TArcIWDlgMessageBox = class(TComponent)
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
    FTextIsHTML: boolean;
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
    property TextIsHTML : boolean read FTextIsHTML write FTextIsHTML;
    property WindowColor : TColor read FWindowColor write FWindowColor;
    property CustomImageURL : string read FCustomImageURL write FCustomImageURL;
    property ElementText : TMBElementText read FElementText write FElementText;
  end;

implementation

uses uAppDialogForm, IWApplication;

{ TArcIWDlgMessageBox }

constructor TArcIWDlgMessageBox.Create(AOwner: TComponent);
begin
  inherited;
  if Assigned(AOwner) and (not (AOwner is TIWAppForm)) then
    raise Exception.Create('This component may only be used on an IW Application Form.');
  FBackground := TIWBackground.Create;
  FElementText := TMBElementText.Create;
  FElementText.FBtnTxtNo := 'No';
  FElementText.FBtnTxtOK := 'Ok';
  FElementText.FBtnTxtAbort := 'Abort';
  FElementText.FBtnTxtYesToAll := 'Yes To All';
  FElementText.FBtnTxtRetry := 'Retry';
  FElementText.FBtnTxtAll := 'All';
  FElementText.FBtnTxtCancel := 'Cancel';
  FElementText.FBtnTxtYes := 'Yes';
  FElementText.FBtnTxtNoToAll := 'No To All';
  FElementText.FBtnTxtIgnore := 'Ignore';

  FCaptionFont := TIWFont.Create;
  FTextFont := TIWFont.Create;
  FCaptionBkColor := clNavy;
  FCaptionFont.Color := clNone;
  FCaptionFont.Style := [fsBold];
  FTextBkColor := clSilver;
  FWindowColor := clBlack;
  FWidth := 350;
  FHeight := 145;
  DlgType := mtCustom;
  FButtons := [mbOK, mbCancel];
  FBackgroundColor := clNone;
end;

destructor TArcIWDlgMessageBox.Destroy;
begin
  FBackground.Free;
  FElementText.Free;
  FCaptionFont.Free;
  FTextFont.Free;
  inherited;
end;

procedure TArcIWDlgMessageBox.DoResultNotify;
begin
  if Assigned(FOnResult) then
    FOnResult(Self,FModalResult);
end;

procedure TArcIWDlgMessageBox.Execute;
var
  frm : TfrmAppDialog;
  iBtn, x : integer;
begin
  frm := TfrmAppDialog.Create(TIWAppForm(Owner).WebApplication);
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
    txtMessage.Lines.Text := FText;
    rgnParent.Color := FWindowColor;
    rgnText.Color   := FTextBkColor;
    txtMessage.Font.Assign(FTextFont);
    txtMessage.RawText := FTextIsHTML;
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
        1: begin btn1.Caption := FElementText.FBtnTxtCancel; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.FBtnTxtCancel; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.FBtnTxtCancel; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.FBtnTxtCancel; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if mbNo in FButtons then
    begin
      case iBtn of
        1: begin btn1.Caption := FElementText.FBtnTxtNo; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.FBtnTxtNo; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.FBtnTxtNo; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.FBtnTxtNo; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if mbNoToAll in FButtons then
    begin
      case iBtn of
        1: begin btn1.Caption := FElementText.FBtnTxtNoToAll; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.FBtnTxtNoToAll; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.FBtnTxtNoToAll; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.FBtnTxtNoToAll; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if mbAll in FButtons then
    begin
      case iBtn of
        1: begin btn1.Caption := FElementText.FBtnTxtAll; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.FBtnTxtAll; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.FBtnTxtAll; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.FBtnTxtAll; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if mbYesToAll in FButtons then
    begin
      case iBtn of
        1: begin btn1.Caption := FElementText.FBtnTxtYesToAll; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.FBtnTxtYesToAll; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.FBtnTxtYesToAll; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.FBtnTxtYesToAll; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if mbYes in FButtons then
    begin
      case iBtn of
        1: begin btn1.Caption := FElementText.FBtnTxtYes; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.FBtnTxtYes; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.FBtnTxtYes; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.FBtnTxtYes; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if mbOK in FButtons then
    begin
      case iBtn of
        1: begin btn1.Caption := FElementText.FBtnTxtOk; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.FBtnTxtOk; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.FBtnTxtOk; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.FBtnTxtOk; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if mbIgnore in FButtons then
    begin
      case iBtn of
        1: begin btn1.Caption := FElementText.FBtnTxtIgnore; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.FBtnTxtIgnore; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.FBtnTxtIgnore; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.FBtnTxtIgnore; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if mbRetry in FButtons then
    begin
      case iBtn of
        1: begin btn1.Caption := FElementText.FBtnTxtRetry; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.FBtnTxtRetry; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.FBtnTxtRetry; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.FBtnTxtRetry; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if mbAbort in FButtons then
    begin
      case iBtn of
        1: begin btn1.Caption := FElementText.FBtnTxtAbort; btn1.Visible := True; end;
        2: begin btn2.Caption := FElementText.FBtnTxtAbort; btn2.Visible := True; end;
        3: begin btn3.Caption := FElementText.FBtnTxtAbort; btn3.Visible := True; end;
        4: begin btn4.Caption := FElementText.FBtnTxtAbort; btn4.Visible := True; end;
      end;
      inc(iBtn);
    end;
    if BrowserIsOpera(TIWAppForm(Self.Owner).WebApplication.Browser) then
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

    BackgroundColor := clNone;
    Background.Fixed := FBackground.Fixed;
    Background.Filename := FBackground.Filename;
    Background.URL      := FBackground.URL;
    BackgroundColor     := FBackgroundColor;
    Show;
  end;
end;

procedure TArcIWDlgMessageBox.Loaded;
begin
  inherited;
end;

procedure TArcIWDlgMessageBox.SetDlgType(const Value: TArcMsgDlgType);
var
  bSetCaption : boolean;
begin
  bSetCaption:= False;
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
