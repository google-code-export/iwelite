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

unit ArcIWDlgProgress;

interface

uses
  SysUtils, Classes, IWAppForm, IWCompButton, IWFont, IWTypes, Controls,
  Graphics, IWForm, ArcIWDlgBase, ArcCommon;

{$I IntraWebVersion.inc}

type
  TArcTestProgressEvent = procedure(Sender : TObject; var Finished : boolean) of object;
  TProgressElementText = class(TPersistent)
  private
    FBtnTxtCancel: string;
  public
  published
    property BtnTxtCancel : string read FBtnTxtCancel write FBtnTxtCancel;
  end;

  TArcIWDlgProgress = class(TComponent)
  private
    FHasCancelButton: boolean;
    FWidth: integer;
    FHeight: integer;
    FCaption: string;
    FWindowColor: TColor;
    FCaptionBkColor: TColor;
    FTextBkColor: TColor;
    FCaptionFont: TIWFont;
    FOnCancel: TNotifyEvent;
    FOnTest: TArcTestProgressEvent;
    FTestInterval: integer;
    FClockImageURL: string;
    FBackgroundColor: TColor;
    FBackground: TIWBackground;
    procedure SetCaption(const Value: string);
  protected
    FElementText: TProgressElementText;

    function DoTestEvent : boolean;
    procedure DoCancelEvent;
  public
    DialogForm : TIWForm;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; overload;
    procedure InvalidateProperties; virtual;
  published
    property Background : TIWBackground read FBackground write FBackground;
    property BackgroundColor : TColor read FBackgroundColor write FBackgroundColor;
    property Width : integer read FWidth write FWidth;
    property Height : integer read FHeight write FHeight;
    property TextBkColor : TColor read FTextBkColor write FTextBkColor;
    property CaptionBkColor : TColor read FCaptionBkColor write FCaptionBkColor;
    property CaptionFont : TIWFont read FCaptionFont write FCaptionFont;
    property Caption : string read FCaption write SetCaption;
    property OnCancel : TNotifyEvent read FOnCancel write FOnCancel;
    property OnTest : TArcTestProgressEvent read FOnTest write FOnTest;
    property WindowColor : TColor read FWindowColor write FWindowColor;
    property HasCancelButton : boolean read FHasCancelButton write FHasCancelButton;
    property TestInterval : integer read FTestInterval write FTestInterval;
    property ClockImageURL : string read FClockImageURL write FClockImageURL;
    property ElementText : TProgressElementText read FElementText write FElementText;
  end;

implementation

uses uAppDlgProgressForm, IWApplication;

{ TArcIWDlgProgress }

constructor TArcIWDlgProgress.Create(AOwner: TComponent);
begin
  inherited;
  if Assigned(AOwner) and (not (AOwner is TIWAppForm)) then
    raise Exception.Create('This component may only be used on an IW Application Form.');
  FBackground := TIWBackground.Create;
  FElementText := TProgressElementText.Create;
  FElementText.FBtnTxtCancel := 'Cancel';

  FCaption := 'Please Wait...';
  FCaptionFont := TIWFont.Create;
  FCaptionBkColor := clNavy;
  FCaptionFont.Color := clWhite;
  FCaptionFont.Style := [fsBold];
  FTextBkColor := clSilver;
  FWindowColor := clBlack;
  FWidth := 270;
  FHeight := 170;
  FTestInterval := 2000;
  FBackgroundColor := clWhite;
end;

destructor TArcIWDlgProgress.Destroy;
begin
  FBackground.Free;
  FCaptionFont.Free;
  inherited;
end;

procedure TArcIWDlgProgress.DoCancelEvent;
begin
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

function TArcIWDlgProgress.DoTestEvent : boolean;
begin
  if Assigned(FOnTest) then
    FOnTest(Self,Result);
end;

procedure TArcIWDlgProgress.Execute;
var
  frm : TfrmDlgProgress;
begin
  DialogForm := TfrmDlgProgress.Create(TIWAppForm(Owner).WebApplication);
  frm := TfrmDlgProgress(DialogForm);
  frm.DlgComponent := Self;

  frm.rgnParent.Width := FWidth;
  frm.rgnParent.Height := FHeight;
  frm.Width := FWidth;
  frm.Height := FHeight;
  frm.rgnParent.Left := 0;
  frm.rgnParent.Top := 0;

  InvalidateProperties;
  frm.Show;
end;

procedure TArcIWDlgProgress.InvalidateProperties;
var
  frm : TfrmDlgProgress;
begin
  if DialogForm = nil then exit;
  frm := TfrmDlgProgress(DialogForm);

  frm.rctCaption.Text := FCaption;
  frm.rctCaption.Color := FCaptionBkColor;
  frm.rctCaption.Font.Assign(FCaptionFont);
  frm.rgnParent.Color := FWindowColor;
  frm.rgnText.Color   := FTextBkColor;
  frm.tmrTest.Interval := FTestInterval;
  frm.btnCancel.Caption := FElementText.BtnTxtCancel;

  if FClockImageURL = '' then
    {$IFDEF INTRAWEB50}
    frm.imgClock.ImageFile.URL := TIWAppForm(Self.Owner).WebApplication.URLBase +'/Files/ArcDlgClock.gif'
    {$ELSE}
    frm.imgClock.ImageFile.URL := TIWAppForm(Self.Owner).WebApplication.AppURLBase +'/Files/ArcDlgClock.gif'
    {$ENDIF}
  else
    frm.imgClock.ImageFile.URL := FClockImageURL;

  if FHasCancelButton then
    frm.btnCancel.Visible := True;

  if BrowserIsOpera(TIWAppForm(Self.Owner).WebApplication.Browser) then
  begin
    frm.btnCancel.Top := frm.btnCancel.Top+frm.rgnParent.Top+frm.rgnText.Top+frm.rgnButtons.Top;
    frm.btnCancel.Left := frm.btnCancel.Left+frm.rgnParent.Left+frm.rgnText.Left+frm.rgnButtons.Left;
  end;

  BackgroundColor := clWhite;
  Background.Fixed := FBackground.Fixed;
  Background.Filename := FBackground.Filename;
  Background.URL      := FBackground.URL;
  BackgroundColor     := FBackgroundColor;
end;

procedure TArcIWDlgProgress.SetCaption(const Value: string);
begin
  FCaption := Value;
  InvalidateProperties;
end;

end.
