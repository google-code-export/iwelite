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

unit ArcIWDlgLogin;

interface

uses
  SysUtils, Classes, IWAppForm, IWCompButton, IWFont, IWTypes, Controls,
  Graphics, IWForm, ArcIWDlgBase;



type
  TArcLoginNotifyEvent = procedure(Sender : TObject; Username, Password : string) of object;

  TLoginElementText = class(TPersistent)
  private
    FLblTxtUsername: string;
    FBtnTxtRegister: string;
    FLblTxtPassword: string;
    FBtnTxtCancel: string;
    FBtnTxtLogin: string;
  public
  published
    property BtnTxtLogin : string read FBtnTxtLogin write FBtnTxtLogin;
    property BtnTxtRegister : string read FBtnTxtRegister write FBtnTxtRegister;
    property BtnTxtCancel : string read FBtnTxtCancel write FBtnTxtCancel;
    property LblTxtUsername : string read FLblTxtUsername write FLblTxtUsername;
    property LblTxtPassword : string read FLblTxtPassword write FLblTxtPassword;
  end;

  TArcIWDlgLogin = class(TComponent)
  private
    FWidth: integer;
    FHeight: integer;
    FCaption: string;
    FWindowColor: TColor;
    FTextBkColor: TColor;
    FCaptionBkColor: TColor;
    FCaptionFont: TIWFont;
    FTextFont: TIWFont;
    FOnLogin: TArcLoginNotifyEvent;
    FOnCancel: TNotifyEvent;
    FOnRegister: TNotifyEvent;
    FHasRegisterButton: boolean;
    FBackgroundColor: TColor;
    FBackground: TIWBackground;
  protected
    FElementText: TLoginElementText;
    procedure DoLoginEvent(Username, Password : string);
    procedure DoRegisterEvent;
    procedure DoCancelEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; overload;
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
    property OnLogin : TArcLoginNotifyEvent read FOnLogin write FOnLogin;
    property OnCancel : TNotifyEvent read FOnCancel write FOnCancel;
    property OnRegister : TNotifyEvent read FOnRegister write FOnRegister;
    property WindowColor : TColor read FWindowColor write FWindowColor;
    property HasRegisterButton : boolean read FHasRegisterButton write FHasRegisterButton;
    property ElementText : TLoginElementText read FElementText write FElementText;
  end;

implementation

uses uAppDlgLoginForm;

{ TArcIWDlgLogin }

constructor TArcIWDlgLogin.Create(AOwner: TComponent);
begin
  inherited;
  if Assigned(AOwner) and (not (AOwner is TIWAppForm)) then
    raise Exception.Create('This component may only be used on an IW Application Form.');
  FBackground := TIWBackground.Create;
  FElementText := TLoginElementText.Create;
  FElementText.FLblTxtUsername := 'Username:';
  FElementText.FLblTxtPassword := 'Password:';
  FElementText.FBtnTxtRegister := 'Register';
  FElementText.FBtnTxtCancel := 'Cancel';
  FElementText.FBtnTxtLogin := 'Login';

  FCaption := 'Login';
  FCaptionFont := TIWFont.Create;
  FTextFont := TIWFont.Create;
  FCaptionBkColor := clNavy;
  FCaptionFont.Color := clWhite;
  FCaptionFont.Style := [fsBold];
  FTextBkColor := clSilver;
  FWindowColor := clBlack;
  FWidth := 270;
  FHeight := 130;
  FBackgroundColor := clWhite;
end;

destructor TArcIWDlgLogin.Destroy;
begin
  FBackground.Free;
  FCaptionFont.Free;
  FTextFont.Free;
  inherited;
end;

procedure TArcIWDlgLogin.DoCancelEvent;
begin
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

procedure TArcIWDlgLogin.DoLoginEvent(Username, Password: string);
begin
  if Assigned(FOnLogin) then
    FOnLogin(Self,Username,Password);
end;

procedure TArcIWDlgLogin.DoRegisterEvent;
begin
  if Assigned(FOnRegister) then
    FOnRegister(Self);
end;

procedure TArcIWDlgLogin.Execute;
var
  frm : TfrmDlgLogin;
  iBtn, x : integer;
begin
  frm := TfrmDlgLogin.Create(TIWAppForm(Owner).WebApplication);
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
    rgnParent.Color := FWindowColor;
    rgnText.Color   := FTextBkColor;
    lblUsername.Font.Assign(FTextFont);
    lblPassword.Font.Assign(FTextFont);
    lblUsername.Caption := FElementText.LblTxtUsername;
    lblPassword.Caption := FElementText.LblTxtPassword;

    if FHasRegisterButton then
    begin
      btn1.Caption := FElementText.FBtnTxtRegister;
      btn2.Caption := FElementText.FBtnTxtCancel;
      btn3.Caption := FElementText.FBtnTxtLogin;
      btn2.DoSubmitValidation := False;
      btn3.Visible := True;
    end else
    begin
      btn1.Caption := FElementText.FBtnTxtCancel;
      btn2.Caption := FElementText.FBtnTxtLogin;
    end;
    if (TIWAppForm(Self.Owner).WebApplication.Browser = brOpera) then
    begin
      btn1.Top := btn1.Top+rgnParent.Top+rgnText.Top+rgnButtons.Top;
      btn2.Top := btn2.Top+rgnParent.Top+rgnText.Top+rgnButtons.Top;
      btn3.Top := btn3.Top+rgnParent.Top+rgnText.Top+rgnButtons.Top;
      edtUsername.Top := edtUsername.Top+rgnParent.Top+rgnText.Top;
      edtPassword.Top := edtPassword.Top+rgnParent.Top+rgnText.Top;
      btn1.Left := btn1.Left+rgnParent.Left+rgnText.Left+rgnButtons.Left;
      btn2.Left := btn2.Left+rgnParent.Left+rgnText.Left+rgnButtons.Left;
      btn3.Left := btn3.Left+rgnParent.Left+rgnText.Left+rgnButtons.Left;
      edtUsername.Left := edtUsername.Left+rgnParent.Left+rgnText.Left;
      edtPassword.Left := edtPassword.Left+rgnParent.Left+rgnText.Left;
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

