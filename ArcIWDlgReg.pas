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

unit ArcIWDlgReg;

interface

uses SysUtils, Classes, IWKlooch;

{$I Eval.inc}
{$I IntrawebVersion.inc}
(*
{$R TArcIWDlgConfirmPopup.bmp}
{$R TArcIWDlgEmailUserLogin.bmp}
{$R TArcIWDlgLogin.bmp}
{$R TArcIWDlgMessageBox.bmp}
{$R TArcIWDlgPopupRegion.bmp}
{$R TArcIWDlgPrint.bmp}
{$R TArcIWDlgProgress.bmp}
{$R TArcIWDlgScrollableMessageBox.bmp}
{$R TArcIWDlgSearchReplace.bmp}
{$R TArcIWDlgTimeout.bmp}
*)

procedure Register;

implementation

uses ArcIWDlgMessageBox, ArcIWDlgLogin, ArcIWDlgEmailUserLogin, ArcIWDlgProgress,
  ArcIWDlgPrint, ArcIWDlgSearchReplace, ArcIWDlgTimeout, ArcIWDlgConfirmPopup,
  ArcIWDlgScrollableMessageBox{$IFDEF INTRAWEB70}, ArcIWDlgPopupRegion{$ENDIF};

procedure Register;
begin
  RegisterComponents('IWES Dialogs', [TArcIWDlgMessageBox, TArcIWDlgLogin,
    TArcIWDlgEmailUserLogin, TArcIWDlgProgress, TArcIWDlgSearchReplace,
    TArcIWDlgTimeout, TArcIWDlgPrint, TArcIWDlgConfirmPopup,
    TArcIWDlgScrollableMessageBox {$IFDEF INTRAWEB70}, TArcIWDlgPopupRegion{$ENDIF}]);
end;

initialization
  {$IFDEF EVAL}
  GLicense.ThirdPartyHook('eval mode',True);
  {$ENDIF}

end.
 
