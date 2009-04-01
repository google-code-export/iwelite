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

unit ArcIWDlgTimeout;

interface

uses
  SysUtils, Classes, IWForm, IWGlobal, IWServerControllerBase, ArcIWDlgBase;

type
  TArcIWDlgTimeout = class(TComponent)
  private
    FWarnAt: integer;
    FWarning: string;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property WarnAt : integer read FWarnAt write FWarnAt default 60;
    property Warning : string read FWarning write FWarning;
  end;

implementation

{ TArcIWDlgTimeout }

constructor TArcIWDlgTimeout.Create(AOwner: TComponent);
begin
  inherited;
  if Assigned(AOwner) and (not (AOwner is TIWForm)) then
    raise Exception.Create('This component may only be used on an IW Application Form.');

  {FNeedsFormTag := True;
  FSupportsSubmit := True;}

  FWarnAt := 60;
  FWarning := 'Your session will timeout in 60 seconds.\n\nDo you need more time?';
end;

procedure TArcIWDlgTimeout.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    TIWForm(Owner).JavaScript.Add('function on'+Name+'Timer(){');
    TIWForm(Owner).JavaScript.Add('  var NeedMoreTime = confirm("'+FWarning+'");');
    TIWForm(Owner).JavaScript.Add('  if (NeedMoreTime == true) {');
    TIWForm(Owner).JavaScript.Add('    SubmitClick("","",false);');
    TIWForm(Owner).JavaScript.Add('  }');
    TIWForm(Owner).JavaScript.Add('}');
    TIWForm(Owner).Javascript.Add('var '+Name+'Timer = setTimeout("on'+Name+'Timer()",'+IntToStr((GServerController.SessionTimeout*60*1000)-(FWarnAt*1000))+');');
  end;
end;

end.
 
