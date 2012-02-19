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

unit ArcIWDlgConfirmPopup;

interface

{$I IntrawebVersion.inc}

uses
  SysUtils, Classes, Controls, IWForm, IWControl, IWHTMLTag, ArcIWDlgBase
  {$IFDEF INTRAWEB70}, IWRenderContext {$ENDIF}, ArcCommon;

type
  TConfirmationResult = (crOK, crCancel);
  TConfirmationEvent = procedure(Sender : TObject; Result : TConfirmationResult) of object;

  TConfirmPopupJavascript = class(TPersistent)
  private
    FCancel: TStrings;
    FOK: TStrings;
    procedure SetCancel(const Value: TStrings);
    procedure SetOK(const Value: TStrings);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
  published
    property OK : TStrings read FOK write SetOK;
    property Cancel : TStrings read FCancel write SetCancel;
  end;

  TArcIWDlgConfirmPopup = class(TIWControl)
  private
    FIWForm : TIWForm;
    FMessage: string;
    FOnConfirmed: TConfirmationEvent;
    FScript : TConfirmPopupJavascript;
  protected
    procedure Loaded; override;
    procedure SetValue(const AValue: String); override;
    {$IFDEF INTRAWEB70}
    procedure GetInputControlNames(ANames: TStringList); override;
    function IsForThisControl(AName: string): boolean; override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; overload;
    {$IFNDEF INTRAWEB70}
    function RenderHTML: TIWHTMLTag; override;
    {$ELSE}
      {$IFDEF INTRAWEB70}
      function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
      {$ELSE}
      function RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag; override;
      {$ENDIF}
    {$ENDIF}
  published
    property Message : string read FMessage write FMessage;
    property Script : TConfirmPopupJavascript read FScript write FScript;
    property OnConfirmed : TConfirmationEvent read FOnConfirmed write FOnConfirmed;
  end;

implementation

{$IFDEF INTRAWEB51}
uses IWBaseControl;
{$ENDIF}

{ TArcIWDlgConfirmPopup }

constructor TArcIWDlgConfirmPopup.Create(AOwner: TComponent);
var
  o : TControl;
begin
  inherited;
  o := self;
  while (not (o is TIWForm)) do
    o := TControl(o.Owner);

  FIWForm := TIWForm(o);
  FNeedsFormTag := True;

  FScript := TConfirmPopupJavascript.Create;
  {$IFNDEF INTRAWEB70}
  FSupportsInput := True;
  FSupportsSubmit := True;
  {$ENDIF}
end;

destructor TArcIWDlgConfirmPopup.Destroy;
begin
  FScript.Free;
  inherited;
end;
procedure TArcIWDlgConfirmPopup.Execute;
begin
  {$IFNDEF INTRAWEB70}
  FIWForm.AddToInitProc('Do'+Name+'Confirmation("'+FMessage+'");');
  {$ELSE}
  FIWForm.AddToInitProc('IWTop().Do'+Name+'Confirmation("'+FMessage+'");');
  {$ENDIF}
end;

procedure TArcIWDlgConfirmPopup.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FIWForm.JavaScript.Add( 'function Do'+Name+'Confirmation( msg ) {');
    FIWForm.JavaScript.Add( '  var '+Name+'Confirmed = confirm(msg);');
    FIWForm.JavaScript.Add( '  if ('+Name+'Confirmed==true) {');
    {$IFNDEF INTRAWEB70}
    FIWForm.JavaScript.Add( '    if (FindElem("'+HTMLName+'")) {');
    FIWForm.JavaScript.Add( '      FindElem("'+HTMLName+'").value="true";');
    FIWForm.JavaScript.Add( '    } ');
    FIWForm.JavaScript.Add( '    if (FindElem("'+HTMLName+'_HIDDEN")) {');
    FIWForm.JavaScript.Add( '      FindElem("'+HTMLName+'_HIDDEN").value="true";');
    FIWForm.JavaScript.Add( '    } ');
    FIWForm.Javascript.AddStrings(FScript.OK);
    FIWForm.JavaScript.Add( '  } else {');
    FIWForm.JavaScript.Add( '    if (FindElem("'+HTMLName+'")) {');
    FIWForm.JavaScript.Add( '      FindElem("'+HTMLName+'").value="false";');
    FIWForm.JavaScript.Add( '    } ');
    FIWForm.JavaScript.Add( '    if (FindElem("'+HTMLName+'_HIDDEN")) {');
    FIWForm.JavaScript.Add( '      FindElem("'+HTMLName+'_HIDDEN").value="false";');
    {$ELSE}
    FIWForm.JavaScript.Add( '    if (IWTop().FindElem("'+HTMLName+'")) {');
    FIWForm.JavaScript.Add( '      IWTop().FindElem("'+HTMLName+'").value="true";');
    FIWForm.JavaScript.Add( '    } ');
    FIWForm.JavaScript.Add( '    if (IWTop().FindElem("'+HTMLName+'_HIDDEN")) {');
    FIWForm.JavaScript.Add( '      IWTop().FindElem("'+HTMLName+'_HIDDEN").value="true";');
    FIWForm.JavaScript.Add( '    } ');
    FIWForm.Javascript.AddStrings(FScript.OK);
    FIWForm.JavaScript.Add( '  } else {');
    FIWForm.JavaScript.Add( '    if (IWTop().FindElem("'+HTMLName+'")) {');
    FIWForm.JavaScript.Add( '      IWTop().FindElem("'+HTMLName+'").value="false";');
    FIWForm.JavaScript.Add( '    } ');
    FIWForm.JavaScript.Add( '    if (IWTop().FindElem("'+HTMLName+'_HIDDEN")) {');
    FIWForm.JavaScript.Add( '      IWTop().FindElem("'+HTMLName+'_HIDDEN").value="false";');
    {$ENDIF}
    FIWForm.JavaScript.Add( '    } ');
    FIWForm.Javascript.AddStrings(FScript.Cancel);
    FIWForm.JavaScript.Add( '  }');
    FIWForm.JavaScript.Add( '  SubmitClick("'+HTMLName+'","",false);');
    FIWForm.JavaScript.Add( '}');
   {$IFDEF INTRAWEB70}
    FIWForm.HiddenFields.Add(HTMLName+'_HIDDEN=none');
   {$ENDIF}
  end;
end;

{$IFNDEF INTRAWEB70}
function TArcIWDlgConfirmPopup.RenderHTML: TIWHTMLTag;
{$ELSE}
  {$IFDEF INTRAWEB70}
  function TArcIWDlgConfirmPopup.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
  {$ELSE}
  function TArcIWDlgConfirmPopup.RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag; 
  {$ENDIF}
{$ENDIF}
begin
  Result := TIWHTMLTag.CreateTag('input');
  Result.AddStringParam('type','hidden');
  Result.AddStringParam('name',HTMLName);    // This should not be _HIDDEN!!!
  Result.AddStringParam('value','none');
end;

procedure TArcIWDlgConfirmPopup.SetValue(const AValue: String);
begin
  inherited;
  if (AValue='none') or (AValue='') then exit;
  if Assigned(FOnConfirmed) then
  begin
    if AValue = 'true' then
      FOnConfirmed(Self,crOK)
    else
      FOnConfirmed(Self,crCancel);
  end;
end;

{$IFDEF INTRAWEB70}
procedure TArcIWDlgConfirmPopup.GetInputControlNames(ANames: TStringList);
begin
  ANames.Add(HTMLName);
  ANames.Add(HTMLName+'_HIDDEN');
end;

function TArcIWDlgConfirmPopup.IsForThisControl(AName: string): boolean;
begin
  result := (HTMLName = AName) or (HTMLName+'_HIDDEN' = AName);
end;
{$ENDIF}

{ TConfirmPopupJavascript }

procedure TConfirmPopupJavascript.AssignTo(Dest: TPersistent);
begin
  inherited;

end;

constructor TConfirmPopupJavascript.Create;
begin
  inherited Create;
  FOK := TStringList.Create;
  FCancel := TStringList.Create;
end;

destructor TConfirmPopupJavascript.Destroy;
begin
  FOK.Free;
  FCancel.Free;
  inherited;
end;

procedure TConfirmPopupJavascript.SetCancel(const Value: TStrings);
begin
  FCancel.Assign(Value);
end;

procedure TConfirmPopupJavascript.SetOK(const Value: TStrings);
begin
  FOK.Assign(Value);
end;

end.

