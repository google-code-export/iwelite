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

unit ArcIWInteropRenderer;

interface

uses
  SysUtils, Classes, ArcIWInteropController, IWBaseHTMLInterfaces, IWBaseForm,
  IWBaseHTMLComponent, IWBaseInterfaces, IWRenderContext, IWHTMLTag, IWHTML40Interfaces,
  IWBaseRenderContext,
{$IFDEF FASTSTRINGS}
  ArcFastStrings,
{$ELSE}
  ArcStrings,
{$ENDIF}
  ArcIWInteropCommon;

type
  TArcIWInteropRenderer = class(TIWBaseHTMLComponent, IIWSubmitControl)
  private
    FSubmitParam : string;
    FInteropController: TArcIWInteropController;
    procedure SetInteropController(const Value: TArcIWInteropController);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    function ComponentContextClass: TIWBaseComponentContextClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Submit(const AValue: String);
    function GetSubmitParam: String;
    function getSubmitProc: TSubmitProc;
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
  published
    property InteropController : TArcIWInteropController read FInteropController write SetInteropController;
  end;

implementation

{ TArcIWInteropRenderer }

function TArcIWInteropRenderer.ComponentContextClass: TIWBaseComponentContextClass;
begin
  Result := TIWComponent40Context;
end;

constructor TArcIWInteropRenderer.Create(AOwner: TComponent);
begin
  if not (AOwner is TIWBaseForm) then
    raise Exception.Create('TArcIWInteropRenderer is intended to be used on an IntraWeb Form');
  inherited;
end;

destructor TArcIWInteropRenderer.Destroy;
begin

  inherited;
end;

function TArcIWInteropRenderer.GetSubmitParam: String;
begin
  Result := FSubmitParam;
end;

function TArcIWInteropRenderer.getSubmitProc: TSubmitProc;
begin
  Result := Submit;
end;

procedure TArcIWInteropRenderer.Loaded;
begin
  inherited;
{  if not (csDesigning in ComponentState) and (not Assigned(FInteropController)) then
    raise Exception.Create('InteropController cannot be nil');}
end;

procedure TArcIWInteropRenderer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FInteropController) then
    FInteropController := nil;
end;

function TArcIWInteropRenderer.RenderHTML(
  AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
begin
  if not (csDesigning in ComponentState) and (not Assigned(FInteropController)) then
    raise Exception.Create('InteropController cannot be nil');
  {FWebapplication := TIWPageContext40(AContext.PageContext).WebApplication;
  TIWPageContext40(AContext.PageContext).AppendHiddenInput(HTMLName+'_val');}
  Result := TIWHTMLTag.CreateHTMLTag('SCRIPT');
  Result.AddStringParam('Language','Javascript'); 
  Result.Contents.AddText(FInteropController.RenderJavaScript(HTMLName));
end;

procedure TArcIWInteropRenderer.SetInteropController(
  const Value: TArcIWInteropController);
begin
  if Assigned(FInteropController) then begin
    FInteropController.RemoveFreeNotification(Self);
  end;
  FInteropController := Value;
  if Assigned(FInteropController) then begin
    FInteropController.FreeNotification(Self);
  end;
end;

procedure TArcIWInteropRenderer.Submit(const AValue: String);
var
  sAction, sValue : string;
  iPos : integer;
begin
  FSubmitParam := AValue;
  iPos := FastCharPos(AValue,'~',1);
  sAction := Copy(AValue,1,iPos-1);
  sValue := Copy(AValue,iPos+1,High(Integer));
  
  FInteropController.ClickActionByName(sAction, sValue);
end;

end.
