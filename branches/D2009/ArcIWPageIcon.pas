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

unit ArcIWPageIcon;

interface

{$I Intrawebversion.inc}

uses
  SysUtils, Classes, Controls, IWHTMLTag, IWControl, IWTypes,
  {$IFDEF INTRAWEB70} IWBaseRenderContext, IWMarkupLanguageTag,{$ENDIF}
  {$IFNDEF INTRAWEB51} IWVCLBaseControl, IWRenderContext, {$ENDIF}
  IWFileReference;

type
  TArcIWPageIcon = class({$IFNDEF INTRAWEB51}TIWCustomControl{$ELSE}TIWControl{$ENDIF})
  private
    FIcon: TIWFileReference;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
  published
    property Icon : TIWFileReference read FIcon write FIcon;
  end;

implementation

{ TArcIWPageIcon }

constructor TArcIWPageIcon.Create(AOwner: TComponent);
begin
  inherited;
  FIcon := TIWFileReference.Create;
end;

destructor TArcIWPageIcon.Destroy;
begin
  FIcon.Free;
  inherited;
end;

{$IFDEF INTRAWEB70}
function TArcIWPageIcon.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; 
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWPageIcon.RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB51}
function TArcIWPageIcon.RenderHTML: TIWHTMLTag;
{$ENDIF}
begin
  {$IFNDEF INTRAWEB70}
  Result := TIWHTMLTag.CreateTag('LINK',cbFalse);
  {$ELSE}
  Result := TIWHTMLTag.CreateTag('LINK');
  Result.ClosingTag := cbFalse;
  {$ENDIF}
  Result.AddStringParam('rel', 'SHORTCUT ICON');
  {$IFNDEF INTRAWEB51}
  Result.AddStringParam('href', FIcon.Location(AContext.WebApplication.AppURLBase));
  {$ELSE}
  Result.AddStringParam('href', FIcon.Location(WebApplication.AppURLBase));
  {$ENDIF}
  Result.IsPreformated := True;
end;

end.
