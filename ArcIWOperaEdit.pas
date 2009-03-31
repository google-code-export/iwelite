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

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  unit ArcIWOperaEdit                                                       //
//    Copyright 2002 by Arcana Technologies Incorporated                      //
//    Written By Jason Southwell                                              //
//                                                                            //
//  Description:                                                              //
//    This component provides an Opera friendly version of the TIWEdit        //
//                                                                            //
//    This component is compatible with both IntraWeb versions 4 and 5.       //
//    To compile for the particular version you have installed, change        //
//    the compiler directive comment in IWVersion.inc.                        //
//                                                                            //
//    Information on IntraWeb can be found at www.atozedsoftware.com          //
//    Arcana Technologies Incorporated has no affilation with IntraWeb        //
//    or Atozed Software with the exception of being a satisfied customer.    //
//                                                                            //
//  Updates:                                                                  //
//    07/27/2002 - Released to TArcIWOperaEdit to Open Source.                //
//    05/12/2003 - Removed support for IW4, Added support for IW6             //
//    10/02/2003 - Added support for IW7                                      //
//                                                                            //
//  License:                                                                  //
//    This code is covered by the Mozilla Public License 1.1 (MPL 1.1)        //
//    Full text of this license can be found at                               //
//    http://www.opensource.org/licenses/mozilla1.1.html                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit ArcIWOperaEdit;

interface

{$I IntrawebVersion.inc}

uses
  Windows, Messages, SysUtils, Classes, Controls, IWControl, IWCompEdit, IWTypes,
  ArcIWOperaFix,
  {$IFDEF IWVERCLASS6} IWRenderContext, IWBaseControlInterface, IWScriptEvents, {$ENDIF}
  {$IFDEF INTRAWEB70} IWRenderContext, {$ENDIF}
  IWHTMLTag;

type
  TArcIWOperaEdit = class(TIWEdit)
  private
  protected
  public
    {$IFDEF IWVERCLASS5}
    function RenderHTML: TIWHTMLTag; override;
    {$ELSE}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ELSE}
    function RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$ENDIF}
    constructor Create(AOwner: TComponent); override;
  published
  end;

implementation

uses IWAppForm;

{ TArcIWOperaEdit }

constructor TArcIWOperaEdit.Create(AOwner: TComponent);
var
  browsers : TIWBrowsers;
begin
  inherited;
  if (csDesigning in ComponentState) then
  begin
    if not (brOpera in  TIWAppForm(Owner).SupportedBrowsers) then
    begin
      browsers := TIWAppForm(Owner).SupportedBrowsers;
      Include(browsers,brOpera);
      TIWAppForm(Owner).SupportedBrowsers := browsers;
    end;
  end;
end;

{$IFDEF IWVERCLASS5}
function TArcIWOperaEdit.RenderHTML: TIWHTMLTag;
begin
  if WebApplication.Browser = brOpera then
    Result := RenderHTML5(Self, inherited RenderHTML)
  else
    Result := inherited RenderHTML;
end;
{$ELSE}
{$IFDEF INTRAWEB70}
function TArcIWOperaEdit.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ELSE}
function TArcIWOperaEdit.RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := inherited RenderHTML(AContext);
end;
{$ENDIF}

end.
