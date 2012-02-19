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
//  unit ArcIWToolWindow                                                      //
//    Copyright 2002 by Arcana Technologies Incorporated                      //
//    Written By Jason Southwell                                              //
//    Based on Javascript written by Jim Silver @ jimsilver47@yahoo.com       //
//                                                                            //
//  Description:                                                              //
//    This component provides an IntraWeb implementation of a tool window.    //
//    This tool window floats on the page.  The user is able to move and      //
//    close the window.  The tool window can contain either HTML text         //
//    or an embedded html page.                                               //
//                                                                            //
//    This component is compatible with both IntraWeb versions 4 and 5,       //
//    though no testing has been performed on IW4.  If you have IW4,          //
//    please let me know if there are any problems.                           //
//
//    To compile for the particular version you have installed, change        //
//    the compiler directive comment in IWVersion.inc.                        //
//                                                                            //
//    Information on IntraWeb can be found at www.atozedsoftware.com          //
//    Arcana Technologies Incorporated has no affilation with IntraWeb        //
//    or Atozed Software with the exception of being a satisfied customer.    //
//                                                                            //
//  Updates:                                                                  //
//    06/12/2002 - Released to TIWToolWindow to Open Source.                  //
//    06/13/2002 - Fixed color properties in the IW4 version                  //
//    07/26/2002 - Changed Prefix from IW to ArcIW.  Added to a package.      //
//                 Moved component registration to common unit for package.   //                            
//    05/12/2003 - Removed support for IW4, Added support for IW6             //
//    10/02/2003 - Added support for IW7                                      //
//                                                                            //
//  License:                                                                  //
//    This code is covered by the Mozilla Public License 1.1 (MPL 1.1)        //
//    Full text of this license can be found at                               //
//    http://www.opensource.org/licenses/mozilla1.1.html                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit ArcIWToolWindow;

interface

{$I IntrawebVersion.inc}

uses
  Windows, Messages, Graphics, SysUtils, Classes, IWControl,
  {$IFDEF IWVERCLASS6} IWRenderContext, IWBaseControlInterface, IWScriptEvents, {$ENDIF}
  {$IFDEF INTRAWEB70} IWRenderContext, {$ENDIF}
  IWHTMLTag, ArcCommon;

type
  TArcIWToolWindow = class(TIWControl)
  private
    FCaption: string;
    FText: string;
    FBackgroundColor: TColor;
    FBorderColor: TColor;
    FURL: string;
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
    property Caption : string read FCaption write FCaption;
    property Text    : string read FText    write FText;
    property URL     : string read FURL     write FURL;
    property BorderColor : TColor read FBorderColor write FBorderColor;
    property BackgroundColor : TColor read FBackgroundColor write FBackgroundColor;
  end;

implementation

{ TArcIWToolWindow }

constructor TArcIWToolWindow.Create(AOwner: TComponent);
begin
  inherited;
  FBackgroundColor := clWhite;
  FBorderColor := clBlue;
end;

{$IFDEF IWVERCLASS5}
function TArcIWToolWindow.RenderHTML : TIWHTMLTag;
var
  table, table2, tag: TIWHTMLTag;
begin
  {$IFNDEF IWVERSION51}
  AddScriptFile(WebApplication.URLBase +'/files/ArcIWToolWindow.js');
  {$ELSE}
  AddScriptFile(WebApplication.AppURLBase +'/files/ArcIWToolWindow.js');
  {$ENDIF}
  Result := TIWHTMLTag.CreateTag('div');
  Result.AddStringParam('id','TheIWToolWindow');
  Result.AddStringParam('style','position:absolute;width:'+IntToStr(Width)+'px;left:'+IntToStr(Left)+';top:'+IntToStr(Top));
  table := Result.Contents.AddTag('table');
  table.AddIntegerParam('border',0);
  table.AddIntegerParam('width',0);
  table.AddColor('bgcolor',FBorderColor);
  table.AddIntegerParam('cellspacing',0);
  table.AddIntegerParam('cellpadding',2);
  tag := table.Contents.AddTag('tr');
  tag := tag.Contents.AddTag('td');
  tag.AddStringParam('width','100%');
  table2 := tag.Contents.AddTag('table');
  table2.AddIntegerParam('border',0);
  table2.AddStringParam('width','100%');
  table2.AddIntegerParam('cellspacing',0);
  table2.AddIntegerParam('cellpadding',0);
  table2.AddIntegerParam('height',36);
  tag := table2.Contents.AddTag('tr');
  with tag.Contents.AddTag('td') do
  begin
    AddStringParam('id','IWToolWindowDragBar');
    AddStringParam('style','cursor:hand');
    AddStringParam('width','100%');
    with Contents.AddTag('ilayer') do
    begin
      AddStringParam('width','100%');
      AddStringParam('onSelectStart','return false');
      with Contents.AddTag('layer') do
      begin
        AddStringParam('width','100%');
        AddStringParam('onMouseover','dragswitch=1;if (ns4) drag_dropns(showimage)');
        AddStringParam('onMouseout','dragswitch=0');
        with Contents.AddTag('font') do
        begin
          AddStringParam('face','Verdana');
          AddColor('color',FBackgroundColor);
          with Contents.AddTag('strong') do
            with Contents.AddTag('small') do
              contents.AddText(FCaption);
        end;
      end;
    end;
  end;
  with tag.Contents.AddTag('td') do
  begin
    AddStringParam('style','cursor:hand');
    with Contents.AddTag('a') do
    begin
      AddStringParam('href','#');
      AddStringParam('onClick','hidebox();return false');
      with Contents.AddTag('img') do
      begin
        {$IFNDEF IWVERSION51}
        AddStringParam('src',WebApplication.URLBase +'/files/ArcIWToolWindowClose.jpg');
        {$ELSE}
        AddStringParam('src',WebApplication.AppURLBase +'/files/ArcIWToolWindowClose.jpg');
        {$ENDIF}
        AddIntegerParam('width',16);
        AddIntegerParam('height',14);
        AddIntegerParam('border',0);
      end;
    end;
  end;
  tag := table2.Contents.AddTag('tr');
  tag.AddIntegerParam('height',Height);
  with tag.Contents.AddTag('td') do
  begin
    AddStringParam('width','100%');
    AddColor('bgcolor',FBackgroundColor);
    AddStringParam('style','padding:4px');
    AddIntegerParam('colspan',2);
    if FURL <>'' then
    begin
      with Contents.AddTag('iframe') do
      begin
        AddStringParam('src',FURL);
        AddIntegerParam('MarginWidth',0);
        AddIntegerParam('MarginHeight',0);
        AddStringParam('Width','100%');
        AddStringParam('Height','100%');
        AddStringParam('Align','top');
        AddStringParam('Scrolling','auto');
      end;
    end else
      Contents.AddText(FText);
  end;
end;
{$ELSE}
{$IFDEF INTRAWEB70}
function TArcIWToolWindow.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ELSE}
function TArcIWToolWindow.RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
var
  table, table2, tag: TIWHTMLTag;
begin
  TIWComponent40Context(Acontext).AddScriptFile(Acontext.WebApplication.AppURLBase +'/files/ArcIWToolWindow.js');
  Result := TIWHTMLTag.CreateTag('div');
  Result.AddStringParam('id','TheIWToolWindow');
  Result.AddStringParam('style','position:absolute;width:'+IntToStr(Width)+'px;left:'+IntToStr(Left)+';top:'+IntToStr(Top));
  table := Result.Contents.AddTag('table');
  table.AddIntegerParam('border',0);
  table.AddIntegerParam('width',0);
  table.AddColor('bgcolor',FBorderColor);
  table.AddIntegerParam('cellspacing',0);
  table.AddIntegerParam('cellpadding',2);
  tag := table.Contents.AddTag('tr');
  tag := tag.Contents.AddTag('td');
  tag.AddStringParam('width','100%');
  table2 := tag.Contents.AddTag('table');
  table2.AddIntegerParam('border',0);
  table2.AddStringParam('width','100%');
  table2.AddIntegerParam('cellspacing',0);
  table2.AddIntegerParam('cellpadding',0);
  table2.AddIntegerParam('height',36);
  tag := table2.Contents.AddTag('tr');
  with tag.Contents.AddTag('td') do
  begin
    AddStringParam('id','IWToolWindowDragBar');
    AddStringParam('style','cursor:hand');
    AddStringParam('width','100%');
    with Contents.AddTag('ilayer') do
    begin
      AddStringParam('width','100%');
      AddStringParam('onSelectStart','return false');
      with Contents.AddTag('layer') do
      begin
        AddStringParam('width','100%');
        AddStringParam('onMouseover','dragswitch=1;if (ns4) drag_dropns(showimage)');
        AddStringParam('onMouseout','dragswitch=0');
        with Contents.AddTag('font') do
        begin
          AddStringParam('face','Verdana');
          AddColor('color',FBackgroundColor);
          with Contents.AddTag('strong') do
            with Contents.AddTag('small') do
              contents.AddText(FCaption);
        end;
      end;
    end;
  end;
  with tag.Contents.AddTag('td') do
  begin
    AddStringParam('style','cursor:hand');
    with Contents.AddTag('a') do
    begin
      AddStringParam('href','#');
      AddStringParam('onClick','hidebox();return false');
      with Contents.AddTag('img') do
      begin
        AddStringParam('src',AContext.WebApplication.AppURLBase +'/files/ArcIWToolWindowClose.jpg');
        AddIntegerParam('width',16);
        AddIntegerParam('height',14);
        AddIntegerParam('border',0);
      end;
    end;
  end;
  tag := table2.Contents.AddTag('tr');
  tag.AddIntegerParam('height',Height);
  with tag.Contents.AddTag('td') do
  begin
    AddStringParam('width','100%');
    AddColor('bgcolor',FBackgroundColor);
    AddStringParam('style','padding:4px');
    AddIntegerParam('colspan',2);
    if FURL <>'' then
    begin
      with Contents.AddTag('iframe') do
      begin
        AddStringParam('src',FURL);
        AddIntegerParam('MarginWidth',0);
        AddIntegerParam('MarginHeight',0);
        AddStringParam('Width','100%');
        AddStringParam('Height','100%');
        AddStringParam('Align','top');
        AddStringParam('Scrolling','auto');
      end;
    end else
      Contents.AddText(FText);
  end;
end;
{$ENDIF}

end.
