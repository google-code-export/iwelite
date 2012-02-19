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
//  unit ArcIWImageURL                                                        //
//    Copyright 2002 by Arcana Technologies Incorporated                      //
//    Written By Jason Southwell                                              //
//                                                                            //
//  Description:                                                              //
//    This component provides "Image as a URL" components.  Essentially they  //
//    work though the TIWImage and TIWURL components were combined into one.  //
//    You can launch a url into a new window with specified parameters.       //
//
//    There are two components included in this unit.  TArcIWImage and        //
//    TArcIWImageFile.  The former includes the image compiled into the IW    //
//    application.  The latter uses a filename or URL property to acquire     //
//    the image.  This is identical to the normal TIWImage and TIWImageFile   //
//    functionality.                                                          //
//                                                                            //
//    These components are compatible with both IntraWeb versions 4 and 5.    //
//    To compile for the particular version you have installed, change        //
//    the compiler directive comment in IWVersion.inc.                        //
//                                                                            //
//    Information on IntraWeb can be found at www.atozedsoftware.com          //
//    Arcana Technologies Incorporated has no affilation with IntraWeb        //
//    or Atozed Software with the exception of being a satisfied customer.    //
//                                                                            //
//  Updates:                                                                  //
//    08/07/2002 - Released to TArcIWImageURL and TArcIWImageFileURL to Open  //
//                 Source.                                                    //
//    05/12/2003 - Removed support for IW4, Added support for IW6             //
//    10/02/2003 - Added support for IW7                                      //
//                                                                            //
//  License:                                                                  //
//    This code is covered by the Mozilla Public License 1.1 (MPL 1.1)        //
//    Full text of this license can be found at                               //
//    http://www.opensource.org/licenses/mozilla1.1.html                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit ArcIWImageURL;

interface

{$I IntrawebVersion.inc}

uses
  Windows, Messages, SysUtils, Classes, Controls, IWControl,
  IWScriptEvents, IWHTMLTag, IWHTMLControls,
  {$IFDEF IWVERCLASS6} IWRenderContext, IWBaseControlInterface, {$ENDIF}
  {$IFDEF INTRAWEB70} IWRenderContext, {$ENDIF}
  ArcIWOperaFix, IWTypes
  {$IFDEF INTRAWEB120}, IWCompExtCtrls {$ELSE}, IWExtCtrls {$ENDIF}, ArcCommon;

type
  TArcIWImageURL = class(TIWImage)
  protected
    FTargetOptions: TIWURLTarget;
    FTerminateApp: boolean;
    FUseTarget: boolean;
    FURL: string;
    //
    procedure SetTerminateApp(const AValue: boolean);
    procedure SetUseTarget(const AValue: boolean);
    {$IFDEF IWVERCLASS5}
    procedure HookEvents(AScriptEvents: TIWScriptEvents); override;
    {$ELSE}
    procedure HookEvents(APageContext: TIWPageContext40; AScriptEvents: TIWScriptEvents); override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF IWVERCLASS5}
    function RenderHTML: TIWHTMLTag; override;
    {$ELSE}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ELSE}
    function RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$ENDIF}
  published
    property TargetOptions: TIWURLTarget read FTargetOptions write FTargetOptions;
    property TerminateApp: boolean read FTerminateApp write FTerminateApp;
    property UseTarget: boolean read FUseTarget write FUseTarget;
    property Enabled;
    property ExtraTagParams;
    property ScriptEvents;
    property URL: string read FURL write FURL;
  end;

  TArcIWImageFileURL = class(TIWImageFile)
  private
    FTargetOptions: TIWURLTarget;
    FTerminateApp: boolean;
    FUseTarget: boolean;
    FURL: string;
    //
    procedure SetTerminateApp(const AValue: boolean);
    procedure SetUseTarget(const AValue: boolean);
    {$IFDEF IWVERCLASS5}
    procedure HookEvents(AScriptEvents: TIWScriptEvents); override;
    {$ELSE}
    procedure HookEvents(APageContext: TIWPageContext40; AScriptEvents: TIWScriptEvents); override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF IWVERCLASS5}
    function RenderHTML: TIWHTMLTag; override;
    {$ELSE}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ELSE}
    function RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$ENDIF}
  published
    property TargetOptions: TIWURLTarget read FTargetOptions write FTargetOptions;
    property TerminateApp: boolean read FTerminateApp write FTerminateApp;
    property UseTarget: boolean read FUseTarget write FUseTarget;
    property Enabled;
    property ExtraTagParams;
    property ScriptEvents;
    property URL: string read FURL write FURL;
  end;

implementation

uses
  {$IFDEF INTRAWEB110} IWSystem {$ELSE} SWSystem {$ENDIF};

{ TArcIWImageURL }

constructor TArcIWImageURL.Create(AOwner: TComponent);
begin
  inherited;
  FTargetOptions := TIWURLTarget.Create;
end;

destructor TArcIWImageURL.Destroy;
begin
  FreeAndNil(FTargetOptions);
  inherited;
end;

{$IFDEF IWVERCLASS5}
procedure TArcIWImageURL.HookEvents(AScriptEvents: TIWScriptEvents); 
begin
  inherited HookEvents(AScriptEvents);
  if Enabled then begin
  // TODO: Move OnClick code here
  end;
end;
{$ELSE}
procedure TArcIWImageURL.HookEvents(APageContext: TIWPageContext40; AScriptEvents: TIWScriptEvents);
begin
  inherited HookEvents(APageContext, AScriptEvents);
  if Enabled then begin
  // TODO: Move OnClick code here
  end;
end;
{$ENDIF}

{$IFDEF IWVERCLASS5}
function TArcIWImageURL.RenderHTML: TIWHTMLTag;
{$ELSE}
{$IFDEF INTRAWEB70}
function TArcIWImageURL.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ELSE}
function TArcIWImageURL.RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$ENDIF}
var
  LURL: string;
  LResult: String;
begin
  if not Enabled then
  begin
    {$IFDEF IWVERCLASS5}
    Result := inherited RenderHTML;
    {$ELSE}
    Result := inherited RenderHTML(AContext);
    {$ENDIF}
  end else
  begin
    {$IFDEF IWVERCLASS5}
    if WebApplication.Browser = brOpera then
      FixForOpera(Self);
    {$ENDIF}

    {$IFDEF IWVERCLASS5}
    Result := inherited RenderHTML;
    {$ELSE}
    Result := inherited RenderHTML(AContext);
    {$ENDIF}
    try
      if UseTarget then
      begin
        LResult := '';
        with TargetOptions do
        begin
          // personalbar
          // menubar
          // location - If yes, creates a Location entry field.
          // status - statusbar
          if Resizable then
          begin
            LResult := LResult + ',resizable=yes';
          end else
          begin
            LResult := LResult + ',resizable=no';
          end;
          if Toolbar then
          begin
            LResult := LResult + ',toolbar=yes';
          end else
          begin
            LResult := LResult + ',toolbar=no';
          end;
          if Scrollbars then
          begin
            LResult := LResult + ',scrollbars=yes';
          end else
          begin
            LResult := LResult + ',scrollbars=no';
          end;
          if Left > -1 then
          begin
            LResult := LResult + ',left=' + IntToStr(Left);
          end;
          if Top > -1 then
          begin
            LResult := LResult + ',top=' + IntToStr(Top);
          end;
          if Width > -1 then
          begin
            LResult := LResult + ',width=' + IntToStr(Width);
          end;
          if Height > -1 then
          begin
            LResult := LResult + ',height=' + IntToStr(Height);
          end;
        end;
        LResult := 'NewWindow(''' + FURL + ''', ''' + TargetOptions.WindowName
          + ''',''' + Copy(LResult, 2, MaxInt) + ''')';
      end else
      begin
        LURL := FURL;
        if TerminateApp then
        begin
          if SameText(Copy(LURL, 1, 7), 'http://') then
          begin
            Delete(LURL, 1, 7);
          end;
          {$IFDEF IWVERCLASS5}
          LURL := '/STOP/' + WebApplication.AppID + '/' + LURL;
          {$ELSE}
          LURL := '/STOP/' + AContext.WebApplication.AppID + '/' + LURL;
          {$ENDIF}
        end;
        LResult := 'parent.LoadURL(''' + LURL + ''')';
      end;
      {$IFDEF INTRAWEB70}
      HintEvents(Result);
      {$ELSE}
      HintEvents(Result, iif(Hint, Hint, FURL));
      {$ENDIF}
      if LResult <>'' then
        result.AddStringParam('OnClick', LResult);
    except
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

procedure TArcIWImageURL.SetTerminateApp(const AValue: boolean);
begin
  FTerminateApp := AValue;
  if AValue then begin
    FUseTarget := False;
  end;
end;

procedure TArcIWImageURL.SetUseTarget(const AValue: boolean);
begin
  FUseTarget := AValue;
  if AValue then begin
    FTerminateApp := False;
  end;
end;

{ TArcIWImageFileURL }

constructor TArcIWImageFileURL.Create(AOwner: TComponent);
begin
  inherited;
  FTargetOptions := TIWURLTarget.Create;
end;

destructor TArcIWImageFileURL.Destroy;
begin
  FreeAndNil(FTargetOptions);
  inherited;
end;

{$IFDEF IWVERCLASS5}
procedure TArcIWImageFileURL.HookEvents(AScriptEvents: TIWScriptEvents); 
begin
  inherited HookEvents(AScriptEvents);
  if Enabled then begin
  // TODO: Move OnClick code here
  end;
end;
{$ELSE}
procedure TArcIWImageFileURL.HookEvents(APageContext: TIWPageContext40; AScriptEvents: TIWScriptEvents);
begin
  inherited HookEvents(APageContext, AScriptEvents);
  if Enabled then begin
  // TODO: Move OnClick code here
  end;
end;
{$ENDIF}

{$IFDEF IWVERCLASS5}
function TArcIWImageFileURL.RenderHTML: TIWHTMLTag;
{$ELSE}
{$IFDEF INTRAWEB70}
function TArcIWImageFileURL.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ELSE}
function TArcIWImageFileURL.RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$ENDIF}
var
  LURL: string;
  LResult: String;
begin
  if not Enabled then
  begin
    {$IFDEF IWVERCLASS5}
    Result := inherited RenderHTML;
    {$ELSE}
    Result := inherited RenderHTML(AContext);
    {$ENDIF}
  end else
  begin
    {$IFDEF IWVERCLASS5}
    if WebApplication.Browser = brOpera then
      FixForOpera(Self);
    {$ENDIF}

    {$IFDEF IWVERCLASS5}
    Result := inherited RenderHTML;
    {$ELSE}
    Result := inherited RenderHTML(AContext);
    {$ENDIF}
    try
      if UseTarget then
      begin
        LResult := '';
        with TargetOptions do
        begin
          // personalbar
          // menubar
          // location - If yes, creates a Location entry field.
          // status - statusbar
          if Resizable then
          begin
            LResult := LResult + ',resizable=yes';
          end else
          begin
            LResult := LResult + ',resizable=no';
          end;
          if Toolbar then
          begin
            LResult := LResult + ',toolbar=yes';
          end else
          begin
            LResult := LResult + ',toolbar=no';
          end;
          if Scrollbars then
          begin
            LResult := LResult + ',scrollbars=yes';
          end else
          begin
            LResult := LResult + ',scrollbars=no';
          end;
          if Left > -1 then
          begin
            LResult := LResult + ',left=' + IntToStr(Left);
          end;
          if Top > -1 then
          begin
            LResult := LResult + ',top=' + IntToStr(Top);
          end;
          if Width > -1 then
          begin
            LResult := LResult + ',width=' + IntToStr(Width);
          end;
          if Height > -1 then
          begin
            LResult := LResult + ',height=' + IntToStr(Height);
          end;
        end;
        LResult := 'NewWindow(''' + FURL + ''', ''' + TargetOptions.WindowName
          + ''',''' + Copy(LResult, 2, MaxInt) + ''')';
      end else
      begin
        LURL := FURL;
        if TerminateApp then
        begin
          if SameText(Copy(LURL, 1, 7), 'http://') then
          begin
            Delete(LURL, 1, 7);
          end;
          {$IFDEF IWVERCLASS5}
          LURL := '/STOP/' + WebApplication.AppID + '/' + LURL;
          {$ELSE}
          LURL := '/STOP/' + AContext.WebApplication.AppID + '/' + LURL;
          {$ENDIF}
        end;
        LResult := 'parent.LoadURL(''' + LURL + ''')';
      end;
      {$IFDEF INTRAWEB70}
      HintEvents(Result);
      {$ELSE}
      HintEvents(Result, iif(Hint, Hint, FURL));
      {$ENDIF}
      if LResult <>'' then
        result.AddStringParam('OnClick', LResult);
    except
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

procedure TArcIWImageFileURL.SetTerminateApp(const AValue: boolean);
begin
  FTerminateApp := AValue;
  if AValue then begin
    FUseTarget := False;
  end;
end;

procedure TArcIWImageFileURL.SetUseTarget(const AValue: boolean);
begin
  FUseTarget := AValue;
  if AValue then begin
    FTerminateApp := False;
  end;
end;


end.
