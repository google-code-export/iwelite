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
//  unit ArcIWPageController                                                  //
//    Copyright 2002 by Arcana Technologies Incorporated                      //
//    Written By Jason Southwell                                              //
//                                                                            //
//  Description:                                                              //
//    This component provides control over browser functionality.             //
//    Specifically, in this version, the component provides the following:    //
//        Disabling of Right Clicks                                           //
//        Notifying the user upon Right Click                                 //
//        Disabling selection of text                                         //
//        Set Minimum and Maximum Hight and Width of the page                 //
//        Pop the page out of any frame that might be holding it hostage      //
//        Show the browser page maximized.                                    //
//                                                                            //
//    Please note that not all browsers allow control over such things        //
//                                                                            //
//    This component is compatible with both IntraWeb versions 4 and 5.       //
//                                                                            //
//    Information on IntraWeb can be found at www.atozedsoftware.com          //
//    Arcana Technologies Incorporated has no affilation with IntraWeb        //
//    or Atozed Software with the exception of being a satisfied customer.    //
//                                                                            //
//  Updates:                                                                  //
//    05/01/2002 - Released to TIWPageController to Open Source.              //
//    07/26/2002 - Changed Prefix from IW to ArcIW.  Added to a package.      //
//                 Moved component registration to common unit for package.   //
//                 Also changed the base component from IWComponent to        //
//                 IWControl.  This was to use the standard IW base class     //
//                 to simplify distribution.                                  //
//    05/12/2003 - Removed support for IW4, Added support for IW6             //
//    10/02/2003 - Added support for IW7                                      //
//                                                                            //
//  License:                                                                  //
//    This code is covered by the Mozilla Public License 1.1 (MPL 1.1)        //
//    Full text of this license can be found at                               //
//    http://www.opensource.org/licenses/mozilla1.1.html                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit ArcIWPageController;

interface

uses
  Windows, Messages, SysUtils, Classes, IWControl;

type
  TScreenConstraint = class(TPersistent)
  private
    FHeight: integer;
    FWidth: integer;
    FRedirect: string;
    FAlert: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Width : integer read FWidth write FWidth;
    property Height : integer read FHeight write FHeight;
    property Alert : string read FAlert write FAlert;
    property Redirect : string read FRedirect write FRedirect;
  end;

  TArcIWPageController = class(TIWControl)
  private
    FMaximize: boolean;
    procedure SetScreenMaximum(const Value: TScreenConstraint);
    procedure SetScreenMinimum(const Value: TScreenConstraint);
  protected
    FPopOut: boolean;
    FRightClickAllowed: boolean;
    FRightClickAlert: string;
    FTextSelectAllowed: boolean;
    FScreenMinimum: TScreenConstraint;
    FScreenMaximum: TScreenConstraint;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property PopOut : boolean read FPopOut write FPopOut;
    property RightClickAllowed : boolean read FRightClickAllowed write FRightClickAllowed default True;
    property RightClickAlert : string read FRightClickAlert write FRightClickAlert;
    property ScreenMinimum : TScreenConstraint read FScreenMinimum write SetScreenMinimum stored True;
    property ScreenMaximum : TScreenConstraint read FScreenMaximum write SetScreenMaximum stored True;
    property TextSelectAllowed : boolean read FTextSelectAllowed write FTextSelectAllowed default True;
    property Maximize : boolean read FMaximize write FMaximize;
  end;

implementation

uses IWAppForm;

{ TArcIWPageController }

constructor TArcIWPageController.Create(AOwner: TComponent);
begin
  inherited;
  FRightClickAllowed := True;
  FScreenMinimum := TScreenConstraint.Create;
  FScreenMaximum := TScreenConstraint.Create;
  FScreenMinimum.Width := 0;
  FScreenMinimum.Height := 0;
  FScreenMaximum.Width := 0;
  FScreenMaximum.Height := 0;
  FTextSelectAllowed := True;
end;

destructor TArcIWPageController.Destroy;
begin
  FScreenMinimum.Free;
  FScreenMaximum.Free;
  inherited;
end;

procedure TArcIWPageController.Loaded;
var
  s : string;
  script : TStringList;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    script := TStringList.Create;
    try
      if FPopOut then
        script.insert(0,'if (window!= top) top.location.href=location.href');

      if not FRightClickAllowed then
      begin
        script.add(' var message="";');
        script.add('function clickIE() { ');
        if FRightClickAlert <> '' then
          script.add('alert('''+FRightClickAlert+'''); ');
        script.add('if (document.all) {(message); return false;} }');
        script.add('function clickNS(e) { ');
        if FRightClickAlert <> '' then
          script.add('alert('''+FRightClickAlert+'''); ');
        script.add('if (document.layers||(document.getElementById&&!document.all)) { if (e.which==2||e.which==3) { (message); return false;}}}');
        script.add('if (document.layers)');
        script.add('{document.captureEvents(Event.MOUSEDOWN);document.onmousedown=clickNS;}');
        script.add('else{document.onmouseup=clickNS;document.oncontextmenu=clickIE;}');
        script.add('document.oncontextmenu=new Function("return false")');
      end;

      if (FScreenMinimum.Width > 0) or (FScreenMinimum.Height > 0) then
      begin
        if FScreenMinimum.Width > 0 then
        begin
          s := 'screen.width<'+IntToStr(FScreenMinimum.Width);
          if FScreenMinimum.Height > 0 then
            s := s+'||';
        end;
        if FScreenMinimum.Height > 0 then
          s := s+'screen.height<'+IntToStr(FScreenMinimum.Height);
        s := 'if ('+s+') {';
        if FScreenMinimum.Alert <> '' then
          s := s+' alert("'+FScreenMinimum.Alert+'");';
        if FScreenMinimum.Redirect <> '' then
          s := s+' window.location.replace("'+FScreenMinimum.Redirect+'");';
        s := s+'}';
        script.add(s);
      end;

      if (FScreenMaximum.Width > 0) or (FScreenMaximum.Height > 0) then
      begin
        if FScreenMaximum.Width > 0 then
        begin
          s := 'screen.width>'+IntToStr(FScreenMaximum.Width);
          if FScreenMaximum.Height > 0 then
            s := s+'||';
        end;
        if FScreenMaximum.Height > 0 then
          s := s+'screen.height>'+IntToStr(FScreenMaximum.Height);
        s := 'if ('+s+') {';
        if FScreenMaximum.Alert <> '' then
          s := s+' alert("'+FScreenMaximum.Alert+'");';
        if FScreenMaximum.Redirect <> '' then
          s := s+' window.location.replace("'+FScreenMaximum.Redirect+'");';
        s := s+'}';
        script.add(s);
      end;

      if not FTextSelectAllowed then
      begin
        script.add('function disableselect(e){ return false }');
        script.add('function reEnable(){ return true }');
        script.add('document.onselectstart=new Function ("return false") //IE4+');
        script.add('if (window.sidebar){ document.onmousedown=disableselect ');
        script.add('document.onclick=reEnable } //NS6');
      end;

      if FMaximize then
      begin
        script.add('top.window.moveTo(0,0);');
        script.add('if (document.all) { top.window.resizeTo(screen.availWidth,screen.availHeight);}');
        script.add('else if (document.layers||document.getElementById) {');
        script.add('if (top.window.outerHeight<screen.availHeight||top.window.outerWidth<screen.availWidth){');
        script.add('top.window.outerHeight = screen.availHeight; top.window.outerWidth = screen.availWidth; }}');
      end;
      if Owner is TIWAppForm then
        TIWAppForm(Owner).Javascript.AddStrings(script)
      else
        exit;
    finally
      script.Free;
    end;
  end;
end;

procedure TArcIWPageController.SetScreenMaximum(const Value: TScreenConstraint);
begin
  FScreenMaximum.Assign(Value);
end;

procedure TArcIWPageController.SetScreenMinimum(const Value: TScreenConstraint);
begin
  FScreenMinimum.Assign(Value);
end;

{ TScreenConstraint }

procedure TScreenConstraint.AssignTo(Dest: TPersistent);
begin
  if not (Dest is TScreenConstraint) then
    raise Exception.Create('You cannot assign to TScreenConstraint');
  TScreenConstraint(Dest).Height := FHeight;
  TScreenConstraint(Dest).Width := FWidth;
  TScreenConstraint(Dest).Redirect := FRedirect;
  TScreenConstraint(Dest).Alert := FAlert;
end;

end.
