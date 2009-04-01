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

unit ArcIWOperaFix;

interface

{$I IntrawebVersion.inc}

uses SysUtils, IWControl, IWGlobal, IWAppForm, IWHTMLTag;

{$IFDEF IWVERCLASS5}

procedure FixForOpera(Control : TIWControl);
procedure FixCSS(Control : TIWControl);
function RenderHTML4(Control : TIWControl; OrigText : string) : string;
function RenderHTML5(Control : TIWControl; OrigTag : TIWHTMLTag): TIWHTMLTag;

{$ENDIF}

implementation

{$IFDEF IWVERCLASS5}
var
  OperaIsFixed : boolean;

procedure FixForOpera(Control : TIWControl);
begin
  //if not OperaIsFixed then
  //begin
    Control.AddScriptFile('/js/IWExplorer.js_'+GVersion);
    OperaIsFixed := True;
  //end;
end;

procedure FixCSS(Control : TIWControl);
var
  sCSS : string;
begin
  sCSS := '<style type="text/css">'#13'.'+Control.HTMLName+'_DIV_CSS {position:absolute;left:'+IntToStr(Control.Left)+
      ';top:'+IntToStr(Control.Top)+';z-index:'+IntToStr(Control.zindex)+
      ';text-decoration:none;}'#13'</style>';
  TIWAppForm(Control.Owner).ExtraHeader.Add(sCSS);
end;

function RenderHTML4(Control : TIWControl; OrigText : string) : string;
var
  sOrig : string;
begin
  FixForOpera(Control);
  FixCSS(Control);
  sOrig := OrigText;
  sOrig := Copy(sOrig,1,Pos('>',sOrig)-1)+
           ' ID="'+Control.HTMLName+'" CLASS="'+Control.HTMLName+'CSS"'+
           Copy(sOrig,Pos('>',sOrig),length(sOrig));
  Result := '<DIV ID="'+Control.HTMLName+'_DIV_" CLASS="'+Control.HTMLName+'_DIV_CSS">'+sOrig+'</DIV>';
end;

function RenderHTML5(Control : TIWControl; OrigTag : TIWHTMLTag): TIWHTMLTag;
var
  InheritedTag : TIWHTMLTag;
begin
  FixForOpera(Control);
  FixCSS(Control);

  Result := TIWHTMLTag.CreateTag('DIV');
  Result.AddStringParam('ID',Control.HTMLName+'_DIV_');
  Result.AddStringParam('CLASS',Control.HTMLName+'_DIV_CSS');
  InheritedTag := OrigTag;
  InheritedTag.AddStringParam('ID',Control.HTMLName);
  InheritedTag.AddStringParam('CLASS',Control.HTMLName+'CSS');
  Result.Contents.AddTagAsObject(InheritedTag);
end;

{$ENDIF}

end.
