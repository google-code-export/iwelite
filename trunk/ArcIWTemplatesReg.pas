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

unit ArcIWTemplatesReg;

interface

{$I IntrawebVersion.inc}

uses
  SysUtils, Classes, {$IFDEF VER130}DsgnIntf{$ELSE}DesignIntf{$ENDIF};

procedure Register;

implementation

uses ArcRuntimeConfig, {$IFNDEF INTRAWEB110} ArcIWLoginManager, {$ENDIF} ArcRuntimeConfigEditor,
  ArcServerConfig, ArcStyleManager,
  {$IFDEF INTRAWEB51}ArcIWTemplateProcessor,{$ELSE}
  {$IFDEF INTRAWEB72}ArcIWTemplateProcessor7,{$ELSE}
  ArcIWTemplateProcessor6,{$ENDIF}{$ENDIF}
  ArcIWServerTimer, ArcIWPageIcon{, ArcIWFilterManager};

procedure Register;
begin
  RegisterComponents( 'IWES NonVisual', [TArcRuntimeConfig, {$IFNDEF INTRAWEB110} TArcIWLoginManager, {$ENDIF}
    TArcIWServerConfig, TArcIWTemplateProcessor, TArcIWServerTimer, TArcIWPageIcon,
    TArcStyleManager]);
    //,TArcIWFilterManager]);
  RegisterComponentEditor( TArcRuntimeConfig, TArcRuntimeConfigEditor);
  RegisterComponentEditor( TArcIWServerConfig, TArcServerConfigEditor);
  RegisterComponentEditor( TArcStyleManager, TArcStyleManagerEditor);
end;

end.
