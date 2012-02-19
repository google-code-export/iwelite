unit ArcCommon;

interface

{$I IntrawebVersion.inc}

{$IFDEF INTRAWEB110}

uses
  Classes, IWRenderContext, IWSystem
  {$IFDEF INTRAWEB120}
  , IWServerInternalFiles, IW.Browser.Browser, IW.Browser.InternetExplorer, IW.Browser.Safari
  {$ELSE}
  , IWTypes
  {$ENDIF};

type
  TIWBaseHTMLComponentContext = TIWCompContext;
  TIWComponent40Context = TIWCompContext;
  TIWBaseComponentContextClass = class of TIWBaseHTMLComponentContext;

const
  DebugEOL = EOL;

{$ENDIF}

{$IFDEF INTRAWEB120}

type
  TIWStringList = TStringList;

  TIWServer = class
  public
    class procedure AddInternalFile(const aResName,aFilename: string);
  end;

function BrowserIsIE(ABrowser: TBrowser): Boolean;
function BrowserIsIE4(ABrowser: TBrowser): Boolean;
function BrowserIsOpera(ABrowser: TBrowser): Boolean;
function BrowserIsNetscape6(ABrowser: TBrowser): Boolean;
function BrowserIsNetscape7(ABrowser: TBrowser): Boolean;
{$ELSE}
function BrowserIsIE(ABrowser: TIWBrowser): Boolean;
function BrowserIsIE4(ABrowser: TIWBrowser): Boolean;
function BrowserIsOpera(ABrowser: TIWBrowser): Boolean;
function BrowserIsNetscape6(ABrowser: TIWBrowser): Boolean;
function BrowserIsNetscape7(ABrowser: TIWBrowser): Boolean;
{$ENDIF}

// AContext.Browser = brIE

implementation

{$IFDEF INTRAWEB120}
function BrowserIsIE(ABrowser: TBrowser): Boolean;
{$ELSE}
function BrowserIsIE(ABrowser: TIWBrowser): Boolean;
{$ENDIF}
begin
{$IFDEF INTRAWEB120}
  Result:= ABrowser is TInternetExplorer;
{$ELSE}
  Result:= ABrowser = brIE;
{$ENDIF}
end;

{$IFDEF INTRAWEB120}
function BrowserIsIE4(ABrowser: TBrowser): Boolean;
{$ELSE}
function BrowserIsIE4(ABrowser: TIWBrowser): Boolean;
{$ENDIF}
begin
{$IFDEF INTRAWEB120}
  Result:= False;
{$ELSE}
  Result:= ABrowser = brIE4;
{$ENDIF}
end;

{$IFDEF INTRAWEB120}
function BrowserIsOpera(ABrowser: TBrowser): Boolean;
{$ELSE}
function BrowserIsOpera(ABrowser: TIWBrowser): Boolean;
{$ENDIF}
begin
{$IFDEF INTRAWEB120}
  Result:= False;
{$ELSE}
  Result:= ABrowser = brOpera;
{$ENDIF}
end;

{$IFDEF INTRAWEB120}
function BrowserIsNetscape6(ABrowser: TBrowser): Boolean;
{$ELSE}
function BrowserIsNetscape6(ABrowser: TIWBrowser): Boolean;
{$ENDIF}
begin
{$IFDEF INTRAWEB120}
  Result:= False;
{$ELSE}
  Result:= ABrowser = brNetscape6;
{$ENDIF}
end;

{$IFDEF INTRAWEB120}
function BrowserIsNetscape7(ABrowser: TBrowser): Boolean;
{$ELSE}
function BrowserIsNetscape7(ABrowser: TIWBrowser): Boolean;
{$ENDIF}
begin
{$IFDEF INTRAWEB120}
  Result:= False;
{$ELSE}
  Result:= ABrowser = brNetscape7;
{$ENDIF}
end;

{$IFDEF INTRAWEB120}

class procedure TIWServer.AddInternalFile(const aResName,aFilename: string);
begin
  gInternalFiles.Add(aResName,aFilename);
end;

{$ENDIF}

end.
