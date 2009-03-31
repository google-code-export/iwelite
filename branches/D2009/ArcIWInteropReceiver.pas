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

unit ArcIWInteropReceiver;

interface

uses
  SysUtils, Classes, ActnList, Menus, IWBaseHTMLInterfaces, SyncObjs,
  IWBaseHTMLComponent, IWBaseInterfaces, IWRenderContext, IWHTMLTag,
  InHTTP, InCookie, InCookieManager, IWBaseForm, IWBaseRenderContext,
  InURI, IWForm, IWScriptEvents, ArcIWInteropCommon, InSSLOpenSSL;

type
  TServerItem = class(TCollectionItem)
  private
    CS : TCriticalSection;
    FHost: string;
    FPort: string;
    FAppName: string;
    FPath: string;
    FActions: TActionList;
    FQueryString: string;
    FRegistered: boolean;
    FRegPath: string;
    FSimplePath: boolean;
    FOrder: integer;
    FSecure : boolean;
    procedure SetPort(const Value: string);
    procedure SetQueryString(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure InternalOnExecute(Sender : TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Actions : TActionList read FActions write FActions;
    property Registered : boolean read FRegistered;
    procedure Register;
  published
    property SimplePath : boolean read FSimplePath write FSimplePath;
    property Host : string read FHost write FHost;
    property Port : string read FPort write SetPort;
    property Path : string read FPath write FPath;
    property RegPath : string read FRegPath write FRegPath;
    property AppName : string read FAppName write FAppName;
    property QueryString : string read FQueryString write SetQueryString;
    property Order : integer read FOrder write FOrder;
    property Secure : boolean read FSecure write FSecure;
  end;

  TArcIWCustomInteropReceiver = class;
  TServerCollection = class(TCollection)
  private
    CS : TCriticalSection;
    FReceiver: TArcIWCustomInteropReceiver;
    function GetServers(idx: integer): TServerItem;
    procedure SetServers(idx: integer; const Value: TServerItem);
  public
    property Receiver : TArcIWCustomInteropReceiver read FReceiver write FReceiver;
    constructor Create(aReceiver : TArcIWCustomInteropReceiver); virtual;
    destructor Destroy; override;
    procedure LoadFromFile(Filename : string);
    procedure SaveToFile(Filename : string);
    function AppIsRegistered(AppName : string) : boolean;
    function AppIsValid(AppName : string) : boolean;
    property Servers[idx : integer] : TServerItem read GetServers write SetServers; default;
  end;


  TControllerLocation = (clIFRAME, clFramesetFrame, clOpenerWindow);

  TArcExceptionEvent = procedure(Sender : TObject; E : Exception) of object;
  TArcServerRegisterEvent = procedure (Sender : TObject; ServerItem : TServerItem; QueryFields : TStrings) of object;
  TArcIWCustomInteropReceiver = class(TIWBaseHTMLComponent)
  private
    FOnHandledException: TArcExceptionEvent;
    FControllerAt: TControllerLocation;
    FOnBeforeRegisterServer: TArcServerRegisterEvent;
    FOnBeforeRegister: TNotifyEvent;
    FCalledOnBeforeRegister : boolean;
    procedure SetRegisteredServers(const Value: TServerCollection);
  protected
    FRegisteredServers: TServerCollection;
    FMenu: TMenu;
    FAppFrameName: string;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function ComponentContextClass: TIWBaseComponentContextClass; override;
    procedure Loaded; override;
    function UseServerSideEvents : boolean; virtual; abstract;
    procedure ConfigureClientSideEvent(AppName, ActionName, MenuItemName : string); virtual; abstract;
    procedure CheckProperties; virtual;
    procedure DoExceptionEvent(E : Exception);

    property ControllerAt : TControllerLocation read FControllerAt write FControllerAt;
    property RegisteredServers : TServerCollection read FRegisteredServers write SetRegisteredServers;
    property AppFrameName : string read FAppFrameName write FAppFrameName;
    property Menu : TMenu read FMenu write FMenu;
    property OnHandledException : TArcExceptionEvent read FOnHandledException write FOnHandledException;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    procedure RebuildMenu(ClearMenu : boolean = True); virtual;
    procedure ActionExecute(Action : TAction); virtual;

    procedure AssignOnClick(ScriptEvents : TStrings; const AppName, ActionName : string; SendValue : string = ''); overload; virtual;
    procedure AssignScriptEvent(ScriptEvents : TStrings; const EventName, AppName, ActionName : string; SendValue : string = ''; Prepend : boolean = false); overload; virtual;
    procedure AssignOnClick(ScriptEvents : TIWScriptEvents; const AppName, ActionName : string; SendValue : string = ''); overload; virtual;
    procedure AssignScriptEvent(ScriptEvents : TIWScriptEvents; const EventName, AppName, ActionName : string; SendValue : string = ''; Prepend : boolean = false); overload; virtual;

    function BuildJSFunctionNameFor(AppName, ActionName : string; Params : string='') : string;
    procedure ExecuteActionOnShow(frm : TIWForm; AppName, ActionName : string; CloseForm : boolean);
  published
    property OnBeforeRegisterServer : TArcServerRegisterEvent read FOnBeforeRegisterServer write FOnBeforeRegisterServer;
    property OnBeforeRegister : TNotifyEvent read FOnBeforeRegister write FOnBeforeRegister;
  end;

  TArcIWInteropReceiver = class(TArcIWCustomInteropReceiver)
  protected
    function UseServerSideEvents: Boolean; override;
  published
    property ControllerAt;
    property RegisteredServers;
    property AppFrameName;
    property Menu;
  end;

implementation

uses
  ArcD5Fix,
{$IFDEF FASTSTRINGS}
  ArcFastStrings,
{$ELSE}
  ArcStrings,
{$ENDIF}
  IWMarkupLanguageTag;

{$IFDEF VER130}
function StrToBool(s : string) : Boolean;
begin
  Result := s='1';
end;
{$ENDIF}

{ TServerItem }

procedure TServerItem.AssignTo(Dest: TPersistent);
var
  i : integer;
  act : TAction;
begin
  if Dest is TServerItem then
  begin
    TServerItem(Dest).FHost        := FHost;
    TServerItem(Dest).FPort        := FPort;
    TServerItem(Dest).FAppName     := FAppName;
    TServerItem(Dest).FPath        := FPath;
    TServerItem(Dest).FQueryString := FQueryString;
    TServerItem(Dest).FRegistered  := FRegistered;
    TServerItem(Dest).FRegPath     := FRegPath;
    TServerItem(Dest).FRegistered  := FRegistered;
    TServerItem(Dest).FOrder       := FOrder;


    while TServerItem(Dest).FActions.ActionCount > 0 do
      TServerItem(Dest).FActions[0].Free;

    for i := 0 to FActions.ActionCount -1 do
    begin
      act := TAction.Create(TServerItem(Dest).FActions);
      act.Assign(FActions[i]);
    end;
  end else
    inherited;
end;

constructor TServerItem.Create(Collection: TCollection);
begin
  inherited;
  FOrder := 99999;
  FActions := TActionList.Create(nil);
  CS := TCriticalSection.Create;
  FRegPath := '/INTEROPREG';
  FPort := ':80';
  FSecure := false;
end;

destructor TServerItem.Destroy;
begin
  CS.Free;
  FActions.Free;
  inherited;
end;

procedure TServerItem.InternalOnExecute(Sender: TObject);
begin
  if Assigned(TServerCollection(Collection).Receiver) then
    TServerCollection(Collection).Receiver.ActionExecute(TAction(TMenuItem(Sender).Action));
end;

procedure TServerItem.Register;
  function DecodeIt(str : string) : string;
  begin
    Result := TInURI.URLDecode(str);
    Result := FastReplace(Result,'+',' ',False);
  end;
  function FindInteropCookie(cookies : TInCookies) : TInCookieRFC2109;
  var
    i : integer;
  begin
    Result := nil;
    for i := 0 to Cookies.Count-1 do
      if Cookies[i].CookieName = 'InteropReg' then
      begin
        Result := Cookies[i];
        break;
      end;
    if not Assigned(Result) then
      raise Exception.Create('Error Registering Interop');
  end;
var
  http : TInHTTP;
  sl, slActions, slQuery : TD7StringList;
  s, sQuery : string;
  i : integer;
  sPrefix : string;
begin
  if FRegistered then exit; // Handle outside the CS so that we don't lock unnecessarily.

  CS.Enter;
  try
    if FRegistered then exit; // Do this inside the CS as well to handle the case where two threads checked close enough together and both bipassed the first check.

    slQuery := TD7StringList.Create;
    try
      slQuery.Delimiter := '&';
      slQuery.QuoteChar := #0;
      slQuery.Values['ReceiverName'] := TServerCollection(Collection).Receiver.Name;

      if Assigned(TServerCollection(Collection).Receiver) and Assigned(TServerCollection(Collection).Receiver.FOnBeforeRegisterServer) then
        TServerCollection(Collection).Receiver.OnBeforeRegisterServer(TServerCollection(Collection).Receiver, Self, slQuery);

      sQuery := FastReplace(slQuery.DelimitedText,#0,'',False);
      try
        while FActions.ActionCount >0 do
          FActions[0].Free;

        http := TInHTTP.Create(nil);
        try
          http.CookieManager := TInCookieManager.Create(http);
          if FSecure then
          begin
            http.IOHandler := TInSSLIOHandlerSocketOpenSSL.Create(http);
            FPort := '';
            sPrefix := 'https://';
          end else
            sPrefix := 'http://';
          try
            if FSimplePath then
              http.Get(FRegPath+'?'+sQuery)
            else
              http.Get(sPrefix+FHost+FPort+FRegPath+'?'+sQuery);
          except
            on e: exception do
              if Assigned(TServerCollection(Collection).Receiver) then
                TServerCollection(Collection).Receiver.DoExceptionEvent(e);
          end;

          slActions := TD7StringList.Create;
          try
            slActions.Delimiter := ',';
            slActions.QuoteChar := '"';

            s := DecodeIt(FindInteropCookie(http.CookieManager.CookieCollection).Value);
            if s = '""' then
              Exit;

            slActions.DelimitedText := s;

            sl := TD7StringList.Create;
            try
              sl.Delimiter := '|';
              sl.QuoteChar := '^';

              for i := 0 to slActions.Count-1 do
              begin
                sl.DelimitedText := slActions[i];

                if sl.Count <> 5 then
                  raise Exception.Create('Error in Interop Registration');
                with TAction.Create(Self.FActions) do
                begin
                  ActionList := FActions; // redundant?
                  Category := sl[0];
                  Caption := sl[1];
                  Name := sl[2];
                  Tag := StrToInt(sl[3]);
                  Enabled := StrToBool(sl[4]);
                  Hint := AppName;
                  if Assigned(TServerCollection(Collection).Receiver) then
                    if TServerCollection(Collection).Receiver.UseServerSideEvents then
                      OnExecute := InternalOnExecute
                end;
              end;
            finally
              sl.Free;
            end;
          finally
            slActions.Free;
          end;

          FRegistered := True;
        finally
          http.Free;
        end;
      except
        on e: exception do
          if Assigned(TServerCollection(Collection).Receiver) then
            TServerCollection(Collection).Receiver.DoExceptionEvent(e);
      end;
    finally
      slQuery.Free;
    end;
  finally
    CS.Leave;
  end;
end;

procedure TServerItem.SetPort(const Value: string);
begin
  FPort := Value;
  if (FPort <> '') and (FPort[1] <> ':') then
    FPort := ':'+FPort;
end;

procedure TServerItem.SetQueryString(const Value: string);
begin
  FQueryString := Value;
  if (FQueryString <> '') and (FQueryString[1] <> '&') then
    FQueryString := '&'+FQueryString;
end;

{ TArcIWCustomInteropReceiver }

procedure TArcIWCustomInteropReceiver.ActionExecute(Action: TAction);
var
  sScript : string;
begin
  sScript := BuildJSFunctionNameFor(Action.Hint, Action.Name)+';'#13#10;
  TIWForm(Owner).get_PageContext.AddToInitProc(sScript);
end;

procedure TArcIWCustomInteropReceiver.AssignOnClick(ScriptEvents: TStrings;
  const AppName, ActionName: string; SendValue : string = '');
begin
  AssignScriptEvent(ScriptEvents, 'OnClick', AppName, ActionName, SendValue);
end;

procedure TArcIWCustomInteropReceiver.AssignScriptEvent(
  ScriptEvents: TStrings; const EventName, AppName, ActionName: string; SendValue : string = ''; Prepend : boolean = false);
var
  idx : integer;
begin
  {$IFDEF VER150}
  idx := ScriptEvents.IndexOf(EventName);
  if idx >= 0 then
  begin
    if Prepend then
      ScriptEvents.ValueFromIndex[idx] := ScriptEvents.ValueFromIndex[idx]+' '+BuildJSFunctionNameFor(AppName, ActionName, SendValue)+';'
    else
      ScriptEvents.ValueFromIndex[idx] := BuildJSFunctionNameFor(AppName, ActionName, SendValue)+'; '+ScriptEvents.ValueFromIndex[idx];
  end else
    ScriptEvents.ValueFromIndex[idx] := BuildJSFunctionNameFor(AppName, ActionName, SendValue)+';';
  {$ELSE}
  if idx >= 0 then
  begin
    if Prepend then
      ScriptEvents.Values[EventName] := ScriptEvents.Values[EventName]+' '+BuildJSFunctionNameFor(AppName, ActionName, SendValue)+';'
    else
      ScriptEvents.Values[EventName] := BuildJSFunctionNameFor(AppName, ActionName, SendValue)+'; '+ScriptEvents.Values[EventName];
  end else
    ScriptEvents.Values[EventName] := BuildJSFunctionNameFor(AppName, ActionName, SendValue)+';';
  {$ENDIF}
end;

procedure TArcIWCustomInteropReceiver.AssignScriptEvent(
  ScriptEvents: TIWScriptEvents; const EventName, AppName,
  ActionName: string; SendValue : string = ''; Prepend : boolean = false);
var
  idx : integer;
begin
  idx := ScriptEvents.EventNamesList.IndexOf(EventName);
  if idx >= 0 then
  begin
    if Prepend then
      ScriptEvents.Items[idx].EventCode.Text := ScriptEvents.Items[idx].EventCode.Text+' '+BuildJSFunctionNameFor(AppName, ActionName, SendValue)+';'
    else
      ScriptEvents.Items[idx].EventCode.Text := BuildJSFunctionNameFor(AppName, ActionName, SendValue)+'; '+ScriptEvents.Items[idx].EventCode.Text;
  end else
    ScriptEvents.Add(EventName).EventCode.Text := BuildJSFunctionNameFor(AppName, ActionName, SendValue)+';';
end;

function TArcIWCustomInteropReceiver.BuildJSFunctionNameFor(AppName,
  ActionName: string; Params : string = ''): string;
begin
  if Params <> '' then
    Result := 'Goto'+AppName+'_'+ActionName+'( '+Params+' )'
  else
    Result := 'Goto'+AppName+'_'+ActionName+'("")';
end;

procedure TArcIWCustomInteropReceiver.CheckProperties;
begin
//  if not Assigned(FMenu) then
//    raise Exception.Create('TArcIWCustomInteropReceiver.Menu property not set');
  if (FControllerAt in [clIFRAME, clFramesetFrame]) and (FAppFrameName = '') then
    raise Exception.Create('TArcIWCustomInteropReceiver.AppFrameName property not set');
end;

function TArcIWCustomInteropReceiver.ComponentContextClass: TIWBaseComponentContextClass;
begin
  Result := TIWComponent40Context;
end;

constructor TArcIWCustomInteropReceiver.Create(AOwner: TComponent);
begin
  if not (AOwner is TIWBaseForm) then
    raise Exception.Create('TArcIWCustomInteropReceiver is intended to be used on an IntraWeb Form');
  inherited;
  FRegisteredServers := TServerCollection.Create(Self);
end;

destructor TArcIWCustomInteropReceiver.Destroy;
begin
  FRegisteredServers.Free;
  inherited;
end;

procedure TArcIWCustomInteropReceiver.DoExceptionEvent(E: Exception);
begin
  if Assigned(FOnHandledException) then
    FOnHandledException(Self, E);
end;

procedure TArcIWCustomInteropReceiver.ExecuteActionOnShow(frm: TIWForm;
  AppName, ActionName: string; CloseForm: boolean);
begin
  frm.AddToInitProc(BuildJSFunctionNameFor(AppName,ActionName)+';');
  if CloseForm then
    frm.AddToInitProc('window.close();');
end;

procedure TArcIWCustomInteropReceiver.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    RebuildMenu;
end;

procedure TArcIWCustomInteropReceiver.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FMenu) then
    FMenu := nil;
end;

function TagOrder(item1, item2 : pointer) : integer;
begin
  Result := TMenuItem(item1).Tag - TMenuItem(item2).Tag;
end;

procedure TArcIWCustomInteropReceiver.RebuildMenu(ClearMenu : boolean = True);
  function FindParentMenuItem(act : TAction; Order : integer) : TMenuItem;
  var
    i : integer;
    mi : TMenuItem;
  begin
    Result := nil;
    if act.Category <> '' then
    begin
      for i := 0 to Menu.Items.Count-1 do
        if Menu.Items[i].Caption = act.Category then
        begin
          Result := Menu.Items[i];
          break;
        end;
      if not Assigned(Result) then
      begin
        mi := TMenuItem.Create(Menu);
        mi.Caption := act.Category;
        mi.Name := FastReplace(act.Category,' ','',False);
        mi.Tag := Order;
        Result := mi;
        Menu.Items.Add(mi);
      end;
    end;
  end;
  function CalcIndex(mi : TMenuItem) : integer;
  begin
    result := mi.Tag;
    if result > mi.Parent.Count-1 then
      result := mi.Parent.Count-1;
    if result < 0 then
      result := 0;
  end;
  procedure BuildMenuList(ls : TList; mi : TMenuItem);
  var
    i : integer;
  begin
    for i := 0 to mi.Count-1 do
    begin
      ls.Add(mi.Items[i]);
      if mi.Items[i].Count > 0 then
        BuildMenuList(ls, mi.Items[i]);
    end;
  end;
var
  ls : TList;
  i, y, idx : integer;
  si : TServerItem;
  mi, miParent : TMenuItem;
begin
  CheckProperties;

  if Menu = nil then exit;

  if ClearMenu then
    Menu.Items.Clear;

  if not FCalledOnBeforeRegister then
  begin
    if Assigned(FOnBeforeRegister) then
      FOnBeforeRegister(Self);
  end;

  for i := 0 to FRegisteredServers.Count-1 do
  begin
    si := TServerItem(FRegisteredServers.Items[i]);
    si.Register;

    for y := 0 to si.Actions.ActionCount-1 do
    begin
      mi := TMenuItem.Create(Menu);
      mi.Action := si.Actions[y];
      mi.Tag := si.Actions[y].Tag;
      mi.Caption := TAction(si.Actions[y]).Caption;
      miParent := FindParentMenuItem(TAction(si.Actions[y]), si.Order);
      if Assigned(miParent) then
        miParent.Add(mi)  // Add Sub level menu item.
      else
        Menu.Items.Add(mi); // Add Root level menu item.
      mi.Name := 'mi'+si.AppName+'_'+mi.Action.Name;
      if not UseServerSideEvents then
        ConfigureClientSideEvent(TAction(mi.Action).Hint, mi.Action.Name, mi.Name);
    end;
  end;
  ls := TList.Create;
  try
    BuildMenuList(ls, Menu.Items);
    ls.Sort(TagOrder);
    for i := 0 to ls.Count-1 do
    begin
      mi := TMenuItem(ls[i]);
      miParent := mi.Parent;

      idx := CalcIndex(mi);
      miParent.Remove(mi);
      miParent.Insert(idx,mi);
    end;
  finally
    ls.free;
  end;
end;

function TArcIWCustomInteropReceiver.RenderHTML(
  AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
  function WindowObjectRef : string;
  begin
    case FControllerAt of
      clIFRAME:        result := 'window.frames["'+AppFrameName+'"]';
      clFramesetFrame: result := 'top.frames["'+AppFrameName+'"]';
      clOpenerWindow:  result := 'window.opener';
    end;
  end;
var
  i, y : integer;
  si : TServerItem;
begin
  CheckProperties;

  Result := TIWHTMLTag.CreateTag('SCRIPT');
  Result.AddStringParam('language','Javascript');

  if not FCalledOnBeforeRegister then
  begin
    if Assigned(FOnBeforeRegister) then
      FOnBeforeRegister(Self);
  end;

  for i := 0 to FRegisteredServers.Count-1 do
  begin
    si := TServerItem(FRegisteredServers.Items[i]);

    si.Register;

    for y := 0 to si.Actions.ActionCount-1 do
    begin
      Result.Contents.AddText(#13#10+
        'function '+BuildJSFunctionNameFor(si.AppName, si.Actions[y].Name, 'val')+' {'#13#10+
        //'  alert("'+BuildJSFunctionNameFor(si.AppName, si.Actions[y].Name)+'");'#13#10+
        //'  if (typeof(window.frames["'+AppFrameName+'"]) == ''undefined'') {'#13#10+
        //'    alert("Cannot find a frame by the name: '+AppFrameName+'");'#13#10+
        //'    return;'#13#10+
        //'  } '#13#10+
        //'  alert(val);'#13#10+
        '  if (typeof('+WindowObjectRef+'.whichAppAmI) == ''undefined'') {'#13#10+
        //'    alert("No whichAppAmI variable.");'#13#10+
        //'    alert("Launching new session.");'#13#10+
        '    '+WindowObjectRef+'.location = "http://'+si.FHost+si.Port+si.Path+'/?Action='+si.Actions[y].Name+si.QueryString+'&Value="+escape(val);'#13#10+
        '  } else {'#13#10+
        //'    alert(window.frames["'+AppFrameName+'"].whichAppAmI);'#13#10+
        '    if ('+WindowObjectRef+'.whichAppAmI != '''+si.AppName+''') {'#13#10+
        //'      alert("Launching new session.");'#13#10+
        '      '+WindowObjectRef+'.location = "http://'+si.FHost+si.Port+si.Path+'/?Action='+si.Actions[y].Name+si.QueryString+'&Value="+escape(val);'#13#10+
        '    } else {'#13#10+
        //'      alert("Moving inside current session.");'#13#10+
        '      '+WindowObjectRef+'.Goto_'+si.Actions[y].Name+'(val);'#13#10+
        '    }'#13#10+
        '  }'#13#10+
        '}'#13#10#13#10);
    end;
  end;
end;

procedure TArcIWCustomInteropReceiver.SetRegisteredServers(
  const Value: TServerCollection);
begin
  FRegisteredServers.Assign(Value);
end;

procedure TArcIWCustomInteropReceiver.AssignOnClick(
  ScriptEvents: TIWScriptEvents; const AppName, ActionName: string; SendValue : string = '');
begin
  AssignScriptEvent(ScriptEvents, 'OnClick', AppName, ActionName, SendValue);
end;

{ TServerCollectionContainer }

type
  TServerCollectionContainer = class(TComponent)
  private
    FRegisteredServers: TServerCollection;
  public
    constructor Create(Collection : TServerCollection); reintroduce;
  published
    property RegisteredServers : TServerCollection read FRegisteredServers write FRegisteredServers;
  end;

constructor TServerCollectionContainer.Create(Collection : TServerCollection);
begin
  inherited Create(Nil);
  FRegisteredServers := Collection;
end;

{ TServerCollection }

function TServerCollection.AppIsRegistered(AppName: string): boolean;
var
  i : integer;
begin
  Result := False;
  CS.Enter;
  try
    for i := 0 to Count-1 do
      if Servers[i].AppName = AppName then
      begin
        Result := Servers[i].Registered;
        break;
      end;
  finally
    CS.Leave;
  end;
end;

function TServerCollection.AppIsValid(AppName: string): boolean;
var
  i : integer;
begin
  Result := False;
  CS.Enter;
  try
    for i := 0 to Count-1 do
      if Servers[i].AppName = AppName then
      begin
        Result := True;
        break;
      end;
  finally
    CS.Leave;
  end;
end;

constructor TServerCollection.Create(aReceiver: TArcIWCustomInteropReceiver);
begin
  inherited Create(TServerItem);
  FReceiver := aReceiver;
  CS := TCriticalSection.Create;
end;

destructor TServerCollection.Destroy;
begin
  CS.Free;
  inherited;
end;

function TServerCollection.GetServers(idx: integer): TServerItem;
begin
  result := TServerItem(Items[idx]);
end;

procedure TServerCollection.LoadFromFile(Filename: string);
var
  cmp : TServerCollectionContainer;
  ms : TMemoryStream;
  fs : TFileStream;
begin
  if not FileExists(FileName) then
    SaveToFile(Filename);

  CS.Enter;
  try
    cmp := TServerCollectionContainer.Create(Self);
    try
      fs := TFileStream.Create(Filename, fmOpenRead);
      ms := TMemoryStream.Create;
      try
        ObjectTextToBinary(fs,ms);
        ms.Position := 0;
        ms.ReadComponent(cmp);
      finally
        ms.Free;
        fs.Free;
      end;
    finally
      cmp.Free;
    end;
  finally
    CS.Leave;
  end;
end;

procedure TServerCollection.SaveToFile(Filename: string);
var
  cmp : TServerCollectionContainer;
  ms : TMemoryStream;
  fs : TFileStream;
begin
  CS.Enter;
  try
    cmp := TServerCollectionContainer.Create(Self);
    try
      fs := TFileStream.Create(Filename, fmCreate or fmOpenReadWrite);
      ms := TMemoryStream.Create;
      try
        ms.WriteComponent(cmp);
        ms.Position := 0;
        ObjectBinaryToText(ms,fs);
      finally
        ms.Free;
        fs.Free;
      end;
    finally
      cmp.Free;
    end;
  finally
    CS.Leave;
  end;
end;

procedure TServerCollection.SetServers(idx: integer;
  const Value: TServerItem);
begin
  Items[idx] := Value;
end;

{ TArcIWInteropReceiver }

function TArcIWInteropReceiver.UseServerSideEvents: Boolean;
begin
  Result := True;
end;

end.
