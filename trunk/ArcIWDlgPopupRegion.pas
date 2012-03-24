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

unit ArcIWDlgPopupRegion;

interface

{$I IntrawebVersion.inc}

uses ArcIWEliteResources, ArcIWDlgBase,
  {$IFDEF VSNET}
    System.ComponentModel,
    System.ComponentModel.Design,
    System.Drawing,

    IWNetBaseControl,
    IWNetBaseContainer,
    IWNetClasses,
  {$ELSE}
    {$IFDEF Linux}
      IWCLXBaseControl,
    {$ELSE}
      IWVCLBaseControl,
      {$IFNDEF INTRAWEB120}
      IWVCLClasses,
      {$ENDIF}
      IWVCLBaseContainer,
    {$ENDIF}
  {$ENDIF}

  {$IFDEF VSNET}
  Borland.VCL.Graphics,
  {$ELSE}
  {$IFDEF Linux}QControls,{$ELSE}Controls,{$ENDIF}
  {$IFDEF Linux}QForms,{$ELSE}Forms,{$ENDIF}
  {$IFDEF Linux}QGraphics,{$ELSE}Graphics,{$ENDIF}
  {$IFDEF Linux}Qt, Types,{$ELSE}Windows, Messages,{$ENDIF}
  {$ENDIF}
  Classes,
  IWContainer, IWControl, IWHTMLTag, IWBaseContainerLayout, IWForm,
  IWColor, IWBaseInterfaces, IWRenderContext, IWContainerLayout, IWBaseHTMLInterfaces,
  IWBaseHTMLControl, IWHTML40Interfaces, IWScriptEvents, IWTypes,
  IWContainerBorderOptions, IWApplication, IWFont, IWBaseControl, IWAppForm, IWBaseComponent,
  IWMarkupLanguageTag, IWHTMLContainer, IWBaseRenderContext, IWCompListbox, IWHTMLControls,
  IWCompObject, IWServer, IWGlobal
  {$IFDEF INTRAWEB72} {$IFDEF INTRAWEB120}, IWRenderStream {$ELSE}, IWStreams {$ENDIF}, IWXMLTag {$ENDIF}
  {$IFDEF INTRAWEB120}, IWCompGridCommon {$ELSE}, IWGridCommon {$ENDIF}, ArcCommon;

type
  TAutoHideOption = (ahComboBoxes, ahListBoxes, ahApplets, ahObjects, ahIFrame);
  TAutoHideOptions = set of TAutoHideOption;

  TIWBaseComponentHelper = class(TIWBaseComponent);

(*  {$IFDEF VSNET}
  {$R icons\IWRegion.TIWRegion.bmp}
  TArcIWDlgPopupRegion = class;
  [
    ToolboxItem(true),
    ToolboxBitmap(typeof(TArcIWDlgPopupRegion), 'TIWRegion.bmp'),
    ToolboxItemFilter('IWAppForm.TIWAppForm'),
    Designer('IntraWeb.Design.RegionDesigner, IntraWeb.Design', typeof(IDesigner)),
    DesignerCategory('Component')
  ]
  {$ENDIF}*)
  TArcIWDlgPopupRegion = class( TIWHTMLContainer, IIWHTML40Container, IIWBaseComponent,
                                IIWBaseHTMLComponent, IIWBaseControl, IIWBaseHTMLControl,
                                IIWHTML40Control, IIWHTML40Component, IIWTabOrder, IIWSubmitControl,
                                IIWSubmitInvisible)
  private
    FOldActiveControl : TIWCustomControl;
    FSubmitParam : String;
    FDefaultControl: TIWCustomControl;
    FOnCreate: TNotifyEvent;
    FSupportTabbing: boolean;
    FExtraTabNames: TStrings;
    function GetSubmitParam: String;
    function getSubmitProc: TSubmitProc;
    procedure SetExtraTabNames(const Value: TStrings);
(*  {$IFDEF CLR}
  strict protected
  {$ELSE}*)
  protected
//  {$ENDIF}
    FOnKeyPressEscape: TNotifyEvent;
    FOnKeyPressEnter: TNotifyEvent;
    OriginalParent : TControl;
    InSetVisible : boolean;
    FAutoHideVisibleList : TList;
    FAutoHide : boolean;
    FAutoHideOptions : TAutoHideOptions;
    FBorderOptions: TIWContainerBorderOptions;
    FIWControlImplementation: TIWHTMLControlImplementation;
    FIWBaseControlImplementation: TIWBaseControlImplementation;
    FContainerImplementation: TIWContainerImplementation;
    FDoRefreshControl: Boolean;
    FUseFrame: Boolean;
    FRegionDIV: TIWHTMLTag;
    FWebApplication: TIWApplication;
    FCentered : boolean;
    FModalColor : TIWColor;
    FModalColorVisible : boolean;
    FModalColorAlpha : integer;
    FModalPictureURL: string;
    procedure SetBorderOptions(const Value: TIWContainerBorderOptions);
    procedure OnBorderChange(ASender: TObject);
    {$IFDEF VSNET}
    {$ELSE}
    procedure AdjustClientRect(var Rect: TRect); override;
    function GetClientRect: TRect; override;
    {$ENDIF}

    function InitPaintHandler: TIWPaintHandler; override;

    // IIWContainer methods
    function InitContainerContext(AWebApplication: TIWApplication): TIWContainerContext; override;

    // IIWControl methods
    function get_HTMLLeft: Integer;
    function get_HTMLTop: Integer;

    function RenderCSSClass(AComponentContext: TIWBaseHTMLComponentContext): string;
    function RenderStyle(AComponentContext: TIWBaseHTMLComponentContext): string;
    procedure RenderScripts(AComponentContext: TIWBaseHTMLComponentContext);

    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;

    function WantsSubmitValidation: Boolean;

    function get_WebCursor: TIWCursor;
    procedure set_WebCursor(AValue: TIWCursor);

    procedure set_WebFont(AValue: TIWFont);
    function get_WebFont: TIWFont;

    function get_Canvas: TCanvas; override;

    function ParentContainer: IIWBaseContainer;
    procedure PaintTo(ACanvas: TCanvas);

    function SupportsSubmit: Boolean;
    function SupportsInput: Boolean;
    procedure AddFreeNotifier(AObject: TObject);
    procedure RemoveFreeNotifier(AObject: TObject);
    function ComponentContextClass: TIWBaseComponentContextClass; virtual;

    {$IFDEF INTRAWEB72}
    procedure MakeHTMLTag(ATag: TIWHTMLTag; ABuffer: TIWRenderStream); overload;
    {$ENDIF}
    function MakeHTMLTag(ATag: TIWHTMLTag): string; overload;
    procedure DoHTMLTag(ATag: TIWHTMLTag);

    //
    procedure InitDesignTime; override;
    procedure InitControl; override;

    procedure SetName(const Value: TComponentName); override;

    procedure RenderComponents(AContainerContext: TIWContainerContext; APageContext: TIWBasePageContext); override;

    function NeedsFormTag: Boolean;

    function get_RenderSize: Boolean;
    procedure set_RenderSize(AValue: Boolean);

    function get_ScriptEvents: TIWScriptEvents;
    procedure set_ScriptEvents(Value: TIWScriptEvents);

    function get_Clip: Boolean;
    procedure set_Clip(AValue: Boolean);

    function HintEvents(ATag: TIWHTMLTag{; const AHint: string}): string;
    procedure HookEvents(APageContext: TIWPageContext40;
      AScriptEvents: TIWScriptEvents); {$IFDEF Linux} reintroduce; {$ENDIF} virtual;

    procedure SetActiveControl(AControl: IIWHTML40Control);

    function GenerateControlPositions: String;

    function CheckComponentForRender(AComponent: TComponent): Boolean; override;
    function DoPostRenderProcessing(ATag: TIWMarkupLanguageTag;
      AComponentContext: TIWBaseComponentContext; AComponent: TComponent): TIWMarkupLanguageTag; override;

    function CheckUpdateTree(AComponent: TComponent): Boolean; override;

    // IIWBaseComponent interface
    function get_ComponentName: String;

    function get_SupportsPartial: Boolean;
    procedure set_SupportsPartial(AValue: BOolean);

    function getTabOrder: TIWTabOrder; override;
    procedure setTabOrder(AValue: TIWTabOrder); override;

    property SupportsPartial: Boolean read get_SupportsPartial write set_SupportsPartial;

    property ControlImplementation: TIWHTMLControlImplementation read FIWControlImplementation; // implements IIWBaseComponent, IIWBaseControl, IIWControl;
    //property ContainerImplementation: TIWContainerImplementation read FContainerImplementation; // implements IIWBaseContainer, IIWContainer40;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    ParentForm : TIWForm;

    function get_IWComponentsCount: Integer;
    procedure Dispose(ADispose: Boolean); override;
    function FindParentForm:TControl;
    procedure SetParent(AParent: TWinControl); override;

    {$IFNDEF VSNET}
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Invalidate; override;
    {$ENDIF}

    function get_DoRefreshControl: Boolean;
    procedure set_DoRefreshControl(AValue: Boolean);

    function get_HTMLName: String;

    function get_UseFrame: Boolean;
    procedure set_UseFrame(AValue: Boolean);

    function get_HTMLWidth: Integer;
    function get_HTMLHeight: Integer;

    {$IFDEF INTRAWEB90}
    function RenderAsyncComponent(AContext: TIWBaseComponentContext): TIWXMLTag;
    procedure AddAsyncStyle(ATag: TIWXMLTag; AStyle: string);
    function get_StyleRenderOptions: TIWStyleRenderOptions; 
    procedure set_StyleRenderOptions(AValue: TIWStyleRenderOptions);
    {$ENDIF}
    function get_Css: string;
    function get_SkinId: string;
    procedure set_SkinId(AValue: string);
    function RequiresRefresh: Boolean;
    procedure set_Css(AValue: string);
    {$IFDEF INTRAWEB90}
    procedure RenderAsyncComponents(AContext: TIWContainerContext;
      APageContext: TIWBasePageContext);
    {$ENDIF}

    property DoRefreshControl : Boolean read get_DoRefreshControl write set_DoRefreshControl;
    property HTMLName: string read get_HTMLName;

    property HTMLWidth: Integer read get_HTMLWidth;
    property HTMLHeight: Integer read get_HTMLHeight;

    property UseFrame: Boolean read get_UseFrame write set_UseFrame;
    function get_Visible: Boolean; override;
    procedure set_Visible(Value: Boolean); override;

    function get_ExtraTagParams: TIWStringList;
    procedure set_ExtraTagParams(const Value: TIWStringList);

    function get_OnHTMLTag: TIWOnHTMLTag;
    procedure set_OnHTMLTag(AValue: TIWOnHTMLTag);

    procedure set_LayoutMgr(Value: TIWContainerLayout);
    function get_LayoutMgr: TIWContainerLayout;

    {$IFNDEF CLR}
    procedure set_WebColor(Value: TIWColor); override;
    function get_WebColor: TIWColor; override;
    {$ENDIF}

    function get_ZIndex: Integer;
    procedure set_ZIndex(AValue: Integer);

    constructor Create(Owner : TComponent); override;
    procedure Submit(const AValue: String);
  published
    property Align;
    property Anchors;
    property TabOrder;
    property WebColor;
    property ShowHint;
    property Hint;

    {$IFDEF CLR}
    // TODO: Remove this when .NET Delphi compiler is fixed
    property Visible read get_Visible write set_Visible;
    {$ELSE}
    property Visible;
    {$ENDIF}

    property AutoHide : boolean read FAutoHide write FAutoHide;
    property AutoHideOptions : TAutoHideOptions read FAutoHideOptions write FAutoHideOptions default [ahComboBoxes, ahListBoxes, ahApplets, ahObjects, ahIFrame];
    property DefaultControl : TIWCustomControl read FDefaultControl write FDefaultControl;
    property ModalColor : TIWColor read FModalColor write FModalColor;
    property ModalColorVisible : boolean read FModalColorVisible write FModalColorVisible;
    property ModalColorAlpha : integer read FModalColorAlpha write FModalColorAlpha;
    property ModalPictureURL: string read FModalPictureURL write FModalPictureURL;
    property ClipRegion: Boolean read get_Clip write set_Clip default true;
    property LayoutMgr: TIWContainerLayout read get_LayoutMgr write set_LayoutMgr;
    property BorderOptions: TIWContainerBorderOptions read FBorderOptions write SetBorderOptions;
    property ExtraTagParams: TIWStringList read get_ExtraTagParams write set_ExtraTagParams;
    property OnHTMLTag: TIWOnHTMLTag read get_OnHTMLTag write set_OnHTMLTag;
    property ZIndex: Integer read get_ZIndex write set_ZIndex;
    property Centered : boolean read FCentered write FCentered;
    property OnKeyPressEnter : TNotifyEvent read FOnKeyPressEnter write FOnKeyPressEnter;
    property OnKeyPressEscape : TNotifyEvent read FOnKeyPressEscape write FOnKeyPressEscape;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property SupportTabbing : boolean read FSupportTabbing write FSupportTabbing;
    property ExtraTabNames : TStrings read FExtraTabNames write SetExtraTabNames;
  end;

  TIWCustomControlHelper = class(TIWCustomControl)
  end;

implementation

uses
  {$IFNDEF VER130}StrUtils, {$ENDIF}SysUtils, IWUtils, IWLayoutMgrForm,
  Math, {$IFDEF INTRAWEB110} IWSystem, {$ELSE} SWSystem, {$ENDIF} IWBaseHTMLComponent, IWCompCheckbox;

{$IFDEF VER130}
function IfThen(AValue: Boolean; const ATrue: string; AFalse: string = ''): string;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;
{$ENDIF}

constructor TArcIWDlgPopupRegion.Create(Owner : TComponent);
begin
  inherited Create(Owner);
  FExtraTabNames := TStringList.Create;
  FAutoHideVisibleList := TList.Create;
  FAutoHideOptions := [ahComboBoxes, ahListBoxes, ahApplets, ahObjects, ahIFrame];
end;

procedure FreeAllChildren(const AComponent:TComponent);
begin
  while AComponent.ComponentCount>0 do begin
    FreeAllChildren(AComponent.Components[0]);
    AComponent.Components[0].Free;
  end;
end;

procedure TArcIWDlgPopupRegion.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) then
  begin
    if AComponent = FDefaultControl then
      FDefaultControl := nil;
      if Assigned(FAutoHideVisibleList) then
        FAutoHideVisibleList.Remove(AComponent);
  end;
end;

function TArcIWDlgPopupRegion.FindParentForm:TControl;
begin
  result := self;
  while Assigned(result) and (not (result is TIWForm))  do
    result := result.Parent;
  if result = nil then
  begin
    if Self.Owner is TFrame then
    begin
      result := TControl(Self.Owner.Owner);
      while (not (result is TIWForm)) and (result <> nil) do
        result := TControl(result.Owner);
    end;
  end;
end;

procedure TArcIWDlgPopupRegion.Loaded;
var
  o : TControl;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
  begin
    o := FindParentForm;
    if o = nil then
      raise Exception.Create('Cannot locate an IWForm as a parent or owner.');
    OriginalParent := Parent;
    Parent := TWinControl(o);
    ParentForm := TIWForm(o);

    if FCentered then
    begin
      Left := (Parent.Width div 2) - (Width div 2);
      Top := (Parent.Height div 2) - (Height div 2);
      Anchors := [];
    end;
  end;
  if Assigned(OnCreate) and not IsDesignMode then begin
    OnCreate(Self);
  end;
end;

procedure TArcIWDlgPopupRegion.SetParent(AParent: TWinControl);
begin
  inherited SetParent( AParent );
  ParentForm := TIWForm(FindParentForm);
end;

function TArcIWDlgPopupRegion.CheckComponentForRender(AComponent: TComponent): Boolean;
begin
  if SupportsInterface(Parent, IIWHTML40Form) then begin
    {$IFDEF INTRAWEB110}
    Result := False;
    {$ELSE}
    Result := HTML40FormInterface(Parent).PageContext.UpdateMode = umPartial;
    {$ENDIF}
  end else begin
    if SupportsInterface(Parent, IIWHTML40Container) then begin
      Result := HTML40ContainerInterface(Parent).CheckComponentForRender(AComponent);
    end else begin
      {$IFNDEF VSNET}
      if Parent is TFrame then begin
        if SupportsInterface(Parent.Parent, IIWHTML40Container) then begin
          Result := HTML40ContainerInterface(Parent.Parent).CheckComponentForRender(AComponent);
        end else begin
          Result := false;
        end;
      end else {$ENDIF} begin
        Result := false;
      end;
    end;
  end;
  //result := True;  JDS- added above to conform with the latest IW region code.
end;

function TArcIWDlgPopupRegion.DoPostRenderProcessing(ATag: TIWMarkupLanguageTag;
  AComponentContext: TIWBaseComponentContext; AComponent: TComponent): TIWMarkupLanguageTag;
var
  LIWControl: IIWHTML40Control;
  LTag: TIWHTMLTag;
begin
  result := ATag;
  LTag := TIWHTMLTag(ATag);
  //BaseHTMLControlInterface(AComponent).DoHTMLTag(LTag);  JDS- removed in latest live version.  Try removing here.
  if SupportsInterface(AComponent, IIWHTML40Control) then begin
    LIWControl := HTML40ControlInterface(AComponent);
    if LIWControl.UseFrame and TIWContainerLayout(ContainerContext.LayoutManager).AllowFrames then begin
      result := TIWContainerBorderOptions.CreateDefaultFrame(LTag, TIWPageCOntext40(AComponentContext.BasePageContext), LIWControl);
    end;

    AComponentContext.MarkupLanguageTag := Result;

    HTML40ComponentInterface(AComponent).RenderScripts(TIWBaseHTMLComponentContext(AComponentContext));
  end;
end;

function TArcIWDlgPopupRegion.CheckUpdateTree(AComponent: TComponent): Boolean;
begin
  result := false;
  if SupportsInterface(AComponent, IIWHTML40Component) then begin
    result := HTML40ComponentInterface(AComponent).DoRefreshControl;
  end;
end;

procedure TArcIWDlgPopupRegion.SetBorderOptions(const Value: TIWContainerBorderOptions);
begin
  FBorderOptions.Assign(Value);
  BorderOptions.Color := FBorderOptions.Color;
  BorderOptions.NumericWidth := FBorderOptions.NumericWidth;
  BorderOptions.Style := FBorderOptions.Style;
  BorderOptions.BorderWidth := FBorderOptions.BorderWidth;
  RequestAlign; //ForceAlign; JDS- changed in latest live version.  
  Invalidate;
end;

procedure TArcIWDlgPopupRegion.InitControl;
begin
  FIWControlImplementation := TIWHTMLControlImplementation.Create(Self);
  FIWBaseControlImplementation := TIWBaseControlImplementation.Create(Self);

  inherited;
  if FCentered then
  begin
    Left := (Parent.Width div 2) - (Width div 2);
    Top := (Parent.Height div 2) - (Height div 2);
    Anchors := [];
  end;
  FModalColor := clNone;
  FModalColorVisible := True;
  FModalColorAlpha := 35;
  FModalPictureURL := '';

  FBorderOptions := TIWContainerBorderOptions.Create;
  FBorderOptions.Control := Self;
  FBorderOptions.Color := fromTColor(clNone);
  FBorderOptions.Style := cbsSolid;
  FBorderOptions.BorderWidth := cbwNumeric;
  FBorderOptions.NumericWidth := 1;


  FBorderOptions.OnChange := OnBorderChange;

  FContainerImplementation := TIWContainerImplementation.Create(Self);

  {$IFNDEF VSNET}
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents, csSetCaption, csDoubleClicks];
  {$ENDIF}
  Color := fromTColor(clNone);
  Width := 60;
  Height := 60;
  Visible := False;
  set_ZIndex(-1);
  ClipRegion:= true;
  ShowHint := True;
end;

procedure TArcIWDlgPopupRegion.InitDesignTime;
begin
  inherited;

  if IsDesignMode then begin
    {$IFNDEF VSNET}
    ControlState := [csCustomPaint];
    {$IFDEF Linux}
    VertScrollBar.Visible := false;
    HorzScrollBar.Visible := false;
    {$ENDIF}
    {$ENDIF}
  end;
  ClipRegion:= true;
end;

function TArcIWDlgPopupRegion.NeedsFormTag: Boolean;
begin
  result := false;
end;

function TabOrderCompare(Item1, Item2 : Pointer) : integer;
var
  LTabOrder1:Integer;
  LTabOrder2:Integer;
begin
  if TObject(Item1) is TIWBaseHTMLControl then
    LTabOrder1:=TIWBaseHTMLControl(Item1).InternalTabOrder
  else
    LTabOrder1:=TIWHTMLContainer(Item1).InternalTabOrder;

  if TObject(Item2) is TIWBaseHTMLControl then
    LTabOrder2:=TIWBaseHTMLControl(Item2).InternalTabOrder
  else
    LTabOrder2:=TIWHTMLContainer(Item2).InternalTabOrder;

  if LTabOrder1 < LTabOrder2 then begin
    Result := -1;
  end else if LTabOrder1 > LTabOrder2 then begin
    Result := 1;
  end else begin
    Result := 0;
  end;
end;

{function TabOrderCompare(Item1, Item2 : Pointer) : integer;
var
  bTab1, bTab2 : boolean;
  intf1 : IIWTabOrder;
  intf2 : IIWTabOrder;
begin
  bTab1 := Supports(TObject(Item1), IIWTabOrder, intf1);
  bTab2 := Supports(TObject(Item2), IIWTabOrder, intf2);
  if bTab1 and bTab2 then
    Result := intf1.TabOrder - intf2.TabOrder
  else
    Result := 0;
end;
}

function TArcIWDlgPopupRegion.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
  function EnumerateControlNames(SkipFirst, SkipLast : string; ctrlParent : TIWHTMLContainer = nil) : string;
  var
    s, sTmp : String;
    i : integer;
    iLowTab : integer;
    iHighTab : integer;
  begin
    if ctrlParent = nil then
      ctrlParent := self;

    iLowTab := High(Integer);
    iHighTab := 0;
    for i := 0 to ctrlParent.ControlCount-1 do
    begin
      if (ctrlParent.Controls[i] is TIWBaseControl) and (TIWBaseControl(ctrlParent.Controls[i]).TabOrder >= 0) then
      begin
        iLowTab := Min(iLowTab,TIWBaseControl(ctrlParent.Controls[i]).TabOrder);
        iHighTab := Max(iHighTab,TIWBaseControl(ctrlParent.Controls[i]).TabOrder);
      end;
    end;

    for i := 0 to ctrlParent.ControlCount-1 do
    begin
      if (Uppercase(ctrlParent.Controls[i].Name) + iif(ctrlParent.Controls[i] is TIWCustomCheckBox,'_CHECKBOX') = Uppercase(SkipFirst)) or
         (Uppercase(ctrlParent.Controls[i].Name) + iif(ctrlParent.Controls[i] is TIWCustomCheckBox,'_CHECKBOX') = Uppercase(SkipLast)) then
        continue;


      if ctrlParent.Controls[i] is TIWHTMLContainer then
      begin
        sTmp := EnumerateControlNames(SkipFirst, SkipLast, TIWHTMLContainer(ctrlParent.Controls[i]));
        if sTmp <> '' then
          s := s+','+sTmp;
      end else if (ctrlParent.Controls[i] is TIWCustomControl) and TIWCustomControlHelper(ctrlParent.Controls[i]).FCanReceiveFocus then
        if ctrlParent.Controls[i] is TIWCustomCheckBox then
          s := s+',"'+TIWCustomControl(ctrlParent.Controls[i]).get_HTMLName+'_CHECKBOX"'
        else
          s := s+',"'+TIWCustomControl(ctrlParent.Controls[i]).get_HTMLName+'"';
      //else
        //s := s+',"'+Uppercase(TComponent(ctrlParent.Controls[i]).Name)+'"';
    end;
    for i := 0 to FExtraTabNames.Count-1 do
      s := s+',"'+FExtraTabNames[i]+'"';
    Delete(s,1,1);
    Result := s;
  end;
var
  tagLocker : TIWHTMLTag;
  s, sOperaFocus, sCtrlList, sFirstTabCtrl, sLastTabCtrl: string;
  i : integer;
  lstTabs : TList;
begin
  TIWPageContext40(AContext.PageContext).AddScriptFile('/js/ArcIWDlgPopupRegion.js');
  TIWPageContext40(AContext.PageContext).AddToInitProc('IWTop().dlgInitialize();');
  TIWPageContext40(AContext.PageContext).AddToJavaScriptOnce(
    'var '+HTMLName+'_lockerIWCL = null;'
  );
  TIWPageContext40(AContext.PageContext).AddToIWCLInitProc(
    HTMLName + '_lockerIWCL = NewIWCL(IWCLForm,"'+HTMLName+'_locker",true);'+
    HTMLName + '_lockerIWCL.SetAlign(alNone);' +
    HTMLName + '_lockerIWCL.SetAnchors(new CreateAnchors(true, true, true, true));'
  );

  if BrowserIsOpera(AContext.Browser) then
  begin
    if FDefaultControl <> nil then
      sOperaFocus := FDefaultControl.HTMLName
    else if ControlCount > 0 then
      sOperaFocus := TIWCustomControl(Controls[0]).HTMLName
    else
      sOperaFocus := HTMLName;
  end;

  //Parent := TWinControl(AContext.PageContext.WebApplication.ActiveForm);
  FIWControlImplementation.SetZIndex(600000);

  s := DebugEOL+'<script language="Javascript">'+DebugEOL;
  {$IFDEF INTRAWEB110}
  if False then
  {$ELSE}
  if TIWPageContext40(AContext.PageContext).UpdateMode = umPartial then
  {$ENDIF}
    s := s+'  IWTop().UMStr = '''';'
  else
    s := s+'  IWTop().UMStr = ''_locker'';';

  (*if AContext.Browser in [brIE, brIE4] then
  begin
    s := s+
      '  function '+HTMLName+'_Reposition() {'+DebugEOL+
      '    IWTop().document.all['''+HTMLName+'_locker''].style.width = document.body.scrollLeft+document.body.scrollWidth;'+DebugEOL+//clientWidth;'+DebugEOL++
      '    IWTop().document.all['''+HTMLName+'_locker''].style.height = document.body.scrollTop+document.body.scrollHeight;'+DebugEOL+
      '    if (IWTop().G'+HTMLName+'_OnScroll != null) { G'+HTMLName+'_OnScroll(); }'+DebugEOL+
      '    if (IWTop().G'+HTMLName+'_OnResize != null) { G'+HTMLName+'_OnResize(); }'+DebugEOL+
      '    return true;'+DebugEOL+
      '  }'+DebugEOL+
      DebugEOL+
      '  IWTop().G'+HTMLName+'_OnScroll = document.body.onscroll;'+DebugEOL+
      '  IWTop().G'+HTMLName+'_OnResize = document.body.onresize;'+DebugEOL+
      DebugEOL+
      '  IWTop().window.onscroll='+HTMLName+'_Reposition;'+DebugEOL+
      '  IWTop().document.body.onresize='+HTMLName+'_Reposition;'+DebugEOL;
    TIWPageContext40( AContext.PageContext).AddToInitProc('  '+HTMLName+'_Reposition();'+DebugEOL);
  end;*)
  sFirstTabCtrl := '';
  sLastTabCtrl := '';

  lstTabs := TList.Create;
  try
    for i := 0 to ControlCount-1 do
      lstTabs.Add(Controls[i]);

    if lstTabs.Count > 0 then
    begin
      lstTabs.Sort(TabOrderCompare);
      for i := 0 to lstTabs.Count-1 do
        if (TObject(lstTabs[i]) is TIWCustomControl) and TIWCustomControlHelper(lstTabs[i]).FCanReceiveFocus then
        begin
          sFirstTabCtrl := Uppercase(TComponent(lstTabs[i]).Name);
          if TComponent(lstTabs[i]) is TIWCustomCheckBox then
            sFirstTabCtrl := sFirstTabCtrl+'_CHECKBOX';
          break;
        end;
      for i := lstTabs.Count-1 downto 0 do
        if (TObject(lstTabs[i]) is TIWCustomControl) and TIWCustomControlHelper(lstTabs[i]).FCanReceiveFocus then
        begin
          sLastTabCtrl := Uppercase(TComponent(lstTabs[i]).Name);
          if TComponent(lstTabs[i]) is TIWCustomCheckBox then
            sLastTabCtrl := sLastTabCtrl+'_CHECKBOX';
          break;
        end;
    end;
  finally
    lstTabs.Free;
  end;
  sCtrlList := EnumerateControlNames(sFirstTabCtrl, sLastTabCtrl);
  if sCtrlList <> '' then
  begin
    sCtrlList := '"'+sFirstTabCtrl+'",'+sCtrlList+',"'+sLastTabCtrl+'"';
  end else
  begin
    sCtrlList := '"'+sFirstTabCtrl+'","'+sLastTabCtrl+'"';
  end;

   s := s+DebugEOL+
      'idx = IWTop().dlgRegister('''+HTMLName+''','''+sOperaFocus+''');'+DebugEOL+
      'IWTop().dlgRegisterControls('''+HTMLName+''',new Array('+sCtrlList+'));'+DebugEOL;

   if BrowserIsIE(AContext.Browser) then
     s := s + 'IWTop().dlgBrowser=''IE'';'
   else if BrowserIsOpera(AContext.Browser) then
     s := s + 'IWTop().dlgBrowser=''Opera'';'
   else
     s := s + 'IWTop().dlgBrowser=''Other'';';

  s := s+DebugEOL+
      ifThen(FSupportTabbing,'IWTop().dlgSupportTabbing[idx]=true;','IWTop().dlgSupportTabbing[idx]=false;')+DebugEOL+
      ifThen(Assigned(FOnKeyPressEnter),'IWTop().dlgTrapEnter[idx]=true;','IWTop().dlgTrapEnter[idx]=false;')+DebugEOL+
      ifThen(Assigned(FOnKeyPressEscape),'IWTop().dlgTrapEscape[idx]=true;','IWTop().dlgTrapEscape[idx]=false;')+DebugEOL+
      '</script>';

  AContext.PageContext.BodyTag.Contents.AddText(s);
  tagLocker := AContext.PageContext.BodyTag.Contents.AddTag('table');
  tagLocker.AddStringParam('Name',HTMLName+'_locker');
  tagLocker.AddStringParam('ID',HTMLName+'_locker');
  tagLocker.AddIntegerParam('border',0);
  tagLocker.AddIntegerParam('cellpadding',0);
  tagLocker.AddIntegerParam('cellspacing',0);

  s :='left: 0px; top: 0px; width: ' + IntToStr( OwnerForm.Width ) + '; height: ' + IntToStr( OwnerForm.Height ) + '; border-collapse: collapse; z-index:100000;';
  if not Visible then
    s := s + 'visibility: hidden;'
  else
    s := s + 'visibility: visible;';

  if BrowserIsIE(AContext.Browser) or BrowserIsIE4(AContext.Browser) then
    s := s + ' position: absolute; filter: Alpha(false);'
  else begin
    s := s + ' position: absolute;';
    if FModalColor <> clNone then begin
      s := s + ' background-image:url('''+AContext.WebApplication.InternalURLBase+'/gfx/AlphaImage.png'');';
    end;
    if (FModalColor = clNone) and (FModalPictureURL <>'') then begin
      s := s + ' background-image:url('''+AContext.WebApplication.InternalURLBase+FModalPictureURL+''');';
    end;

  end;
  tagLocker.AddStringParam('Style',s);

  if FModalColor <> clNone then
    if (not FModalColorVisible) or (FModalColorVisible and (BrowserIsIE(AContext.Browser) or BrowserIsIE4(AContext.Browser))) then
      tagLocker.AddStringParam('bgcolor', ColorToRGBString(FModalColor));

  tagLocker.Contents.AddTag('tr').Contents.AddTag('td');
  if BrowserIsIE(AContext.Browser) or BrowserIsIE4(AContext.Browser) then
  begin
    s := #13+
      '<script language="JavaScript"> '#13+
      '  IWTop().document.getElementById('''+HTMLName+'_locker'').filters.item(0).enabled = 1;'#13+
      '  IWTop().document.getElementById('''+HTMLName+'_locker'').filters.item(0).opacity = '+IntToStr(FModalColorAlpha)+';'#13+
      #13;
    // supportinputcontrols was here
    s := s+
      '</script>'#13;
    tagLocker.Contents.AddText(s);
  end else
  begin
    // addeventlistener supportinputcontrols
    s := #13+
      '<script language="JavaScript1.2"> '#13+
      '  G_'+HTMLName+'_Submitted = false;'#13;
    // supportinputcontrols here
    s := s+
      '</script>'#13;
      tagLocker.Contents.AddText(s);
  end;
  //<table><tr><td></td></tr></table>}

  FWebApplication := AContext.WebApplication;
  BorderOptions.BGColor := Color;
  {$IFNDEF VSNET}
  BorderOptions.HorizScrollBar := HorzScrollBar.Visible;
  BorderOptions.VertScrollBar := VertScrollBar.Visible;
  {$ENDIF}

  FRegionDiv := BorderOptions.CreateFrame(nil, TIWPageContext40(AContext.PageContext), Self);
  if Visible and FAutoHide and (ahComboBoxes in FAutoHideOptions) then
  begin
    (AContext as TIWComponent40Context).AddScriptFile({AContext.WebApplication.InternalURLBase+}'/js/IWMenu.js');
    TIWPageContext40( AContext.PageContext).AddToInitProc('HideComboBoxes(document.body);');
    TIWPageContext40( AContext.PageContext).AddToInitProc('ShowComboBoxes(document.getElementById('''+Self.HTMLName+'''));');
  end;
  result := FRegionDiv;
end;

function TArcIWDlgPopupRegion.get_ExtraTagParams: TIWStringList;
begin
  result := FIWControlImplementation.get_ExtraTagParams;
end;

procedure TArcIWDlgPopupRegion.set_ExtraTagParams(const Value: TIWStringList);
begin
  FIWControlImplementation.set_ExtraTagParams(Value);
end;

function TArcIWDlgPopupRegion.get_OnHTMLTag: TIWOnHTMLTag;
begin
  result := FIWControlImplementation.OnHTMLTag;
end;

procedure TArcIWDlgPopupRegion.set_OnHTMLTag(AValue: TIWOnHTMLTag);
begin
  FIWControlImplementation.setOnHTMLTag(AValue);
end;

procedure TArcIWDlgPopupRegion.Dispose(ADispose: Boolean);
begin
  Hide;
  FreeAllChildren(self);
  FreeAndNil(FExtraTabNames);
  FreeAndNil(FBorderOptions);
  LayoutMgr := nil; // Cleanup layout manager
  inherited;
  if ADispose then
  begin
    FreeAndNil(FIWControlImplementation);
    FreeAndNil(FIWBaseControlImplementation);
    FreeAndNil(FAutoHideVisibleList);
  end;
  FreeAndNil(FContainerImplementation);
end;

function TArcIWDlgPopupRegion.get_HTMLName: String;
begin
  result := FIWControlImplementation.HTMLName;
end;

function TArcIWDlgPopupRegion.get_DoRefreshControl: Boolean;
begin
  result := FDoRefreshControl;
end;

procedure TArcIWDlgPopupRegion.set_DoRefreshControl(AValue: Boolean);
begin
  FDoRefreshControl := AValue;
end;

function TArcIWDlgPopupRegion.get_Clip: Boolean;
begin
  result := FIWControlImplementation.GetClip;
end;

function TArcIWDlgPopupRegion.get_RenderSize: Boolean;
begin
  result := true;
end;

function TArcIWDlgPopupRegion.get_ScriptEvents: TIWScriptEvents;
begin
  result := FIWControlImplementation.GetScriptEvents;
end;

function TArcIWDlgPopupRegion.get_SkinId: string;
begin
  Result := FIWControlImplementation.getSkinId;
end;

function TArcIWDlgPopupRegion.get_ZIndex: Integer;
begin
  result := FIWControlImplementation.GetZIndex;
end;

procedure TArcIWDlgPopupRegion.set_Clip(AValue: Boolean);
begin
  FIWControlImplementation.SetClip(AValue);
end;

procedure TArcIWDlgPopupRegion.set_Css(AValue: string);
begin

end;

procedure TArcIWDlgPopupRegion.set_RenderSize(AValue: Boolean);
begin
  // Constant
end;

procedure TArcIWDlgPopupRegion.set_ScriptEvents(Value: TIWScriptEvents);
begin
  FIWControlImplementation.SetScriptEvents(Value);
end;

procedure TArcIWDlgPopupRegion.set_SkinId(AValue: string);
begin

end;

procedure TArcIWDlgPopupRegion.set_ZIndex(AValue: Integer);
begin
  FIWControlImplementation.SetZIndex(AValue);
end;

function TArcIWDlgPopupRegion.HintEvents(ATag: TIWHTMLTag{;
  const AHint: string}): string;
begin
  if ShowHint and (Hint <> '') then begin
    ATag.AddStringParam('title', StringReplace(Hint, '''', '&#39', [rfReplaceAll]));
  end;
end;

procedure TArcIWDlgPopupRegion.HookEvents(APageContext: TIWPageContext40;
  AScriptEvents: TIWScriptEvents);
begin

end;

procedure TArcIWDlgPopupRegion.set_Visible(Value: Boolean);

  procedure HideComponents( AParent:TComponent);
  var
    i : integer;
    bAutoHide : boolean;
  begin
    for i := 0 to AParent.ComponentCount -1 do
    begin
      bAutoHide := False;
      if (AParent.Components[i] is TIWCustomComboBox) then
        bAutoHide := (ahComboBoxes in FAutoHideOptions)
      else if (AParent.Components[i] is TIWCustomListBox) then
        bAutoHide := (ahListBoxes in FAutoHideOptions)
      else if (AParent.Components[i] is TIWApplet) then
        bAutoHide := (ahApplets in FAutoHideOptions)
      else if (AParent.Components[i] is TIWCustomObject) then
        bAutoHide := (ahObjects in FAutoHideOptions)
      else if (AParent.Components[i] is TIWURLWindow) then
        bAutoHide := (ahIFrame in FAutoHideOptions);

      if bAutoHide then
      begin
        if (TIWCustomListCombo(AParent.Components[i]).Parent <> Self) and
           TIWCustomListCombo(AParent.Components[i]).Visible then
        begin
          if Assigned(FAutoHideVisibleList) then
            FAutoHideVisibleList.Add(AParent.Components[i]);
          TIWCustomListCombo(AParent.Components[i]).Visible := False;
        end;
      end;
      HideComponents(AParent.Components[i]);
    end;
  end;

var
  i : integer;

begin
  if InSetVisible then exit;
  InSetVisible := True;
  try
    inherited;
    if Value and (Value <> get_Visible) and (not (csDesigning in ComponentState)) then
    begin
      if (ParentForm is TIWAppForm) then
        {$IFDEF INTRAWEB110}
        if False then
        {$ELSE}
        if (ParentForm <> nil) and (TIWAppForm(ParentForm).UpdateMode = umPartial) then
        {$ENDIF}
          ParentForm.AddToInitProc('IWTop().document.all["'+HTMLName+'_locker"].style.visibility = "visible";');
      if FCentered then
      begin
        Left := (Parent.Width div 2) - (Width div 2);
        Top := (Parent.Height div 2) - (Height div 2);
        Anchors := [];
      end;
      if ParentForm <> nil then
      begin
        FOldActiveControl := ParentForm.ActiveControl;
        if FDefaultControl <> nil then
          FDefaultControl.SetFocus;
      end;
    end;
    if (not Value) and (Value <> get_Visible) and (not (csDesigning in ComponentState)) then
    begin
      if ParentForm is TIWAppForm then
      begin
        {$IFDEF INTRAWEB110}
        if False then
        {$ELSE}
        if (ParentForm <> nil) and (TIWAppForm(ParentForm).UpdateMode = umPartial) then
        {$ENDIF}
          ParentForm.AddToInitProc('IWTop().document.all["'+HTMLName+'_locker"].style.visibility = "hidden";');
      end;
      if ParentForm <> nil then
        ParentForm.ActiveControl := FOldActiveControl;
    end;

    FIWControlImplementation.SetVisible(Value);
    FDoRefreshControl := true;
    if FAutoHide and (not (csDesigning in componentstate)) then
    begin
      if Value then
      begin
        HideComponents(ParentForm);
      end else if Assigned(FAutoHideVisibleList) then
      begin
        for i := 0 to FAutoHideVisibleList.Count-1 do
          TIWCustomListCombo(FAutoHideVisibleList[i]).Visible := True;
        FAutoHideVisibleList.Clear;
      end;
    end;
  finally
    InSetVisible := False;
  end;
end;

procedure TArcIWDlgPopupRegion.SetActiveControl(AControl: IIWHTML40Control);
begin
  IWHTML40Interfaces.SetActiveControl(Parent, AControl);
end;

procedure TArcIWDlgPopupRegion.set_LayoutMgr(Value: TIWContainerLayout);
begin
  if Assigned(FLayoutMgr) then begin
    FLayoutMgr.SetContainer(nil);
    FLayoutMgr.RemoveFreeNotification(Self);
  end;
  FLayoutMgr := Value;
  if Assigned(FLayoutMgr) then begin
    FLayoutMgr.FreeNotification(Self);
    FLayoutMgr.SetContainer(Self);
  end;
end;

function TArcIWDlgPopupRegion.get_LayoutMgr: TIWContainerLayout;
begin
  result := TIWContainerLayout(FLayoutMgr);
end;

{$IFDEF INTRAWEB90}
function TArcIWDlgPopupRegion.RenderAsyncComponent(AContext: TIWBaseComponentContext): TIWXMLTag;
begin
  if not Visible or (ControlCount <= 0) then
  begin
    Result:= nil;
    Exit;
  end;

  Result := TIWXMLTag.CreateTag('control');
  try
    Result.AddStringParam('id', HTMLName);
    Result.AddStringParam('type', 'IWREGION');

    AddAsyncStyle(Result, 'background-color:' + ColorToRGBString(Color) + ';');
    AddAsyncStyle(Result, 'color:' + ColorToRGBString(BorderOptions.Color) + ';');
    if Visible then
      AddAsyncStyle(Result, 'visibility: visible;')
    else
      AddAsyncStyle(Result, 'visibility: hidden;');

    FIWControlImplementation.RenderAsyncCommonProperties(TIWBaseHTMLComponentContext(AContext), Result, [acpEnabled..acpAlignment]);
  except
    FreeAndNil(Result);
  end;
end;

procedure TArcIWDlgPopupRegion.RenderAsyncComponents(
  AContext: TIWContainerContext; APageContext: TIWBasePageContext);
begin
// todo?
end;
{$ENDIF}

procedure TArcIWDlgPopupRegion.RenderComponents(AContainerContext: TIWContainerContext; APageContext: TIWBasePageContext);
Var
  AStream: {$IFDEF INTRAWEB72} TIWRenderStream{$ELSE}TStringStream{$ENDIF};
begin
  ContainerContext := AContainerContext;
  inherited RenderComponents(ContainerContext, APageContext);
  TIWContainerLayout(ContainerContext.LayoutManager).ProcessControls(ContainerContext, TIWBaseHTMLPageContext(APageContext));
  AStream := {$IFDEF INTRAWEB72}TIWRenderStream.Create{$ELSE}TStringStream.Create(''){$ENDIF};
  try
    with TIWContainerLayout(ContainerContext.LayoutManager) do
    try
      Process(AStream, ContainerContext, APageContext);
      SetContainer(nil);
    finally
      FRegionDiv.Contents.AddBuffer(AStream);
      if FDefaultControl <> nil then
      begin
        {FRegionDiv.Contents.AddText(
          DebugEOL+
          '<script language="JavaScript1.2"> '+DebugEOL+
          '  document.getElementById('''+FDefaultControl.HTMLName+''').focus();'+DebugEOL+
          IfThen(TIWBaseComponentHelper(FDefaultControl).SupportsInput,'  document.getElementById('''+FDefaultControl.HTMLName+''').select();'+DebugEOL)+
          '</script>'+DebugEOL);}
          {DebugEOL+
          '<script language="JavaScript1.2"> '+DebugEOL+
          '  document.getElementById('''+FDefaultControl.HTMLName+''').focus();'+DebugEOL+
          IfThen(TIWBaseComponentHelper(FDefaultControl).SupportsInput,'  document.getElementById('''+FDefaultControl.HTMLName+''').select();'+DebugEOL)+
          '</script>'+DebugEOL);}
      end;
    end;
  finally
    AStream.Free;
  end;
end;

function TArcIWDlgPopupRegion.get_UseFrame: Boolean;
begin
  result := FUseFrame;
end;

procedure TArcIWDlgPopupRegion.set_UseFrame(AValue: Boolean);
begin
  FUseFrame := AValue;
end;

procedure TArcIWDlgPopupRegion.OnBorderChange(ASender: TObject);
begin
  {BorderOptions.Style := TIWContainerBorderOptions(ASender).Style;
  BorderOptions.NumericWidth := TIWContainerBorderOptions(ASender).NumericWidth;
  BorderOptions.Color := TIWContainerBorderOptions(ASender).Color;
  BorderOptions.Width := TIWContainerBorderOptions(ASender).Width;}
  RequestAlign;//ForceAlign; // JDS Change to comply with IWRegion changes.
  Invalidate;
end;

function TArcIWDlgPopupRegion.GenerateControlPositions: String;
Var
  i: Integer;
begin
  result := '';
  with HTML40ContainerInterface(Self) do begin
    for i := 0 to IWComponentsCount - 1 do begin
      if SupportsInterface(Component[i], IIWHTML40Control) then begin
        with HTML40ControlInterface(Component[i]) do begin
          if DoRefreshControl then
            result := result + EOL 
              + 'IWTop().' + HTMLName + Format('IWCL.SetRect(new parent.Rect(%d, %d, %d, %d));',
              //+ 'IWTop().' + HTMLName + Format('IWCL.SetRect(new top.Rect(%d, %d, %d, %d));',  // JDS Change to comply with IWRegion changes.
                [HTMLLeft, HTMLTop, HTMLWidth, HTMLHeight]) + EOL;
        end;
      end;
      if SupportsInterface(Component[i], IIWHTML40Container) then begin
        result := result + EOL
          + HTML40ContainerInterface(Component[i]).GenerateControlPositions;
      end;
    end;
  end;
end;

{$IFNDEF VSNET}
procedure TArcIWDlgPopupRegion.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  DoRefreshControl := (ALeft <> Left) or (ATop <> Top) or (AWidth <> Width) or (AHeight <> Height) or DoRefreshControl;
  inherited;
end;
{$ENDIF}

procedure TArcIWDlgPopupRegion.SetName(const Value: TComponentName);
begin
  if Name <> 'IWFrameRegion' then
    inherited
  else
    Align := alClient;
end;

function TArcIWDlgPopupRegion.ComponentContextClass: TIWBaseComponentContextClass;
begin
  result := TIWComponent40Context;
end;

// IIWContainer methods

function TArcIWDlgPopupRegion.InitContainerContext(AWebApplication: TIWApplication): TIWContainerContext;
Var
  LLayoutMgr: TIWBaseContainerLayout;
begin
  result := IWBaseRenderContext.TIWContainerContext.Create(AWebApplication);
  LLayoutMgr := LayoutMgr;

  if LLayoutMgr = nil then begin
    LLayoutMgr := TIWLayoutMgrForm.Create(Self);
  end else begin
    LLayoutMgr.SetContainer(Self);
    if not LLayoutMgr.Able then begin
      LLayoutMgr := TIWLayoutMgrForm.Create(Self);
    end;
  end;

  LLayoutMgr.SetContainer(Self);
  result.LayoutManager := LLayoutMgr;
end;

procedure TArcIWDlgPopupRegion.DoHTMLTag(ATag: TIWHTMLTag);
begin
  FIWControlImplementation.DoHTMLTag(ATag);
end;

function TArcIWDlgPopupRegion.get_Canvas: TCanvas;
begin
  result := FIWBaseControlImplementation.GetCanvas;
end;

function TArcIWDlgPopupRegion.get_WebCursor: TIWCursor;
begin
  result := FIWControlImplementation.GetCursor;
end;

function TArcIWDlgPopupRegion.get_WebFont: TIWFont;
begin
  result := FIWControlImplementation.Font;
end;

function TArcIWDlgPopupRegion.get_HTMLHeight: Integer;
begin
  if IsDesignMode then begin
    result := FIWControlImplementation.HTMLHeight;
  end else begin
    result := FIWControlImplementation.HTMLHeight;
   if Assigned(FWebApplication) then
   begin { Hidden region have FWebApplication = nil but are resized at list if they are inside another region }
     Result := Result - iif(BrowserIsNetscape6(FWebApplication.Browser) or BrowserIsNetscape7(FWebApplication.Browser), FBorderOptions.PixelWidth * 2, 0);
   end;
  end;
end;

function TArcIWDlgPopupRegion.get_HTMLLeft: Integer;
begin
  result := FIWControlImplementation.GetHTMLLeft;
  {$IFDEF VSNET}
  {$ELSE}
  if Parent is TFrame then begin
    Result := Parent.Left;
  end;
  {$ENDIF}
end;

function TArcIWDlgPopupRegion.get_HTMLTop: Integer;
begin
  result := FIWControlImplementation.GetHTMLTop;
  {$IFDEF VSNET}
  {$ELSE}
  if Parent is TFrame then begin
    Result := Parent.Top;
  end;
  {$ENDIF}
end;

function TArcIWDlgPopupRegion.get_HTMLWidth: Integer;
begin
  if IsDesignMode then begin
    result := FIWControlImplementation.HTMLWidth;
  end else begin
    result := FIWControlImplementation.HTMLWidth;
    if Assigned(FWebApplication) then begin { Hidden region have FWebApplication = nil
       but are resized at list if they are inside another region }
       result := result -
         iif(BrowserIsNetscape6(FWebApplication.Browser) or BrowserIsNetscape6(FWebApplication.Browser), FBorderOptions.PixelWidth * 2, 0);
    end;
  end;
end;

function TArcIWDlgPopupRegion.get_IWComponentsCount: Integer;
begin
  Result := inherited get_IWComponentsCount;
end;

{$IFNDEF CLR}
function TArcIWDlgPopupRegion.get_WebColor: TIWColor;
begin
  result := FIWControlImplementation.GetIWColor;
end;

procedure TArcIWDlgPopupRegion.set_WebColor(Value: TIWColor);
begin
//  BorderOptions.BGColor := Value;
  FIWControlImplementation.SetIWColor(Value);
end;
{$ENDIF}

function TArcIWDlgPopupRegion.getTabOrder: TIWTabOrder;
begin
  {$IFNDEF VSNET}
  if Parent is tframe then
    result := inherited getTabOrder
  else
  {$ENDIF}
    result := FIWControlImplementation.getTabOrder;
end;

procedure TArcIWDlgPopupRegion.setTabOrder(AValue: TIWTabOrder);
begin
  {$IFNDEF VSNET}
  if Parent is tframe then
    inherited setTabOrder(AValue)
  else
  {$ENDIF}
    FIWControlImplementation.SetTabOrder(AValue);
end;

{$IFNDEF VSNET}
procedure TArcIWDlgPopupRegion.Invalidate;
begin
  inherited;
  FDoRefreshControl := true;
end;
{$ENDIF}

{$IFDEF INTRAWEB72}
procedure TArcIWDlgPopupRegion.MakeHTMLTag(ATag: TIWHTMLTag; ABuffer: TIWRenderStream);
begin
  FIWControlImplementation.MakeHTMLTag(ATag,ABuffer);
  ABuffer.WriteString(ParentContainer.ContainerContext.ComponentContext[Name].PostRender);
end;
{$ENDIF}

function TArcIWDlgPopupRegion.MakeHTMLTag(ATag: TIWHTMLTag): string;
var
  LBuffer : TIWRenderStream;
begin
  LBuffer := TIWRenderStream.Create;
  try
    MakeHTMLTag(ATag,LBuffer);
    Result := LBuffer.Extract;
  finally
    LBuffer.Free;
  end;
end;

procedure TArcIWDlgPopupRegion.PaintTo(ACanvas: TCanvas);
begin
  FIWBaseControlImplementation.PaintTo(ACanvas);
end;

function TArcIWDlgPopupRegion.ParentContainer: IIWBaseContainer;
begin
  result := FIWBaseControlImplementation.ParentContainer;
end;

function TArcIWDlgPopupRegion.RenderCSSClass(
  AComponentContext: TIWBaseHTMLComponentContext): string;
begin
  result := FIWControlImplementation.RenderCSSClass(AComponentContext);
end;

{$IFDEF VSNET}
{$ELSE}
function TArcIWDlgPopupRegion.GetClientRect: TRect;
begin
  result := inherited GetClientRect;

  if not IsDesignMode then begin
    InflateRect(result, -FBorderOptions.PixelWidth, -FBorderOptions.PixelWidth);
  end;
end;

procedure TArcIWDlgPopupRegion.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  if IsDesignMode then begin
    InflateRect(Rect, -FBorderOptions.PixelWidth, -FBorderOptions.PixelWidth);
  end;
end;
{$ENDIF}

procedure TArcIWDlgPopupRegion.RenderScripts(
  AComponentContext: TIWBaseHTMLComponentContext);
begin
  FIWControlImplementation.RenderScripts(AComponentContext);
  if AComponentContext.ContainerContext.LayoutManager is TIWLayoutMgrForm then begin
    with TIWPageContext40(AComponentContext.PageContext) do begin
      // if (WebApplication.Browser <> brOpera) then begin
        AddToIWCLInitProc('if (' + HTMLName + 'IWCL) {');
        AddToIWCLInitProc(HTMLName + 'IWCL.BorderWidthPixels = ' + IntToStr(FBorderOptions.PixelWidth) + ';');
        AddToIWCLInitProc(HTMLName + 'IWCL.BorderWidth = ' + IntToStr(FBorderOptions.PixelWidth) + ';');
        AddToIWCLInitProc('}');
      // end;
    end;
  end;
end;

function TArcIWDlgPopupRegion.RenderStyle(
  AComponentContext: TIWBaseHTMLComponentContext): string;
var
  intf: IIWHTML40Container;
begin
  result := FIWControlImplementation.RenderStyle(AComponentContext);
  if (Parent <> nil) then
  begin
    intf := HTML40ContainerInterface(Parent);
    if (intf <> nil) and (intf.get_LayoutMgr <> nil) and (intf.get_LayoutMgr.Enabled) then
    begin
      // Then the popupregion is in an active template and must override the relative positioning
      Result := ReplaceStr(Result,'relative','absolute;left:50%;top:50%;margin-left:-'+IntToStr(Width div 2)+'px;margin-top:-'+IntToStr(Height div 2)+'px;');
    end;
  end;
  
end;

function TArcIWDlgPopupRegion.RequiresRefresh: Boolean;
begin

end;

procedure TArcIWDlgPopupRegion.set_WebCursor(AValue: TIWCursor);
begin
  FIWControlImplementation.SetCursor(AValue);
end;

procedure TArcIWDlgPopupRegion.set_WebFont(AValue: TIWFont);
begin
  FIWControlImplementation.Font.Assign(AValue);
end;

function TArcIWDlgPopupRegion.SupportsInput: Boolean;
begin
  result := false;
  //result := FIWControlImplementation.SupportsInput;
end;

function TArcIWDlgPopupRegion.SupportsSubmit: Boolean;
begin
  result := True;
  //result := FIWControlImplementation.SupportsSubmit;
end;

function TArcIWDlgPopupRegion.WantsSubmitValidation: Boolean;
begin
  result := FIWControlImplementation.WantsSubmitValidation;
end;

function TArcIWDlgPopupRegion.get_Visible: Boolean;
begin
  result := FIWControlImplementation.GetVisible;
end;

function TArcIWDlgPopupRegion.InitPaintHandler: TIWPaintHandler;
begin
  result := TIWBaseControl.CreatePaintHandler(ClassName, Self);
end;

function TArcIWDlgPopupRegion.get_ComponentName: String;
begin
  result := HTMLName;
end;

{$IFDEF INTRAWEB90}
procedure TArcIWDlgPopupRegion.AddAsyncStyle(ATag: TIWXMLTag; AStyle: string);
begin
  FIWControlImplementation.AddAsyncStyle(ATag, AStyle);
end;

function TArcIWDlgPopupRegion.get_StyleRenderOptions: TIWStyleRenderOptions;
begin
  Result := FIWControlImplementation.GetStyleRenderOptions;
end;

procedure TArcIWDlgPopupRegion.set_StyleRenderOptions(AValue: TIWStyleRenderOptions);
begin
  FIWControlImplementation.GetStyleRenderOptions.Assign(AValue);
end;

{$ENDIF}

function TArcIWDlgPopupRegion.get_Css: string;
begin
  Result := FIWControlImplementation.getCss;
end;

procedure TArcIWDlgPopupRegion.AddFreeNotifier(AObject: TObject);
begin
end;

procedure TArcIWDlgPopupRegion.RemoveFreeNotifier(AObject: TObject);
begin
end;

function TArcIWDlgPopupRegion.get_SupportsPartial: Boolean;
begin
  result := FIWControlImplementation.get_SupportsPartial;
end;

procedure TArcIWDlgPopupRegion.set_SupportsPartial(AValue: BOolean);
begin
  FIWControlImplementation.set_SupportsPartial(AValue);
end;

procedure TArcIWDlgPopupRegion.Submit(const AValue: String);
var
  i : integer;
begin
  FSubmitParam := AValue;
  i := StrToIntDef(AValue,-1);
  case i of
    0: if Assigned(FOnKeyPressEnter) then FOnKeyPressEnter(Self);
    1: if Assigned(FOnKeyPressEscape) then FOnKeyPressEscape(Self);
  end;
end;

function TArcIWDlgPopupRegion.GetSubmitParam: String;
begin
  result := FSubmitParam;
end;

function TArcIWDlgPopupRegion.getSubmitProc: TSubmitProc;
begin
  Result := Submit;
end;

procedure TArcIWDlgPopupRegion.SetExtraTabNames(const Value: TStrings);
begin
  FExtraTabNames.Assign( Value);
end;

initialization
  TIWServer.AddInternalFile('IW_GFX_AlphaImage', '/gfx/AlphaImage.png');
  TIWServer.AddInternalFile('IW_JS_ArcIWDlgPopupRegion', '/files/ArcIWDlgPopupRegion.js');

end.
