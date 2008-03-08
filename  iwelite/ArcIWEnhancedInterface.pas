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

unit ArcIWEnhancedInterface;

interface

{$I IntrawebVersion.inc}
{$I Eval.inc}

uses SysUtils, Classes, IWColor, IWFont, IWHTMLTag, IWControl, Graphics,
  IWTypes, Controls, IWFileReference
  {$IFNDEF INTRAWEB51},IWBaseControl, IWRenderContext{$ENDIF}
  {$IFDEF INTRAWEB70}, IWBaseInterfaces, IWMarkupLanguageTag, IWLayoutMgrForm {$ENDIF}
  , IWKlooch;


type
  TIWControlHelper = class(TIWControl)
  public
    property Editable;
  end;


  TArcIWNotEditableOffsets = class(TPersistent)
  private
    FIE_XP: integer;
    FNS6: integer;
    FIE_Mac: integer;
    FIE_Win: integer;
    FOther: integer;
    FOpera: integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
  published
    property Opera : integer read FOpera write FOpera default 2;
    property Other : integer read FOther write FOther default 4;
    property IE_XP : integer read FIE_XP write FIE_XP default 4;
    property IE_Win : integer read FIE_Win write FIE_Win default 4;
    property IE_Mac : integer read FIE_Mac write FIE_Mac default 4;
    property NS6 : integer read FNS6 write FNS6 default 4;
  end;

  TArcIWAutoResize = (arLabel, arComponent);
  TArcIWLabelPosition = (lpLeft, lpRight, lpTop, lpBottom);
  TArcIWBorderStyle = (bsNone, bsLowered, bsRaised, bsSolid, bsRidge);
  TArcIWMarkerType = (lmNone, lmDisc, lmCircle, lmSquare, lmDecimal,
    lmLowerRoman, lmUpperRoman, lmLowerAlpha, lmUpperAlpha);

  TArcIWTextStyle = (ldUnderline, ldOverline, ldLineThrough, ldBlink, ldSmallCaps);
  TArcIWTextStyles = set of TArcIWTextStyle;

  TArcIWMarker = class(TPersistent)
  private
    FMarkerType: TArcIWMarkerType;
    FMarkerFile: TIWFileReference;
    procedure SetMarkerFile(const Value: TIWFileReference);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    function RenderStyle(aURLBase : string)  : string;
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property MarkerType : TArcIWMarkerType read FMarkerType write FMarkerType;
    property MarkerFile : TIWFileReference read FMarkerFile write SetMarkerFile;
  end;

  {TArcIWShadow = class(TCollectionItem)
  private
    FxOffset: integer;
    FyOffset: integer;
    FBlurRadius: integer;
    FColor: TIWColor;
  public
    function RenderStyle : string;
  published
    property xOffset : integer read FxOffset write FxOffset;
    property yOffset : integer read FyOffset write FyOffset;
    property Color : TIWColor read FColor write FColor;
    property BlurRadius : integer read FBlurRadius write FBlurRadius;
  end;

  TArcIWShadows = class(TCollection)
  private
    function GetShadows(idx: integer): TArcIWShadow;
    procedure SetShadows(idx: integer; const Value: TArcIWShadow);
  public
    property Shadows[idx : integer] : TArcIWShadow read GetShadows write SetShadows; default;
    function RenderStyle : string;
  end;        }

  TArcIWEnhancer = class(TPersistent)
  private
    FLabelPadding: integer;
    FLabelCaption: string;
    FLabelAlignment: TAlignment;
    FBorderStyle: TArcIWBorderStyle;
    FLabelPosition: TArcIWLabelPosition;
    FLabelMarker: TArcIWMarker;
    //FLabelShadows: TArcIWShadows;
    FLoaded : boolean;
    FLabelTextStyles: TArcIWTextStyles;
    FBorderColor: TIWColor;
    FBackgroundColor: TIWColor;
    FLabelFont: TIWFont;
    {$IFNDEF INTRAWEB51}
    FComp : TIWCustomControl;
    {$ELSE}
    FComp : TIWControl;
    {$ENDIF}
    FBorderSize: integer;
    FBackgroundFixed: boolean;
    FBackgroundImage: TIWFileReference;
    FBorderPadding: integer;
    FCompHeight: integer;
    FCompWidth: integer;
    FCompLeft: integer;
    FCompTop: integer;
    FDesignTime : boolean;
    InternalResizing : boolean;
    FAutoResize: TArcIWAutoResize;
    FNotEditableOffsets : TArcIWNotEditableOffsets;
    FLabelVisible: boolean;
    FFixedPosAndSize : boolean;
    FCompExtraTagParams : TStrings;
    FLabelExtraTagParams : TStrings;

    procedure SetLabelFont(const Value: TIWFont);
    //procedure SetLabelShadows(const Value: TArcIWShadows);
    procedure SetLabelMarker(const Value: TArcIWMarker);
    procedure SetBackgroundImage(const Value: TIWFileReference);
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelPosition(const Value: TArcIWLabelPosition);
    procedure SetCompHeight(const Value: integer);
    procedure SetCompLeft(const Value: integer);
    procedure SetCompTop(const Value: integer);
    procedure SetCompWidths(const Value: integer);
    procedure SetLabelAlignment(const Value: TAlignment);
    procedure SetBackgroundColor(const Value: TIWColor);
    procedure SetBackgroundFixed(const Value: boolean);
    procedure SetBorderColor(const Value: TIWColor);
    procedure SetBorderPadding(const Value: integer);
    procedure SetBorderSize(const Value: integer);
    procedure SetBorderStyle(const Value: TArcIWBorderStyle);
    procedure SetLabelPadding(const Value: integer);
    procedure SetLabelTextStyles(const Value: TArcIWTextStyles);
    procedure SetNotEditableOffsets(const Value: TArcIWNotEditableOffsets);
    procedure SetLabelVisible(const Value: boolean);

    procedure SetCompExtraTagParams(const Value : TStrings);
    procedure SetLabelExtraTagParams(const Value : TStrings);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function BorderOffsets : integer;
    function DesigntimeLabelColor : TColor;
    function DesigntimeCompColor : TColor;
  public
    constructor Create(Comp : {$IFNDEF INTRAWEB51}TIWCustomControl{$ELSE}TIWControl{$ENDIF}; DesignTime : boolean); reintroduce; virtual;
    {$IFDEF INTRAWEB51}
    function Render(InheritedRender : TIWHTMLTag) : TIWHTMLTag;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function Render(AContext : TIWBaseComponentContext; InheritedRender : TIWHTMLTag) : TIWHTMLTag;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function Render(AContext: TIWBaseHTMLComponentContext; InheritedRender : TIWHTMLTag): TIWHTMLTag;
    {$ENDIF}
    procedure Loaded;
    procedure Paint;
    procedure Resize;
    destructor Destroy; override;
    function get_ShouldRenderTabOrder: Boolean;
  published
    property AutoResize : TArcIWAutoResize read FAutoResize write FAutoResize;
    property LabelVisible : boolean read FLabelVisible write SetLabelVisible default True;
    property LabelCaption : string read FLabelCaption write SetLabelCaption;
    property LabelPosition : TArcIWLabelPosition read FLabelPosition write SetLabelPosition default lpLeft;
    property LabelPadding : integer read FLabelPadding write SetLabelPadding;
    property LabelAlignment : TAlignment read FLabelAlignment write SetLabelAlignment default taRightJustify;
    property LabelFont : TIWFont read FLabelFont write SetLabelFont;
    //property LabelShadows : TArcIWShadows read FLabelShadows write SetLabelShadows;
    property LabelMarker : TArcIWMarker read FLabelMarker write SetLabelMarker;
    property LabelTextStyles : TArcIWTextStyles read FLabelTextStyles write SetLabelTextStyles;
    property LabelExtraTagParams : TStrings read FLabelExtraTagParams write SetLabelExtraTagParams;

    property BorderStyle : TArcIWBorderStyle read FBorderStyle write SetBorderStyle;
    property BorderColor : TIWColor read FBorderColor write SetBorderColor;
    property BorderPadding : integer read FBorderPadding write SetBorderPadding;
    property BorderSize : integer read FBorderSize write SetBorderSize;

    property BackgroundColor : TIWColor read FBackgroundColor write SetBackgroundColor;
    property BackgroundImage : TIWFileReference read FBackgroundImage write SetBackgroundImage;
    property BackgroundFixed : boolean read FBackgroundFixed write SetBackgroundFixed;

    property CompLeft : integer read FCompLeft write SetCompLeft;
    property CompTop : integer read FCompTop write SetCompTop;
    property CompWidth : integer read FCompWidth write SetCompWidths;
    property CompHeight : integer read FCompHeight write SetCompHeight;
    property CompExtraTagParams : TStrings read FCompExtraTagParams write SetCompExtraTagParams;
    property NotEditableOffsets : TArcIWNotEditableOffsets read FNotEditableOffsets write SetNotEditableOffsets;
    property FixedPosAndSize : boolean read FFixedPosAndSize write FFixedPosAndSize default true;
  end;

implementation

uses Math;

{$IFNDEF INTRAWEB51}
function Canvas(FComp : TIWCustomControl) : TCanvas;
begin
  {$IFDEF INTRAWEB60}
    Result := FComp.Canvas;
  {$ELSE}
    Result := BaseControlinterface(FComp).Canvas;
  {$ENDIF}
end;
{$ELSE}
function Canvas(FComp : TIWControl) : TCanvas;
begin
  result := FComp.Canvas;
end;
{$ENDIF}

{$IFDEF VER130}

function ifThen(b : boolean; val1 : integer; val2 : integer = 0) : integer;
begin
  if b then Result := val1 else Result := val2;
end;

{$ENDIF}

(*{ TArcIWShadows }

function TArcIWShadows.GetShadows(idx: integer): TArcIWShadow;
begin
  Result := TArcIWShadow(Items[idx]);
end;

function TArcIWShadows.RenderStyle: string;
var
  i : integer;
begin
  result := '';
  if count = 0 then exit;

  result := 'text-shadow: ';
  for i := 0 to Count-1 do
  begin
    if i > 0 then
      Result := Result+',';
    Result := Result + Shadows[i].RenderStyle;
  end;
  Result := Result+';';
end;

procedure TArcIWShadows.SetShadows(idx: integer;
  const Value: TArcIWShadow);
begin
  Items[idx] := value;
end;
*)

{ TArcIWEnhancer }

function TArcIWEnhancer.get_ShouldRenderTabOrder;
begin
  Result := False;
end;

procedure TArcIWEnhancer.SetCompExtraTagParams(const Value : TStrings);
begin
  FCompExtraTagParams.Assign(Value);
end;

procedure TArcIWEnhancer.SetLabelExtraTagParams(const Value : TStrings);
begin
  FLabelExtraTagParams.Assign(Value);
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhancer.Render(InheritedRender : TIWHTMLTag) : TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhancer.Render(AContext : TIWBaseComponentContext; InheritedRender : TIWHTMLTag) : TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhancer.Render(AContext: TIWBaseHTMLComponentContext; InheritedRender : TIWHTMLTag): TIWHTMLTag;
{$ENDIF}
  function IsNotTemplate(str : string; alt : string='') : string;
  begin
    {$IFDEF INTRAWEB70}
      if AContext.ContainerContext.LayoutManager is TIWLayoutMgrForm then
        Result := str
      else
        Result := alt;
    {$ELSE}
      Result := str;
    {$ENDIF}
  end;
  procedure DoLabelTag(tag : TIWHtmlTag);
    procedure DrawLabel(l,t,r,b : integer);
    var
      sStyle : string;
    begin
      if FFixedPosAndSize then
      begin
        sStyle := IsNotTemplate('position: absolute;'+
                  'left: '+IntToStr(l)+'px;'+
                  'top: '+IntToStr(t)+'px;')+
                  'height: '+IntToStr(b-t)+'px;'+
                  'width: '+IntToStr(r-l)+'px;';
      end else
        sStyle := '';
      sStyle := sStyle+'Padding-top: '+IntToStr(FLabelPadding)+'px;';

      sStyle := sStyle+FLabelFont.FontToStringStyle(
        {$IFDEF INTRAWEB51}FComp.WebApplication.{$ELSE}AContext.{$ENDIF}Browser,
        {$IFNDEF CLR}FComp.Font{$ELSE}FComp.WebFont{$ENDIF});

      case FLabelAlignment of
        taLeftJustify:   sStyle := sStyle + 'text-align: left;';
        taRightJustify:  sStyle := sStyle + 'text-align: right;';
        taCenter:        sStyle := sStyle + 'text-align: center;';
      end;

      sStyle := sStyle+
                //TArcIWShadows(FLabelShadows).RenderStyle+
                FLabelMarker.RenderStyle({$IFDEF INTRAWEB51}FComp.{$ELSE}AContext.{$ENDIF}WebApplication.AppURLBase);

      if (FLabelTextStyles <> []) and (FLabelTextStyles <> [ldSmallCaps]) then
      begin
        sStyle := sStyle+'text-decoration: ';
        if ldUnderline in FLabelTextStyles then
          sStyle := sStyle+'underline ';
        if ldOverline in FLabelTextStyles then
          sStyle := sStyle+'overline ';
        if ldLineThrough in FLabelTextStyles then
          sStyle := sStyle+'line-through ';
        if ldBlink in FLabelTextStyles then
          sStyle := sStyle+'blink ';
        sStyle := sStyle+';'
      end;
      if ldSmallCaps in FLabelTextStyles then
        sStyle := sStyle+'font-variant: small-caps;';

      tag.AddStringParam('style',sStyle+'white-space : nowrap;');
      tag.AddParmsList(TStringList(FLabelExtraTagParams));

      tag.Contents.AddText(FLabelCaption);
    end;
  begin
    if FLabelVisible then
      case FLabelPosition of
        lpLeft:   DrawLabel((BorderOffsets div 2), (BorderOffsets div 2), FCompLeft-4, FComp.Height-(BorderOffsets div 2));
        lpRight:  DrawLabel(FCompWidth+4, (BorderOffsets div 2), FComp.Width - (BorderOffsets div 2)-4, FComp.Height-(BorderOffsets div 2));
        lpTop:    DrawLabel((BorderOffsets div 2), (BorderOffsets div 2), FCompWidth+(BorderOffsets div 2), FCompTop);
        lpBottom: DrawLabel((BorderOffsets div 2), FCompHeight + (BorderOffsets div 2), FComp.Width-(BorderOffsets div 2), FComp.Height-(BorderOffsets div 2));
      end;
  end;

  procedure DoCompTag(tag : TIWHtmlTag);
  var
    s, sWH : string;
    iTopOffset : integer;
  begin
    iTopOffset := 0;
    
    if (not TIWControlHelper(FComp).Editable) then
    begin
      case {$IFDEF INTRAWEB51}FComp.WebApplication.{$ELSE}AContext.{$ENDIF}Browser of
        brIE:
          begin
            s := lowercase({$IFDEF INTRAWEB51}FComp.{$ELSE}AContext.{$ENDIF}WebApplication.Request.UserAgent);
            if Pos('windows nt 5.1',s)>0 then
              iTopOffset := FNotEditableOffsets.IE_XP
            else
              if Pos('windows',s)>0 then
                iTopOffset := FNotEditableOffsets.IE_Win
              else
                iTopOffset := FNotEditableOffsets.IE_Mac;
          end;
        brNetscape6:  iTopOffset := FNotEditableOffsets.NS6;
        brOpera:      iTopOffset := FNotEditableOffsets.Opera;
        else          iTopOffset := FNotEditableOffsets.Other;
      end;
    end;

    if Assigned(FComp.OnHTMLTag) then
      FComp.OnHTMLTag(FComp,InheritedRender);

    if FFixedPosAndSize then
    begin
      s :=   IsNotTemplate('position: absolute;'+
             'left: '+IntToStr(FCompLeft+ifThen(not FLabelVisible,FBorderSize+FBorderPadding))+'px;'+
             'top: '+IntToStr(FCompTop+ifThen(not FLabelVisible,FBorderSize+FBorderPadding)+iTopOffset)+'px;',
             'position: relative;');
      sWH := 'height: '+IntToStr(FCompHeight+ifThen(not FLabelVisible,FBorderSize+FBorderPadding)-iTopOffset)+'px;'+
             'width: '+IntToStr(FCompWidth+ifThen(not FLabelVisible,FBorderSize+FBorderPadding))+'px;';
    end else
    begin
      s := '';
      sWH := '';
    end;

    tag.Params.Values['Style'] := tag.Params.Values['Style'] + 'white-space : nowrap;'+s+sWH;

    InheritedRender.AddStringParam('ID',FComp.HTMLName);
    {$IFDEF INTRAWEB70}
    InheritedRender.AddStringParam('style',FComp.{$IFNDEF CLR}Font{$ELSE}WebFont{$ENDIF}.FontToStringStyle(AContext.Browser,nil));
    {$ENDIF}
    if CompareText(InheritedRender.Tag,'input')=0 then
      InheritedRender.AddStringParam('Name',FComp.HTMLName);
    with tag.Contents.AddTagAsObject(InheritedRender) do
      if FLabelPosition in [lpLeft, lpRight] then
        Params.Values['Style'] := Params.Values['Style'] + sWH;
    InheritedRender.AddParmsList(TStringList(FCompExtraTagParams));
    if FComp.HasTabOrder then
      TIWControlHelper(FComp).RenderTabOrder(InheritedRender);
  end;
var
  s, sStyle : string;
begin
  Result := TIWHtmlTag.CreateTag('SPAN');
  Result.AddStringParam('ID',FComp.HTMLName+'_Border');
  Result.AddStringParam('CLASS',FComp.HTMLName+'CSS');

  if CompareText(InheritedRender.Tag,'input')=0 then
    Result.AddStringParam('name','XXX');
  case FBorderStyle of
    bsLowered: sStyle := 'border-style: inset;';
    bsRaised: sStyle := 'border-style: outset;';
    bsSolid: sStyle := 'border-style: solid;';
    bsRidge: sStyle := 'border-style: ridge;';
  end;

  if FComp.Align = alNone then
  begin
    sStyle := sStyle+
        'width: '+IntToStr(FComp.Width)+'px;'+
        'height: '+IntToStr(FComp.Height)+'px;';
  end;
  if FBorderSize > 0 then
    sStyle := sStyle+'border-width: '+IntToStr(FBorderSize)+';';
  if FBorderPadding <> 0 then
    sStyle := sStyle+'padding: '+IntToStr(FBorderPadding)+';';
  if FBorderColor <> clNone then
    sStyle := sStyle+'border-color: '+{$IFDEF INTRAWEB51}FComp.{$ENDIF}ColorToRGBString(FBorderColor)+';';
  if FBackgroundColor <> clNone then
    sStyle := sStyle+'background-color: '+{$IFDEF INTRAWEB51}FComp.{$ENDIF}ColorToRGBString(FBackgroundColor)+';';
  s := FBackgroundImage.Location({$IFDEF INTRAWEB51}FComp.{$ELSE}AContext.{$ENDIF}WebApplication.AppURLBase);
  if s <> '' then
    sStyle := sStyle+'background-image: url('+s+');';
  if FBackgroundFixed then
    sStyle := sStyle+'background-attachment: fixed;';

  Result.AddStringParam('style',sStyle+'white-space : nowrap;');

{
  border-width
  text-indext: 0px
  background-repeat: repeat no-repeat repeat-x repeat-y

  overflow: visible | hidden | scroll | auto | inherit
  cursor: auto crosshair default pointer move e-resize, ne-resize, nw-resize, n-resize, se-resize, sw-resize, s-resize, w-resize, text, wait, help
}
  case FLabelPosition of
    lpLeft:
      begin
        DoLabelTag(Result.Contents.AddTag('SPAN'));
        DoCompTag(Result.Contents.AddTag('SPAN'));
      end;
    lpRight:
      begin
        DoCompTag(Result.Contents.AddTag('SPAN'));
        DoLabelTag(Result.Contents.AddTag('SPAN'));
      end;
    lpTop:
      begin
        DoLabelTag(Result.Contents.AddTag('SPAN'));
        {$IFNDEF INTRAWEB70}
        Result.Contents.AddTag('BR',cbFalse);
        {$ELSE}
        Result.Contents.AddTag('BR').ClosingTag := cbFalse;
        {$ENDIF}
        DoCompTag(Result.Contents.AddTag('SPAN'));
      end;
    lpBottom:
      begin
        DoCompTag(Result.Contents.AddTag('SPAN'));
        {$IFNDEF INTRAWEB70}
        Result.Contents.AddTag('BR',cbFalse);
        {$ELSE}
        Result.Contents.AddTag('BR').ClosingTag := cbFalse;
        {$ENDIF}
        DoLabelTag(Result.Contents.AddTag('SPAN'));
      end;
  end;
end;

constructor TArcIWEnhancer.Create(Comp: {$IFNDEF INTRAWEB51}TIWCustomControl{$ELSE}TIWControl{$ENDIF}; DesignTime : Boolean);
begin
  inherited Create;
  FCompExtraTagParams := TStringList.Create;
  FLabelExtraTagParams := TStringList.Create;
  FFixedPosAndSize := True;
  FLabelVisible := True;
  FComp := Comp;
  FDesignTime := DesignTime;

  FLabelMarker     := TArcIWMarker.Create;
  //FLabelShadows    := TArcIWShadows.Create(TArcIWShadow);
  FBackgroundImage := TIWFileReference.Create;
  FLabelFont       := TIWFont.Create;
  FNotEditableOffsets := TArcIWNotEditableOffsets.Create;

  FCompTop    := 0;
  FCompLeft   := Trunc(FComp.Width * (0.3));
  FCompWidth  := FComp.Width-FCompLeft;
  FCompHeight := FComp.Height;

  FLabelPosition := lpLeft;

  {$IFDEF INTRAWEB51}
  if FComp.WebApplication <> nil then
  begin
    case FComp.WebApplication.Browser of
      brIE:        FLabelPadding := 4;
      else         FLabelPadding := 2;
    end;
  end;
  {$ELSE}
  FLabelPadding := 4;
  {$ENDIF}

  FLabelAlignment := taRightJustify;

  FBorderStyle := bsNone;
  FBorderColor := clNone;
  FBackgroundColor := clNone;
  Resize;
end;

procedure TArcIWEnhancer.Paint;
  procedure DrawLabel(l,t,r,b : integer);
  var
    x, y, w{, h} : integer;
  begin
    if not FLabelVisible then exit;
    Canvas(FComp).Pen.Color := DesigntimeLabelColor;
    Canvas(FComp).Brush.Color := DesigntimeLabelColor;
    Canvas(FComp).Rectangle(l,t,r,b);

    Canvas(FComp).Font.Assign(FLabelFont);
    w := Canvas(FComp).TextWidth(FLabelCaption);
//    h := Canvas(FComp).TextHeight(FLabelCaption);
    y := t+FLabelPadding;
    case FLabelAlignment of
      taLeftJustify:  x := l;
      taRightJustify: x := r-w;
      taCenter:       x := l+((r-l) div 2)-(w div 2);
      else x:=0;
    end;
    Canvas(FComp).Pen.Color := FLabelFont.Color;
    Canvas(FComp).TextOut(x,y,FLabelCaption);
  end;
  procedure DrawComp;
  begin
    Canvas(FComp).Pen.Color := DesigntimeCompColor;
    Canvas(FComp).Brush.Color := DesigntimeCompColor;
    Canvas(FComp).Rectangle(FCompLeft, FCompTop, FCompLeft+FCompWidth, FCompTop+FCompHeight);
  end;
begin
  if {(not FLoaded) or }(not FDesignTime) then exit;

  if FBackgroundColor = clNone then
    Canvas(FComp).Brush.Color := clDkGray
  else
    Canvas(FComp).Brush.Color := FBackgroundColor;

  if FBorderColor = clNone then
    Canvas(FComp).Pen.Color := Canvas(FComp).Brush.Color
  else
    Canvas(FComp).Pen.Color := FBorderColor;

  if FBorderStyle <> bsNone then
    Canvas(FComp).Pen.Width := FBorderSize
  else
    Canvas(FComp).Pen.Width := 0;

  Canvas(FComp).Rectangle(0,0,FComp.Width,FComp.Height);
  case FLabelPosition of
    lpLeft:
      begin
        DrawLabel((BorderOffsets div 2), (BorderOffsets div 2), FCompLeft, FComp.Height-(BorderOffsets div 2));
        DrawComp;
      end;
    lpRight:
      begin
        DrawComp;
        DrawLabel(FCompWidth, (BorderOffsets div 2), FComp.Width - (BorderOffsets div 2), FComp.Height-(BorderOffsets div 2));
      end;
    lpTop:
      begin
        DrawLabel((BorderOffsets div 2), (BorderOffsets div 2), FCompWidth+(BorderOffsets div 2), FCompTop);
        DrawComp;
      end;
    lpBottom:
      begin
        DrawComp;
        DrawLabel((BorderOffsets div 2), FCompHeight + (BorderOffsets div 2), FComp.Width-(BorderOffsets div 2), FComp.Height-(BorderOffsets div 2));
      end;
  end;
end;

procedure TArcIWEnhancer.SetLabelFont(const Value: TIWFont);
begin
  FLabelFont.Assign(Value);
  Canvas(FComp).Font.Assign(FLabelFont);
  Resize;
end;

procedure TArcIWEnhancer.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TArcIWEnhancer(Dest).LabelCaption := FLabelCaption;
  TArcIWEnhancer(Dest).LabelPosition := FLabelPosition;
  TArcIWEnhancer(Dest).LabelAlignment := FLabelAlignment;
  TArcIWEnhancer(Dest).LabelFont := FLabelFont;
  //TArcIWEnhancer(Dest).LabelShadows := FLabelShadows;
  TArcIWEnhancer(Dest).LabelMarker := FLabelMarker;
  TArcIWEnhancer(Dest).LabelPadding := FLabelPadding;
  TArcIWEnhancer(Dest).LabelTextStyles := FLabelTextStyles;

  TArcIWEnhancer(Dest).BorderStyle := FBorderStyle;
  TArcIWEnhancer(Dest).BorderColor := FBorderColor;
  TArcIWEnhancer(Dest).BackgroundColor := FBackgroundColor;
end;

{procedure TArcIWEnhancer.SetLabelShadows(const Value: TArcIWShadows);
begin
  FLabelShadows.Assign(Value);
end;}

procedure TArcIWEnhancer.SetLabelMarker(const Value: TArcIWMarker);
begin
  FLabelMarker.Assign(Value);
  Resize;
end;

procedure TArcIWEnhancer.SetBackgroundImage(const Value: TIWFileReference);
begin
  FBackgroundImage.Assign(Value);
  Resize;
end;

destructor TArcIWEnhancer.Destroy;
begin
  FCompExtraTagParams.Free;
  FLabelExtraTagParams.Free;
  FNotEditableOffsets.Free;
  FLabelMarker.Free;
  //FLabelShadows.Free;
  FBackgroundImage.Free;
  FLabelFont.Free;
  inherited;
end;

procedure TArcIWEnhancer.Resize;
begin
  if InternalResizing then
    exit;
  if not FDesignTime then
    exit;
  if not FLoaded then
    FLoaded := True;


  case FAutoResize of
    arLabel:
      begin
        case FLabelPosition of
          lpLeft:
            begin
              FCompTop := (BorderOffsets div 2);
              FCompLeft := FComp.Width-(BorderOffsets div 2)-FCompWidth;
              FCompHeight := FComp.Height - (BorderOffsets);
            end;
          lpRight:
            begin
              FCompTop := (BorderOffsets div 2);
              FCompLeft := (BorderOffsets div 2);
              FCompHeight := FComp.Height - (BorderOffsets);
            end;
          lpTop:
            begin
              FCompTop := FComp.Height - FCompHeight - (BorderOffsets div 2);
              FCompLeft := (BorderOffsets div 2);
              FCompWidth := FComp.Width - (BorderOffsets);
            end;
          lpBottom:
            begin
              FCompTop := (BorderOffsets div 2);
              FCompLeft := (BorderOffsets div 2);
              FCompWidth := FComp.Width - (BorderOffsets);
            end;
        end;
      end;
    arComponent:
      begin
        case FLabelPosition of
          lpLeft:
            begin
              FCompWidth := FComp.Width - FCompLeft - (BorderOffsets)
            end;
          lpRight:
            begin
              FCompWidth := FComp.Width - FCompLeft - (BorderOffsets)
            end;
          lpTop:
            begin
              FCompHeight := FComp.Height - FCompTop - (BorderOffsets);
            end;
          lpBottom:
            begin
              FCompHeight := FComp.Height - FCompTop - (BorderOffsets);
            end;
        end;
      end;
  end;
  FComp.Invalidate;
end;

procedure TArcIWEnhancer.SetLabelCaption(const Value: string);
begin
  FLabelCaption := Value;
  Resize;
end;

procedure TArcIWEnhancer.SetLabelPosition(
  const Value: TArcIWLabelPosition);
var
  s : string;
begin
  if FLabelPosition = Value then
    exit;
  InternalResizing := True;
  try
    if FLoaded then
    begin
      s := FLabelCaption;
      if s = '' then
        s := Uppercase(FComp.Name);
      Canvas(FComp).Font.Assign(FLabelFont);
      if (FLabelPosition in [lpLeft, lpRight]) and (Value in [lpTop, lpBottom]) then
      begin
        FComp.Height := FCompHeight+Canvas(FComp).TextHeight(s)+FLabelPadding+BorderOffsets;
        FComp.Width := FCompWidth+BorderOffsets;
        FCompLeft := (BorderOffsets div 2);
        if Value = lpTop then
          FCompTop := FComp.Height-FCompHeight-(BorderOffsets div 2)
        else
          FCompTop := (BorderOffsets div 2);
      end else
      begin
        if (FLabelPosition in [lpTop, lpBottom]) and (Value in [lpLeft, lpRight]) then
        begin
          FComp.Height := FCompHeight+BorderOffsets;
          FComp.Width := FCompWidth+BorderOffsets+Canvas(FComp).TextWidth(s);
          FCompTop := (BorderOffsets div 2);
          if Value = lpLeft then
            FCompLeft := FComp.Width-FCompWidth-(BorderOffsets div 2)
          else
            FCompLeft := (BorderOffsets div 2);
        end else
        begin
          if (FLabelPosition = lpTop) and (value = lpBottom) then
            FCompTop := (BorderOffsets div 2);
          if (FLabelPosition = lpBottom) and (value = lpTop) then
            FCompTop := FComp.Height-FCompHeight-(BorderOffsets div 2);

          if (FLabelPosition = lpLeft) and (value = lpRight) then
            FCompLeft := (BorderOffsets div 2);
          if (FLabelPosition = lpRight) and (value = lpLeft) then
            FCompLeft := FComp.Width-FCompWidth-(BorderOffsets div 2);
        end;
      end;
    end;
  finally
    InternalResizing := False;
  end;
  FLabelPosition := Value;
  FComp.Invalidate;
end;

procedure TArcIWEnhancer.Loaded;
begin
  FLoaded := True;
  Resize;
end;

procedure TArcIWEnhancer.SetCompHeight(const Value: integer);
begin
  if (not FLoaded) or (not FDesignTime) then
  begin
    FCompHeight := Value;
    exit;
  end;

  if (not FLabelVisible) or (FLabelPosition in [lpLeft, lpRight]) then
  begin
    FCompHeight := FComp.Height;
    FCompTop := 0;
  end else
  begin
    InternalResizing := True;
    try
      if Value < 0 then
        exit;

      if Value > FComp.Height then
        exit;

      case FLabelPosition of
        lpTop:
          begin
            FCompTop := FComp.Height-Value-(BorderOffsets div 2);
            FCompHeight := Value;
          end;
        lpBottom:
          begin
            FCompTop := (BorderOffsets div 2);
            FCompHeight := Value;
          end;
      end;

      if FCompHeight > FComp.Height-BorderOffsets then
      begin
        FComp.Height := FCompHeight+BorderOffsets;
        FCompTop := FComp.Height-(BorderOffsets div 2) -FCompHeight;
      end;
    finally
      InternalResizing := False;
    end;
  end;
  FComp.Invalidate;
end;

procedure TArcIWEnhancer.SetCompLeft(const Value: integer);
begin
  if (not FLoaded) or (not FDesignTime) then
  begin
    FCompLeft := Value;
    exit;
  end;

  if (not FLabelVisible) or (FLabelPosition in [lpRight, lpTop, lpBottom]) then
  begin
    FCompLeft := 0;
    FCompWidth := FComp.Width;
  end else
  begin
    InternalResizing := True;
    try
      if Value < 0 then
        exit;

      if Value > FComp.Width then
        exit;

      FCompLeft := Value;
      FCompWidth := FComp.Width-FCompLeft-(BorderOffsets div 2);
    finally
      InternalResizing := False;
    end;
  end;
  FComp.Invalidate;
end;

procedure TArcIWEnhancer.SetCompTop(const Value: integer);
begin
  if (not FLoaded) or (not FDesignTime) then
  begin
    FCompTop := Value;
    exit;
  end;

  if (not FLabelVisible) or (FLabelPosition in [lpLeft, lpRight, lpTop]) then
  begin
    FCompTop := 0;
    FCompHeight := FComp.Height;
  end else
  begin
    InternalResizing := True;
    try
      if Value < 0 then
        exit;

      if Value > FComp.Width then
        exit;

      FCompTop := Value;
      FCompHeight := FComp.Height-FCompTop-(BorderOffsets div 2);
    finally
      InternalResizing := False;
    end;
  end;
  FComp.Invalidate;
end;

procedure TArcIWEnhancer.SetCompWidths(const Value: integer);
begin
  if (not FLoaded) or (not FDesignTime) then
  begin
    FCompWidth := Value;
    exit;
  end;

  if (not FLabelVisible) or (FLabelPosition in [lpTop, lpBottom]) then
  begin
    FCompWidth := FComp.Width;
    FCompLeft := 0;
  end else
  begin
    InternalResizing := True;
    try
      if Value < 0 then
        exit;

      if Value > FComp.Width then
        exit;

      case FLabelPosition of
        lpLeft:
          begin
            FCompLeft := FComp.Width - Value - (BorderOffsets div 2);
            FCompWidth := Value;
          end;
        lpRight:
          begin
            FCompLeft := (BorderOffsets div 2);
            FCompWidth := Value;
          end;
      end;

      if FCompWidth > FComp.Width-BorderOffsets then
      begin
        FComp.Width := FCompWidth+BorderOffsets;
        FCompLeft := FComp.Width-(BorderOffsets div 2) -FCompWidth;
      end;
    finally
      InternalResizing := False;
    end;
  end;
  FComp.Invalidate;
end;

procedure TArcIWEnhancer.SetLabelAlignment(const Value: TAlignment);
begin
  FLabelAlignment := Value;
  Resize;
end;

procedure TArcIWEnhancer.SetBackgroundColor(const Value: TIWColor);
begin
  FBackgroundColor := Value;
  Resize;
end;

procedure TArcIWEnhancer.SetBackgroundFixed(const Value: boolean);
begin
  FBackgroundFixed := Value;
  Resize;
end;

procedure TArcIWEnhancer.SetBorderColor(const Value: TIWColor);
begin
  FBorderColor := Value;
  Resize;
end;

procedure TArcIWEnhancer.SetBorderPadding(const Value: integer);
begin
  FBorderPadding := Value;
  Resize;
end;

procedure TArcIWEnhancer.SetBorderSize(const Value: integer);
begin
  FBorderSize := Value;
  Resize;
end;

procedure TArcIWEnhancer.SetBorderStyle(const Value: TArcIWBorderStyle);
begin
  FBorderStyle := Value;
  Resize;
end;

procedure TArcIWEnhancer.SetLabelPadding(const Value: integer);
begin
  FLabelPadding := Value;
  Resize;
end;

procedure TArcIWEnhancer.SetLabelTextStyles(const Value: TArcIWTextStyles);
begin
  FLabelTextStyles := Value;
  Resize;
end;

function TArcIWEnhancer.BorderOffsets: integer;
begin
  Result := (FBorderSize*2)+(FBorderPadding*2);
end;

function AdjustColorBy(Darken : boolean; color : TColor; count : byte) : TColor;
var
  s, s1, s2, s3, sColor : string;
  i1, i2, i3 : integer;
begin
  s := '$'+IntToHex(count,2);
  sColor := IntToHex(ColorToRGB(color),6);

  if Darken then
  begin
    i1 := StrToInt('$'+Copy(sColor,1,2))-StrToInt(s);
    i2 := StrToInt('$'+Copy(sColor,3,2))-StrToInt(s);
    i3 := StrToInt('$'+Copy(sColor,5,2))-StrToInt(s);
  end else
  begin
    i1 := StrToInt('$'+Copy(sColor,1,2))+StrToInt(s);
    i2 := StrToInt('$'+Copy(sColor,3,2))+StrToInt(s);
    i3 := StrToInt('$'+Copy(sColor,5,2))+StrToInt(s);
  end;

  if i1 < 0 then i1 := 0;
  if i2 < 0 then i2 := 0;
  if i3 < 0 then i3 := 0;
  if i1 > 255 then i1 := 255;
  if i2 > 255 then i2 := 255;
  if i3 > 255 then i3 := 255;

  s1 := IntToHex(i1,2);
  s2 := IntToHex(i2,2);
  s3 := IntToHex(i3,2);
  result := StrToInt('$'+s1+s2+s3);
end;

function TArcIWEnhancer.DesigntimeCompColor: TColor;
begin
  if FBackgroundColor <> clNone then
    Result := AdjustColorBy(True, FBackgroundColor, 75)
  else
    Result := clGray;
end;

function TArcIWEnhancer.DesigntimeLabelColor: TColor;
begin
  if FBackgroundColor <> clNone then
    Result := AdjustColorBy(True, FBackgroundColor, 10)
  else
    Result := clLtGray;
end;

procedure TArcIWEnhancer.SetNotEditableOffsets(const Value: TArcIWNotEditableOffsets);
begin
  FNotEditableOffsets.Assign(Value);
  Resize;
end;

procedure TArcIWEnhancer.SetLabelVisible(const Value: boolean);
begin
  if FDesignTime and (not FLabelVisible) and Value then
  begin
    case FLabelPosition of
      lpLeft, lpRight:
        begin
          FCompLeft := FComp.Width div 33;
          FCompWidth := FComp.Width-FCompLeft;
        end;
      lpTop, lpBottom:
        begin
          FCompTop := FComp.Height div 33;
          FCompHeight := FComp.Height-FCompTop;
        end;
    end;
  end;
  if FDesignTime and (FLabelVisible) and (not Value) then
  begin
    FCompLeft := 0;
    FCompWidth := FComp.Width;
    FCompTop := 0;
    FCompHeight := FComp.Height;
  end;
  FLabelVisible := Value;
  Resize;
end;

{ TArcIWMarker }

procedure TArcIWMarker.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TArcIWMarker(Dest).MarkerType := FMarkerType;
  TArcIWMarker(Dest).MarkerFile := FMarkerFile;
end;

constructor TArcIWMarker.Create;
begin
  inherited Create;
  FMarkerFile := TIWFileReference.Create;
end;

destructor TArcIWMarker.Destroy;
begin
  FMarkerFile.Free;
  inherited;
end;

function TArcIWMarker.RenderStyle(aURLBase : string) : string;
var
  sURL : string;
begin
  Result := '';
  if (FMarkerType = lmNone) then exit;

  Result := 'list-style-position: outside; list-style-type: ';

  case FMarkerType of
    lmDisc:       Result := Result+'disc;';
    lmCircle:     Result := Result+'circle;';
    lmSquare:     Result := Result+'square;';
    lmDecimal:    Result := Result+'decimal;';
    lmLowerRoman: Result := Result+'lower-roman;';
    lmUpperRoman: Result := Result+'upper-roman;';
    lmLowerAlpha: Result := Result+'lower-alpha;';
    lmUpperAlpha: Result := Result+'upper-alpha;';
  end;
  sURL := FMarkerFile.Location(aURLBase);
  if sURL <> '' then
    Result := Result+'list-style-image: url('+sURL+')';
end;

procedure TArcIWMarker.SetMarkerFile(const Value: TIWFileReference);
begin
  FMarkerFile.Assign(Value);
end;

(*
{ TArcIWShadow }

function TArcIWShadow.RenderStyle: string;
begin
  result := '';
  if FColor <> clNone then
    Result := Result+TIWCustomControl.ColorToRGBString(FColor)+' ';
  Result := Result+IntToStr(FxOffset)+'px '+IntToStr(FyOffset)+'px '+IntToStr(FBlurRadius)+'px';
end;
*)

{ TArcIWNotEditableOffsets }

procedure TArcIWNotEditableOffsets.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TArcIWNotEditableOffsets(Dest).Opera := FOpera;
  TArcIWNotEditableOffsets(Dest).Other := FOther;
  TArcIWNotEditableOffsets(Dest).IE_XP := FIE_XP;
  TArcIWNotEditableOffsets(Dest).IE_Win := FIE_Win;
  TArcIWNotEditableOffsets(Dest).IE_Mac := FIE_Mac;
  TArcIWNotEditableOffsets(Dest).NS6 := FNS6;
end;

constructor TArcIWNotEditableOffsets.Create;
begin
  inherited;
  FOpera := 2;
  FOther := 4;
  FIE_XP := 4;
  FIE_Win := 4;
  FIE_Mac := 4;
  FNS6 := 4;
end;

initialization
{$IFDEF EVAL}
   GLicense.ThirdPartyHook('Arcana Enhanced Controls Evaluation',True);
{$ENDIF}

end.
