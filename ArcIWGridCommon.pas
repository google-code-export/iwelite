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

unit ArcIWGridCommon;

interface

{$I IntraWebVersion.inc}
{$I Eval.inc}

uses SysUtils, Classes, Graphics, IWColor, IWFont, IWTypes, IWKlooch
     {$IFDEF INTRAWEB72},IWFileReference, IWRenderContext{$ENDIF};

type
  TArcScrollbarStyle = (scrNone, scrHorizontal, scrVertical, scrBoth);
  TArcIconAlign = (iaLeft, iaRight);
  TArcControlAlign = (caNoText, caLeft, caRight);
  TArcVertAlign = (vaDefault, vaTop, vaMiddle, vaBottom);

  TCaptionButtonType = (cbtFirstPage, cbtPriorPage, cbtNextPage, cbtLastPage, cbtFirst, cbtPrior, cbtNext,
    cbtLast, cbtNew, cbtEdit, cbtDelete, cbtRefresh, cbtSave, cbtCancel);

  TBorderStyle = (brdNone, brdHidden, brdDashed, brdDotted, brdDouble, brdGroove, brdInset, brdOutset, brdRidge, brdSolid);
  TBorderSide = (brsTop, brsBottom, brsLeft, brsRight);
  TBorderSides = set of TBorderSide;

  TArcIWGraphic = class(TPersistent)
  private
    FHeight: integer;
    FWidth: integer;
    FURL: TIWFileReference;
    FDefaultURL : string;
  public
    constructor Create(DefaultURL : string); virtual;
    destructor Destroy; override;
    function RenderTag(const URLBase, Attributes : string) : string;
  published
    property URL : TIWFileReference read FURL write FURL;
    property Width : integer read FWidth write FWidth;
    property Height : integer read FHeight write FHeight;
  end;

  TArcBorderStyle = class(TPersistent)
  private
    FWidth: integer;
    FStyle: TBorderStyle;
    FColor: TIWColor;
    FSides: TBorderSides;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    {$IFNDEF INTRAWEB72}
    function RenderCSS : string;
    {$ELSE}
    function RenderCSS(AContext: TIWBaseHTMLComponentContext) : string;
    {$ENDIF}
    constructor Create(Top, Bottom, Left, Right : boolean); virtual;
  published
    property Style : TBorderStyle read FStyle write FStyle;
    property Width : integer read FWidth write FWidth;
    property Color : TIWColor read FColor write FColor;
    property Sides : TBorderSides read FSides write FSides;
  end;

  TArcGridStyle = class(TPersistent)
  private
    FBorderStyle: TArcBorderStyle;
    FBackgroundColor: TIWColor;
    {$IFNDEF INTRAWEB72}
    FBackgroundImageURL : string;
    {$ELSE}
    FBackgroundImage: TIWFileReference;
    {$ENDIF}
    FFont: TIWFont;
    FMargin: integer;
    FPadding: integer;
    FTextAlign : TAlignment;
    FTextVertAlign : TArcVertAlign;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    {$IFNDEF INTRAWEB72}
    function RenderCSS(Browser : TIWBrowser) : string;
    {$ELSE}
    function RenderCSS(AContext: TIWBaseHTMLComponentContext) : string;
    {$ENDIF}
    constructor Create(const Top, Bottom, Left, Right : boolean); virtual;
    destructor Destroy; override;
    function MinHeight(RowHeight : integer) : integer; virtual;
  published
    property TextAlign : TAlignment read FTextAlign write FTextAlign;
    property TextVertAlign : TArcVertAlign read FTextVertAlign write FTextVertAlign;
    property BorderStyle : TArcBorderStyle read FBorderStyle write FBorderStyle;
    property BackgroundColor : TIWColor read FBackgroundColor write FBackgroundColor;
    {$IFNDEF INTRAWEB72}
    property BackgroundImageURL : string read FBackgroundImageURL write FBackgroundImageURL;
    {$ELSE}
    property BackgroundImage : TIWFileReference read FBackgroundImage write FBackgroundImage;
    {$ENDIF}
    property Font : TIWFont read FFont write FFont;
    property Margin : integer read FMargin write FMargin;
    property Padding : integer read FPadding write FPadding;
  end;

{$IFNDEF INTRAWEB72}
function ColorToRGBString(AColor : TIWColor) : string;
{$ENDIF}

function AlignmentToStr(Align : TAlignment) : string;
function VertAlignmentToStr(Align : TArcVertAlign) : string;

implementation

function AlignmentToStr(Align : TAlignment) : string;
begin
  case Align of
    taLeftJustify:   result := 'text-align: left;';
    taRightJustify:  result := 'text-align: right;';
    taCenter:        result := 'text-align: center;';
  end;
end;

function VertAlignmentToStr(Align : TArcVertAlign) : string;
begin
  case Align of
    vaDefault: result := '';
    vaTop:     result := 'vertical-align: top;';
    vaMiddle:  result := 'vertical-align: middle;';
    vaBottom:  result := 'vertical-align: bottom;';
  end;
end;

{$IFNDEF INTRAWEB72}

function toTColor(AColor: TIWColor): TColor;
{$IFDEF VSNET}
Var
  i: Integer;
{$ENDIF}
begin
  {$IFDEF VSNET}
  if AColor.IsEmpty then begin
     result := clNone;
  end else begin
    if (AColor = System.Drawing.Color.Empty) or
       (AColor = System.Drawing.Color.Transparent) then
    begin
      Result := clNone;
      Exit;
    end;

    if AColor.IsSystemColor then begin
      if not IdentToColor('cl' + AColor.Name, i) then begin
        i := RGB(AColor.R, AColor.G, AColor.B);// ColorTranslator.ToWin32(AColor);
      end;
      result := i;
    end else begin
      // if AColor.IsKnownColor then begin
        result := RGB(AColor.R, AColor.G, AColor.B);// ColorTranslator.ToWin32(AColor);
      {end else begin
        result := ColorTranslator.ToWin32(AColor);
      end;}
   end;
  end;
  {$ELSE}
  result := AColor;
  {$ENDIF}
end;

function ColorToRGBString(AColor : TIWColor) : string;
begin
  Result := '';
  if toTColor(AColor) = TColor(-1) then begin
    Result := 'transparent';
  end else begin
    if toTColor(AColor) <> clNone then begin
      Result := IntToHex(ColorToRGB(toTColor(AColor)), 6);
      Result := '#' + Result[5] + Result[6] + Result[3] + Result[4] + Result[1] + Result[2];
    end;
  end;
end;
{$ENDIF}

{ TArcBorderStyle }

procedure TArcBorderStyle.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TArcBorderStyle(Dest).FWidth := FWidth;
  TArcBorderStyle(Dest).FStyle := FStyle;
  TArcBorderStyle(Dest).FColor := FColor;
  TArcBorderStyle(Dest).FSides := FSides;
end;

constructor TArcBorderStyle.Create(Top, Bottom, Left, Right : boolean);
begin
  inherited Create;
  FWidth := 1;
  FStyle := brdNone;
  FColor := clNone;
  FSides := [];
  if Top then Include(FSides,brsTop);
  if Bottom then Include(FSides,brsBottom);
  if Left then Include(FSides,brsLeft);
  if Right then Include(FSides,brsRight);
end;

{$IFNDEF INTRAWEB72}
function TArcBorderStyle.RenderCSS : string;
{$ELSE}
function TArcBorderStyle.RenderCSS(AContext: TIWBaseHTMLComponentContext) : string;
{$ENDIF}
var
  sStyle : string;
begin
  if FColor <> clNone then
  begin
    case FStyle of
      brdNone:   sStyle := 'none';
      brdHidden: sStyle := 'hidden';
      brdDashed: sStyle := 'dashed';
      brdDotted: sStyle := 'dotted';
      brdDouble: sStyle := 'double';
      brdGroove: sStyle := 'groove';
      brdInset:  sStyle := 'inset';
      brdOutset: sStyle := 'outset';
      brdRidge:  sStyle := 'ridge';
      brdSolid:  sStyle := 'solid';
    end;
    if not ( (brsTop in FSides) and (brsBottom in FSides) and
             (brsLeft in FSides) and (brsRight in FSides)
           ) then
    begin
      Result := '';
      if brsTop in FSides then
        result := result+'border-top: '+sStyle+' '+IntToStr(FWidth)+'px '+ColorToRGBString(FColor)+';'
      else
        result := result+'border-top: none;';

      if brsBottom in FSides  then
        result := result+'border-bottom: '+sStyle+' '+IntToStr(FWidth)+'px '+ColorToRGBString(FColor)+';'
      else
        result := result+'border-bottom: none;';

      if brsLeft in FSides  then
        result := result+'border-left: '+sStyle+' '+IntToStr(FWidth)+'px '+ColorToRGBString(FColor)+';'
      else
        result := result+'border-left: none;';

      if brsRight in FSides  then
        result := result+'border-right: '+sStyle+' '+IntToStr(FWidth)+'px '+ColorToRGBString(FColor)+';'
      else
        result := result+'border-right: none;';
    end else
      result := 'border: '+sStyle+' '+IntToStr(FWidth)+'px '+ColorToRGBString(FColor)+';';
  end else
    result := 'border: none;';
end;

{ TArcGridStyle }

procedure TArcGridStyle.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TArcGridStyle(Dest).FBorderStyle.Assign(FBorderStyle);
  TArcGridStyle(Dest).FBackgroundColor := FBackgroundColor;
  {$IFNDEF INTRAWEB72}
  TArcGridStyle(Dest).FBackgroundImageURL := FBackgroundImageURL;
  {$ELSE}
  TArcGridStyle(Dest).FBackgroundImage.Assign(FBackgroundImage);
  {$ENDIF}
  TArcGridStyle(Dest).FFont.Assign(FFont);
  TArcGridStyle(Dest).FMargin := FMargin;
  TArcGridStyle(Dest).FPadding := FPadding;
  TArcGridStyle(Dest).FTextAlign     := FTextAlign;
  TArcGridStyle(Dest).FTextVertAlign := FTextVertAlign;
end;

constructor TArcGridStyle.Create(const Top, Bottom, Left, Right : boolean);
begin
  inherited Create;
  FFont := TIWFont.Create;
  {$IFDEF INTRAWEB72}
  FBackgroundImage := TIWFileReference.Create;
  {$ENDIF}
  FBorderStyle := TArcBorderStyle.Create(Top, Bottom, Left, Right);
  FBackgroundColor := clNone;
  FMargin := 0;
  FPadding := 0;
  FTextAlign := taLeftJustify;
  FTextVertAlign := vaTop;
end;

destructor TArcGridStyle.Destroy;
begin
  FBorderStyle.Free;
  FFont.Free;
  {$IFDEF INTRAWEB72}
  FBackgroundImage.Free;
  {$ENDIF}
  inherited;
end;

function TArcGridStyle.MinHeight(RowHeight : integer) : integer;
  function GetFontSize : integer;
  begin
    Result := FFont.Size;
    if Result = 0 then
      Result := 10;
  end;
begin
  Result := ((FBorderStyle.Width*2)+(Padding*2));
  if RowHeight <= 0 then
    Result := Result + Round(GetFontSize*1.4)
  else
    Result := Result + RowHeight;
end;

{$IFNDEF INTRAWEB72}
function TArcGridStyle.RenderCSS(Browser : TIWBrowser) : string;
{$ELSE}
function TArcGridStyle.RenderCSS(AContext: TIWBaseHTMLComponentContext) : string;
{$ENDIF}
begin

  result := FBorderStyle.RenderCSS{$IFDEF INTRAWEB72}(AContext){$ENDIF}+
            'margin: '+IntToStr(FMargin)+'px;'+
            'padding: '+IntToStr(FPadding)+'px;'+
            AlignmentToStr(FTextAlign)+
            VertAlignmentToStr(FTextVertAlign)+
            FFont.FontToStringStyle({$IFDEF INTRAWEB72}AContext.{$ENDIF}Browser)+';';
  if FBackgroundColor <> clWebTransparent then
    Result := Result+
            'background-color: '+{$IFNDEF INTRAWEB72}IWColorToString(FBackgroundColor){$ELSE}ColorToRGBString(FBackgroundColor){$ENDIF}+';';

  if ({$IFDEF INTRAWEB72}FBackgroundImage.URL{$ELSE}FBackgroundImageURL{$ENDIF}<>'') {$IFDEF INTRAWEB72}or (FBackgroundImage.Filename <> '') {$ENDIF} then
    Result := Result+'background-image: url('+{$IFDEF INTRAWEB72}FBackgroundImage.Location(AContext.WebApplication.AppURLBase){$ELSE}FBackgroundImageURL{$ENDIF}+');'
end;

{ TArcIWGraphic }

constructor TArcIWGraphic.Create(DefaultURL : string);
begin
  inherited Create;
  FURL := TIWFileReference.Create;
  FDefaultURL := DefaultURL;
  FWidth := 0;
  FHeight := 0;
end;

destructor TArcIWGraphic.Destroy;
begin
  FURL.Free;
  inherited;
end;

function TArcIWGraphic.RenderTag(const URLBase, Attributes: string): string;
var
  s : string;
begin
  Result := '';
  s :=FURL.Location(URLBase);
  if s = '' then
    s := FDefaultURL;
  if s <> '' then
  begin
    Result := '<img src="'+s+'" ';
    if Width <> 0 then
      Result := Result+'width="'+IntToStr(Width)+'" ';
    if Height <> 0 then
      Result := Result+'height="'+IntToStr(Height)+'" ';
    Result := Result+Attributes+'>';
  end;
end;

initialization
{$IFDEF EVAL}
   GLicense.ThirdPartyHook('Arcana Grids Evaluation',True);
{$ENDIF}

end.
