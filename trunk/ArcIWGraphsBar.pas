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

unit ArcIWGraphsBar;

interface

{$I IntrawebVersion.inc}

uses SysUtils, Classes, {$IFNDEF VER130}Types, {$ENDIF}Graphics, IWColor, IWFont,
  IWControl, IWTypes, IWHTMLTag
  {$IFNDEF INTRAWEB51}, IWRenderContext {$ENDIF}, ArcCommon;

type
  TArcIWBorderStyle = (bsNone, bsLowered, bsRaised, bsSolid, bsRidge);

  TArcIWGraphAxis = class(TPersistent)
  private
    FShowValues: boolean;
    FShowLines: boolean;
    FWidth: integer;
    FMaxValue: real;
    FMinValue: real;
    FCaption: string;
    FValueMask: string;
    FFont: TIWFont;
    FComp : TIWControl;
    FLineColor: TIWColor;
    FLineCount: integer;
    FRoundValue: boolean;
    procedure SetFont(const Value: TIWFont);
    procedure SetCaption(const Value: string);
    procedure SetLineColor(const Value: TIWColor);
    procedure SetMaxValue(const Value: real);
    procedure SetMinValue(const Value: real);
    procedure SetShowLines(const Value: boolean);
    procedure SetShowValues(const Value: boolean);
    procedure SetValueMask(const Value: string);
    procedure SetWidth(const Value: integer);
    procedure SetLineCount(const Value: integer);
    procedure SetRoundValue(const Value: boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Comp : TIWControl); virtual;
    destructor Destroy; override;
  published
    property Caption : string read FCaption write SetCaption;
    property Font : TIWFont read FFont write FFont;
    property MinValue : real read FMinValue write SetMinValue;
    property MaxValue : real read FMaxValue write SetMaxValue;
    property LineColor : TIWColor read FLineColor write SetLineColor;
    property ShowLines : boolean read FShowLines write SetShowLines;
    property LineCount : integer read FLineCount write SetLineCount;
    property ShowValues : boolean read FShowValues write SetShowValues;
    property ValueMask : string read FValueMask write SetValueMask;
    property RoundValue : boolean read FRoundValue write SetRoundValue;
    property Width : integer read FWidth write SetWidth;
  end;

  TArcIWGraphBar = class(TCollectionItem)
  private
    FShowValue: boolean;
    FBorderSize: integer;
    FValue: real;
    FCaption: string;
    FBorderStyle: TArcIWBorderStyle;
    FBorderColor: TIWColor;
    FColor: TIWColor;
    FShowValueHint: boolean;
    FValueHintFormat: string;
    procedure SetBorderColor(const Value: TIWColor);
    procedure SetBorderSize(const Value: integer);
    procedure SetBorderStyle(const Value: TArcIWBorderStyle);
    procedure SetCaption(const Value: string);
    procedure SetColor(const Value: TIWColor);
    procedure SetShowValue(const Value: boolean);
    procedure SetValue(const Value: real);
  protected
    function CalcValueHint : string; virtual;
  public
    constructor Create( Collection : TCollection) ; override;
    destructor Destroy; override;
    function Top : integer;
  published
    property Value : real read FValue write SetValue;
    property ShowValue : boolean read FShowValue write SetShowValue;
    property ShowValueHint : boolean read FShowValueHint write FShowValueHint;
    property ValueHintFormat : string read FValueHintFormat write FValueHintFormat;
    property Color : TIWColor read FColor write SetColor;
    property Caption : string read FCaption write SetCaption;
    property BorderColor : TIWColor read FBorderColor write SetBorderColor;
    property BorderSize : integer read FBorderSize write SetBorderSize;
    property BorderStyle : TArcIWBorderStyle read FBorderStyle write SetBorderStyle;
  end;

  TArcIWGraphBars = class(TCollection)
  private
    FComp : TIWControl;
    function GetBars(idx: integer): TArcIWGraphBar;
    procedure SetBars(idx: integer; const Value: TArcIWGraphBar);
  public
    constructor Create(Comp : TIWControl); virtual;
    property Bars[idx : integer] : TArcIWGraphBar read GetBars write SetBars; default;
    function Render(GraphTag : TIWHtmlTag; GraphWidth : integer) : string;
  end;

  TArcIWGraphBackground = class(TPersistent)
  private
    FBorderSize: integer;
    FBorderStyle: TArcIWBorderStyle;
    FGraphColor: TIWColor;
    FBorderColor: TIWColor;
    FFont: TIWFont;
    FComp : TIWControl;
    FAxisColor: TIWColor;
    procedure SetFont(const Value: TIWFont);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Comp : TIWControl); virtual;
    destructor Destroy; override;
  published
    property GraphColor : TIWColor read FGraphColor write FGraphColor;
    property AxisColor: TIWColor read FAxisColor write FAxisColor;
    property BorderColor : TIWColor read FBorderColor write FBorderColor;
    property BorderSize : integer read FBorderSize write FBorderSize;
    property BorderStyle : TArcIWBorderStyle read FBorderStyle write FBorderStyle;
    property Font : TIWFont read FFont write SetFont;
  end;

  TArcIWGraphCaption = class(TPersistent)
  private
    FCaption: string;
    FSubCaption: string;
    FFont: TIWFont;
    FComp : TIWControl;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Comp : TIWControl); virtual;
    destructor Destroy; override;
  published
    property Caption : string read FCaption write FCaption;
    property Font : TIWFont read FFont write FFont;
    property SubCaption : string read FSubCaption write FSubCaption;
  end;

  TArcIWGraphOrientation = (goVertical, goHorizontal);
  TArcIWBarGraph = class(TIWControl)
  private
    FAxis: TArcIWGraphAxis;
    FBackground: TArcIWGraphBackground;
    FCaption: TArcIWGraphCaption;
    FOrientation: TArcIWGraphOrientation;
    FBars: TArcIWGraphBars;
    FOnClick : TNotifyEvent;
    procedure SetAxis(const Value: TArcIWGraphAxis);
    procedure SetBackground(const Value: TArcIWGraphBackground);
    procedure SetBars(const Value: TArcIWGraphBars);
    procedure SetCaption(const Value: TArcIWGraphCaption);
    procedure SetOrientation(const Value: TArcIWGraphOrientation);
  protected
    procedure Resize; override;
    {$IFDEF INTRAWEB51}
    procedure Paint; override;
    {$ELSE}
    procedure IWPaint; override;
    {$ENDIF}
    procedure AssignTo(Dest: TPersistent); override;
    // For future use
    property Orientation : TArcIWGraphOrientation read FOrientation write SetOrientation;
    procedure Submit(const AValue: String); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML : TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext) : TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    procedure ReadjustMaxValue(MinimumMaxValue : real; OverFactor : real = 0);
  published
    property Axis : TArcIWGraphAxis read FAxis write SetAxis;
    property Bars : TArcIWGraphBars read FBars write SetBars;
    property Background : TArcIWGraphBackground read FBackground write SetBackground;
    property Caption : TArcIWGraphCaption read FCaption write SetCaption;
    property OnClick : TNotifyEvent read FOnClick write FOnClick;
  end;

implementation

uses {$IFNDEF VER130}StrUtils, {$ENDIF}IWBaseControl, Math;

{$IFDEF VER130}

function IfThen(b : boolean; val1, val2 : string) : string;
begin
  if b then Result := val1 else Result := val2;
end;

{$ENDIF}

function RenderBorderStyle( bs : TArcIWBorderStyle) : string;
begin
  case bs of
    bsNone:    Result := 'none';
    bsLowered: Result := 'inset';
    bsRaised:  Result := 'outset';
    bsSolid:   Result := 'solid';
    bsRidge:   Result := 'ridge';
  end;
end;

{ TArcIWBarGraph }

procedure TArcIWBarGraph.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TArcIWBarGraph(Dest).Axis           := Axis;
  TArcIWBarGraph(Dest).Background     := Background;
  TArcIWBarGraph(Dest).Caption        := Caption;
  TArcIWBarGraph(Dest).Orientation    := Orientation;
  TArcIWBarGraph(Dest).Bars           := Bars;
end;

constructor TArcIWBarGraph.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Width := 400;
  Height := 300;
  FAxis := TArcIWGraphAxis.Create(Self);
  FBackground := TArcIWGraphBackground.Create(Self);
  FCaption := TArcIWGraphCaption.Create(Self);
  FOrientation := goVertical;
  FBars := TArcIWGraphBars.Create(Self);
end;

destructor TArcIWBarGraph.Destroy;
begin
  FAxis.Free;
  FBackground.Free;
  FCaption.Free;
  FBars.Free;
  inherited;
end;

{$IFDEF INTRAWEB51}
procedure TArcIWBarGraph.Paint;
{$ELSE}
procedure TArcIWBarGraph.IWPaint;
{$ENDIF}
  procedure DoLines;
  var
    s  : string;
    i, iPerc, iW, iX, iH  : integer;
    f, fPerc : real;
  begin
    case FOrientation of
      goVertical:   iPerc := Height div FAxis.LineCount;
      goHorizontal: iPerc := Width div FAxis.LineCount;
    end;

    if iPerc <= 0 then
      iPerc := 10;

    Canvas.Font.Assign(FAxis.FFont);
    Canvas.Pen.Color := FAxis.LineColor;
    Canvas.Pen.Width := 1;
    Canvas.Brush.Color := FBackground.AxisColor;

    case FOrientation of
      goVertical:
        begin
          i := Height;
          while i >= 0 do
          begin
            dec(i, iPerc);

            if (i > (FAxis.Font.Size * 1.5)) and FAxis.ShowValues then
            begin
              try
                fPerc :=  1-(i / Height);
                f := ((FAxis.MaxValue-FAxis.MinValue) * fPerc)+FAxis.MinValue;
                if FAxis.RoundValue then
                begin
                  if FAxis.ValueMask = '' then
                    s := IntToStr(Round(f))
                  else
                    s := Format( FAxis.ValueMask,[Round(f)]);
                end else
                begin
                  if FAxis.ValueMask = '' then
                    s := FloatToStr(f)
                  else
                    s := Format( FAxis.ValueMask,[f]);
                end;
              except
                s := 'Format Error';
              end;
              iW := Canvas.TextWidth(s);

              case FOrientation of
                goVertical:
                  begin
                    Canvas.TextOut(FAxis.Width-iW-4,i-( (Round(FAxis.Font.Size * 1.5)) div 2) ,s);
                  end;
                goHorizontal:
                  begin
                    Canvas.TextOut(i -(iW div 2),Height-FAxis.Width +4,s);
                  end;
              end;
            end;
            if FAxis.ShowLines then
            begin
              case FOrientation of
                goVertical:
                  begin
                    Canvas.MoveTo(FAxis.Width+FBackground.BorderSize,i);
                    Canvas.LineTo(Width-FBackground.BorderSize,i);
                  end;
                goHorizontal:
                  begin
                    Canvas.MoveTo(i,FBackground.BorderSize);
                    Canvas.LineTo(i,Height-FAxis.Width);
                  end;
              end;
            end;
          end;
        end;
      goHorizontal:
        begin
        end;
    end;
  end;
var
  i, iBarWidth, iSpacerWidth, iW, iX, iY, iLeft : integer;
  s : string;
  iGraphWidth : integer;
begin
  if not (csDesigning in ComponentState) then
    exit;
  if FBackground.AxisColor <> clNone then
  begin
    Canvas.Brush.Color := FBackground.AxisColor;
    Canvas.Pen.Color := FBackground.AxisColor;
  end else
  begin
    Canvas.Brush.Color := clWhite;
    Canvas.Pen.Color := clWhite;
  end;

  Canvas.FillRect(Rect(0,0,Width,Height));

  Canvas.Font.Assign(FAxis.Font);
  Canvas.Pen.Color := FAxis.Font.Color;

  iW := Canvas.TextWidth(FAxis.Caption);

  case FOrientation of
    goVertical:
      begin
        iX := (FAxis.Width div 2) - (iW div 2);
        iY := 0; 
      end;
    goHorizontal:
      begin
        iX := (Width div 2) - (iW div 2);
        iY := Height-Round(FAxis.Font.Size * 1.5);
      end;
  end;
  Canvas.TextOut(iX,iY,FAxis.Caption);


  if FBackground.GraphColor <> clNone then
  begin
    Canvas.Brush.Color := FBackground.GraphColor;
    Canvas.Pen.Color := FBackground.GraphColor;
  end else
  begin
    Canvas.Brush.Color := clWhite;
    Canvas.Pen.Color := clWhite;
  end;

  if FBackground.BorderColor <> clNone then
    Canvas.Pen.Color := FBackground.BorderColor;
  Canvas.Pen.Width := FBackground.BorderSize;

  case FOrientation of
    goVertical:
      begin
        Canvas.Rectangle(FAxis.Width,0,Width,height);
        DoLines;

        iGraphWidth := Width - FAxis.Width;
        iBarWidth := (Width - FAxis.Width) div ((FBars.Count*2) + 1);
        iSpacerWidth := Round((Width - FAxis.Width) / ((FBars.Count*2) + 1));
        iLeft := FAxis.Width;
        for i := 0 to FBars.Count -1 do
        begin
          iLeft := iLeft + iSpacerWidth;
          if FBars[i].Color = clNone then
          begin
            Canvas.Pen.Color := clWhite;
            Canvas.Brush.Color := clWhite;
          end else
          begin
            Canvas.Pen.Color := FBars[i].Color;
            Canvas.Brush.Color := FBars[i].Color;
          end;
          if FBars[i].BorderColor <> clNone then
            Canvas.Pen.Color := FBars[i].BorderColor;
          Canvas.Rectangle(iLeft,Height-1,iLeft+iBarWidth,FBars[i].Top);
          Canvas.Font.Assign(FAxis.FFont);
          Canvas.Pen.Color := FAxis.FFont.Color;
          Canvas.Brush.Color := FBackground.GraphColor;
          if FBars[i].FCaption <> '' then
          begin
            iW := Canvas.TextWidth(FBars[i].FCaption);
            Canvas.TextOut(iLeft + (iBarWidth div 2) - (iW div 2),FBars[i].Top-2-(Round(FAxis.Font.Size * 1.5)),FBars[i].Caption);
          end;
          if FBars[i].ShowValue then
          begin
            if FAxis.RoundValue then
            begin
              if FAxis.ValueMask = '' then
                s := IntToStr(Round(FBars[i].Value))
              else
                s := Format( FAxis.ValueMask,[Round(FBars[i].Value)]);
            end else
            begin
              if FAxis.ValueMask = '' then
                s := FloatToStr(FBars[i].Value)
              else
                s := Format( FAxis.ValueMask,[FBars[i].Value]);
            end;
            iW := Canvas.TextWidth(s);
            Canvas.Brush.Color := FBars[i].Color;
            Canvas.TextOut(iLeft + (iBarWidth div 2) - (iW div 2),FBars[i].Top+2,s);
          end;
          iLeft := iLeft+iBarWidth;
        end;
      end;
    goHorizontal:
      begin
        Canvas.Rectangle(0, Height-FAxis.Width, Width, Height+FAxis.Width);
        DoLines;
        iBarWidth := (Height - FAxis.Width) div (FBars.Count + 1);
      end;
  end;

  Canvas.Font.Assign(FCaption.FFont);
  Canvas.Pen.Color := FCaption.Font.Color;
  Canvas.Brush.Color := FBackground.GraphColor;
  case FOrientation of
    goVertical:
      begin
         Canvas.TextOut(FAxis.Width+4,4,FCaption.FCaption);
         Canvas.Font.Size := Canvas.Font.Size div 2;
         Canvas.TextOut(FAxis.Width+4,Round(FCaption.FFont.Size*1.5)+4,FCaption.FSubCaption);
      end;
    goHorizontal:
      begin
         Canvas.TextOut(4,4,FCaption.FCaption);
         Canvas.Font.Size := Canvas.Font.Size div 2;
         Canvas.TextOut(4,Round(FCaption.FFont.Size*1.5)+4,FCaption.FSubCaption);
      end;
  end;
end;

procedure TArcIWBarGraph.ReadjustMaxValue(MinimumMaxValue : real; OverFactor : real = 0);
var
  rMaxBarValue : real;
  i : integer;
begin
  rMaxBarValue := 0;

  for i := 0 to FBars.Count -1 do
    rMaxBarValue := Max(rMaxBarValue,FBars[i].Value);

  if rMaxBarValue = 0 then
    rMaxBarValue := 1;

  if OverFactor <> 0 then
    rMaxBarValue := rMaxBarValue * (1+OverFactor); 

  FAxis.MaxValue := Max(MinimumMaxValue,rMaxBarValue);
end;

procedure TArcIWBarGraph.Submit(const AValue: String);
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

{$IFDEF INTRAWEB51}
function TArcIWBarGraph.RenderHTML : TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWBarGraph.RenderHTML(AContext : TIWBaseComponentContext) : TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWBarGraph.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
  procedure DoAxisSection(tag : TIWHTMLTag);
    procedure DoValues;
    var
      s, sStyle  : string;
      i, iPerc, iW, iX, iH  : integer;
      f, fPerc : real;
      t : TIWHTMLTag;
    begin
      case FOrientation of
        goVertical:   iPerc := Height div FAxis.LineCount;
        goHorizontal: iPerc := Width div FAxis.LineCount;
      end;

      if iPerc <= 0 then
        iPerc := 10;

      case FOrientation of
        goVertical:
          begin
            i := Height;
            while i >= 0 do
            begin
              dec(i, iPerc);

              if (i > (FAxis.Font.Size * 1.5)) and FAxis.ShowValues then
              begin
                try
                  fPerc :=  1-(i / Height);
                  f := ((FAxis.MaxValue-FAxis.MinValue) * fPerc)+FAxis.MinValue;
                  if FAxis.RoundValue then
                  begin
                    if FAxis.ValueMask = '' then
                      s := IntToStr(Round(f))
                    else
                      s := Format( FAxis.ValueMask,[Round(f)]);
                  end else
                  begin
                    if FAxis.ValueMask = '' then
                      s := FloatToStr(f)
                    else
                      s := Format( FAxis.ValueMask,[f]);
                  end;
                except
                  s := 'Format Error';
                end;

                //iW := Canvas.TextWidth(s);
                      
                case FOrientation of
                  goVertical:
                    begin
                      t := tag.Contents.AddTag('span');
                      t.Contents.AddText(s);
                      sStyle := 'position: absolute;text-align: right;'+
                                'right: '+IntToStr(4)+'px;'+
                                'left: auto;'+
                                'top: '+IntToStr(i-( (Round(FAxis.Font.Size * 1.5)) div 2))+'px;'+
                                FAxis.Font.FontToStringStyle({$IFNDEF INTRAWEB51}AContext.{$ENDIF}WebApplication.Browser,FBackground.FFont)+';'+
                                'color: '+ColorToRGBString(FAxis.Font.Color)+';';
                      t.AddStringParam('style',sStyle);
                    end;
                  goHorizontal:
                    begin
                      t := tag.Contents.AddTag('span');
                      t.Contents.AddText(s);
                      sStyle := 'position: absolute;text-align: right;'+
                                'left: '+IntToStr(i -(iW div 2))+'px;'+
                                'top: '+IntToStr(Height-FAxis.Width +4)+'px;'+
                                FAxis.Font.FontToStringStyle({$IFNDEF INTRAWEB51}AContext.{$ENDIF}WebApplication.Browser,FBackground.FFont)+';'+
                                'color: '+ColorToRGBString(FAxis.Font.Color)+';';
                      t.AddStringParam('style',sStyle);
                    end;
                end;
              end;
            end;
          end;
        goHorizontal:
          begin
          end;
      end;
    end;
  var
    sStyle : string;
    fnt : TIWFont;
    t : TIWHTMLTag;
  begin
    tag.AddStringParam('ID',HTMLName+'_AXISSECTION');
    sStyle := 'position: absolute;';
    case FOrientation of
      goVertical:   sStyle := sStyle+
                    'top: '+IntToStr(0)+'px;'+
                    'left: '+IntToStr(0)+'px;'+
                    'width: '+IntToStr(FAxis.Width)+'px;'+
                    'Height: '+IntToStr(Height)+'px;';
      goHorizontal: sStyle := sStyle+
                    'top: '+IntToStr( Height - FAxis.Width)+'px;'+
                    'left: '+IntToStr(0)+'px;'+
                    'width: '+IntToStr(Width)+'px;'+
                    'Height: '+IntToStr(FAxis.Width)+'px;';
    end;
    sStyle := sStyle+'background-color: '+ColorToRGBString(FBackground.FAxisColor)+';';
    tag.AddStringParam('style',sStyle);

    t := tag.Contents.AddTag('span');
    t.Contents.AddText(FAxis.Caption);

    sStyle := 'position: absolute;'+
              'top: '+IntToStr(0)+'px;'+
              'left: '+IntToStr(0)+'px;'+
              'width: '+IntToStr(FAxis.Width)+'px;'+
              FAxis.Font.FontToStringStyle({$IFNDEF INTRAWEB51}AContext.{$ENDIF}WebApplication.Browser)+
              'color: '+ColorToRGBString(FAxis.FFont.Color)+';';

    t.AddStringParam('style',sStyle);

    DoValues;
  end;
  procedure DoGraphSection(tag : TIWHTMLTag);
    procedure DoBars;
    var
      i : integer;
      t : TIWHTMLTag;
      s, sStyle : string;
      iBarWidth, iSpacerWidth, iW, iLeft, iX, iY : integer;
      iGraphWidth : integer;
    begin
      case FOrientation of
        goVertical:
          begin
            iGraphWidth := Width-FAxis.Width;
            iBarWidth := (Width - FAxis.Width) div ((FBars.Count*2) + 1);
            iSpacerWidth := Round((Width - FAxis.Width) / ((FBars.Count*2) + 1));
            iLeft := 0;
            for i := 0 to FBars.Count -1 do
            begin
              iLeft := iLeft + iSpacerWidth;
              sStyle := FAxis.Font.FontToStringStyle({$IFNDEF INTRAWEB51}AContext.{$ENDIF}WebApplication.Browser)+
                        'color: '+ColorToRGBString(FAxis.FFont.Color)+';'+
                        'background-color: '+ColorToRGBString(FBars[i].Color)+';'+
                        'border-style: '+RenderBorderStyle(FBars[i].BorderStyle)+';'+
                        'border-color: '+ColorToRGBString(FBars[i].FBorderColor)+';'+
                        'border-width: '+IntToStr(FBars[i].BorderSize)+'px;'+
                        'text-align: center; overflow: visible;position: absolute;';

              sStyle := sStyle+
                        'left: '+IntToStr(iLeft)+'px;'+
                        'Top: '+IntToStr(FBars[i].Top)+'px;'+
                        'Width: '+IntToStr(iBarWidth)+'px;'+
                        'Height: '+IntToStr(Height-FBars[i].Top)+'px;';
              t := tag.Contents.AddTag('span');
              t.AddStringParam('Style',sStyle);

              if FBars[i].ShowValueHint then
                t.AddStringParam('title',FBars[i].CalcValueHint);
              if FBars[i].ShowValue then
              begin
                if FAxis.RoundValue then
                begin
                  if FAxis.ValueMask = '' then
                    s := IntToStr(Round(FBars[i].Value))
                  else
                    s := Format( FAxis.ValueMask,[Round(FBars[i].Value)]);
                end else
                begin
                  if FAxis.ValueMask = '' then
                    s := FloatToStr(FBars[i].Value)
                  else
                    s := Format( FAxis.ValueMask,[FBars[i].Value]);
                end;
                {t := t.Contents.AddTag('span');

                sStyle := 'position: absolute;'+
                          'left: '+IntToStr(iLeft + (iBarWidth div 2))+'px;'+
                          'width: '+IntToStr(iBarWidth)+'px;'+
                          'text-align: center;'+
                          'top: '+IntToStr(FBars[i].Top+2)+'px;';

                t.AddStringParam('style',sStyle);
                }
                t.Contents.AddText(s);
              end;

              if FBars[i].FCaption <> '' then
              begin
                t := tag.Contents.AddTag('span');
                sStyle := 'position: absolute; '+
                          'left: '+IntToStr(iLeft - (iBarWidth div 2))+'px;'+
                          'width: '+IntToStr(iBarWidth*2)+'px;'+
                          'top: '+IntToStr(FBars[i].Top-(Round(FAxis.Font.Size * 1.5)))+'px;'+
                          'color: '+ColorToRGBString(FAxis.FFont.Color)+';'+
                          'background-color: '+ColorToRGBString(FBackground.GraphColor)+';'+
                          'text-align: center;'+
                          FAxis.FFont.FontToStringStyle({$IFNDEF INTRAWEB51}AContext.{$ENDIF}WebApplication.Browser);

                t.Contents.AddText(FBars[i].Caption);
                t.AddStringParam('title',FBars[i].CalcValueHint);
                t.AddStringParam('style',sStyle);
              end;
              iLeft := iLeft+iBarWidth;
            end;
          end;
        goHorizontal:
          begin
          end;
      end;
    end;
    procedure DoLines;
    var
      s, sStyle  : string;
      i, iPerc, iW, iX, iH  : integer;
      f, fPerc : real;
    begin
      case FOrientation of
        goVertical:   iPerc := Height div FAxis.LineCount;
        goHorizontal: iPerc := Width div FAxis.LineCount;
      end;

      if iPerc <= 0 then
        iPerc := 10;

      case FOrientation of
        goVertical:
          begin
            i := Height;
            while i >= 0 do
            begin
              dec(i, iPerc);

              if FAxis.ShowLines then
              begin
                sStyle :=
                  'position: absolute; font: 0px;'+
                  'left: 0px;'+
                  'top: '+IntToStr(i)+'px;'+
                  'Width: '+IntToStr(Width-FAxis.Width)+'px;'+
                  'Height: 1px;'+
                  'background-color: '+ColorToRGBString(FAxis.LineColor)+';'+
                  'z-index: '+IntToStr(ZIndex+110)+'px;';
                with Tag.Contents.AddTag('span') do
                begin
                  AddStringParam('style',sStyle);
                end;
              end;
            end;
          end;
        goHorizontal:
          begin
          end;
      end;
    end;
  var
    sStyle : string;
    fnt : TIWFont;
    t : TIWHTMLTag;
  begin
    tag.AddStringParam('ID',HTMLName+'_GRAPHSECTION');
    sStyle := 'position: absolute;';
    case FOrientation of
      goVertical:   sStyle := sStyle+
                    'top: '+IntToStr(0)+'px;'+
                    'left: '+IntToStr(FAxis.Width)+'px;'+
                    'width: '+IntToStr(Width-FAxis.Width)+'px;'+
                    'Height: '+IntToStr(Height)+'px;';
      goHorizontal: sStyle := sStyle+
                    'top: '+IntToStr(0)+'px;'+
                    'left: '+IntToStr(0)+'px;'+
                    'width: '+IntToStr(Width)+'px;'+
                    'Height: '+IntToStr(Height-FAxis.Width)+'px;';
    end;
    sStyle := sStyle+
            //'position: absolute;'+
            'background-color: '+ColorToRGBString(FBackground.FGraphColor)+';'+
            'border-style: '+RenderBorderStyle(FBackground.FBorderStyle)+';'+
            'border-width: '+IntToStr(FBackground.FBorderSize)+';'+
            'border-color: '+ColorToRGBString(FBackground.FBorderColor)+';'+
            'overflow: hidden;';
    tag.AddStringParam('style',sStyle);

    DoLines;
    DoBars;
    case FOrientation of
      goVertical:
        begin
          t := tag.Contents.AddTag('span');
          t.Contents.AddText(FCaption.FCaption+'&nbsp;');
          sStyle := 'position: absolute;'+
                    'left: 0px; top: 2px;'+
                    'height: '+IntToStr(Round(FCaption.Font.Size*1.5))+';'+
                    'text-align: left;'+
                    'padding-left: 4px;'+
                    'background-color: '+ColorToRGBString(FBackground.FGraphColor)+';'+
                    'color: '+ColorToRGBString(FCaption.Font.Color)+';'+
                    FCaption.FFont.FontToStringStyle({$IFNDEF INTRAWEB51}AContext.{$ENDIF}WebApplication.Browser);
          t.AddStringParam('style',sStyle);
          fnt := TIWFont.Create;
          try
            fnt.Assign(FCaption.FFont);
            fnt.Size := fnt.Size div 2;
            t := tag.Contents.AddTag('span');
            t.Contents.AddText(FCaption.FSubCaption+'&nbsp;');
            sStyle := 'position: absolute;'+
                      'left: 0px; '+
                      'top: '+IntToStr(Round(FCaption.Font.Size*1.5))+'px;'+
                      'height: '+IntToStr(Round(fnt.Size*1.5)+2)+';'+
                      'text-align: center;'+
                      'padding-top: 2px;'+
                      'padding-left: 4px;'+
                      'background-color: '+ColorToRGBString(FBackground.FGraphColor)+';'+
                      'color: '+ColorToRGBString(fnt.Color)+';'+
                      fnt.FontToStringStyle({$IFNDEF INTRAWEB51}AContext.{$ENDIF}WebApplication.Browser);
            t.AddStringParam('style',sStyle);
          finally
            fnt.Free;
          end;
        end;
      goHorizontal:
        begin
           {Canvas.TextOut(4,4,FCaption.FCaption);
           Canvas.Font.Size := Canvas.Font.Size div 2;
           Canvas.TextOut(4,Round(FCaption.FFont.Size*1.5)+4,FCaption.FSubCaption);}
        end;
    end;
  end;
var
  sStyle : string;
  tag : TIWHTMLTag;
begin
  Result := TIWHTMLTag.CreateTag('SPAN');
  sStyle := 'position: absolute; white-space : nowrap;overflow: visible;'+
            'width: '+IntToStr(Width)+'px;'+
            'height: '+IntToStr(Height)+'px;'+
            'background-color: '+ColorToRGBString(FBackground.FAxisColor)+';';
  result.AddStringParam('style',sStyle);

  if Assigned(FOnClick) then
    result.AddStringParam('onClick','SubmitClick(''' + HTMLName+ ''','''', false);');

  tag := Result.Contents.AddTag('SPAN');
  case FOrientation of
    goVertical: DoAxisSection(tag);
    goHorizontal: DoGraphSection(tag);
  end;
  tag := Result.Contents.AddTag('SPAN');
  case FOrientation of
    goVertical: DoGraphSection(tag);
    goHorizontal: DoAxisSection(tag);
  end;
end;

procedure TArcIWBarGraph.Resize;
begin
  inherited;

end;

procedure TArcIWBarGraph.SetAxis(const Value: TArcIWGraphAxis);
begin
  FAxis.Assign(Value);
  Invalidate;
end;

procedure TArcIWBarGraph.SetBackground(const Value: TArcIWGraphBackground);
begin
  FBackground.Assign(Value);
  Invalidate;
end;

procedure TArcIWBarGraph.SetBars(const Value: TArcIWGraphBars);
begin
  FBars.Assign(Value);
  Invalidate;
end;

procedure TArcIWBarGraph.SetCaption(const Value: TArcIWGraphCaption);
begin
  FCaption.Assign(Value);
  Invalidate;
end;

procedure TArcIWBarGraph.SetOrientation(
  const Value: TArcIWGraphOrientation);
begin
  FOrientation := Value;
  Invalidate;
end;

{ TArcIWGraphBackground }

procedure TArcIWGraphBackground.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TArcIWGraphBackground(Dest).BorderSize      := BorderSize;
  TArcIWGraphBackground(Dest).BorderStyle     := BorderStyle;
  TArcIWGraphBackground(Dest).GraphColor      := GraphColor;
  TArcIWGraphBackground(Dest).AxisColor  := AxisColor;
  TArcIWGraphBackground(Dest).BorderColor     := BorderColor;
  TArcIWGraphBackground(Dest).Font            := Font;
end;

constructor TArcIWGraphBackground.Create(Comp : TIWControl);
begin
  inherited Create;
  FBorderSize      := 2;
  FComp            := Comp;
  FBorderStyle     := bsLowered;
  FGraphColor      := $EBB99D;
  FAxisColor  := clWhite;
  FBorderColor     := clWebNavy;
  FFont            := TIWFont.Create;
end;

destructor TArcIWGraphBackground.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TArcIWGraphBackground.SetFont(const Value: TIWFont);
begin
  FFont := Value;
end;

{ TArcIWGraphAxis }

procedure TArcIWGraphAxis.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TArcIWGraphAxis(Dest).ShowValues  := ShowValues;
  TArcIWGraphAxis(Dest).ShowLines   := ShowLines ;
  TArcIWGraphAxis(Dest).Width       := Width     ;
  TArcIWGraphAxis(Dest).MaxValue    := MaxValue  ;
  TArcIWGraphAxis(Dest).MinValue    := MinValue  ;
  TArcIWGraphAxis(Dest).Caption     := Caption   ;
  TArcIWGraphAxis(Dest).ValueMask   := ValueMask ;
  TArcIWGraphAxis(Dest).Font        := Font      ;
end;

constructor TArcIWGraphAxis.Create(Comp : TIWControl);
begin
  inherited Create;
  FShowValues := True;
  FShowLines := True;
  FLineColor := $d0a080;
  FComp := Comp;
  FLineCount := 5;
  FWidth := 75;
  FMaxValue := 100;
  FMinValue := 0;
  FCaption := '';
  FValueMask := '';
  FFont := TIWFont.Create;
  FFont.FontName := 'Arial';
  FFont.Size := 14;
  FFont.Style := [fsBold];
end;

destructor TArcIWGraphAxis.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TArcIWGraphAxis.SetCaption(const Value: string);
begin
  FCaption := Value;
  FComp.Invalidate;
end;

procedure TArcIWGraphAxis.SetFont(const Value: TIWFont);
begin
  FFont := Value;
  FComp.Invalidate;
end;

procedure TArcIWGraphAxis.SetLineColor(const Value: TIWColor);
begin
  FLineColor := Value;
  FComp.Invalidate;
end;

procedure TArcIWGraphAxis.SetLineCount(const Value: integer);
begin
  if Value <=0 then exit;
  FLineCount := Value;
  FComp.Invalidate;
end;

procedure TArcIWGraphAxis.SetMaxValue(const Value: real);
begin
  FMaxValue := Value;
  FComp.Invalidate;
end;

procedure TArcIWGraphAxis.SetMinValue(const Value: real);
begin
  FMinValue := Value;
  FComp.Invalidate;
end;

procedure TArcIWGraphAxis.SetRoundValue(const Value: boolean);
begin
  FRoundValue := Value;
  FComp.Invalidate;
end;

procedure TArcIWGraphAxis.SetShowLines(const Value: boolean);
begin
  FShowLines := Value;
  FComp.Invalidate;
end;

procedure TArcIWGraphAxis.SetShowValues(const Value: boolean);
begin
  FShowValues := Value;
  FComp.Invalidate;
end;

procedure TArcIWGraphAxis.SetValueMask(const Value: string);
begin
  FValueMask := Value;
  FComp.Invalidate;
end;

procedure TArcIWGraphAxis.SetWidth(const Value: integer);
begin
  FWidth := Value;
  FComp.Invalidate;
end;

{ TArcIWGraphBars }

constructor TArcIWGraphBars.Create(Comp : TIWControl);
begin
  inherited Create(TArcIWGraphBar);
  FComp := Comp;
end;

function TArcIWGraphBars.GetBars(idx: integer): TArcIWGraphBar;
begin
  Result := TArcIWGraphBar(Items[idx]);
end;

function TArcIWGraphBars.Render(GraphTag: TIWHtmlTag;
  GraphWidth: integer): string;
begin

end;

procedure TArcIWGraphBars.SetBars(idx: integer;
  const Value: TArcIWGraphBar);
begin
  Items[idx].Assign(Value);
end;

{ TArcIWGraphBar }

function TArcIWGraphBar.CalcValueHint: string;
begin
  Result := ReplaceStr(FValueHintFormat,'<#caption>',FCaption);
  Result := ReplaceStr(Result,'<#value>',FloatToStr(FValue));
end;

constructor TArcIWGraphBar.Create(Collection : TCollection);
begin
  inherited Create(Collection);
  FShowValue := False;
  FShowValueHint := True;
  FValueHintFormat := '<#Caption> = <#Value>';
  FBorderSize := 1;
  FValue := 10;
  FCaption := '';
  FBorderStyle := bsRaised;
  FBorderColor := clWebBlack;
  FColor       := clWebNavy;
end;

destructor TArcIWGraphBar.Destroy;
begin
  inherited;
end;

procedure TArcIWGraphBar.SetBorderColor(const Value: TIWColor);
begin
  FBorderColor := Value;
  TArcIWGraphBars(Collection).FComp.Invalidate;
end;

procedure TArcIWGraphBar.SetBorderSize(const Value: integer);
begin
  FBorderSize := Value;
  TArcIWGraphBars(Collection).FComp.Invalidate;
end;

procedure TArcIWGraphBar.SetBorderStyle(const Value: TArcIWBorderStyle);
begin
  FBorderStyle := Value;
  TArcIWGraphBars(Collection).FComp.Invalidate;
end;

procedure TArcIWGraphBar.SetCaption(const Value: string);
begin
  FCaption := Value;
  TArcIWGraphBars(Collection).FComp.Invalidate;
end;

procedure TArcIWGraphBar.SetColor(const Value: TIWColor);
begin
  FColor := Value;
  TArcIWGraphBars(Collection).FComp.Invalidate;
end;

procedure TArcIWGraphBar.SetShowValue(const Value: boolean);
begin
  FShowValue := Value;
  TArcIWGraphBars(Collection).FComp.Invalidate;
end;

procedure TArcIWGraphBar.SetValue(const Value: real);
begin
  FValue := Value;
  TArcIWGraphBars(Collection).FComp.Invalidate;
end;

function TArcIWGraphBar.Top: integer;
var
  Range, perc : real;
  graph : TArcIWBarGraph;
  Y : integer;
begin
  graph := TArcIWBarGraph(TArcIWGraphBars(Collection).FComp);

  Range := graph.Axis.MaxValue-graph.Axis.MinValue;
  perc   := (Value-graph.Axis.MinValue) / Range;

  case graph.Orientation of
    goVertical:   result := round(graph.Height - (graph.Height * perc));
    goHorizontal: result := round(graph.Width - (graph.Width * perc));
  end;
end;

{ TArcIWGraphCaption }

procedure TArcIWGraphCaption.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TArcIWGraphCaption(Dest).Caption    := Caption;
  TArcIWGraphCaption(Dest).SubCaption := SubCaption;
  TArcIWGraphCaption(Dest).Font       := Font;
end;

constructor TArcIWGraphCaption.Create(Comp : TIWControl);
begin
  inherited Create;
  FCaption    := '';
  FSubCaption := '';
  FFont := TIWFont.Create;
  FComp := Comp;
end;

destructor TArcIWGraphCaption.Destroy;
begin
  FFont.Free;
  inherited;
end;

end.
