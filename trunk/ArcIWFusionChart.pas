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

unit ArcIWFusionChart;

interface

uses
  Graphics, StrUtils, SyncObjs,
  SysUtils, Classes, Controls, IWVCLBaseControl, IWBaseControl, IWHTMLTag,
  IWBaseHTMLControl, IWControl, IWRenderContext, IWFileReference, IWColor,
  IWFont, IWTypes, IWBaseInterfaces, ArcCommon;

type
  TAnimationSpeed = 1..10;
  TPercent = 0..100;
  TAxisSelection = (asNone, asLeft, asRight);
  TSeriesType = (stSingle, stMulti, stCombo);

  TArcIWFusionNumberFormat = class(TPersistent)
  private
    FTruncate: boolean;
    FFormatted: boolean;
    FDecimalPrecision: integer;
    FPrefix: String;
    FSuffix: string;
    FThousandSymbol: string;
    FDecimalSymbol: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
  published
    property Formatted : boolean read FFormatted write FFormatted;
    property Prefix : String read FPrefix write FPrefix;
    property Suffix : string read FSuffix write FSuffix;
    property DecimalSymbol : string read FDecimalSymbol write FDecimalSymbol;
    property ThousandSymbol : string read FThousandSymbol write FThousandSymbol;
    property DecimalPrecision : integer read FDecimalPrecision write FDecimalPrecision;
    property Truncate : boolean read FTruncate write FTruncate;
  end;

  TArcIWFusionAxis = class(TPersistent)
  private
    FComp : TIWControl;
    FShowLimit: boolean;
    FShowValue: boolean;
    FLineThickness: integer;
    FLineCount: integer;
    FMinValue: real;
    FMaxValue: real;
    FCaption: string;
    FLineColor: TIWColor;
    FLineTransparency: TPercent;
    FShowLineValue: boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Comp : TIWControl); virtual;
    destructor Destroy; override;
  published
    property Caption : string read FCaption write FCaption;
    property MinValue : real read FMinValue write FMinValue;
    property MaxValue : real read FMaxValue write FMaxValue;
    property ShowValue : boolean read FShowValue write FShowValue;
    property ShowLimit : boolean read FShowLimit write FShowLimit;
    property LineCount : integer read FLineCount write FLineCount;
    property LineThickness : integer read FLineThickness write FLineThickness;
    property LineColor : TIWColor read FLineColor write FLineColor;
    property LineTransparency : TPercent read FLineTransparency write FLineTransparency;
    property ShowLineValue : boolean read FShowLineValue write FShowLineValue;
  end;

  TArcIWFusionDataItem = class;
  
  TArcIWDataItemClick = procedure(Sender : TObject; Item : TArcIWFusionDataItem) of object;

  TArcIWFusionDataItem = class(TCollectionItem)
  private
    FShowCaption: boolean;
    FSliced: boolean;
    FTransparency: TPercent;
    FValue: real;
    FCaption: string;
    FLinkURL: string;
    FColor: TIWColor;
    FOnClick: TArcIWDataItemClick;
    FHint: string;
    FVisible: boolean;
    FShowHint: boolean;
  protected
    function GetDisplayName: String; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    function GetLink : string;
    constructor Create(Collection: TCollection); override;
  published
    property Caption : string read FCaption write FCaption;
    property Value : real read FValue write FValue;
    property Color : TIWColor read FColor write FColor;
    property LinkURL : string read FLinkURL write FLinkURL;
    property OnClick : TArcIWDataItemClick read FOnClick write FOnClick;
    property Transparency : TPercent read FTransparency write FTransparency;
    property Sliced : boolean read FSliced write FSliced;
    property ShowCaption : boolean read FShowCaption write FShowCaption;
    property ShowHint : boolean read FShowHint write FShowHint;
    property Hint : string read FHint write FHint;
    property Visible : boolean read FVisible write FVisible;
  end;

  TArcIWFusionDataItems = class(TOwnedCollection)
  private
    function GetItem(idx: integer): TArcIWFusionDataItem;
    procedure SetItem(idx: integer; const Value: TArcIWFusionDataItem);
  public
    property Items[idx : integer] : TArcIWFusionDataItem read GetItem write SetItem; default;
    function Add : TArcIWFusionDataItem;
  end;

  TArcIWFusionSeriesDataItem = class(TCollectionItem)
  private
    FLineThickness: integer;
    FSeriesName: string;
    FColor: TIWColor;
    FAxis: TAxisSelection;
    FPrefix: string;
    FAnchorColor: TIWColor;
    FData: TArcIWFusionDataItems;
  protected
    function GetDisplayName: String; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property SeriesName : string read FSeriesName write FSeriesName;
    property Data : TArcIWFusionDataItems read FData write FData;
    property Color : TIWColor read FColor write FColor;
    property Axis : TAxisSelection read FAxis write FAxis;
    property Prefix : string read FPrefix write FPrefix;
    property LineThickness : integer read FLineThickness write FLineThickness;
    property AnchorColor : TIWColor read FAnchorColor write FAnchorColor;
  end;

  TArcIWFusionSeriesDataItems = class(TOwnedCollection)
  private
    function GetItem(idx: integer): TArcIWFusionSeriesDataItem;
    procedure SetItem(idx: integer; const Value: TArcIWFusionSeriesDataItem);
  public
    property Items[idx : integer] : TArcIWFusionSeriesDataItem read GetItem write SetItem; default;
    function Add : TArcIWFusionSeriesDataItem;
  end;

  TArcIWFusionTrendItem = class(TCollectionItem)
  private
    FThickness: integer;
    FValue: real;
    FDisplayValue: string;
    FColor: TIWColor;
    FTransparency: TPercent;
  protected
    function GetDisplayName: String; override;
  public
    constructor Create(Collection: TCollection); override;
  published
    property Value : real read FValue write FValue;
    property Color : TIWColor read FColor write FColor;
    property DisplayValue : string read FDisplayValue write FDisplayValue;
    property Thickness : integer read FThickness write FThickness;
    property Transparency : TPercent read FTransparency write FTransparency;
  end;

  TArcIWFusionTrendItems = class(TOwnedCollection)
  private
    function GetItem(idx: integer): TArcIWFusionTrendItem;
    procedure SetItem(idx: integer; const Value: TArcIWFusionTrendItem);
  public
    property Items[idx : integer] : TArcIWFusionTrendItem read GetItem write SetItem; default;
  end;

  TArcIWFusionChart = class(TIWControl, IIWSubmitControl)
  private
    FShowLineValues: boolean;
    FAnimate: boolean;
    FShowBackground: boolean;
    FShowActualValues: boolean;
    FLineThickness: integer;
    FAnimationSpeed: TAnimationSpeed;
    FCaption: string;
    FCaptionSubTitle: string;
    FCacheOverrideURL: string;
    FAxisY: TArcIWFusionAxis;
    FAxisX: TArcIWFusionAxis;
    FSeriesData: TArcIWFusionSeriesDataItems;
    FFormat: TArcIWFusionNumberFormat;
    FTrends: TArcIWFusionTrendItems;
    FCacheOverridePath: TFilename;
    FBackgroundColor: TIWColor;
    FNavigationButtonColor: TIWColor;
    FAnchorBackgroundColor: TIWColor;
    FZeroPlaneColor: TIWColor;
    FLegendBorderColor: TIWColor;
    FLegendBackgroundColor: TIWColor;
    FGraphColor: TIWColor;
    FAnchorBorderColor: TIWColor;
    FAreaColor: TIWColor;
    FFusionChartFilename: TIWFileReference;
    FGraphFont: TIWFont;
    FFont: TIWFont;
    FAnchorScale: TPercent;
    FZeroPlaneTransparency: TPercent;
    FAreaTransparency: TPercent;
    FShowGraph: boolean;
    FSeriesType: TSeriesType;
    FEmbedData: boolean;
    FCacheData: boolean;
    FOnClick: TNotifyEvent;
  protected
    FSubmitParam : String;
    procedure AssignTo(Dest: TPersistent); override;
  public
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetSubmitParam: String;
    procedure Submit(const AValue: String); override;
  published
    property Caption : string read FCaption write FCaption;
    property CaptionSubTitle : string read FCaptionSubTitle write FCaptionSubTitle;
    property Format : TArcIWFusionNumberFormat read FFormat write FFormat;
    property Animate : boolean read FAnimate write FAnimate;
    property AnimationSpeed : TAnimationSpeed read FAnimationSpeed write FAnimationSpeed;
    property GraphFont : TIWFont read FGraphFont write FGraphFont;
    property Font : TIWFont read FFont write FFont;
    property FusionChartFilename : TIWFileReference read FFusionChartFilename write FFusionChartFilename;
    property CacheOverridePath : TFilename read FCacheOverridePath write FCacheOverridePath;
    property CacheOverrideURL : string read FCacheOverrideURL write FCacheOverrideURL;
    property BackgroundColor : TIWColor read FBackgroundColor write FBackgroundColor;
    property GraphColor : TIWColor read FGraphColor write FGraphColor;
    property AxisX : TArcIWFusionAxis read FAxisX write FAxisX;
    property AxisY : TArcIWFusionAxis read FAxisY write FAxisY;
    property ShowGraph : boolean read FShowGraph write FShowGraph;
    property ShowBackground : boolean read FShowBackground write FShowBackground;
    property ShowLineValues : boolean read FShowLineValues write FShowLineValues;
    property ZeroPlaneColor : TIWColor read FZeroPlaneColor write FZeroPlaneColor;
    property ZeroPlaneTransparency : TPercent read FZeroPlaneTransparency write FZeroPlaneTransparency;
    property LegendBackgroundColor : TIWColor read FLegendBackgroundColor write FLegendBackgroundColor;
    property LegendBorderColor : TIWColor read FLegendBorderColor write FLegendBorderColor;
    property NavigationButtonColor : TIWColor read FNavigationButtonColor write FNavigationButtonColor;
    property ShowActualValues : boolean read FShowActualValues write FShowActualValues;
    property LineThickness : integer read FLineThickness write FLineThickness;
    property AnchorBackgroundColor : TIWColor read FAnchorBackgroundColor write FAnchorBackgroundColor;
    property AnchorBorderColor : TIWColor read FAnchorBorderColor write FAnchorBorderColor;
    property AnchorScale : TPercent read FAnchorScale write FAnchorScale;
    property AreaColor : TIWColor read FAreaColor write FAreaColor;
    property AreaTransparency : TPercent read FAreaTransparency write FAreaTransparency;
    property SeriesType : TSeriesType read FSeriesType write FSeriesType; 
    property SeriesData : TArcIWFusionSeriesDataItems read FSeriesData write FSeriesData;
    property Trends : TArcIWFusionTrendItems read FTrends write FTrends;
    property EmbedData : boolean read FEmbedData write FEmbedData;
    property CacheData : boolean read FCacheData write FCacheData;
    property OnClick : TNotifyEvent read FOnClick write FOnClick;
  end;

implementation

{$J+}
const
  CacheBuster : Cardinal = 0;
{$J-}

var
  CacheBusterCS : TCriticalSection;

{ TArcIWFusionChart }

procedure TArcIWFusionChart.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TArcIWFusionChart(Dest).FShowLineValues := FShowLineValues;
  TArcIWFusionChart(Dest).FAnimate := FAnimate;
  TArcIWFusionChart(Dest).FShowBackground := FShowBackground;
  TArcIWFusionChart(Dest).FShowActualValues := FShowActualValues;
  TArcIWFusionChart(Dest).FLineThickness := FLineThickness;
  TArcIWFusionChart(Dest).FAnimationSpeed := FAnimationSpeed;
  TArcIWFusionChart(Dest).FCaption := FCaption;
  TArcIWFusionChart(Dest).FCaptionSubTitle := FCaptionSubTitle;
  TArcIWFusionChart(Dest).FCacheOverrideURL := FCacheOverrideURL;
  TArcIWFusionChart(Dest).FAxisY.Assign(FAxisY);
  TArcIWFusionChart(Dest).FAxisX.Assign(FAxisX);
  TArcIWFusionChart(Dest).FSeriesData.Assign(FSeriesData);
  TArcIWFusionChart(Dest).FFormat.Assign(FFormat);
  TArcIWFusionChart(Dest).FTrends.Assign(FTrends);
  TArcIWFusionChart(Dest).FCacheOverridePath := FCacheOverridePath;
  TArcIWFusionChart(Dest).FBackgroundColor := FBackgroundColor;
  TArcIWFusionChart(Dest).FNavigationButtonColor := FNavigationButtonColor;
  TArcIWFusionChart(Dest).FAnchorBackgroundColor := FAnchorBackgroundColor;
  TArcIWFusionChart(Dest).FZeroPlaneColor := FZeroPlaneColor;
  TArcIWFusionChart(Dest).FLegendBorderColor := FLegendBorderColor;
  TArcIWFusionChart(Dest).FLegendBackgroundColor := FLegendBackgroundColor;
  TArcIWFusionChart(Dest).FGraphColor := FGraphColor;
  TArcIWFusionChart(Dest).FAnchorBorderColor := FAnchorBorderColor;
  TArcIWFusionChart(Dest).FAreaColor := FAreaColor;
  TArcIWFusionChart(Dest).FFusionChartFilename.Assign(FFusionChartFilename);
  TArcIWFusionChart(Dest).FGraphFont.Assign(FGraphFont);
  TArcIWFusionChart(Dest).FFont.Assign(FFont);
  TArcIWFusionChart(Dest).FAnchorScale := FAnchorScale;
  TArcIWFusionChart(Dest).FZeroPlaneTransparency := FZeroPlaneTransparency;
  TArcIWFusionChart(Dest).FAreaTransparency := FAreaTransparency;
  TArcIWFusionChart(Dest).FShowGraph := FShowGraph;
  TArcIWFusionChart(Dest).FSeriesType := FSeriesType;
  TArcIWFusionChart(Dest).Width := Width;
  TArcIWFusionChart(Dest).Height := Height;
end;

constructor TArcIWFusionChart.Create(AOwner: TComponent);
begin
  inherited;
  Width := 300;
  Height := 200;
  FFusionChartFilename := TIWFileReference.Create;
  FAxisY := TArcIWFusionAxis.Create(Self);
  FAxisX := TArcIWFusionAxis.Create(Self);
  FSeriesData := TArcIWFusionSeriesDataItems.Create(Self,TArcIWFusionSeriesDataItem);
  FFormat := TArcIWFusionNumberFormat.Create;
  FTrends := TArcIWFusionTrendItems.Create(Self,TArcIWFusionTrendItem);
  FGraphFont := TIWFont.Create;
  FFont := TIWFont.Create;

  FCacheData := True;
  FEmbedData := False;
  FShowGraph := True;
  FShowLineValues := True;
  FAnimate := True;
  FShowBackground := True;
  FShowActualValues := False;
  FLineThickness := 2;
  FAnimationSpeed := 5;
  FCaption := '';
  FCaptionSubTitle := '';
  FCacheOverrideURL := '';
  FCacheOverridePath := '';
  FBackgroundColor := clNone;
  FNavigationButtonColor := clNone;
  FAnchorBackgroundColor := clNone;
  FZeroPlaneColor := clNone;
  FLegendBorderColor := clNone;
  FLegendBackgroundColor := clNone;
  FGraphColor := clNone;
  FAnchorBorderColor := clNone;
  FAreaColor := clNone;
  FAnchorScale := 100;
  FZeroPlaneTransparency := 80;
  FAreaTransparency := 80;
  FShowGraph := True;
end;

destructor TArcIWFusionChart.Destroy;
begin
  FFusionChartFilename.Free;
  FAxisY.Free;
  FAxisX.Free;
  FSeriesData.Free;
  FFormat.Free;
  FTrends.Free;
  FGraphFont.Free;
  FFont.Free;
  inherited;
end;

function TArcIWFusionChart.GetSubmitParam: String;
begin
  Result := FSubmitParam;
end;

{$O-}

function TArcIWFusionChart.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
  function IncCB : integer;
  begin
    if not FCacheData then
    begin
      CacheBusterCS.Enter;
      try
        inc(CacheBuster);
        Result := CacheBuster;
      finally
        CacheBusterCS.Leave;
      end;
    end else
      Result := 0;
  end;
  function CB(I : integer) : string;
  begin
    if not FCacheData then
      Result := '.'+IntToStr(I)
    else
      Result := '';
  end;
var
  tagObj, tag: TIWHTMLTag;
  sXML, sXMLPath, sXMLFile : string;
  sl : TStringList;
  i, j, iCB : integer;
begin
  iCB := IncCB;
  if FCacheOverridePath = '' then
  begin
    sXMLFile := AContext.WebApplication.UserCacheDir+Owner.Name+'.'+Name+CB(iCB)+'.xml';
    sXMLPath := AContext.WebApplication.UserCacheURL+Owner.Name+'.'+Name+CB(iCB)+'.xml';
  end else
  begin
    sXMLFile := FCacheOverridePath+Owner.Name+'.'+Name+CB(iCB)+'.xml';
    sXMLPath := FCacheOverrideURL+Owner.Name+'.'+Name+CB(iCB)+'.xml';
  end;

  sl := TStringList.Create;
  try
    sl.Add( '<graph '+
            ifThen(FBackgroundColor <> clNone,'bgcolor="'+ColorToRGBString(FBackgroundColor)+'" ')+
            ifThen(FGraphColor <> clNone, 'canvasbgcolor="'+ColorToRGBString(FGraphColor)+'" ')+
            //IfThen(Assigned(FOnClick), 'link="javascript:IWTop().SubmitClick(''' + HTMLName+ ''',''-1,-1'', false);" ')+
            //IfThen(Assigned(FOnClick), 'link="http://arcanatech.com" ')+
            'caption="'+FCaption+'" '+
            'subCaption="'+FCaptionSubTitle+'" '+
            'yaxisname="'+FAxisY.Caption+'" '+
            'xaxisname="'+FAxisX.Caption+'" '+
            'yaxisminvalue="'+FloatToStr(FAxisY.MinValue)+'" '+
            'xaxisminvalue="'+FloatToStr(FAxisX.MinValue)+'" '+
            'yaxismaxvalue="'+FloatToStr(FAxisY.MaxValue)+'" '+
            'xaxismaxvalue="'+FloatToStr(FAxisX.MaxValue)+'" '+
            'formatNumber="'+IfThen(FFormat.Formatted,'1','0')+'" '+
            'numberPrefix="'+FFormat.Prefix+'" '+
            'numberSuffix="'+FFormat.Suffix+'" '+
            'decimalSeparator="'+FFormat.DecimalSymbol+'" '+
            'thousandSeparator="'+FFormat.ThousandSymbol+'" '+
            'decimalPrecision="'+IntToStr(FFormat.DecimalPrecision)+'" '+
            'formatNumberScale="'+IfThen(FFormat.Truncate,'1','0')+'" '+
            'animation="'+IfThen(FAnimate,'1','0')+'" '+
            'animspeed="'+IntToStr(FAnimationSpeed)+'" '+
            'basefont="'+FFont.FontName+'" '+
            'basefontsize="'+IntToStr(FFont.Size)+'" '+
            ifThen(FFont.Color <> clNone, 'basefontcolor="'+ColorToRGBString(FFont.Color)+'" ')+
            'outcnvbasefont="'+FGraphFont.FontName+'" '+
            'outcnvbasefontsize="'+IntToStr(FGraphFont.Size)+'" '+
            ifThen(FGraphFont.Color <> clNone, 'outcnvbasefontcolor="'+ColorToRGBString(FGraphFont.Color)+'" ')+
            'shownames="'+IfThen(FAxisX.ShowValue,'1','0')+'" '+
            'showvalues="'+IfThen(FAxisY.ShowValue,'1','0')+'" '+
            'showYLimits="'+IfThen(FAxisY.ShowLimit,'1','0')+'" '+
            'showCanvas="'+IfThen(FShowGraph,'1','0')+'" '+
            'showgridbg="'+IfThen(FShowBackground,'1','0')+'" '+
            ifThen(FGraphColor <> clNone, 'gridbgcolor="'+ColorToRGBString(FGraphColor)+'" ')+
            'numdivlines="'+IntToStr(FAxisX.LineCount)+'" '+
            ifThen(FAxisX.LineColor <> clNone, 'divlinecolor="'+ColorToRGBString(FAxisX.LineColor)+'" ')+
            'divlinethickness="'+IntToStr(FAxisX.LineThickness)+'" '+
            'showDivLineValues="'+IfThen(FAxisX.ShowLineValue,'1','0')+'" '+
            ifThen(FZeroPlaneColor <> clNone, 'zeroPlaneColor="'+ColorToRGBString(FZeroPlaneColor)+'" ')+
            'zeroPlaneAlpha="'+IntToStr(100-FZeroPlaneTransparency)+'" '+
            ifThen(FLegendBackgroundColor <> clNone, 'legendboxbgcolor="'+ColorToRGBString(FLegendBackgroundColor)+'" ')+
            ifThen(FLegendBorderColor <> clNone, 'legendboxbrdcolor="'+ColorToRGBString(FLegendBorderColor)+'" ')+
            ifThen(FNavigationButtonColor <> clNone, 'navbtncolor="'+ColorToRGBString(FNavigationButtonColor)+'" ')+
            'showActualValues="'+IfThen(FShowActualValues,'1','0')+'" '+
            'linethickness="'+IntToStr(FLineThickness)+'" '+
            'numVdivlines="'+IntToStr(FAxisY.LineCount)+'" '+
            ifThen(FAxisY.LineColor <> clNone, 'vDivlinecolor="'+ColorToRGBString(FAxisY.LineColor)+'" ')+
            'vDivlinethickness="'+IntToStr(FAxisY.LineThickness)+'" '+
            'vDivlineAlpha="'+IntToStr(100-FAxisY.FLineTransparency)+'" '+
            ifThen(FAnchorBackgroundColor<> clNone, 'anchorbgcolor="'+ColorToRGBString(FAnchorBackgroundColor)+'" ')+
            ifThen(FAnchorBorderColor<> clNone, 'anchorbrdcolor="'+ColorToRGBString(FAnchorBorderColor)+'" ')+
            'anchorScale="'+IntToStr(FAnchorScale)+'" '+
            ifThen(FAreaColor <> clNone, 'areaColor="'+ColorToRGBString(FAreaColor)+'" ')+
            'areaAlpha="'+IntToStr(100-FAreaTransparency)+'" '+
            '>' );
    case FSeriesType of
      stSingle:
        begin
          if FSeriesData.Count > 0 then
          begin
            for i := 0 to FSeriesData[0].FData.Count -1 do
            begin
              sl.Add( '<set '+
                      'name="'+FSeriesData[0].FData[i].Caption+'" '+
                      'value="'+FloatToStr(FSeriesData[0].FData[i].Value)+'" '+
                      ifThen(FSeriesData[0].FData[i].Color <> clNone,'color="'+ColorToRGBString(FSeriesData[0].FData[i].Color)+'" ')+
                      ifThen(Assigned(FSeriesData[0].FData[j].OnClick),'link="'+FSeriesData[0].FData[i].GetLink+'" ')+
                      'alpha="'+IntToStr(100-FSeriesData[0].FData[i].Transparency)+'" '+
                      'isSliced="'+IfThen(FSeriesData[0].FData[i].Sliced,'1','0')+'" '+
                      'showName="'+IfThen(FSeriesData[0].FData[i].ShowCaption,'1','0')+'" '+
                      '/>');
            end;
          end;
        end;
      stMulti, stCombo:
        begin
          if FSeriesData.Count >0 then
          begin
            sl.Add('<categories>');
            for j := 0 to FSeriesData[0].FData.Count -1 do
            begin
              sl.Add( '<category '+
                      'name="'+FSeriesData[0].FData[j].Caption+'" '+
                      //'hoverText="'+FSeriesData[0].FData[j].Caption+'" '+
                      '/>');
            end;
            sl.Add('</categories>');
          end;
          for i := 0 to FSeriesData.Count -1 do
          begin
            sl.Add('<dataset '+
              IfThen(FSeriesData[i].SeriesName <> '','seriesName="'+FSeriesData[i].SeriesName+'" ')+
              IfThen(FSeriesData[i].Color <> clNone, 'color="'+ColorToRGBString(FSeriesData[i].Color)+'" ')+
              IfThen(FSeriesData[i].Prefix <> '','numberPrefix="'+FSeriesData[i].Prefix+'" ')+
              IfThen(FSeriesData[i].LineThickness <> 0, 'lineThickness="'+IntToStr(FSeriesData[i].FLineThickness)+'" ')+
              IfThen(FSeriesData[i].AnchorColor <> clNone,'anchorBorderColor="'+ColorToRGBString(FSeriesData[i].AnchorColor)+'" ')+
              IfThen((FSeriesType = stCombo),IfThen(FSeriesData[i].Axis = asRight, 'parentYAxis="S" ','parentYAxis="P" '))+
              '>');
            for j := 0 to FSeriesData[i].FData.Count -1 do
            begin
              sl.Add( '<set '+
                      Ifthen(FSeriesData[i].FData[j].Visible,
                      //IfThen(Assigned(FOnClick), 'link="http://arcanatech.com" ')+
                      //'name="'+FSeriesData[i].FData[j].Caption+'" '+
                      'value="'+FloatToStr(FSeriesData[i].FData[j].Value)+'" '+
                      //ifThen(FSeriesData[i].FData[j].Color <> clNone,'color="'+ColorToRGBString(FSeriesData[i].FData[j].Color)+'" ')+
                      ifThen(Assigned(FSeriesData[i].FData[j].OnClick),'link="'+FSeriesData[i].FData[j].GetLink+'" ')+
                      'alpha="'+IntToStr(100-FSeriesData[i].FData[j].Transparency)+'" '+
                      'isSliced="'+IfThen(FSeriesData[i].FData[j].Sliced,'1','0')+'" '+
                      'showName="'+IfThen(FSeriesData[i].FData[j].ShowCaption,'1','0')+'" '+
                      IfThen(FSeriesData[i].FData[j].ShowHint,'hoverText="'+FSeriesData[i].FData[j].Hint+'" ')
                      )+
                      '/>');
            end;
            sl.Add('</dataset>');
          end;
        end;
    end;
    if FTrends.Count > 0 then
    begin
      sl.Add( '<trendlines>');
      for i := 0 to FTrends.Count -1 do
      begin
        sl.Add( '<line '+
                'value="'+FloatToStr(FTrends[i].Value)+'" '+
                'displayValue="'+FTrends[i].DisplayValue+'" '+
                ifThen(FTrends[i].Color <> clNone,'color="'+ColorToRGBString(FTrends[i].Color)+'" ')+
                'thickness="'+IntToStr(FTrends[i].Thickness)+'" '+
                'alpha="'+IntToStr(100-FTrends[i].Transparency)+'" '+
                '/>');
      end;
      sl.Add( '</trendlines>');
    end;
    sl.Add('</graph>');

    if FEmbedData then
      sXML := StringReplace(StringReplace(sl.Text,'"','''',[rfReplaceAll]),#13#10,'',[rfReplaceAll])
    else
      sl.SaveToFile(sXMLFile);
  finally
    sl.Free;
  end;

  Result := TIWHTMLTag.CreateHTMLTag('div');
  //Result.Contents.AddTag('div').Contents.AddText('DEBUG: '+FFusionChartFilename.Location(AContext.WebApplication.InternalURLBase)+'?dataURL='+sXMLPath);
  tagObj := Result.Contents.AddTag('object');
  tagObj.AddStringParam('classid','clsid:D27CDB6E-AE6D-11cf-96B8-444553540000');
  tagObj.AddStringParam('codebase','https://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=6,0,0,0');
  tagObj.AddStringParam('width','100%');
  tagObj.AddStringParam('height','100%');
  tagObj.AddStringParam('ID',HTMLName+'_obj');
  tagObj.AddStringParam('align','');

  tag := tagObj.Contents.AddTag('param');
  tag.AddStringParam('name','movie');
  tag.AddStringParam('value',FFusionChartFilename.Location(AContext.WebApplication.InternalURLBase));
  if FEmbedData then
  begin
    tag := tagObj.Contents.AddTag('param');
    tag.AddStringParam('name','FlashVars');
    tag.AddStringParam('value','&dataXML='+sXML);
  end else
  begin
    tag := tagObj.Contents.AddTag('param');
    tag.AddStringParam('name','FlashVars');
    //tag.AddStringParam('value','&dataURL='+sXMLPath+'&chartWidth=100%&chartHeight=100%');
    tag.AddStringParam('value','&dataURL='+sXMLPath);
  end;
  tag := tagObj.Contents.AddTag('param');
  tag.AddStringParam('name','quality');
  tag.AddStringParam('value','high');
  tag := tagObj.Contents.AddTag('param');
  tag.AddStringParam('name','bgcolor');
  tag.AddStringParam('value',ColorToRGBString(FBackgroundColor));
  tag := tagObj.Contents.AddTag('embed');
  tag.AddStringParam('src',FFusionChartFilename.Location(AContext.WebApplication.InternalURLBase));
  if FEmbedData then
    //tag.AddStringParam('FlashVars','&dataXML='+sXML+'&chartWidth=100%&chartHeight=100%')
    tag.AddStringParam('FlashVars','&dataXML='+sXML)
  else
    //tag.AddStringParam('FlashVars','&dataURL='+sXMLPath+'&chartWidth=100%&chartHeight=100%');
    tag.AddStringParam('FlashVars','&dataURL='+sXMLPath);

  tag.AddStringParam('quality','high');
  tag.AddStringParam('bgcolor',ColorToRGBString(FBackgroundColor));
  tag.AddStringParam('width','100%');
  tag.AddStringParam('height','100%');
  tag.AddStringParam('name',HTMLName+'_embed');
  tag.AddStringParam('align','');
  tag.AddStringParam('type','application/x-shockwave-flash');
  tag.AddStringParam('pluginspage','https://www.macromedia.com/go/getflashplayer');

  //Result := TIWHTMLTag.CreateHTMLTag('IFRAME');
  //Result.AddStringParam('src',FFusionChartFilename.Location(AContext.WebApplication.InternalURLBase)+'?dataURL='+sXMLPath);
end;
{$O+}

procedure TArcIWFusionChart.Submit(const AValue: String);
var
  i, j : integer;
begin
  FSubmitParam := AValue;
  i := StrToIntDef(Copy(AValue,1,Pos(',',AValue)-1),-1);
  j := StrToIntDef(Copy(AValue,Pos(',',AValue)+1,high(Integer)),-1);
  if (i >= 0) and (j >= 0) then
  begin
    if Assigned(FSeriesData[i].FData[j].OnClick) then
      FSeriesData[i].FData[j].OnClick(Self, FSeriesData[i].FData[j]);
  end;
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

{ TArcIWFusionTrendItems }

function TArcIWFusionTrendItems.GetItem(
  idx: integer): TArcIWFusionTrendItem;
begin
  Result := TArcIWFusionTrendItem(inherited Items[idx]);
end;

procedure TArcIWFusionTrendItems.SetItem(idx: integer;
  const Value: TArcIWFusionTrendItem);
begin
  inherited Items[idx] := Value;
end;

{ TArcIWFusionDataItems }

function TArcIWFusionDataItems.Add: TArcIWFusionDataItem;
begin
  Result := TArcIWFusionDataItem(Inherited Add);
end;

function TArcIWFusionDataItems.GetItem(idx: integer): TArcIWFusionDataItem;
begin
  Result := TArcIWFusionDataItem(inherited Items[idx]);
end;

procedure TArcIWFusionDataItems.SetItem(idx: integer;
  const Value: TArcIWFusionDataItem);
begin
  inherited Items[idx] := Value;
end;

{ TArcIWFusionAxis }

procedure TArcIWFusionAxis.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');

  TArcIWFusionAxis(Dest).FShowLimit := FShowLimit;
  TArcIWFusionAxis(Dest).FShowValue := FShowValue;
  TArcIWFusionAxis(Dest).FLineThickness := FLineThickness;
  TArcIWFusionAxis(Dest).FLineCount := FLineCount;
  TArcIWFusionAxis(Dest).FMinValue := FMinValue;
  TArcIWFusionAxis(Dest).FMaxValue := FMaxValue;
  TArcIWFusionAxis(Dest).FCaption := FCaption;
  TArcIWFusionAxis(Dest).FLineColor := FLineColor;
  TArcIWFusionAxis(Dest).FLineTransparency := FLineTransparency;
  TArcIWFusionAxis(Dest).FShowLineValue := FShowLineValue;
end;

constructor TArcIWFusionAxis.Create(Comp: TIWControl);
begin
  inherited Create;
  FComp := Comp;
  FShowLimit := True;
  FShowValue := True;
  FLineThickness := 2;
  FLineCount := 5;
  FMinValue := 0;
  FMaxValue := 100;
  FCaption := '';
  FLineColor := clNone;
  FLineTransparency := 0;
  FShowLineValue := True;
end;

destructor TArcIWFusionAxis.Destroy;
begin

  inherited;
end;

{ TArcIWFusionDataItem }

procedure TArcIWFusionDataItem.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TArcIWFusionDataItem(Dest).FShowCaption := FShowCaption;
  TArcIWFusionDataItem(Dest).FSliced := FSliced;
  TArcIWFusionDataItem(Dest).FTransparency := FTransparency;
  TArcIWFusionDataItem(Dest).FValue := FValue;
  TArcIWFusionDataItem(Dest).FCaption := FCaption;
  TArcIWFusionDataItem(Dest).FLinkURL := FLinkURL;
  TArcIWFusionDataItem(Dest).FColor := FColor;
  TArcIWFusionDataItem(Dest).FOnClick := FOnClick;
  TArcIWFusionDataItem(Dest).FHint := FHint;
  TArcIWFusionDataItem(Dest).FVisible := FVisible;
  TArcIWFusionDataItem(Dest).FShowHint := FShowHint;
end;

constructor TArcIWFusionDataItem.Create(Collection: TCollection);
begin
  inherited;
  FShowCaption := True;
  FSliced := False;
  FTransparency := 0;
  FValue := 0;
  FCaption := '';
  FLinkURL := '';
  FColor := clNone;
  FOnClick := nil;
  FHint := '';
  FVisible := True;
  FShowHint := False;
end;

function TArcIWFusionDataItem.GetDisplayName: String;
begin
  if FCaption <> '' then
    result := FCaption
  else
    result := '(item '+IntToStr(Index)+')';
end;

function TArcIWFusionDataItem.GetLink: string;
begin
  if FLinkURL = '' then
    Result := 'javascript:IWTop().SubmitClick(''' + TIWControl(TCollectionItem(Collection.Owner).Collection.Owner).HTMLName+ ''','''+IntToStr(TCollectionItem(Collection.Owner).Index)+','+IntToStr(Index)+''', false);'
  else
    Result := FLinkURL;
end;

{ TArcIWFusionNumberFormat }

procedure TArcIWFusionNumberFormat.AssignTo(Dest: TPersistent);
begin
  if not (Dest is TArcIWFusionNumberFormat) then
    raise Exception.Create('Cannot assign a '+Dest.ClassName+' to a TArcIWFusionAxis.');

  TArcIWFusionNumberFormat(Dest).FTruncate := FTruncate;
  TArcIWFusionNumberFormat(Dest).FFormatted := FFormatted;
  TArcIWFusionNumberFormat(Dest).FDecimalPrecision := FDecimalPrecision;
  TArcIWFusionNumberFormat(Dest).FPrefix := FPrefix;
  TArcIWFusionNumberFormat(Dest).FSuffix := FSuffix;
  TArcIWFusionNumberFormat(Dest).FThousandSymbol := FThousandSymbol;
  TArcIWFusionNumberFormat(Dest).FDecimalSymbol := FDecimalSymbol;
end;

constructor TArcIWFusionNumberFormat.Create;
begin
  inherited Create;
  FTruncate := True;
  FFormatted := True;
  FDecimalPrecision := 0;
  FPrefix := '';
  FSuffix := '';
  FThousandSymbol := ',';
  FDecimalSymbol := '.';
end;

{ TArcIWFusionTrendItem }

constructor TArcIWFusionTrendItem.Create(Collection: TCollection);
begin
  inherited;
  FThickness := 2;
  FValue := 0;
  FDisplayValue := '';
  FColor := clNone;
  FTransparency := 0;
end;

function TArcIWFusionTrendItem.GetDisplayName: String;
begin
  if FDisplayValue <> '' then
    result := FDisplayValue
  else
    result := '(item '+IntToStr(Index)+')';
end;

{ TArcIWFusionSeriesDataItem }

procedure TArcIWFusionSeriesDataItem.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TArcIWFusionSeriesDataItem(Dest).FLineThickness := FLineThickness;
  TArcIWFusionSeriesDataItem(Dest).FSeriesName := FSeriesName;
  TArcIWFusionSeriesDataItem(Dest).FColor := FColor;
  TArcIWFusionSeriesDataItem(Dest).FAxis := FAxis;
  TArcIWFusionSeriesDataItem(Dest).FPrefix := FPrefix;
  TArcIWFusionSeriesDataItem(Dest).FAnchorColor := FAnchorColor;
  TArcIWFusionSeriesDataItem(Dest).FData.Assign(FData);
end;

constructor TArcIWFusionSeriesDataItem.Create(Collection: TCollection);
begin
  inherited;
  FData := TArcIWFusionDataItems.Create(Self,TArcIWFusionDataItem);
  FColor := clNone;
  FAnchorColor := clNone;
end;

destructor TArcIWFusionSeriesDataItem.Destroy;
begin
  FData.Free;
  inherited;
end;

function TArcIWFusionSeriesDataItem.GetDisplayName: String;
begin
  if FSeriesName = '' then
    Result := '(series '+IntToStr(self.Index)+')'
  else
    Result := FSeriesName;
end;

{ TArcIWFusionSeriesDataItems }

function TArcIWFusionSeriesDataItems.Add: TArcIWFusionSeriesDataItem;
begin
  Result := TArcIWFusionSeriesDataItem(inherited Add);
end;

function TArcIWFusionSeriesDataItems.GetItem(
  idx: integer): TArcIWFusionSeriesDataItem;
begin
  Result := TArcIWFusionSeriesDataItem(inherited Items[idx])
end;

procedure TArcIWFusionSeriesDataItems.SetItem(idx: integer;
  const Value: TArcIWFusionSeriesDataItem);
begin
  inherited Items[idx] := Value;
end;

initialization
  CacheBusterCS := TCriticalSection.Create;

finalization
  CacheBusterCS.Free;

end.
