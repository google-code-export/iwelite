unit ArcIWStringGridCalendarContent;

interface

uses SysUtils, {$IFNDEF VER130}StrUtils, Variants, {$ENDIF}
  Classes, ArcIWStringGridContent, ArcIWStringGrid, controls,
  IWBaseControl, IWCompCheckbox, IWCompEdit, IWCompMemo, IWCompButton, IWExtCtrls,
  Graphics, TypInfo, ArcFastStrings, IWCompListbox, IWBaseInterfaces,
  IWRenderContext, IWHTMLTag, IWTypes, IWColor, db, IWFileReference, IWServer,
  ArcIWGridCommon;

type
  TArcIWStringGridCalendarContent = class;
  TArcCalendarScope = (csYear, csQuarter, csMonth, csWeek, csDay);
  TArcCalendarDay = (cdSunday, cdMonday, cdTuesday, cdWednesday, cdThursday,
    cdFriday, cdSaturday);
  TArcCalendarMonth = (cmJanuary, cmFebruary, cmMarch, cmApril, cmMay, cmJune,
    cmJuly, cmAugust, cmSeptember, cmOctober, cmNovember, cmDecember);
  TArcCalendarQuarter = (cqQ1, cqQ2, cqQ3, cqQ4);
  TArcCalendarPrecision = (cpYear, cpMonth, cpDay, cpHour, cpMinute, cpSecond, cpExact);

  TArcCalendarEvent = class(TCollectionItem)
  private
    FHint: string;
    FTitle: string;
    FDescription: string;
    FScheduled: TDateTime;
    FStyle: TArcGridStyle;
    FIcon: TIWFileReference;
    FIconHeight: integer;
    FIconWidth: integer;
    procedure SetIcon(const Value: TIWFileReference);
    procedure SetStyle(const Value: TArcGridStyle);
    function GetScheduledDate: TDate;
    function GetScheduledTime: TTime;
    function IconLocation(URLBase: string): string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Title : string read FTitle write FTitle;
    property Description : string read FDescription write FDescription;
    property Hint : string read FHint write FHint;
    property Scheduled : TDateTime read FScheduled write FScheduled;
    property ScheduledDate : TDate read GetScheduledDate;
    property ScheduledTime : TTime read GetScheduledTime;
    property Style : TArcGridStyle read FStyle write SetStyle;
    property Icon : TIWFileReference read FIcon write SetIcon;
    property IconWidth : integer read FIconWidth write FIconWidth default 16;
    property IconHeight : integer read FIconHeight write FIconHeight default 16;
    //property Style : TArcCalendarStyle read FStyle write SetStyle;
    //property StyleSelected
  end;

  TArcCalendarEvents = class(TOwnedCollection)
  private
    FContent: TArcIWStringGridCalendarContent;
    function GetItem(idx: integer): TArcCalendarEvent;
    procedure SetItem(idx: integer; const Value: TArcCalendarEvent);
  public
    constructor Create(content : TArcIWStringGridCalendarContent); virtual;
    destructor Destroy; override;
    property Items[idx : integer] : TArcCalendarEvent read GetItem write SetItem; default;
    function Add : TArcCalendarEvent;
  end;

  TArcCalendarLabelsMonths = class(TPersistent)
  private
    FApril: string;
    FNovember: string;
    FMay: string;
    FDecember: string;
    FAugust: string;
    FFebruary: string;
    FJune: string;
    FMarch: string;
    FJuly: string;
    FOctober: string;
    FSeptember: string;
    FJanuary: string;
    FJun: string;
    FFeb: string;
    FNov: string;
    FJul: string;
    FDec: string;
    FMar: string;
    FOct: string;
    FJan: string;
    FAug: string;
    FApr: string;
    FSep: string;
    FMa: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    function TextFor(Month : TArcCalendarMonth; Abbreviated : boolean) : string;
  published
    property January : string read FJanuary write FJanuary;
    property Jan : string read FJan write FJan;
    property February : string read FFebruary write FFebruary;
    property Feb : string read FFeb write FFeb;
    property March : string read FMarch write FMarch;
    property Mar : string read FMar write FMar;
    property April : string read FApril write FApril;
    property Apr : string read FApr write FApr;
    property May : string read FMay write FMay;
    property Ma : string read FMa write FMa;
    property June : string read FJune write FJune;
    property Jun : string read FJun write FJun;
    property July : string read FJuly write FJuly;
    property Jul : string read FJul write FJul;
    property August : string read FAugust write FAugust;
    property Aug : string read FAug write FAug;
    property September : string read FSeptember write FSeptember;
    property Sep : string read FSep write FSep;
    property October : string read FOctober write FOctober;
    property Oct : string read FOct write FOct;
    property November : string read FNovember write FNovember;
    property Nov : string read FNov write FNov;
    property December : string read FDecember write FDecember;
    property Dec : string read FDec write FDec;
  end;

  TArcCalendarLabelsQuarters = class(TPersistent)
  private
    FQtr2: string;
    FQtr3: string;
    FQtr1: string;
    FQtr4: string;
    FQuarter2: string;
    FQuarter3: string;
    FQuarter1: string;
    FQuarter4: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    function TextFor(Qtr : TArcCalendarQuarter; Abbreviated : boolean) : string;
  published
    property Quarter1 : string read FQuarter1 write FQuarter1;
    property Qtr1 : string read FQtr1 write FQtr1;
    property Quarter2 : string read FQuarter2 write FQuarter2;
    property Qtr2 : string read FQtr2 write FQtr2;
    property Quarter3 : string read FQuarter3 write FQuarter3;
    property Qtr3 : string read FQtr3 write FQtr3;
    property Quarter4 : string read FQuarter4 write FQuarter4;
    property Qtr4 : string read FQtr4 write FQtr4;
  end;


  TArcCalendarLabelsDays = class(TPersistent)
  private
    FSunday: string;
    FWednesday: string;
    FTuesday: string;
    FFriday: string;
    FThursday: string;
    FMonday: string;
    FSaturday: string;
    FFri: string;
    FTue: string;
    FMon: string;
    FWed: string;
    FSun: string;
    FThu: string;
    FSat: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    function TextFor(Day : TArcCalendarDay; Abbreviated : boolean) : string;
  published
    property Sunday : string read FSunday write FSunday;
    property Sun : string read FSun write FSun;
    property Monday : string read FMonday write FMonday;
    property Mon : string read FMon write FMon;
    property Tuesday : string read FTuesday write FTuesday;
    property Tue : string read FTue write FTue;
    property Wednesday : string read FWednesday write FWednesday;
    property Wed : string read FWed write FWed;
    property Thursday : string read FThursday write FThursday;
    property Thu : string read FThu write FThu;
    property Friday : string read FFriday write FFriday;
    property Fri : string read FFri write FFri;
    property Saturday : string read FSaturday write FSaturday;
    property Sat : string read FSat write FSat;
  end;

  TArcCalendarLabels = class(TPersistent)
  private
    FDays: TArcCalendarLabelsDays;
    FMonths: TArcCalendarLabelsMonths;
    FQuarters: TArcCalendarLabelsQuarters;
    procedure SetDays(const Value: TArcCalendarLabelsDays);
    procedure SetMonths(const Value: TArcCalendarLabelsMonths);
    procedure SetQuarters(const Value: TArcCalendarLabelsQuarters);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Months : TArcCalendarLabelsMonths read FMonths write SetMonths;
    property Days : TArcCalendarLabelsDays read FDays write SetDays;
    property Quarters : TArcCalendarLabelsQuarters read FQuarters write SetQuarters;
  end;

  TDateTimeHolder = record
    DateTime : TDateTime;
  end;
  PDateTimeHolder = ^TDateTimeHolder;

  TArcClickDateTimeEvent = procedure(Sender : TObject; DateTime : TDateTime) of object;
  TArcClickEventEvent = procedure(Sender : TObject; Event : TArcCalendarEvent)  of object;

  TArcIWStringGridCalendarContent = class(TArcIWStringGridContent)
  private
    FSelected : TList;
    FLabels: TArcCalendarLabels;
    FVisibleStart: TDateTime;
    FDayEnd: TTime;
    FToday: TDateTime;
    FUse24HourClock: boolean;
    FWeekStart: TArcCalendarDay;
    FScope: TArcCalendarScope;
    FEvents: TArcCalendarEvents;
    FDayStart: TTime;
    FDaysPerWeek: integer;
    FOnClickDateTime: TArcClickDateTimeEvent;
    FOnClickEvent: TArcClickEventEvent;
    FCaptionFormat: string;
    function CFDefault(Scope : TArcCalendarScope) : string;
    procedure SetEvents(const Value: TArcCalendarEvents);
    procedure SetLabels(const Value: TArcCalendarLabels);
    function GetSelCount: integer;
    function GetSelected(idx : integer): TDateTime;
    procedure SetCaptionFormat(const Value: string);
    procedure SetScope(const Value: TArcCalendarScope);
    function FindEndOfWeek(dt: TDateTime): TDateTime;
    function FindStartOfWeek(dt: TDateTime): TDateTime;
    function StartOfWeek : integer; inline;
    function EndOfWeek : integer; inline;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetIsFirstPage: boolean; override;
    function GetIsFirstRecord: boolean; override;
    function GetIsLastPage: boolean; override;
    function GetIsLastRecord: boolean; override;
    function GetIsNextRecord: boolean; override;
    function GetIsPriorRecord: boolean; override;
    procedure UpdateNavButtonState; override;
    procedure SetActive(const Value: Boolean); override;
    procedure SetSelectedDataRow(const Value: Integer); override;
    procedure Loaded; override;
  public
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
    procedure CacheToGrid(Col: integer = -1; Row: integer = -1); override;
    procedure DoAfterRenderHTML(Sender: TObject); override;
    procedure DoBeforeRenderHTML(Sender: TObject; AContext: TIWBaseHTMLComponentContext); override;
    procedure DoCellClick(Sender: TObject; const Col: Integer; const Row: Integer; var Data: string); override;
    procedure DoCellData(Sender: TObject; x: Integer; y: Integer; var sValue: string); override;
    procedure DoClickCaption(Sender: TObject; const Col: Integer); override;
    function DoNeedOnClickEvent(Sender: TObject): Boolean; override;
    procedure DoPopulateRow(Sender: TObject; const y: Integer; slValues: TStrings); override;
    procedure DoReloadData(Sender: TObject); override;
    procedure DoRender(Sender: TObject); override;
    procedure DoRetrieveObject(Sender: TObject; x: Integer; y: Integer; var ctrl: TControl); override;
    procedure DoRetrieveCBObject(Sender: TObject; x: Integer; var ctrl: TControl); override;
    procedure DoRowClick(Sender: TObject; const Row: Integer); override;
    procedure DoSelectCell(Sender: TObject; const x: Integer; const y: Integer; Value: Boolean); override;
    procedure DoSelectCol(Sender: TObject; const x: Integer; Value: Boolean); override;
    procedure DoSelectRow(Sender: TObject; const y: Integer; Value: Boolean); override;
    procedure DoRenameColumn(Sender: TObject; OldName, NewName: string); override;
    procedure DoSubmit(Sender: TObject; const AValue: string); override;
    function DoNeedScript : string; override;

    procedure Append; override;
    procedure Delete; override;
    procedure Post; override;
    procedure Cancel; override;
    procedure Edit; override;

    procedure FirstPage; override;
    procedure NextPage; override;
    procedure PriorPage; override;
    procedure LastPage; override;

    procedure First; override;
    procedure Prior; override;
    procedure Next; override;
    procedure Last; override;
    procedure Refresh; override;

    function ProcessCaption(str: string): string; override;
    property SelCount : integer read GetSelCount;
    property Selected[idx : integer] : TDateTime read GetSelected;
    procedure Select(dt : TDateTime);
    procedure Deselect(dt : TDateTime);
    function FindSelected(dt : TDateTime; Precision : TArcCalendarPrecision = cpExact ) : integer;
    function IsSelected(dt : TDateTime; Precision : TArcCalendarPrecision = cpExact) : Boolean;
  published
    property CaptionFormat : string read FCaptionFormat write SetCaptionFormat;
    property Today : TDateTime read FToday write FToday;
    property Scope : TArcCalendarScope read FScope write SetScope default csMonth;
    property VisibleStart : TDateTime read FVisibleStart write FVisibleStart;
    property Events : TArcCalendarEvents read FEvents write SetEvents;
    property WeekStart : TArcCalendarDay read FWeekStart write FWeekStart;
    property DaysPerWeek : integer read FDaysPerWeek write FDaysPerWeek default 7;
    property DayStart : TTime read FDayStart write FDayStart;
    property DayEnd : TTime read FDayEnd write FDayEnd;
    property Use24HourClock : boolean read FUse24HourClock write FUse24HourClock;
    property Labels : TArcCalendarLabels read FLabels write SetLabels;
    property OnClickDateTime : TArcClickDateTimeEvent read FOnClickDateTime write FOnClickDateTime;
    property OnClickEvent : TArcClickEventEvent read FOnClickEvent write FOnClickEvent;
  end;


implementation

uses DateUtils;

const
  SNotSupported = 'This function is not supported by the CalendarContent Component';
  MaxDate = 23242572;

function SortEvents(Item1, Item2 : pointer) : integer;
begin
  if TArcCalendarEvent(Item1).Scheduled < TArcCalendarEvent(Item2).Scheduled then
    Result := -1
  else if TArcCalendarEvent(Item1).Scheduled > TArcCalendarEvent(Item2).Scheduled then
    Result := 1
  else
    Result := CompareText(TArcCalendarEvent(Item1).Title, TArcCalendarEvent(Item2).Title);
end;


{ TArcIWStringGridCalendarContent }

procedure TArcIWStringGridCalendarContent.Append;
begin
  raise Exception.Create(SNotSupported);
end;

procedure TArcIWStringGridCalendarContent.CacheToGrid(Col, Row: integer);
begin
  inherited;

end;

procedure TArcIWStringGridCalendarContent.Cancel;
begin
  raise Exception.Create(SNotSupported);
end;

function TArcIWStringGridCalendarContent.CFDefault(
  Scope: TArcCalendarScope): string;
begin
  case Scope of
    csYear:    Result := 'YYYY';
    csQuarter: Result := 'MMMM, YYYY';
    csMonth:   Result := 'MMMM, YYYY';
    csWeek:    Result := 'MMMM, YYYY';
    csDay:     Result := 'MMMM D, YYYY';
  end;
end;

constructor TArcIWStringGridCalendarContent.Create(AOwner: TComponent);
begin
  inherited;
  FCaptionFormat := 'MMMM, YYYY';
  FSelected := TList.Create;
  FVisibleStart := EncodeDate(YearOf(Now),MonthOf(Now),1);
  FToday := Now;
  FDayStart := EncodeTime(8,0,0,0);
  FDayEnd := EncodeTime(17,0,0,0);
  FUse24HourClock := True;
  FWeekStart := cdSunday;

  FEvents := TArcCalendarEvents.Create(Self);
  FLabels := TArcCalendarLabels.Create;
  FScope := csMonth;
  FDaysPerWeek := 7;
end;

procedure TArcIWStringGridCalendarContent.Delete;
begin
  raise Exception.Create(SNotSupported);
end;

procedure TArcIWStringGridCalendarContent.Deselect(dt: TDateTime);
var
  idx : integer;
begin
  idx := FindSelected(dt);
  if idx >= 0 then
  begin
    Dispose(PDateTimeHolder(FSelected[idx]));
    FSelected.Delete(idx);
  end;
end;

destructor TArcIWStringGridCalendarContent.Destroy;
var
  i: Integer;
begin
  for i := 0 to FSelected.Count - 1 do
    Dispose(PDateTimeHolder(FSelected[i]));

  FSelected.Free;
  FEvents.Free;
  FLabels.Free;
  inherited;
end;

procedure TArcIWStringGridCalendarContent.DoAfterRenderHTML(Sender: TObject);
begin
  inherited;

end;

procedure TArcIWStringGridCalendarContent.DoBeforeRenderHTML(Sender: TObject;
  AContext: TIWBaseHTMLComponentContext);
  function RenderEvents(dt : TDateTime) : string;
  var
    i : integer;
    ls : TList;
    sImg : string;
    evt : TArcCalendarEvent;
  begin
    Result := '';
    dt := Trunc(dt);
    ls := TList.Create;
    try
      for i := 0 to FEvents.Count - 1 do
        if FEvents[i].ScheduledDate = dt then
          ls.Add(FEvents[i]);

      if ls.Count > 0 then
      begin
        if ls.Count > 1 then
          ls.Sort(SortEvents);

        for i := 0 to ls.Count - 1 do
        begin
          evt :=  TArcCalendarEvent(ls[i]);
          sImg := evt.IconLocation(AContext.WebApplication.AppURLBase);
          Result := Result+
            '<div style="width:auto;'+evt.FStyle.RenderCSS(AContext)+
            //IfThen(sImg = '','margin-left:'+IntToStr(evt.FIconWidth)+'px;')+
            '" title="'+evt.Hint+'">'+
            IfThen(sImg <> '','<img src="'+sImg+'" width='+IntToStr(evt.FIconWidth)+' height='+IntToStr(evt.FIconHeight)+'>')+
            evt.Title+'</div>';
        end;
      end;
    finally
      ls.Free;
    end;
  end;
  function RenderDayTime(dt : TDateTime) : string;
  begin
    case FScope of
      csMonth: Result := '<span style="width:auto;height:100%;">'+IntToStr(DayOf(dt))+'</span>';
      csWeek:
        begin
          Result := '<span style="width:auto;height:100%;">';
          if DayOf(dt)=1 then
            Result := Result+FLabels.FMonths.TextFor(TArcCalendarMonth(MonthOf(dt)-1),True)+' '+IntToStr(DayOf(dt))+'</span>'
          else
            Result := Result+IntToStr(DayOf(dt))+'</span>';
        end;
      csDay:
        begin
          if (dt / 100) = (Trunc(dt) div 100) then
            dt :=  EncodeTime(Trunc(dt) div 100,0,0,0)
          else
            dt :=  EncodeTime(Trunc(dt) div 100,30,0,0);


          Result := '<span style="width:auto;height:100%;font: 9px ''arial'';">'+TimeToStr(dt)+'</span>';
        end;
    end;
  end;
  function RenderMonth(dt : TDateTime) : string;
  var
    x, y, iStart, iCnt : integer;
    dtStart : TDateTime;
    s : string;
  begin
    Result := '<table border=0 style="width:100%;height:100$;'+Grid.StyleTable.RenderCSS(AContext)+'">'+
              '<tr><td colspan=7 style="'+Grid.StyleDetail.RenderCSS(AContext)+'">'+FLabels.Months.TextFor(TArcCalendarMonth(MonthOf(dt)),False)+'</td></tr>';

    dtStart := EncodeDate(YearOf(dt),MonthOf(dt),1);
    iStart := DayOfWeek(dtStart);
    iCnt := DaysInMonth(dtStart);

    for y := 1 to 7 do
    begin
      Result := Result+'<tr>';
      for x := 1 to 5 do
      begin
        if x < DayOfWeek(dtStart) then
          s := '&nbsp;'
        else
          s := IntToStr(DayOf(dtStart+x-1));

        Result := Result+'<td style="'+Grid.StyleDetail.RenderCSS(AContext)+'">'+s+'</td>';
      end;
      Result := Result+'</tr>';
    end;
    Result := Result+'</table>';
  end;
var
  x, y : integer;
begin
  inherited;
  if not (FScope in [csYear, csQuarter]) then
  begin
    for x := 0 to Grid.Columns.Count - 1 do
      for y := 0 to Grid.RowCount - 1 do
        if Integer(Objects[x,y]) > 0 then
          Grid.Cells[x, y] :=
            RenderDayTime(Integer(Objects[x,y]))+
            '<span style="width:100%;height:100%;display:block;overflow:auto;">'+RenderEvents(Integer(Objects[x,y]))+'</span>'
        else
          Grid.Cells[x,y] := '';
  end else
  begin
    for x := 0 to Grid.Columns.Count - 1 do
      for y := 0 to Grid.RowCount - 1 do
        Grid.Cells[x, y] := RenderMonth(Integer(Objects[x,y]));
  end;
end;

procedure TArcIWStringGridCalendarContent.DoCellClick(Sender: TObject;
  const Col, Row: Integer; var Data: string);
begin
  inherited;
  if Assigned(FOnClickDateTime) then
    case FScope of
      csYear: ;
      csQuarter: ;
      csMonth: FOnClickDateTime(Self,Integer(Objects[Col,Row]));
      csWeek: ;
      csDay: ;
    end;
end;

procedure TArcIWStringGridCalendarContent.DoCellData(Sender: TObject; x,
  y: Integer; var sValue: string);
begin
  inherited;

end;

procedure TArcIWStringGridCalendarContent.DoClickCaption(Sender: TObject;
  const Col: Integer);
begin
  inherited;

end;

function TArcIWStringGridCalendarContent.DoNeedOnClickEvent(
  Sender: TObject): Boolean;
begin

end;

function TArcIWStringGridCalendarContent.DoNeedScript: string;
begin

end;

procedure TArcIWStringGridCalendarContent.DoPopulateRow(Sender: TObject;
  const y: Integer; slValues: TStrings);
begin
  inherited;

end;

procedure TArcIWStringGridCalendarContent.DoReloadData(Sender: TObject);
begin
  inherited;

end;

procedure TArcIWStringGridCalendarContent.DoRenameColumn(Sender: TObject;
  OldName, NewName: string);
begin
  inherited;

end;

procedure TArcIWStringGridCalendarContent.DoRender(Sender: TObject);
  procedure RestructureGrid(x,y : integer);
  begin
    Grid.RowCount := y;
    while Grid.Columns.Count < x do
      Grid.Columns.Add;
    while Grid.Columns.Count > x do
      Grid.Columns[Grid.Columns.Count-1].Free;
    Grid.Refresh;
  end;
  procedure RenderYear;
  var
    x, y : integer;
    dt : TDateTime;
  begin
    RestructureGrid(4,3);
    Grid.ColumnsWidthsType := cwtPercent;
    Grid.AutoRowHeight := True;

    dt := EncodeDate(YearOf(FVisibleStart),1,1);
    
    for x := 0 to Grid.Columns.Count - 1 do
    begin
      Grid.Columns[x].Width := 100 div 7;
      Grid.Columns[x].Caption := FLabels.Quarters.TextFor(TArcCalendarQuarter(x),False);
      for y := 0 to Grid.RowCount - 1 do
      begin
        Objects[x,y] := TObject(MonthOf(dt));
        Grid.Cells[x,y] := '';
        incMonth(dt);
      end;
    end;
  end;
  procedure RenderQuarter;
  begin
    RestructureGrid(1,3);
  end;
  procedure RenderMonth;
  var
    dtStart : TDateTime;
    iStart, iCnt : integer;
    i, x, y: Integer;
  begin
    RestructureGrid(7,6);
    Grid.AutoRowHeight := False;
    //Grid.ColumnsWidthsType := cwtPercent;
    //Grid.RowHeaderHeight := 12;
    //Grid.RowHeight := 100;
    for x := 0 to Grid.Columns.Count - 1 do
    begin
      Grid.Columns[x].Width := 100 div 7;
      Grid.Columns[x].Caption := FLabels.Days.TextFor(TArcCalendarDay(x),False);
      for y := 0 to Grid.RowCount - 1 do
      begin
        Objects[x,y] := nil;
        Grid.Cells[x,y] := '';
      end;
    end;

    dtStart := EncodeDate(YearOf(FVisibleStart),MonthOf(FVisibleStart),1);

    iStart := DayOfWeek(dtStart);
    iCnt := DaysInMonth(dtStart);

    x := DayOfWeek(dtStart)-1;
    y := 0;

    for i := 0 to iCnt-1 do
    begin
      if x > Grid.Columns.Count-1 then
      begin
        x := 0;
        inc(y);
      end;
      Objects[x,y] := TObject(Trunc(dtStart));
      Grid.Selected[x,y] := IsSelected(Trunc(dtStart),cpDay);
      inc(x);
      dtStart := IncDay(dtStart,1);
    end;
    if y < 5 then
      Grid.RowCount := 5;
  end;
  procedure RenderWeek;
  var
    x : integer;
    dt : TDateTime;
  begin
    RestructureGrid(FDaysPerWeek,1);
    Grid.ColumnsWidthsType := cwtPercent;
    Grid.AutoRowHeight := True;

    dt := FindStartOfWeek(FVisibleStart);

    for x := 0 to FDaysPerWeek - 1 do
    begin
      Grid.Columns[x].Width := 100 div FDaysPerWeek;
      Grid.Columns[x].Caption := FLabels.Days.TextFor(TArcCalendarDay(x),True);
      Objects[x,0] := TObject(Trunc(dt));
      Grid.Selected[x,0] := IsSelected(Trunc(dt),cpDay);

      dt := incDay(dt);
    end;
  end;
  procedure RenderDay;
  var
    y : integer;
    dt : TDateTime;
    iRowCnt : integer;
  begin
    iRowCnt := (HoursBetween(FDayStart, FDayEnd)*2)+12;
    RestructureGrid(1,iRowCnt);
    Grid.ColumnsWidthsType := cwtPercent;
    Grid.AutoRowHeight := True;
    Grid.Columns[0].Width := 100;

    dt := IncHour(FDayStart,-3);
    y := 0;
    while y < iRowCnt do
    begin
      //Grid.Columns[x].Caption := FLabels.Days.TextFor(TArcCalendarDay(x));
      Objects[0,y] := TObject(HourOf(dt)*100);
      Grid.Selected[0,y] := IsSelected(Trunc(dt),cpHour);

      inc(y);

      Objects[0,y] := TObject(HourOf(dt)*100+50);
      Grid.Selected[0,y] := IsSelected(Trunc(dt),cpHour);

      dt := IncHour(dt);
      inc(y);
    end;

  end;
begin
  case FScope of
    csYear:    RenderYear;
    csQuarter: RenderQuarter;
    csMonth:   RenderMonth;
    csWeek:    RenderWeek;
    csDay:     RenderDay;
  end;
end;

procedure TArcIWStringGridCalendarContent.DoRetrieveCBObject(Sender: TObject;
  x: Integer; var ctrl: TControl);
begin
  inherited;

end;

procedure TArcIWStringGridCalendarContent.DoRetrieveObject(Sender: TObject; x,
  y: Integer; var ctrl: TControl);
begin
  inherited;

end;

procedure TArcIWStringGridCalendarContent.DoRowClick(Sender: TObject;
  const Row: Integer);
begin
  inherited;

end;

procedure TArcIWStringGridCalendarContent.DoSelectCell(Sender: TObject; const x,
  y: Integer; Value: Boolean);
begin
  inherited;

end;

procedure TArcIWStringGridCalendarContent.DoSelectCol(Sender: TObject;
  const x: Integer; Value: Boolean);
begin
  inherited;

end;

procedure TArcIWStringGridCalendarContent.DoSelectRow(Sender: TObject;
  const y: Integer; Value: Boolean);
begin
  inherited;

end;

procedure TArcIWStringGridCalendarContent.DoSubmit(Sender: TObject;
  const AValue: string);
begin
  inherited;

end;

procedure TArcIWStringGridCalendarContent.Edit;
begin
  inherited;
  raise Exception.Create(SNotSupported);
end;

function TArcIWStringGridCalendarContent.EndOfWeek: integer;
begin
  Result := Integer(FWeekStart);
  if Result = 0 then
    Result := Integer(cdSaturday);
end;

function TArcIWStringGridCalendarContent.FindSelected(dt: TDateTime; Precision : TArcCalendarPrecision = cpExact): integer;
  function Compare(dt1, dt2 : TDateTime) : boolean;
  begin
    case Precision of
      cpYear:   Result := (YearOf(dt1) = YearOf(dt2));
      cpMonth:  Result := (YearOf(dt1) = YearOf(dt2)) and (MonthOf(dt1) = MonthOf(dt2));
      cpDay:    Result := (Trunc(dt1) = Trunc(dt2));
      cpHour:   Result := (Trunc(dt1) = Trunc(dt2)) and (HourOf(dt1) = HourOf(dt2));
      cpMinute: Result := (Trunc(dt1) = Trunc(dt2)) and (HourOf(dt1) = HourOf(dt2)) and (Minuteof(dt1) = Minuteof(dt2));
      cpSecond: Result := (Trunc(dt1) = Trunc(dt2)) and (HourOf(dt1) = HourOf(dt2)) and (Minuteof(dt1) = Minuteof(dt2)) and (SecondOf(dt1) = Secondof(dt2));
      cpExact:  Result := dt1 = dt2;
    end;
  end;
var
  i : integer;
begin
  Result := -1;
  for i := 0 to FSelected.Count - 1 do
    if Compare(PDateTimeHolder(FSelected[i])^.DateTime, dt) then
    begin
      Result := i;
      break;
    end;
end;

procedure TArcIWStringGridCalendarContent.First;
begin
  case FScope of
    csYear    : FVisibleStart := 0;
    csQuarter : FVisibleStart := EncodeDate(YearOf(FVisibleStart),1,1);
    csMonth   : FVisibleStart := EncodeDate(YearOf(FVisibleStart),1,1);
    csWeek    : FVisibleStart := EncodeDate(YearOf(FVisibleStart),1,1);
    csDay     : FVisibleStart := Trunc(FVisibleStart);
  end;
end;

procedure TArcIWStringGridCalendarContent.FirstPage;
begin
  case FScope of
    csYear    : FVisibleStart := 0;
    csQuarter : FVisibleStart := 0;
    csMonth   : FVisibleStart := 0;
    csWeek    : FVisibleStart := 0;
    csDay     : FVisibleStart := 0;
  end;
end;

function TArcIWStringGridCalendarContent.GetIsFirstPage: boolean;
begin
  Result := YearOf(FVisibleStart) = YearOf(0);
  case FScope of
    csQuarter : Result := Result and (Monthof(FVisibleStart) = 1);
    csMonth   : Result := Result and (Monthof(FVisibleStart) = 1);
    csWeek    : Result := Result and (Monthof(FVisibleStart) = 1) and (WeekOf(FVisibleStart)=Weekof(0));
    csDay     : Result := Trunc(FVisibleStart) = 0;
  end;
end;

function TArcIWStringGridCalendarContent.GetIsFirstRecord: boolean;
begin
  case FScope of
    csYear    : Result := FVisibleStart = 0;
    csQuarter : Result := FVisibleStart = EncodeDate(YearOf(FVisibleStart),1,1);
    csMonth   : Result := FVisibleStart = EncodeDate(YearOf(FVisibleStart),1,1);
    csWeek    : Result := FVisibleStart = EncodeDate(YearOf(FVisibleStart),1,1);
    csDay     : Result := FVisibleStart = Trunc(FVisibleStart);
  end;
end;

function TArcIWStringGridCalendarContent.GetIsLastPage: boolean;
begin
  case FScope of
    csYear    : Result := EncodeDate(YearOf(FVisibleStart),1,1) = EncodeDate(YearOf(MaxDate),1,1);
    csQuarter : Result := EncodeDate(YearOf(FVisibleStart),10,1) = EncodeDate(YearOf(MaxDate),10,1);
    csMonth   : Result := EncodeDate(YearOf(FVisibleStart),12,1) = EncodeDate(YearOf(MaxDate),12,1);
    csWeek    : Result := FindStartOfWeek(EncodeDate(YearOf(FVisibleStart),12,31)) = FindStartOfWeek(EncodeDate(YearOf(MaxDate),12,31));
    csDay     : Result := EncodeDate(YearOf(FVisibleStart),12,31) = EncodeDate(YearOf(MaxDate),12,31);
  end;
end;

function TArcIWStringGridCalendarContent.GetIsLastRecord: boolean;
begin

end;

function TArcIWStringGridCalendarContent.GetIsNextRecord: boolean;
begin

end;

function TArcIWStringGridCalendarContent.GetIsPriorRecord: boolean;
begin

end;

function TArcIWStringGridCalendarContent.GetSelCount: integer;
begin

end;

function TArcIWStringGridCalendarContent.GetSelected(idx : integer): TDateTime;
begin

end;

function TArcIWStringGridCalendarContent.IsSelected(dt: TDateTime; Precision : TArcCalendarPrecision = cpExact): Boolean;
begin
  Result := FindSelected(dt, Precision) >= 0;
end;

function TArcIWStringGridCalendarContent.FindStartOfWeek(dt : TDateTime) : TDateTime;
begin
  Result := dt;
  while DayOfWeek(Result) <> StartOfWeek do
    Result := IncDay(Result,-1);
end;

function TArcIWStringGridCalendarContent.FindEndOfWeek(dt : TDateTime) : TDateTime;
begin
  Result := dt;
  while DayOfWeek(Result) <> EndOfWeek do
    Result := IncDay(Result,1);
end;

procedure TArcIWStringGridCalendarContent.Last;
begin
  case FScope of
    csYear    : FVisibleStart := EncodeDate(YearOf(MaxDate),1,1);
    csQuarter : FVisibleStart := EncodeDate(YearOf(FVisibleStart),10,1);
    csMonth   : FVisibleStart := EncodeDate(YearOf(FVisibleStart),12,1);
    csWeek    : FVisibleStart := FindStartOfWeek(EncodeDate(YearOf(FVisibleStart),12,31));
    csDay     : FVisibleStart := EncodeDate(YearOf(FVisibleStart),12,31);
  end;
end;

procedure TArcIWStringGridCalendarContent.LastPage;
begin
  case FScope of
    csYear    : FVisibleStart := EncodeDate(YearOf(MaxDate),1,1);
    csQuarter : FVisibleStart := EncodeDate(YearOf(MaxDate),10,1);
    csMonth   : FVisibleStart := EncodeDate(YearOf(MaxDate),12,1);
    csWeek    : FVisibleStart := FindStartOfWeek(EncodeDate(YearOf(MaxDate),12,31));
    csDay     : FVisibleStart := EncodeDate(YearOf(MaxDate),12,31);
  end;
end;

procedure TArcIWStringGridCalendarContent.Loaded;
begin
  inherited;

end;

procedure TArcIWStringGridCalendarContent.Next;
begin
  case FScope of
    csYear    : FVisibleStart := IncYear(FVisibleStart,1);
    csQuarter : FVisibleStart := IncMonth(FVisibleStart,3);
    csMonth   : FVisibleStart := IncMonth(FVisibleStart,1);
    csWeek    : FVisibleStart := IncWeek(FVisibleStart,1);
    csDay     : FVisibleStart := IncDay(FVisibleStart,1);
  end;
  if (FVisibleStart < 0) or (FVisibleStart > MaxDate) then
    FVisibleStart := MaxDate;
end;

procedure TArcIWStringGridCalendarContent.NextPage;
begin
  case FScope of
    csYear    : FVisibleStart := IncYear(FVisibleStart,1);
    csQuarter : FVisibleStart := IncYear(FVisibleStart,1);
    csMonth   : FVisibleStart := IncYear(FVisibleStart,1);
    csWeek    : FVisibleStart := IncMonth(FVisibleStart,1);
    csDay     : FVisibleStart := IncMonth(FVisibleStart,1);
  end;
  if (FVisibleStart < 0) or (FVisibleStart > MaxDate) then
    FVisibleStart := MaxDate;
end;

procedure TArcIWStringGridCalendarContent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

end;

procedure TArcIWStringGridCalendarContent.Post;
begin
  raise Exception.Create(SNotSupported);
end;

procedure TArcIWStringGridCalendarContent.Prior;
begin
  case FScope of
    csYear    : FVisibleStart := IncYear(FVisibleStart,-1);
    csQuarter : FVisibleStart := IncMonth(FVisibleStart,-3);
    csMonth   : FVisibleStart := IncMonth(FVisibleStart,-1);
    csWeek    : FVisibleStart := IncWeek(FVisibleStart,-1);
    csDay     : FVisibleStart := IncDay(FVisibleStart,-1);
  end;
  if FVisibleStart < 0 then
    FVisibleStart := 0;
end;

procedure TArcIWStringGridCalendarContent.PriorPage;
begin
  case FScope of
    csYear    : FVisibleStart := IncYear(FVisibleStart,-1);
    csQuarter : FVisibleStart := IncYear(FVisibleStart,-1);
    csMonth   : FVisibleStart := IncYear(FVisibleStart,-1);
    csWeek    : FVisibleStart := IncMonth(FVisibleStart,-1);
    csDay     : FVisibleStart := IncMonth(FVisibleStart,-1);
  end;
  if FVisibleStart < 0 then
    FVisibleStart := 0;
end;

function TArcIWStringGridCalendarContent.ProcessCaption(str: string): string;
var
  sDate : string;
begin
  DateTimeToString(sDate,FCaptionFormat,FVisibleStart);
  Result := FastReplace(str, '<#>', sDate, true);
end;

procedure TArcIWStringGridCalendarContent.Refresh;
begin
  inherited;

end;

procedure TArcIWStringGridCalendarContent.Select(dt: TDateTime);
var
  p : PDateTimeHolder;
begin
  New(p);
  p^.DateTime := dt;
  FSelected.Add(p);
end;

procedure TArcIWStringGridCalendarContent.SetActive(const Value: Boolean);
begin
  inherited;

end;

procedure TArcIWStringGridCalendarContent.SetCaptionFormat(const Value: string);
begin
  FCaptionFormat := Value;
end;

procedure TArcIWStringGridCalendarContent.SetEvents(
  const Value: TArcCalendarEvents);
begin
  FEvents.Assign(Value);
end;

procedure TArcIWStringGridCalendarContent.SetLabels(
  const Value: TArcCalendarLabels);
begin
  FLabels.Assign(Value);
end;

procedure TArcIWStringGridCalendarContent.SetScope(
  const Value: TArcCalendarScope);
begin
  if FCaptionFormat = CFDefault(FScope) then
    FCaptionFormat := CFDefault(Value);
  FScope := Value;
end;

procedure TArcIWStringGridCalendarContent.SetSelectedDataRow(
  const Value: Integer);
begin
  inherited;

end;

function TArcIWStringGridCalendarContent.StartOfWeek: integer;
begin
  Result := Integer(FWeekStart)+1;
end;

procedure TArcIWStringGridCalendarContent.UpdateNavButtonState;
begin
  inherited;

end;

{ TArcCalendarEvent }

procedure TArcCalendarEvent.AssignTo(Dest: TPersistent);
begin
  TArcCalendarEvent(Dest).FHint := FHint;
  TArcCalendarEvent(Dest).FTitle := FTitle;
  TArcCalendarEvent(Dest).FDescription := FDescription;
  TArcCalendarEvent(Dest).FScheduled := FScheduled;
  TArcCalendarEvent(Dest).FStyle.Assign(FStyle);
  TArcCalendarEvent(Dest).FIcon.Assign(FIcon);
end;

constructor TArcCalendarEvent.Create(Collection: TCollection);
begin
  inherited;
  FStyle := TArcGridStyle.Create(False, False, False, False);
  FStyle.BackgroundColor := clNone;
  FStyle.BorderStyle.Style := brdNone;
  FStyle.Font.FontName := 'Arial';
  FStyle.Font.Size := 9;
  FIcon := TIWFileReference.Create;
  FIconWidth := 16;
  FIconHeight := 16;
end;

destructor TArcCalendarEvent.Destroy;
begin
  FStyle.Free;
  FIcon.Free;
  inherited;
end;

function TArcCalendarEvent.IconLocation(URLBase: string): string;
begin
  Result := FIcon.Location(URLBase);
end;

function TArcCalendarEvent.GetDisplayName: string;
begin
  Result := FTitle;
  if Result = '' then
    Result := '(untitled)';
end;

function TArcCalendarEvent.GetScheduledDate: TDate;
begin
  Result := Trunc(Scheduled);
end;

function TArcCalendarEvent.GetScheduledTime: TTime;
begin
  Result := Scheduled-Trunc(Scheduled);
end;

procedure TArcCalendarEvent.SetIcon(const Value: TIWFileReference);
begin
  FIcon := Value;
end;

procedure TArcCalendarEvent.SetStyle(const Value: TArcGridStyle);
begin
  FStyle := Value;
end;

{ TArcCalendarEvents }

function TArcCalendarEvents.Add: TArcCalendarEvent;
begin
  Result := TArcCalendarEvent(inherited Add);
end;

constructor TArcCalendarEvents.Create(content: TArcIWStringGridCalendarContent);
begin
  inherited Create(content, TArcCalendarEvent);
  FContent := Content;
end;

destructor TArcCalendarEvents.Destroy;
begin

  inherited;
end;

function TArcCalendarEvents.GetItem(idx: integer): TArcCalendarEvent;
begin
  Result := TArcCalendarEvent(inherited Items[idx]);
end;

procedure TArcCalendarEvents.SetItem(idx: integer;
  const Value: TArcCalendarEvent);
begin
  inherited Items[idx] := Value;
end;

{ TArcCalendarLabelsMonths }

procedure TArcCalendarLabelsMonths.AssignTo(Dest: TPersistent);
begin
  TArcCalendarLabelsMonths(Dest).FApril := FApril;
  TArcCalendarLabelsMonths(Dest).FApr := FApr;
  TArcCalendarLabelsMonths(Dest).FNovember := FNovember;
  TArcCalendarLabelsMonths(Dest).FNov := FNov;
  TArcCalendarLabelsMonths(Dest).FMay := FMay;
  TArcCalendarLabelsMonths(Dest).FMa := FMa;
  TArcCalendarLabelsMonths(Dest).FDecember := FDecember;
  TArcCalendarLabelsMonths(Dest).FDec := FDec;
  TArcCalendarLabelsMonths(Dest).FAugust := FAugust;
  TArcCalendarLabelsMonths(Dest).FAug := FAug;
  TArcCalendarLabelsMonths(Dest).FFebruary := FFebruary;
  TArcCalendarLabelsMonths(Dest).FFeb := FFeb;
  TArcCalendarLabelsMonths(Dest).FJune := FJune;
  TArcCalendarLabelsMonths(Dest).FJun := FJun;
  TArcCalendarLabelsMonths(Dest).FMarch := FMarch;
  TArcCalendarLabelsMonths(Dest).FMar := FMar;
  TArcCalendarLabelsMonths(Dest).FJuly := FJuly;
  TArcCalendarLabelsMonths(Dest).FJul := FJul;
  TArcCalendarLabelsMonths(Dest).FOctober := FOctober;
  TArcCalendarLabelsMonths(Dest).FOct := FOct;
  TArcCalendarLabelsMonths(Dest).FSeptember := FSeptember;
  TArcCalendarLabelsMonths(Dest).FSep := FSep;
  TArcCalendarLabelsMonths(Dest).FJanuary := FJanuary;
  TArcCalendarLabelsMonths(Dest).FJan := FJan;
end;

constructor TArcCalendarLabelsMonths.Create;
begin
  inherited Create;

  FApril := 'April';
  FNovember := 'November';
  FMay := 'May';
  FDecember := 'December';
  FAugust := 'August';
  FFebruary := 'February';
  FJune := 'June';
  FMarch := 'March';
  FJuly := 'July';
  FOctober := 'October';
  FSeptember := 'September';
  FJanuary := 'January';

  FApr := 'Apr';
  FNov := 'Nov';
  FMa  := 'May';
  FDec := 'Dec';
  FAug := 'Aug';
  FFeb := 'Feb';
  FJun := 'Jun';
  FMar := 'Mar';
  FJul := 'Jul';
  FOct := 'Oct';
  FSep := 'Sep';
  FJan := 'Jan';
end;

function TArcCalendarLabelsMonths.TextFor(Month : TArcCalendarMonth;
  Abbreviated: boolean): string;
begin
  if Abbreviated then
    case Month of
      cmJanuary: Result := FJan;
      cmFebruary: Result := FFeb;
      cmMarch: Result := FMar;
      cmApril: Result := FApr;
      cmMay: Result := FMay;
      cmJune: Result := FJun;
      cmJuly: Result := FJul;
      cmAugust: Result := FAug;
      cmSeptember: Result := FSep;
      cmOctober: Result := FOct;
      cmNovember: Result := FNov;
      cmDecember: Result := FDec;
    end
  else
    case Month of
      cmJanuary: Result := FJanuary;
      cmFebruary: Result := FFebruary;
      cmMarch: Result := FMarch;
      cmApril: Result := FApril;
      cmMay: Result := FMa;
      cmJune: Result := FJune;
      cmJuly: Result := FJuly;
      cmAugust: Result := FAugust;
      cmSeptember: Result := FSeptember;
      cmOctober: Result := FOctober;
      cmNovember: Result := FNovember;
      cmDecember: Result := FDecember;
    end;
end;

{ TArcCalendarLabelsDays }

procedure TArcCalendarLabelsDays.AssignTo(Dest: TPersistent);
begin
  TArcCalendarLabelsDays(Dest).FSunday := FSunday;
  TArcCalendarLabelsDays(Dest).FSun := FSun;
  TArcCalendarLabelsDays(Dest).FWednesday := FWednesday;
  TArcCalendarLabelsDays(Dest).FWed := FWed;
  TArcCalendarLabelsDays(Dest).FTuesday := FTuesday;
  TArcCalendarLabelsDays(Dest).FTue := FTue;
  TArcCalendarLabelsDays(Dest).FFriday := FFriday;
  TArcCalendarLabelsDays(Dest).FFri := FFri;
  TArcCalendarLabelsDays(Dest).FThursday := FThursday;
  TArcCalendarLabelsDays(Dest).FThu := FThu;
  TArcCalendarLabelsDays(Dest).FMonday := FMonday;
  TArcCalendarLabelsDays(Dest).FMon := FMon;
  TArcCalendarLabelsDays(Dest).FSaturday := FSaturday;
  TArcCalendarLabelsDays(Dest).FSat := FSat;
end;

constructor TArcCalendarLabelsDays.Create;
begin
  inherited Create;
  
  FSunday := 'Sunday';
  FWednesday := 'Wednesday';
  FTuesday := 'Tuesday';
  FFriday := 'Friday';
  FThursday := 'Thursday';
  FMonday := 'Monday';
  FSaturday := 'Saturday';
end;

function TArcCalendarLabelsDays.TextFor(Day: TArcCalendarDay; Abbreviated : boolean): string;
begin
  if Abbreviated then
    case Day of
      cdSunday:    Result := FSun;
      cdMonday:    Result := FMon;
      cdTuesday:   Result := FTue;
      cdWednesday: Result := FWed;
      cdThursday:  Result := FThu;
      cdFriday:    Result := FFri;
      cdSaturday:  Result := FSat;
    end
  else
    case Day of
      cdSunday:    Result := FSunday;
      cdMonday:    Result := FMonday;
      cdTuesday:   Result := FTuesday;
      cdWednesday: Result := FWednesday;
      cdThursday:  Result := FThursday;
      cdFriday:    Result := FFriday;
      cdSaturday:  Result := FSaturday;
    end;
end;

{ TArcCalendarLabels }

constructor TArcCalendarLabels.Create;
begin
  inherited Create;
  FQuarters := TArcCalendarLabelsQuarters.Create;
  FDays := TArcCalendarlabelsDays.Create;
  FMonths := TArcCalendarLabelsMonths.Create;
end;

destructor TArcCalendarLabels.Destroy;
begin
  FQuarters.Free;
  FDays.Free;
  FMonths.Free;
  inherited;
end;

procedure TArcCalendarLabels.SetDays(const Value: TArcCalendarLabelsDays);
begin
  FDays.Assign(Value);
end;

procedure TArcCalendarLabels.SetMonths(const Value: TArcCalendarLabelsMonths);
begin
  FMonths.Assign(Value);
end;

procedure TArcCalendarLabels.SetQuarters(
  const Value: TArcCalendarLabelsQuarters);
begin
  FQuarters.Assign(Value);
end;

{ TArcCalendarLabelsQuarters }

procedure TArcCalendarLabelsQuarters.AssignTo(Dest: TPersistent);
begin
  TArcCalendarLabelsQuarters(Dest).FQtr1 := FQtr1;
  TArcCalendarLabelsQuarters(Dest).FQuarter1 := FQuarter1;
  TArcCalendarLabelsQuarters(Dest).FQtr2 := FQtr2;
  TArcCalendarLabelsQuarters(Dest).FQuarter2 := FQuarter2;
  TArcCalendarLabelsQuarters(Dest).FQtr3 := FQtr3;
  TArcCalendarLabelsQuarters(Dest).FQuarter3 := FQuarter3;
  TArcCalendarLabelsQuarters(Dest).FQtr4 := FQtr4;
  TArcCalendarLabelsQuarters(Dest).FQuarter4 := FQuarter4;
end;

constructor TArcCalendarLabelsQuarters.Create;
begin
  inherited Create;
  FQuarter1 := 'Quarter 1';
  FQtr1 := 'Qtr 1';
  FQuarter2 := 'Quarter 2';
  FQtr2 := 'Qtr 2';
  FQuarter3 := 'Quarter 3';
  FQtr3 := 'Qtr 3';
  FQuarter4 := 'Quarter 4';
  FQtr4 := 'Qtr 4';
end;

function TArcCalendarLabelsQuarters.TextFor(Qtr: TArcCalendarQuarter;
  Abbreviated: boolean): string;
begin
  if Abbreviated then
    case Qtr of
      cqQ1: Result := FQtr1;
      cqQ2: Result := FQtr2;
      cqQ3: Result := FQtr3;
      cqQ4: Result := FQtr4;
    end
  else
    case Qtr of
      cqQ1: Result := FQuarter1;
      cqQ2: Result := FQuarter2;
      cqQ3: Result := FQuarter3;
      cqQ4: Result := FQuarter4;
    end;
end;

end.
