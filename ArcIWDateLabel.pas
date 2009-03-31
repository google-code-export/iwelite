unit ArcIWDateLabel;

interface

uses SysUtils, Classes, IWTypes, IWHTMLTag, IWControl, IWScriptEvents,
  IWRenderContext, IWBaseInterfaces, IWColor, IWFileReference, IWGLobal,
  IWFont, IWForm, ArcIWGridCommon;

type
  TArcDateNotifyEvent = procedure(Sender : TObject; NewDate : TDateTime) of object;

  TArcIWDateDisplay = class(TPersistent)
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
  public
    constructor Create;
    function LookupMonth(Month : integer) : string; overload;
    function LookupMonth(Month : string) : integer; overload;
  published
    property January : string read FJanuary write FJanuary;
    property February : string read FFebruary write FFebruary;
    property March : string read FMarch write FMarch;
    property April : string read FApril write FApril;
    property May : string read FMay write FMay;
    property June : string read FJune write FJune;
    property July : string read FJuly write FJuly;
    property August : string read FAugust write FAugust;
    property September : string read FSeptember write FSeptember;
    property October : string read FOctober write FOctober;
    property November : string read FNovember write FNovember;
    property December : string read FDecember write FDecember;
  end;

  TArcIWDateLabel = class(TIWControl, IIWSubmitControl)
  private
    FDate: TDateTime;
    FShowDay: boolean;
    FShowMonth: boolean;
    FShowYear: boolean;
    FOnDateChanged: TArcDateNotifyEvent;
    FDateDisplay: TArcIWDateDisplay;
    FStyle: TArcGridStyle;
    procedure SetStyle(const Value: TArcGridStyle);
  protected
    FSubmitParam : string;
    function GetSubmitParam: string;
    procedure Submit(const AValue: string);
    function getSubmitProc: TSubmitProc;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
  published
    property Date : TDateTime read FDate write FDate;
    property ShowMonth : boolean read FShowMonth write FShowMonth;
    property ShowDay : boolean read FShowDay write FShowDay;
    property ShowYear : boolean read FShowYear write FShowYear;
    property OnDateChanged : TArcDateNotifyEvent read FOnDateChanged write FOnDateChanged;
    property DateDisplay : TArcIWDateDisplay read FDateDisplay write FDateDisplay;
    property Style : TArcGridStyle read FStyle write SetStyle;
  end;

implementation

uses ArcFastStrings, DateUtils;

{ TArcIWDateLabel }

constructor TArcIWDateLabel.Create(AOwner: TComponent);
begin
  inherited;
  Width := 120;
  Height := 22;

  ShowDay := True;
  ShowMonth := True;
  ShowYear := True;

  FDateDisplay := TArcIWDateDisplay.Create;
  FStyle := TArcGridStyle.Create(True,True,True,True);
end;

destructor TArcIWDateLabel.Destroy;
begin
  FStyle.Free;
  FDateDisplay.Free;
  inherited;
end;

function TArcIWDateLabel.GetSubmitParam: string;
begin
  Result := FSubmitParam;
end;

function TArcIWDateLabel.getSubmitProc: TSubmitProc;
begin
  Result := Submit;
end;

function TArcIWDateLabel.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
var
  tagYear, tagDay, tagMonth, tagScript : TIWHTMLTag;
  sMonthHTML, sDayHTML, sYearHTML : string;
  i: Integer;
begin
  Result := TIWHTMLTag.CreateHTMLTag('div');
  Result.AddStringParam('style',FStyle.RenderCSS(AContext));

  sMonthHTML :=
    '<select onchange=''sme_'+HTMLName+'=this.value;dme_'+HTMLName+'(true);''>'+
    '<option value ='''+FDateDisplay.FJanuary+'''>'+FDateDisplay.FJanuary+'</option>' +
    '<option value ='''+FDateDisplay.FFebruary+'''>'+FDateDisplay.FFebruary+'</option>' +
    '<option value ='''+FDateDisplay.FMarch+'''>'+FDateDisplay.FMarch+'</option>' +
    '<option value ='''+FDateDisplay.FApril+'''>'+FDateDisplay.FApril+'</option>' +
    '<option value ='''+FDateDisplay.FMay+'''>'+FDateDisplay.FMay+'</option>' +
    '<option value ='''+FDateDisplay.FJune+'''>'+FDateDisplay.FJune+'</option>' +
    '<option value ='''+FDateDisplay.FJuly+'''>'+FDateDisplay.FJuly+'</option>' +
    '<option value ='''+FDateDisplay.FAugust+'''>'+FDateDisplay.FAugust+'</option>' +
    '<option value ='''+FDateDisplay.FSeptember+'''>'+FDateDisplay.FSeptember+'</option>' +
    '<option value ='''+FDateDisplay.FOctober+'''>'+FDateDisplay.FOctober+'</option>' +
    '<option value ='''+FDateDisplay.FNovember+'''>'+FDateDisplay.FNovember+'</option>' +
    '<option value ='''+FDateDisplay.FDecember+'''>'+FDateDisplay.FDecember+'</option>' +
    '</select>';
                                       
  sMonthHTML := FastReplace(sMonthHTML,' value ='''+FDateDisplay.LookupMonth(MonthOf(FDate))+'''',' selected value ='''+FDateDisplay.LookupMonth(MonthOf(FDate))+'''');

  sDayHTML := '<select onchange=''sde_'+HTMLName+'=this.value;dde_'+HTMLName+'(true);''>';
  for i := 1 to DateUtils.DaysInMonth(FDate) do
    sDayHTML := sDayHTML+'<option value ='''+IntToStr(i)+'''>'+IntToStr(i)+'</option>';
  sDayHTML := sDayHTML+'</select>';

  sDayHTML := FastReplace(sDayHTML,' value ='''+IntToStr(DayOf(FDate))+'''',' selected value ='''+IntToStr(DayOf(FDate))+'''');

  sYearHTML := '<input size=4 type=''text'' id='''+HTMLName+'_y'' style=''width:'+IntToStr(FStyle.Font.Size*4)+'px'' onblur=''sye_'+HTMLName+'=this.value;dye_'+HTMLName+'(true);'' value='''+IntToStr(YearOf(FDate))+'''/>';

  tagScript := Result.Contents.AddTag('script');
  tagScript.AddStringParam('language','Javascript');
  tagScript.Contents.AddText(
    'sme_'+HTMLName+'="";'#13#10 +
    'sde_'+HTMLName+'="";'#13#10 +
    'sye_'+HTMLName+'="";'#13#10 +
    'function dme_'+HTMLName+'(bOK){'#13#10 +
    '  if ((bOK) || (sme_'+HTMLName+' == "")) {'#13#10+
    '    if (sme_'+HTMLName+' == "") {'#13#10+
    '      sme_'+HTMLName+' = '+HTMLName+'_Month.innerHTML;'#13#10+
    '      '+HTMLName+'_Month.innerHTML = "'+sMonthHTML+'"'#13#10+
    '    } else {'#13#10+
    '      '+HTMLName+'_Month.innerHTML = sme_'+HTMLName+#13#10 +
    '      SubmitClick('''+HTMLName+''',''m''+sme_'+HTMLName+',true);'#13#10+
    '      sme_'+HTMLName+' = "";'#13#10+
    '    }'#13#10+
    '  }'#13#10+
    '}'#13#10+
    'function dde_'+HTMLName+'(bOK){'#13#10 +
    '  if ((bOK) || (sme_'+HTMLName+' == "")) {'#13#10+
    '    if (sde_'+HTMLName+' == "") {'#13#10+
    '      sde_'+HTMLName+' = '+HTMLName+'_Day.innerHTML;'#13#10+
    '      '+HTMLName+'_Day.innerHTML = "'+sDayHTML+'"'#13#10+
    '    } else {'#13#10+
    '      '+HTMLName+'_Day.innerHTML = sde_'+HTMLName+#13#10+
    '      SubmitClick('''+HTMLName+''',''d''+sde_'+HTMLName+',true);'#13#10+
    '      sde_'+HTMLName+' = "";'#13#10+
    '    }'#13#10+
    '  }'#13#10+
    '}'#13#10+
    'function dye_'+HTMLName+'(bOK){'#13#10 +
    '  if ((bOK) || (sme_'+HTMLName+' == "")) {'#13#10+
    '    if (sye_'+HTMLName+' == "") {'#13#10+
    '      sye_'+HTMLName+' = '+HTMLName+'_Year.innerHTML;'#13#10+
    '      '+HTMLName+'_Year.innerHTML = "'+sYearHTML+'"'#13#10+
    '      document.getElementById('''+HTMLName+'_y'').focus();'+
    '      document.getElementById('''+HTMLName+'_y'').select();'#13#10+
    '    } else {'#13#10+
    '      '+HTMLName+'_Year.innerHTML = sye_'+HTMLName+#13#10+
    '      SubmitClick('''+HTMLName+''',''y''+sye_'+HTMLName+',true);'#13#10+
    '      sye_'+HTMLName+' = "";'#13#10+
    '    }'#13#10+
    '  }'#13#10+
    '}'#13#10
    );

  if FShowMonth then
  begin
    tagMonth := Result.Contents.AddTag('div');
    tagMonth.AddStringParam('id',HTMLName+'_Month');
    tagMonth.AddStringParam('onclick','dme_'+HTMLName+'();');
    tagMonth.AddStringParam('style','display:inline;padding-right:4px;');
    tagMonth.Contents.AddText(FDateDisplay.LookupMonth(MonthOf(FDate)));
    if FShowYear and (not FShowDay) then
      tagMonth.Contents.AddText(',');
  end;

  if FShowDay then
  begin
    tagDay := Result.Contents.AddTag('div');
    tagDay.AddStringParam('id',HTMLName+'_Day');
    tagDay.AddStringParam('onclick','dde_'+HTMLName+'();');
    tagDay.AddStringParam('style','display:inline;padding-right:4px;');
    tagDay.Contents.AddText(IntToStr(DayOf(FDate)));
    if FShowYear then
      tagDay.Contents.AddText(',');
  end;

  if FShowYear then
  begin
    tagYear := Result.Contents.AddTag('div');
    tagYear.AddStringParam('id',HTMLName+'_Year');
    tagYear.AddStringParam('onclick','dye_'+HTMLName+'();');
    tagYear.AddStringParam('style','display:inline;');
    tagYear.Contents.AddText(IntToStr(YearOf(FDate)));
  end;  
end;

procedure TArcIWDateLabel.SetStyle(const Value: TArcGridStyle);
begin
  FStyle.Assign(Value);
end;

procedure TArcIWDateLabel.Submit(const AValue: string);
var
  sVal : String;
begin
  if AValue <> '' then
  begin
    sVal := Copy(AValue,2,High(Integer));
    if (sVal <> '') and (sVal[length(sVal)] = ',') then
      Delete(sVal,length(sVal),1);

    case AValue[1] of
      'm': FDate := DateUtils.EncodeDateTime(YearOf(FDate),FDateDisplay.LookupMonth(sVal),DayOf(FDate),HourOf(FDate),MinuteOf(FDate),SecondOf(FDate),MillisecondOf(FDate));
      'd': FDate := DateUtils.EncodeDateTime(YearOf(FDate),MonthOf(FDate),StrToInt(sVal),HourOf(FDate),MinuteOf(FDate),SecondOf(FDate),MillisecondOf(FDate));
      'y': begin
        if (DayOf(FDate)=29) and (MonthOf(FDate)=2) then
        begin
          if not DateUtils.IsInLeapYear(StrToInt(sVal)) then
            FDate := DateUtils.IncDay(FDate,-1);
        end;

        FDate := DateUtils.EncodeDateTime(StrToInt(sVal),MonthOf(FDate),DayOf(FDate),HourOf(FDate),MinuteOf(FDate),SecondOf(FDate),MillisecondOf(FDate));
      end;
    end;
    if Assigned(FOnDateChanged) then
      FOnDateChanged(Self,FDate);
  end;
  FSubmitParam := AValue;
end;

{ TArcIWDateDisplay }

constructor TArcIWDateDisplay.Create;
begin
  FJanuary := 'January' ;
  FFebruary := 'February' ;
  FMarch := 'March' ;
  FApril := 'April';
  FMay := 'May' ;
  FJune := 'June' ;
  FJuly := 'July' ;
  FAugust := 'August' ;
  FSeptember := 'September' ;
  FOctober := 'October' ;
  FNovember := 'November' ;
  FDecember := 'December' ;
end;

function TArcIWDateDisplay.LookupMonth(Month : integer) : string;
begin
  case Month of
    1: Result := FJanuary;
    2: Result := FFebruary;
    3: Result := FMarch;
    4: Result := FApril;
    5: Result := FMay;
    6: Result := FJune;
    7: Result := FJuly;
    8: Result := FAugust;
    9: Result := FSeptember;
    10: Result := FOctober;
    11: Result := FNovember;
    12: Result := FDecember;
  end;
end;

function TArcIWDateDisplay.LookupMonth(Month: string): integer;
begin
  Result := -1;
  if Month = FJanuary then
    Result := 1
  else if Month = FFebruary then
    Result := 2
  else if Month = FMarch then
    Result := 3
  else if Month = FApril then
    Result := 4
  else if Month = FMay then
    Result := 5
  else if Month = FJune then
    Result := 6
  else if Month = FJuly then
    Result := 7
  else if Month = FAugust then
    Result := 8
  else if Month = FSeptember then
    Result := 9
  else if Month = FOctober then
    Result := 10
  else if Month = FNovember then
    Result := 11
  else if Month = FDecember then
    Result := 12;
end;

end.
