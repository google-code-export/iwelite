unit ArcIWTabRegion;

interface

{$I IntrawebVersion.inc}

uses SysUtils, Classes, IWRenderContext, IWHTMLTag, IWRegion, IWFont, IWColor,
  IWBaseInterfaces, Messages, ArcCommon;

type
  TArcTabNotifyEvent = procedure(Sender : TObject; TabIndex : integer; Caption : string) of object;
  TArcIWTabRegion = class(TIWRegion, IIWInputControl, IIWSubmitControl)
  private
    FTabs: TStrings;
    FOnTabSelected: TArcTabNotifyEvent;
    //FOnAsyncTabSelected: TArcTabNotifyEvent;
    FTabIndex: integer;
    FColorInactive: TIWColor;
    FTabFont: TIWFont;
    FTabHeight: integer;
    FSubmitParam: string;
    FColorHover: TIWColor;
    procedure SetTabs(const Value: TStrings);
    procedure SetTabIndex(const Value: integer);
    procedure SetTabFont(const Value: TIWFont);
  protected
    procedure OnParentNotify(var msg : TWMParentNotify); message WM_PARENTNOTIFY;
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    function SupportsSubmit: Boolean;
    procedure GetInputControlNames(ANames: TStringList);
    function IsForThisControl(AName: string): Boolean;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Submit(const AValue: string);
    function getSubmitProc: TSubmitProc;
    function GetSubmitParam: string;
    property TabHeight : integer read FTabHeight write FTabHeight;
  published
    property Tabs : TStrings read FTabs write SetTabs;
    property TabIndex : integer read FTabIndex write SetTabIndex;
    property OnTabSelected : TArcTabNotifyEvent read FOnTabSelected write FOnTabSelected;
    //property OnAsyncTabSelected : TArcTabNotifyEvent read FOnAsyncTabSelected write FOnAsyncTabSelected;
    property ColorInactive : TIWColor read FColorInactive write FColorInactive;
    property ColorHover : TIWColor read FColorHover write FColorHover;
    property TabFont : TIWFont read FTabFont write SetTabFont;
  end;

implementation

uses Windows, Graphics;

{ TArcIWTabRegion }

constructor TArcIWTabRegion.Create(AOwner: TComponent);
begin
  inherited;
  FTabs := TStringlist.Create;
  FTabFont := TIWFont.Create;
  FTabFont.Color := RGB($E8,$E9,$BE);
  FTabFont.Style := [fsBold];
  FTabFont.Size := 16;
  FTabFont.FontFamily := 'Trebuchet MS, Arial, sans-serif';

  FTabIndex := -1;
  FTabHeight := 36;
  FColorInactive := RGB($DE,$DE,$CF);
  Color := RGB($AB,$AD,$85);
  //FColorHover := RGB($AD,$C0,$9F);
  FColorHover := RGB($89,$8B,$5e);
  //#898B5E
end;

destructor TArcIWTabRegion.Destroy;
begin
  FTabFont.Free;
  FTabs.Free;
  inherited;
end;

procedure TArcIWTabRegion.GetInputControlNames(ANames: TStringList);
var
  i: Integer;
begin
  for i := 0 to FTabs.Count - 1 do
    ANames.Add(HTMLName+'_'+IntToStr(i));
end;

function TArcIWTabRegion.GetSubmitParam: string;
begin
  Result := FSubmitParam;
end;

function TArcIWTabRegion.getSubmitProc: TSubmitProc;
begin
  Result := Submit;
end;

function TArcIWTabRegion.IsForThisControl(AName: string): Boolean;
begin
  Result :=  Copy(AName,1,length(HTMLName)+1)=HTMLName+'_';

end;

procedure TArcIWTabRegion.OnParentNotify(var msg: TWMParentNotify);
begin
  if (csDesigning in ComponentState) then
  begin
    case msg.Event of
      WM_CREATE:;
      WM_DESTROY:;
    end;
  end;
end;

function TArcIWTabRegion.RenderHTML(
  AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
var
  tag, tagTabs, tagContents : TIWHTMLTag;
  i: Integer;
  sFont, sStyle, sStyleNew : string;
  iPos, iPosEnd: Integer;
begin
  FTabHeight := FTabFont.Size+12+12+2;

  Result := TIWHTMLTag.CreateHTMLTag('div');
  Result.AddStringParam('id','contents');
  tagContents := inherited RenderHTML(AContext);
  tagContents.AddStringParam('class',HTMLName+'_tabMain');

  sStyleNew := '';
  sStyle := tagContents.Params.Values['style']+'border-top-style:none;';
  {iPos := FastPosNoCase(sStyle,'border-',length(sStyle),7,1);
  while iPos > 0 do
  begin
    iPosEnd := FastCharPos(sStyle,';',iPos);
    sStyleNew := sStyleNew+Copy(sStyle,iPos,iPosEnd-iPos+1);
    Delete(sStyle,iPos,iPosEnd);
    iPos := FastPosNoCase(sStyle,'border-',length(sStyle),7,1);
  end;
  Result.AddStringParam('style',sStyle);}

  tagContents.params.values['style'] := sStyle+'height:'+IntToStr(Height-FTabHeight)+'px;';

  sFont := FTabFont.FontToStringStyle(AContext.Browser,get_WebFont);
  
  tag := Result.Contents.AddTag('style');
  tag.Contents.AddText(
    '#tabmenu {color: #000;border-bottom: 2px solid black;margin: 12px 0px 0px 0px;padding: 0px;z-index: '+IntToStr(ZIndex+1)+';padding-left: 10px }'#13#10 +
    '#tabmenu li {display: inline;overflow: hidden;list-style-type: none; }'#13#10 +
    '#tabmenu a, a.active {color: '+ColorToRGBString(FColorInactive)+';background: '+ColorToRGBString(FColorHover)+';font: '+sFont+';border: 2px solid black;padding: 2px 5px 0px 5px;margin: 0;text-decoration: none; }'#13#10 +
    '#tabmenu a.active {background: '+ColorToRGBString(Color)+';border-bottom: 3px solid '+ColorToRGBString(Color)+'; }'#13#10 +
    '#tabmenu a:hover {color: #fff;background: '+ColorToRGBString(FColorHover)+'; }'#13#10 +
    '#tabmenu a:visited {color: '+ColorToRGBString(FTabFont.Color)+'; }'#13#10 +
    '#tabmenu a.active:hover {background: '+ColorToRGBString(Color)+';color: '+ColorToRGBString(FColorInactive)+'; }'#13#10 +
    '#content {font: 0.9em/1.3em "bitstream vera sans", verdana, sans-serif;text-align: justify;background: '+ColorToRGBString(Color)+';padding: 20px;border: 2px solid black;border-top: none;z-index: '+IntToStr(ZIndex+2)+';	}'#13#10+
    '#content a {text-decoration: none;color: '+ColorToRGBString(FTabFont.Color)+'; }'#13#10+
    '#content a:hover { background: '+ColorToRGBString(FColorHover)+'; }'#13#10
    );

  tagTabs := Result.Contents.AddTag('ul');
  tagTabs.AddStringParam('id', 'tabmenu');
  for i := 0 to FTabs.Count - 1 do
  begin
    tag := tagTabs.Contents.AddTag('li').Contents.AddTag('a');
    if i = FTabIndex then
      tag.AddStringParam('class','active');
    if Assigned(FOnTabSelected) then
      tag.AddStringParam('href','javascript:SubmitClickConfirm('''+HTMLNAME+'_'+IntToStr(i)+''','''+IntToStr(i)+''', true, '''');');
    {if Assigned(FOnAsyncTabSelected) then
      tag.AddStringParam('href','javascript:alert(''Clicked AJAX'');');}
    tag.Contents.AddText(FTabs[i]);
  end;

  Result.Contents.AddTagAsObject(tagContents);
end;

procedure TArcIWTabRegion.Submit(const AValue: string);
var
  i : integer;
  iPos: Integer;
begin
  FSubmitParam := AValue;
  if Assigned(FOnTabSelected) then
  begin
    i := StrToInt(AValue);
    if (i >= 0) and (i < FTabs.Count) then
    begin
      FTabIndex := i;
      FOnTabSelected(Self,i,FTabs[i]);
    end;
  end;

end;

function TArcIWTabRegion.SupportsSubmit: Boolean;
begin
  Result := True;
end;

procedure TArcIWTabRegion.SetTabFont(const Value: TIWFont);
begin
  FTabFont.Assign(Value);
end;

procedure TArcIWTabRegion.SetTabIndex(const Value: integer);
begin
  if (Value >= -1) and (Value < FTabs.Count) then
    FTabIndex := Value;
end;

procedure TArcIWTabRegion.SetTabs(const Value: TStrings);
begin
  FTabs.Assign(Value);
end;

end.
