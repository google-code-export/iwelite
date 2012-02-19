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

unit ArcIWRegionGrid;

{$I IntraWebVersion.inc}
{$I Eval.inc}

{$IFDEF INTRAWEB51}
ERROR: This unit is only valid for // IW 7.2
{$ENDIF}

interface

uses
  SysUtils, Classes, Controls, Graphics, IWHTMLTag, IWControl, IWColor, IWTypes,
  IWVCLBaseControl, IWBaseControl, IWBaseHTMLControl, IWRenderContext, IWApplication,
  ArcIWGridCommon, ArcIWCustomGrid, IWRegion, DB, TypInfo, IWBaseRenderContext,
  IWBaseInterfaces, IWBaseContainerLayout, IWLayoutMgrForm, IWVCLComponent, IWBaseForm,
  IWMarkupLanguageTag, Forms, IWContainerLayout, IWBaseHTMLInterfaces,
  IWContainer, IWHTML40Interfaces
  {$IFDEF INTRAWEB120}, IWRenderStream, IWCompExtCtrls, {$ELSE}, IWStreams, IWExtCtrls, {$ENDIF} ArcCommon;

type
  TArcIWRegionGrid = class;
  TIWGridRegion = class;

  TArcAfterCloneRegionEvent = procedure(ASender: TObject; ASource: TIWCustomRegion; ADest: TIWGridRegion) of object;
  TArcAfterCloneComponentEvent = procedure(ASender: TObject; ASource, ADest: TComponent) of object;
  TArcRegionEvent = procedure(ASender: TObject; ARegion: TIWGridRegion) of object;
{AS}TArcGetRecordKeyEvent = procedure(ASender: TObject; ADataset : TDataset; var AKey : integer) of object;
{AS}TArcMoveToRecordEvent = procedure(ASender: TObject; ADataset : TDataset; AKey : integer) of object;

  TCustomRegionHelper = class(TIWCustomRegion)
  end;

  TIWGridRegion = class(TIWRegion, IIWUpdateNotified, IIWSubmitInvisible, IIWBaseHTMLComponent, IIWHTML40Container)
  private
    FRecordNumber: integer;
    FBookmark: TBookmark;
    FGrid: TArcIWRegionGrid;
    function GetClonedControls(OriginalName: string): TComponent;
  protected
    procedure NotifyUpdate(AObject: TObject; AValue: string); virtual;
    function RenderStyle(AComponentContext: TIWBaseHTMLComponentContext): string; override;
    procedure ResetPosition(RgnNo: integer);
  public
    constructor Create(AOwner: TComponent; AGrid: TArcIWRegionGrid; ARecordNumber: Integer; ABookmark: TBookmark); reintroduce; virtual;

    function DoClonedRender(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; virtual;
    property RecordNumber: integer read FRecordNumber write FRecordNumber;
    property Bookmark: TBookmark read FBookmark;
    property Grid: TArcIWRegionGrid read FGrid;
    property ClonedControls[OriginalName: string]: TComponent read GetClonedControls;
  end;

  TArcIWRegionGrid = class(TIWControl)
  private
    FScrollbars: TArcScrollbarStyle;
    FContainerImplementation: TIWContainerImplementation;
    RegionList: TStringList;
    FDataRegion: TIWCustomRegion;
    FDataSource: TDataSource;
    FRecordsShown: Integer;
    FRecordStart: Integer;
    FOnAfterCloneComponent: TArcAfterCloneComponentEvent;
    FOnAfterCloneRegion: TArcAfterCloneRegionEvent;
    FOnBeforeCloneRegion: TNotifyEvent;
    FOnAfterCloneComplete: TArcAfterCloneRegionEvent;
    FOnAfterRenderRegion: TArcRegionEvent;
    FOnBeforeRenderRegion: TArcRegionEvent;
    FCacheRegions: Boolean;
    FOnFreeClonedRegion: TArcRegionEvent;
    FRegionsPerRow: integer;
    FOnGetRecordKey : TArcGetRecordKeyEvent;
    FOnMoveToRecord : TArcMoveToRecordEvent;
    procedure SetDataRegion(const Value: TIWCustomRegion);
    procedure SetRecordsShown(const Value: Integer);
    procedure SetRecordStart(const Value: Integer);
    function GetVisibleRegions(idx: integer): TIWGridRegion;
    function GetClonedRegionCount: integer;
    function GetClonedRegions(idx: integer): TIWGridRegion;
    function GetVisibleRegionCount: integer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoBeforeCloneRegion;
    procedure DoAfterCloneRegion(ASource: TIWCustomRegion; ADest: TIWGridRegion); virtual;
    procedure DoAfterCloneComplete(ASource: TIWCustomRegion; ADest: TIWGridRegion); virtual;
    procedure DoAfterCloneComponent(ASource, ADest: TComponent); virtual;
    procedure DoBeforeRenderRegion(ARegion: TIWGridRegion); virtual;
    procedure DoAfterRenderRegion(ARegion: TIWGridRegion); virtual;
    procedure DoFreeClonedRegion(ARegion: TIWGridRegion); virtual;
    procedure FreeChildRegions; virtual;
    procedure SetValue(const AValue: string); override;
    function SupportsInput: Boolean; override;
    procedure PositionDatasetForRegion(ARegion: TIWGridRegion); virtual;
    procedure Dispose(ADispose: Boolean); override;
  public
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    constructor Create(AOwner: TComponent); override;
    function RenderStyle(AComponentContext: TIWBaseHTMLComponentContext): string; override;
    procedure NextPage; virtual;
    procedure PriorPage; virtual;
    procedure EmptyRegionCache;
    function IsPriorPage: boolean; virtual;
    function IsNextPage: boolean; virtual;
    function FindRegionByRecord(ARecNo: integer): TIWGridRegion; virtual;
    property VisibleRegions[idx: integer]: TIWGridRegion read GetVisibleRegions;
    property VisibleRegionCount: integer read GetVisibleRegionCount;
    property ClonedRegions[idx: integer]: TIWGridRegion read GetClonedRegions;
    property ClonedRegionCount: integer read GetClonedRegionCount;
    function NeedsFormTag: Boolean;

  published
    property RegionsPerRow: integer read FRegionsPerRow write FRegionsPerRow default 1;
    property DataRegion: TIWCustomRegion read FDataRegion write SetDataRegion;
    property DataSource: TDataSource read FDataSource write FDataSource;
    property RecordStart: Integer read FRecordStart write SetRecordStart;
    property RecordsShown: Integer read FRecordsShown write SetRecordsShown;
    property Scrollbars: TArcScrollbarStyle read FScrollbars write FScrollbars;
    property OnBeforeCloneRegion: TNotifyEvent read FOnBeforeCloneRegion write FOnBeforeCloneRegion;
    property OnBeforeRenderRegion: TArcRegionEvent read FOnBeforeRenderRegion write FOnBeforeRenderRegion;
    property OnAfterRenderRegion: TArcRegionEvent read FOnAfterRenderRegion write FOnAfterRenderRegion;
    property OnAfterCloneRegion: TArcAfterCloneRegionEvent read FOnAfterCloneRegion write FOnAfterCloneRegion;
    property OnAfterCloneComponent: TArcAfterCloneComponentEvent read FOnAfterCloneComponent write FOnAfterCloneComponent;
    property OnAfterCloneComplete: TArcAfterCloneRegionEvent read FOnAfterCloneComplete write FOnAfterCloneComplete;
    property CacheRegions: Boolean read FCacheRegions write FCacheRegions;
    property OnFreeClonedRegion: TArcRegionEvent read FOnFreeClonedRegion write FOnFreeClonedRegion;
    property OnGetRecordKey : TArcGetRecordKeyEvent read FOnGetRecordKey write FOnGetRecordKey;
    property OnMoveToRecord : TArcMoveToRecordEvent read FOnMoveToRecord write FOnMoveToRecord;
  end;

implementation

uses {$IFNDEF VER130}StrUtils, Math, MaskUtils, {$ENDIF}IWHTMLContainer, SyncObjs;

var
  ControlCounter: integer;
  ControlCounterCS: TCriticalSection;

function GetControlCounter: string;
begin
  ControlCounterCS.Enter;
  try
    Inc(ControlCounter);
    Result := IntToStr(ControlCounter);
  finally
    ControlCounterCS.Leave;
  end;
end;

{ TArcIWRegionGrid }

constructor TArcIWRegionGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScrollbars := scrBoth;
  RegionList := TStringList.Create;
  RegionList.Sorted := True;
  FCacheRegions := True;
  FRecordsShown := 20;
  FRecordStart := 1;
  FNeedsFormTag := False;
  FRegionsPerRow := 1;
  FContainerImplementation := TIWContainerImplementation.Create(Self);
end;

function TArcIWRegionGrid.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
  function RenderScrollbarStyle: string;
  begin
{$IFDEF INTRAWEB51}
    case FScrollbars of
      scrNone: result := 'overflow:hidden;';
      scrHorizontal: result := IfThen(WebApplication.Browser in [brIE], 'overflow-y:hidden;overflow-x: auto;', 'overflow:auto;');
      scrVertical: result := IfThen(WebApplication.Browser in [brIE], 'overflow-x:hidden;overflow-y: auto;', 'overflow:auto;');
      scrBoth: result := 'overflow:auto;';
    end;
{$ELSE}
    case FScrollbars of
      scrNone: result := 'overflow:hidden;';
      scrHorizontal: result := IfThen(BrowserIsIE(AContext.Browser), 'overflow-y:hidden;overflow-x: auto;', 'overflow:auto;');
      scrVertical: result := IfThen(BrowserIsIE(AContext.Browser), 'overflow-x:hidden;overflow-y: auto;', 'overflow:auto;');
      scrBoth: result := 'overflow:auto;';
    end;
{$ENDIF}
  end;
// {ORIG} function CreateClonedRegion(RecordNum: integer; Bookmark, RegionName: string): TIWGridRegion;
   {AS}   function CreateClonedRegion(RecordNum: integer; RegionNum : integer; Bookmark: TBookmark;RegionName: string): TIWGridRegion;
  {$IFNDEF CLR}
    procedure CopyProperties(src, dest: TObject; PropOnly: boolean = false);
    var
      ppl: PPropList;
      i, iCnt: integer;
    begin
      if src = nil then
        exit;

      {$IFDEF VER130}
      iCnt := ArcIWCustomGrid.GetPropList(src, ppl);
      {$ELSE}
      iCnt := GetPropList(src, ppl);
      {$ENDIF}
      try

        if (iCnt = 0) and
          (src is TPersistent) and
          (not (src is TComponent)) then
        begin
          TPersistent(dest).Assign(TPersistent(src));
          exit;
        end;

        for i := 0 to iCnt - 1 do
        begin
          if not IsStoredProp(src, ppl[i]^.Name) then
            continue;
          if (ppl[i]^.Name <> 'Name') then
          begin
            case ppl[i]^.PropType^^.Kind of
              tkClass:
                begin
                  if PropOnly then
                    continue;

                  if GetObjectProp(dest, ppl[i]^.Name) = nil then
                    SetObjectProp(dest, ppl[i]^.Name, GetObjectProp(src, ppl[i]^.Name))
                  else
                    CopyProperties(GetObjectProp(src, ppl[i]^.Name), GetObjectProp(dest, ppl[i]^.Name));
                end;
              tkMethod:
                begin
                   //I find it seem that Set TIWImageFile OnMouseDown property will cause OnClick be set the same value. So I filter OnMouseDown if there already have OnClick event.
                   //In fact, I guess there only one event will be fired when TIWImageFile OnMouseDown and OnClick all are created. Please see this in TIWCustomImage.ImageHTML.
                   //comment by peter 2005/05/10
                  if (src is TIWImageFile)
                    and (ppl[i]^.Name = 'OnMouseDown')
                    and Assigned((src as TIWImageFile).OnClick) then
                    continue;

                  SetMethodProp(dest, ppl[i]^.Name, GetMethodProp(src, ppl[i]^.Name));
                end;
            else SetPropValue(dest, ppl[i]^.Name, GetPropValue(src, ppl[i]^.Name));
            end;
          end;
        end;
      finally
        FreeMem(ppl)
      end;
      if IsPublishedProp(dest, 'ItemIndex') then // copy this last
        SetPropValue(dest, 'ItemIndex', GetPropValue(src, 'ItemIndex'));
    end;
  {$ELSE}
    procedure CopyProperties(src, dest: TObject; PropOnly: boolean = false);
    var
      ppl: PPropList;
      i, iCnt: integer;
    begin
      if src = nil then
        exit;

      ppl := GetPropList(src);
      iCnt := Length(ppl);

      if (iCnt = 0) and
        (src is TPersistent) and
        (not (src is TComponent)) then
      begin
        TPersistent(dest).Assign(TPersistent(src));
        exit;
      end;

      for i := 0 to iCnt - 1 do
      begin
        if ppl[i].Name <> 'Name' then
        begin
          case ppl[i].PropType.Kind of
            tkClass:
              begin
                if PropOnly then
                  continue;

                if GetObjectProp(dest, ppl[i].Name) = nil then
                  SetObjectProp(dest, ppl[i].Name, GetObjectProp(src, ppl[i].Name))
                else
                  CopyProperties(GetObjectProp(src, ppl[i].Name), GetObjectProp(dest, ppl[i].Name));
              end;
            tkMethod:
              begin
                SetMethodProp(dest, ppl[i].Name, GetMethodProp(src, ppl[i].Name));
              end;
          else SetPropValue(dest, ppl[i].Name, GetPropValue(src, ppl[i].Name));
          end;
        end;
      end;
      if IsPublishedProp(dest, 'ItemIndex') then // copy this last
        SetPropValue(dest, 'ItemIndex', GetPropValue(src, 'ItemIndex'));
    end;
  {$ENDIF}
  var
    i: integer;
    ctrl: TIWControl;
    sCounter: string;
    intfRegion, intfForm: IIWBaseContainer;
  begin
    sCounter := GetControlCounter;
    Result := TIWGridRegion.Create(Owner, Self, RecordNum, Bookmark);
    Result.Name := RegionName + 'qq' + sCounter;

    DoBeforeCloneRegion;
    CopyProperties(FDataRegion, Result, true);
    CopyProperties(FDataRegion.BorderOptions, Result.BorderOptions, true);
    DoAfterCloneRegion(FDataRegion, Result);

    Result.Visible := False;
// {ORIG}Result.ResetPosition(RecordNum - FRecordStart);
   {AS}  Result.ResetPosition(RegionNum);

{$IFDEF CLR}
    intfForm := (Owner as IIWBaseContainer);
{$ELSE}
    Owner.GetInterface(IIWBaseContainer, intfForm);
{$ENDIF}
    intfForm.IWAddComponent(Result);

    for i := 0 to FDataRegion.ControlCount - 1 do
    begin
      if FDataRegion.Controls[i] is TIWContainer then
        continue;
      ctrl := TIWControl(TComponentClass(TControl(FDataRegion.Controls[i]).ClassType).Create(Result));
      ctrl.Parent := Result;
      ctrl.Name := TControl(FDataRegion.Controls[i]).Name + 'c' + RegionName + 'qq' + sCounter;

      CopyProperties(TControl(FDataRegion.Controls[i]), ctrl);
      DoAfterCloneComponent(TControl(FDataRegion.Controls[i]), ctrl);

{$IFDEF CLR}
      intfRegion := (Result as IIWBaseContainer);
{$ELSE}
      Result.GetInterface(IIWBaseContainer, intfRegion);
{$ENDIF}
      intfRegion.IWAddComponent(ctrl);
    end;
    DoAfterCloneComplete(FDataRegion, Result);
  end;
var
  reg: TIWGridRegion;
  intf: IIWBaseContainer;
  iIdx, iCnt: Integer;
  tagReg: TIWHtmlTag;
  sRegion: string;
  sBookmark: TBookmark;
{AS}recKey: integer;
begin
  FNeedsFormTag := False;
  Result := nil;
  if (FDataSource = nil) or (FDataRegion = nil) then
    exit;

  if BrowserIsNetscape6(AContext.Browser) or BrowserIsNetscape7(AContext.Browser) then
    Result := TIWHtmlTag.CreateTag('div')
  else
    Result := TIWHtmlTag.CreateTag('span');
  result.AddStringParam('style', 'width:' + IntToStr(Width) + 'px;Height:' + IntToStr(Height) + 'px;' + RenderScrollbarStyle);

{$IFDEF CLR}
  intf := (Owner as IIWBaseContainer);
{$ELSE}
  Owner.GetInterface(IIWBaseContainer, intf);
{$ENDIF}

  if not FCacheRegions then
    FreeChildRegions;

  if (not Assigned(FDatasource)) or
    (not Assigned(FDatasource.DataSet)) then
    exit;

  if FDataSource.DataSet.Active then
  begin
    if FDataSource.DataSet.State = dsInsert then
    begin
      sRegion := Self.HTMLName + 'new';
      iIdx := RegionList.IndexOf(sRegion);
      if iIdx < 0 then
      begin
// {ORIG}  reg := CreateClonedRegion(FDataSource.DataSet.RecordCount, '', sRegion);
   {AS}    reg := CreateClonedRegion(FDataSource.DataSet.RecordCount, 0, nil, sRegion);
        RegionList.AddObject(sRegion, reg);
      end else
        reg := TIWGridRegion(RegionList.Objects[iIdx]);

      reg.Top := 0;
      DoBeforeRenderRegion(reg);
      tagReg := reg.DoClonedRender(AContext);
      DoAfterRenderRegion(reg);
      Result.Contents.AddTagAsObject(tagReg);
    end else
    begin

      if FDataSource.DataSet.State in [dsEdit, dsInsert] then
        FDataSource.DataSet.Cancel;

      sBookmark := FDataSource.DataSet.Bookmark;
      FDataSource.DataSet.DisableControls;
      try
        FDataSource.DataSet.First;
        if FRecordStart > 1 then
          FDataSource.DataSet.MoveBy(FRecordStart - 1);
        iCnt := 0;
        while not FDataSource.DataSet.EOF do
        begin
{AS ---------------------------------}
          if Assigned(FOnGetRecordKey) then
            FOnGetRecordKey(self, FDataSource.DataSet, recKey)
          else
            recKey := FDataSource.DataSet.RecNo;
{AS ---------------------------------}

// {ORIG} sRegion := Self.HTMLName + 'r' + IntToStr(FDataSource.DataSet.RecNo);
   {AS}   sRegion := Self.HTMLName + 'r' + IntToStr(recKey);

          iIdx := RegionList.IndexOf(sRegion);
          if iIdx < 0 then
          begin

// {ORIG}   reg := CreateClonedRegion(FDataSource.DataSet.RecNo, FDataSource.DataSet.Bookmark, sRegion);
   {AS}     reg := CreateClonedRegion(recKey, iCnt, FDataSource.DataSet.Bookmark, sRegion);
            RegionList.AddObject(sRegion, reg);
          end else
          begin
            reg := TIWGridRegion(RegionList.Objects[iIdx]);
            reg.FBookmark := FDataSource.Dataset.Bookmark;
          end;

          DoBeforeRenderRegion(reg);
          tagReg := reg.DoClonedRender(AContext);
          DoAfterRenderRegion(reg);
          Result.Contents.AddTagAsObject(tagReg);

          inc(iCnt);
          if iCnt >= FRecordsShown then
            break;
          FDataSource.DataSet.Next;
        end;
      finally
        FDataSource.DataSet.EnableControls;
        FDataSource.DataSet.Bookmark := sBookmark;
      end;
    end;
  end;

  //tagTmp := Result;
  //Result := TIWHTMLTag.Create('/Form',cbFalse); // doesn't work.
  //Result.Contents.AddTagAsObject(tagTmp);
  Result.Contents.AddTagAsObject(TIWHTMLTag.CreateHTMLTag('form', cbFalse)).AddStringParam('onsubmit', 'return FormDefaultSubmit();');

end;

procedure TArcIWRegionGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDataRegion) then
    FDataRegion := nil;
  if (Operation = opRemove) and (AComponent = FDataSource) then
    FDataSource := nil;
end;

procedure TArcIWRegionGrid.SetDataRegion(const Value: TIWCustomRegion);
begin
  if ((Value = nil) and (FDataRegion <> nil)) then
  begin
    FDataRegion.Align := alNone;
    FDataRegion.Parent := TWinControl(Owner);
  end;
  FDataRegion := Value;
  if FDataRegion <> nil then
  begin
    FDataRegion.Left := Left;
    FDataRegion.Top := Top;
    //FDataRegion.Width := Width;
    FDataRegion.Visible := False;
  end;
end;

function TArcIWRegionGrid.RenderStyle(
  AComponentContext: TIWBaseHTMLComponentContext): string;
begin
  Result := inherited RenderStyle(AComponentContext);
end;

procedure TArcIWRegionGrid.NextPage;
begin
  RecordStart := RecordStart + FRecordsShown;
end;

procedure TArcIWRegionGrid.PriorPage;
begin
  RecordStart := RecordStart - FRecordsShown;
end;

procedure TArcIWRegionGrid.SetRecordsShown(const Value: Integer);
begin
  FRecordsShown := Value;
  if FRecordsShown < 1 then
    FRecordsShown := 1;
end;

procedure TArcIWRegionGrid.SetRecordStart(const Value: Integer);
begin
  FRecordStart := Value;
  if FRecordStart < 1 then
    RecordStart := 1;
end;

procedure TArcIWRegionGrid.FreeChildRegions;
var
  i, j: integer;
  rgn: TIWGridRegion;
  intfRegion, intfForm: IIWBaseContainer;
begin
{$IFDEF CLR}
  intfForm := (Owner as IIWBaseContainer);
{$ELSE}
  Owner.GetInterface(IIWBaseContainer, intfForm);
{$ENDIF}
  for i := RegionList.Count - 1 downto 0 do
  begin
    rgn := TIWGridRegion(RegionList.Objects[i]);

{$IFDEF CLR}
    intfRegion := (rgn as IIWBaseContainer);
{$ELSE}
    rgn.GetInterface(IIWBaseContainer, intfRegion);
{$ENDIF}

    DoFreeClonedRegion(rgn);
    for j := rgn.ControlCount - 1 downto 0 do
    begin
      rgn.Controls[j].Parent := nil;
      //intfRegion.IWRemoveComponent(rgn.Controls[j]);
    end;

    rgn.Parent := nil;
    intfForm.IWRemoveComponent(TComponent(RegionList.Objects[i]));
    rgn.Free;
    //RegionList.Objects[i].Free;
  end;
  RegionList.Clear;
end;

procedure TArcIWRegionGrid.DoAfterCloneComponent(ASource, ADest: TComponent);
begin
  if Assigned(FOnAfterCloneComponent) then
    FOnAfterCloneComponent(Self, ASource, ADest);
end;

procedure TArcIWRegionGrid.DoAfterCloneRegion(ASource: TIWCustomRegion; ADest: TIWGridRegion);
begin
  if Assigned(FOnAfterCloneRegion) then
    FOnAfterCloneRegion(Self, ASource, ADest);
end;

procedure TArcIWRegionGrid.DoBeforeCloneRegion;
begin
  if Assigned(FOnBeforeCloneRegion) then
    FOnBeforeCloneRegion(Self);
end;

procedure TArcIWRegionGrid.DoAfterCloneComplete(ASource: TIWCustomRegion;
  ADest: TIWGridRegion);
begin
  if Assigned(FOnAfterCloneComplete) then
    FOnAfterCloneComplete(Self, ASource, ADest);
end;

procedure TArcIWRegionGrid.DoAfterRenderRegion(ARegion: TIWGridRegion);
begin
  if Assigned(FOnAfterRenderRegion) then
    FOnAfterRenderRegion(Self, ARegion);
end;

procedure TArcIWRegionGrid.DoBeforeRenderRegion(ARegion: TIWGridRegion);
begin
  if Assigned(FOnBeforeRenderRegion) then
    FOnBeforeRenderRegion(Self, ARegion);
end;

function TArcIWRegionGrid.IsNextPage: boolean;
begin
  if (not Assigned(FDatasource)) or
    (not Assigned(FDatasource.DataSet)) then
    raise Exception.Create('No Datasource or Dataset assigned.');
  //Current last Record No ought to be FRecordStart + FRecordsShown -1 because init FRecordStart = 1
  //comment by peter 2005/05/05
  //Result := Assigned(DataSource.DataSet) and (RecordStart + RecordsShown < DataSource.DataSet.RecordCount);
  Result := Assigned(DataSource.DataSet) and DataSource.DataSet.Active and (RecordStart + RecordsShown - 1 < DataSource.DataSet.RecordCount);
end;

function TArcIWRegionGrid.IsPriorPage: boolean;
begin
  Result := RecordStart > 1;
end;

function TArcIWRegionGrid.GetVisibleRegions(idx: integer): TIWGridRegion;
begin
  Result := FindRegionByRecord(FRecordStart + idx);
end;

function TArcIWRegionGrid.GetClonedRegionCount: integer;
begin
  Result := RegionList.Count;
end;

function TArcIWRegionGrid.GetClonedRegions(idx: integer): TIWGridRegion;
begin
  Result := TIWGridRegion(RegionList.Objects[idx]);
end;

function TArcIWRegionGrid.GetVisibleRegionCount: integer;
begin
  if (not Assigned(FDatasource)) or
    (not Assigned(FDatasource.DataSet)) then
    raise Exception.Create('No Datasource or Dataset assigned.');
  //Current last Record No ought to be FRecordStart + FRecordsShown -1 because init FRecordStart = 1
  //comment by peter 2005/05/05
  //if FRecordStart + FRecordsShown >= FDataSource.DataSet.RecordCount then
  if (FRecordStart + FRecordsShown - 1) >= FDataSource.DataSet.RecordCount then
    Result := FDataSource.DataSet.RecordCount - FRecordStart + 1 // +1 to account for the count and not index.
  else
    Result := FRecordsShown;
end;

function TArcIWRegionGrid.FindRegionByRecord(
  ARecNo: integer): TIWGridRegion;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to RegionList.Count - 1 do
    if TIWGridRegion(RegionList.Objects[i]).RecordNumber = ARecNo then
      result := TIWGridRegion(RegionList.Objects[i]);
end;

procedure TArcIWRegionGrid.EmptyRegionCache;
begin
  FreeChildRegions;
end;

procedure TArcIWRegionGrid.DoFreeClonedRegion(ARegion: TIWGridRegion);
begin
  if Assigned(FOnFreeClonedRegion) then
    FOnFreeClonedRegion(Self, ARegion);
end;

procedure TArcIWRegionGrid.SetValue(const AValue: string);
begin
  inherited;

end;

function TArcIWRegionGrid.SupportsInput: Boolean;
begin
  Result := True;
end;

function TArcIWRegionGrid.NeedsFormTag: Boolean;
begin
  Result := False;
end;

procedure TArcIWRegionGrid.PositionDatasetForRegion(ARegion: TIWGridRegion);
var
{AS}recKey : integer;
begin
  if (not Assigned(FDatasource)) or
    (not Assigned(FDatasource.DataSet)) then
    raise Exception.Create('No Datasource or Dataset assigned.');

{ORIG -------------------------------}
//  if (ARegion.RecordNumber <> FDataSource.Dataset.RecNo) then
//  begin
//    FDataSource.DataSet.DisableControls;
//    try
//      FDataSource.DataSet.First; // May optomize by doing RecordNumber-RecNo and eliminating the First.
//      FDataSource.DataSet.MoveBy(ARegion.RecordNumber - 1);
//    finally
//      FDataSource.DataSet.EnableControls;
//    end;
//  end;
{ORIG -------------------------------}

{AS ---------------------------------}
  if Assigned(FOnGetRecordKey) then
    FOnGetRecordKey(Self, FDataSource.DataSet, recKey)
  else
    recKey := FDataSource.DataSet.RecNo;

  if(recKey <> ARegion.RecordNumber) then
  begin
    FDataSource.DataSet.DisableControls;
    try
      if Assigned(FOnMoveToRecord) then
        FOnMoveToRecord(self, FDatasource.Dataset, ARegion.RecordNumber)
      else begin
        FDataSource.DataSet.First;
        FDataSource.DataSet.MoveBy(ARegion.RecordNumber - 1);
      end;
    finally
      FDataSource.DataSet.EnableControls;
    end;
  end;
{AS ---------------------------------}
end;

procedure TArcIWRegionGrid.Dispose(ADispose: Boolean);
begin
  FreeAndNil(FContainerImplementation);
  //FreeChildRegions;
  FreeAndNil(RegionList);
  inherited;
end;

{ TIWGridRegion }

constructor TIWGridRegion.Create(AOwner: TComponent; AGrid: TArcIWRegionGrid; ARecordNumber: Integer; ABookmark: TBookmark);
begin
  inherited Create(AOwner);
  FGrid := AGrid;

  ParentChanging(Parent,AGrid);
  //Parent := TWinControl(AGrid);
  FRecordNumber := ARecordNumber;
  FBookmark := ABookmark;
end;

function TIWGridRegion.DoClonedRender(AContext: TIWBaseHTMLComponentContext): TIWHtmlTag;
var
  cc: TIWContainerContext;
begin
  result := RenderHTML(AContext);

  cc := InitContainerContext(AContext.WebApplication);

  RenderComponents(cc, AContext.PageContext);
  RenderStyle(AContext);
  RenderScripts(AContext);
  result.AddStringParam('style', result.Params.Values['style'] + 'position:absolute;left:' + IntToStr(Left) + 'px;Top:' + IntToStr(Top) + 'px;Width: ' + IntToStr(Width) + 'px;Height:' + IntToStr(Height) + 'px;');
end;

function TIWGridRegion.GetClonedControls(OriginalName: string): TComponent;
var
  iLen, i: integer;
begin
  result := nil;
  iLen := Length(OriginalName);
  for i := 0 to ControlCount - 1 do
  begin
    if Copy(Controls[i].Name, 1, iLen) = OriginalName then
      Result := Controls[i];
  end;
end;

procedure TIWGridRegion.NotifyUpdate(AObject: TObject; AValue: string);
begin
  FGrid.PositionDatasetForRegion(Self);
end;

function TIWGridRegion.RenderStyle(AComponentContext: TIWBaseHTMLComponentContext): string;
var
  s: string;
begin
  s := ControlImplementation.RenderStyle(AComponentContext);
  result := s;
end;

procedure TIWGridRegion.ResetPosition(RgnNo: integer);
begin
  if RgnNo >= 0 then
  begin
    Top := (Height * (RgnNo div FGrid.RegionsPerRow));

    while RgnNo > (FGrid.RegionsPerRow - 1) do
      dec(RgnNo, FGrid.RegionsPerRow);
    Left := Width * (RgnNo);
  end else
  begin
    Top := 0;
    Left := 0;
  end;
end;

initialization
  ControlCounterCS := TCriticalSection.Create;

finalization
  ControlCounterCS.Free;

end.

