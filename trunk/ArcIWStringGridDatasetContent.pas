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

unit ArcIWStringGridDatasetContent;

{$I IntraWebVersion.inc}

interface

uses SysUtils, {$IFNDEF VER130}StrUtils, Variants, {$ENDIF}
  Classes, ArcIWStringGridContent, ArcIWStringGrid, controls,
  IWBaseControl, IWCompCheckbox, IWCompEdit, IWCompMemo, IWCompButton,
  Graphics, TypInfo, IWCompListbox, IWBaseInterfaces,
  IWRenderContext, IWHTMLTag, IWTypes, IWColor, db
  {$IFDEF INTRAWEB120}, IWCompExtCtrls, IW.Browser.Browser {$ELSE}, IWExtCtrls {$ENDIF}, ArcCommon;

type
  TIWBaseControlHack = class(TIWBaseControl)
  end;

  TDataFieldName = string;
  TDataFieldItem = class;
  TDatasetEvent = procedure(Sender: TObject; var Dataset: TDataset) of object;

  TPostingGridCellEvent = procedure(Sender: TObject; Dataset: TDataset; x, y: integer; var Value: string; var ContinuePost: boolean) of object;
  TCreateControlEvent = procedure(Sender: TObject; DataField: TDataFieldItem; var Control: TIWBaseControl; var FreeControl: boolean) of object;
  TNeedsRepositioningEvent = procedure(Sender: TObject; Dataset: TDataset; SelectedDataRow: integer; RowData: array of string; var NeedsReposition: boolean) of object;
  TRepositionDatasetEvent = procedure(Sender: TObject; Dataset: TDataset; SelectedDataRow: integer; RowData: array of string; var Handled: boolean) of object;

  TEditStyle = (esReadOnly, esAutoEdit, esExplicitEdit);
  TCellSource = (csCache, csGrid);

  TDataLookupSettings = class(TPersistent)
  private
    FKeyField: TDataFieldName;
    FDataField: TDataFieldName;
    FLookupDataset: TDataset;
    FControlProperty: TControlProperty;
    FKeyValues: TStringList;
    FFieldItem: TDataFieldItem;
    FIndexProperty: TControlProperty;
    FDisplayValues: TStringList;
    FUseDataset: boolean;
    procedure SetLookupDataset(const Value: TDataset);
  public
    constructor Create(FI: TDataFieldItem); reintroduce; virtual;
    destructor Destroy; override;

    function LockDataset: TDataset; virtual;
    procedure UnlockDataset(ADataset: TDataset); virtual;
    property KeyValues: TStringList read FKeyValues;
    property DisplayValues: TStringList read FDisplayValues;
  published
    property UseDataset: boolean read FUseDataset write FUseDataset;
    property Dataset: TDataset read FLookupDataset write SetLookupDataset;
    property ControlProperty: TControlProperty read FControlProperty write FControlProperty;
    property IndexProperty: TControlProperty read FIndexProperty write FIndexProperty;
    property DataField: TDataFieldName read FDataField write FDataField;
    property KeyField: TDataFieldName read FKeyField write FKeyField;
  end;

  TFilterStyle = (fsNone, fsText, fsCombo);
  TOnGetFilterItemsEvent = procedure(Sender: TObject; DataField: TDataFieldItem; Items: TStrings) of object;
  TOnChangeFilterTextEvent = procedure(Sender: TObject; DataField: TDataFieldItem; var FilterText: string) of object;

  TDataFieldItem = class(TCollectionItem)
  private
    FAlwaysRenderControl: boolean;
    FColumnIndex: integer;
    FColumn: string;
    FOnCreateControl: TCreateControlEvent;
    FKeyField: boolean;
    FFilterControl: TIWBaseControl;
    FControl: TIWBaseControl;
    FDataType: TFieldType;
    FReadOnly: boolean;
    FInternalRename: boolean;
    FUseDefaultEditor: boolean;
    FLookup: TDataLookupSettings;
    FControlProperty: TControlProperty;
    FDataField: TDataFieldName;
    FOnUnlockLookupDataset: TDatasetEvent;
    FOnLockLookupDataset: TDatasetEvent;
    FTmpFileExt: string;
    FUpdateControlTag: boolean;
    FPasswordPrompt: boolean;
    FMaxLength: integer;
    FHint: string;
    FFilterStyle: TFilterStyle;
    FOnChangeFilterText: TOnChangeFilterTextEvent;
    FOnGetFilterItems: TOnGetFilterItemsEvent;
    FFilterAllString: string;
    FBoolType: string;
    FDataBoolTrue: string;
    FDataBoolFalse: string;
    procedure SetColumn(const Value: string);
    function GetColumnIndex: integer;
    procedure SetTmpFileExt(const Value: string);
    procedure SetControl(const Value: TIWBaseControl);
    procedure SetFilterStyle(const Value: TFilterStyle);
    procedure SetFilterControl(const Value: TIWBaseControl);
  protected
    function CloneControl(ctrl: TIWBaseControl; Row: integer): TIWBaseControl;
    property InternalRename: boolean read FInternalRename write FInternalRename;
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
    procedure BuildEditor(List: TList; Row: integer; var Ctrl: TIWBaseControl); virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property FilterStyle: TFilterStyle read FFilterStyle write SetFilterStyle;
    property FilterAllString: string read FFilterAllString write FFilterAllString;
    property UseDefaultEditor: boolean read FUseDefaultEditor write FUseDefaultEditor;
    property DataType: TFieldType read FDataType write FDataType;
    property DataBoolTrue: string read FDataBoolTrue write FDataBoolTrue;
    property DataBoolFalse: string read FDataBoolFalse write FDataBoolFalse;
    property KeyField: boolean read FKeyField write FKeyField default False;
    property Column: string read FColumn write SetColumn;
    property ColumnIndex: integer read GetColumnIndex write FColumnIndex default -1;
    property DataField: TDataFieldName read FDataField write FDataField;
    property FilterControl: TIWBaseControl read FFilterControl write SetFilterControl;
    property Control: TIWBaseControl read FControl write SetControl;
    property OnCreateControl: TCreateControlEvent read FOnCreateControl write FOnCreateControl;
    property AlwaysRenderControl: boolean read FAlwaysRenderControl write FAlwaysRenderControl;
    property ReadOnly: boolean read FReadOnly write FReadOnly;
    property ControlProperty: TControlProperty read FControlProperty write FControlProperty;
    property Lookup: TDataLookupSettings read FLookup write FLookup;
    property OnLockLookupDataset: TDatasetEvent read FOnLockLookupDataset write FOnLockLookupDataset;
    property OnUnlockLookupDataset: TDatasetEvent read FOnUnlockLookupDataset write FOnUnlockLookupDataset;
    property OnGetFilterItems: TOnGetFilterItemsEvent read FOnGetFilterItems write FOnGetFilterItems;
    property OnChangeFilterText: TOnChangeFilterTextEvent read FOnChangeFilterText write FOnChangeFilterText;

    property TmpFileExt: string read FTmpFileExt write SetTmpFileExt;
    property UpdateControlTag: boolean read FUpdateControlTag write FUpdateControlTag;
    property MaxLength: integer read FMaxLength write FMaxLength;
    property PasswordPrompt: boolean read FPasswordPrompt write FPasswordPrompt;
    property Hint: string read FHint write FHint;
  end;

  TArcIWStringGridDatasetContent = class;

  TDataFieldsCollection = class(TCollection)
  private
    FContent: TArcIWStringGridDatasetContent;
    function GetFields(idx: integer): TDataFieldItem;
    procedure SetFields(idx: integer; const Value: TDataFieldItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TArcIWStringGridDatasetContent); reintroduce; virtual;
    property Fields[idx: integer]: TDataFieldItem read GetFields write SetFields; default;
    function HasField(fieldname: string): boolean;
    function IndexOf(fieldname: string): integer;
    function FieldFromColumnIndex(idx: integer): TDataFieldItem;
    function Add: TDataFieldItem; virtual;
    property Content: TArcIWStringGridDatasetContent read FContent write FContent;
    function CountKeyFields: integer;
    function ListKeyFields: string;
    function ListKeyValues(Row: integer): Variant;
  end;

  TNotifyDatasetEvent = procedure(Sender: TObject; Dataset: TDataset) of object;
  TRecordNumberEvent = procedure(Sender: TObject; Dataset: TDataset; var RecNo: integer) of object;

  TDefaultStateChange = (scRaiseError, scSave, scCancel);
  TArcIWStringGridDatasetContent = class(TArcIWStringGridContent)
  private
    FDataset: TDataset;
    FOnLockDataset: TDatasetEvent;
    FOnUnlockDataset: TDatasetEvent;
    FPageRecordCount: integer;
    FEditStyle: TEditStyle;
    FDataFields: TDataFieldsCollection;
    FOnPostingGridCell: TPostingGridCellEvent;
    FDatasetState: TDataSetState;
    FDefaultStateChange: TDefaultStateChange;
    FEditors: TList;
    TopVisibleRow: integer;
    FSyncDataset: boolean;
    FOnAfterCancel: TNotifyDatasetEvent;
    FOnAfterPost: TNotifyDatasetEvent;
    FOnBeforeAppend: TNotifyDatasetEvent;
    FOnBeforeDelete: TNotifyDatasetEvent;
    FOnBeforeEdit: TNotifyDatasetEvent;
    FOnAfterEdit: TNotifyDatasetEvent;
    FOnAfterAppend: TNotifyDatasetEvent;
    FOnAfterDelete: TNotifyDatasetEvent;
    FOnBeforeCancel: TNotifyDatasetEvent;
    FOnBeforePost: TNotifyDatasetEvent;
    FOnCheckPosition: TNeedsRepositioningEvent;
    FOnRepositionDataset: TRepositionDatasetEvent;
    FOnBeforeRefreshData: TNotifyDatasetEvent;
    FManageFilters: boolean;
    FOnOverrideRecordNumber: TRecordNumberEvent;
    FOkToRefresh : boolean;

    procedure SetDataset(const Value: TDataset);
    procedure TestBrowseMode;
    procedure TestEditMode;
    procedure TestReadWrite;
    procedure SetSyncDataset(const Value: boolean);
    procedure EnsureBrowseMode;
    procedure SetEditStyle(const Value: TEditStyle);
    procedure SetDataFields(const Value: TDataFieldsCollection);
  protected
    FOldSuppressRowClick: boolean;
    FAvoidReturnToEditMode: boolean;

    {$IFDEF INTRAWEB120}
    LastKnownBrowser: TBrowser;
    {$ELSE}
    LastKnownBrowser: TIWBrowser;
    {$ENDIF}
    FFilterString: string;
    procedure ClearEditorList(All: boolean = false); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function LockDataset: TDataset; virtual;
    procedure UnlockDataset(ADataset: TDataset); virtual;
    procedure ResetGridData; virtual;
    procedure ChangeDatasetState(state: TDatasetState); virtual;
    procedure RenderEditorsForRow(row: integer); virtual;
    function GetIsFirstPage: boolean; override;
    function GetIsFirstRecord: boolean; override;
    function GetIsLastPage: boolean; override;
    function GetIsLastRecord: boolean; override;
    function GetIsNextRecord: boolean; override;
    function GetIsPriorRecord: boolean; override;
    procedure UpdateNavButtonState; override;
    procedure MoveToCurrent(ds: TDataset = nil); virtual;
    procedure FixRecordCount(ds: TDataset = nil); virtual;
    procedure SetActive(const Value: Boolean); override;
    procedure SetSelectedDataRow(const Value: Integer); override;
    procedure MaskTagStyle(ASender: TObject; ATag: TIWHTMLTag);
    function RecNo(ds: TDataset): integer; virtual;
    procedure Loaded; override;
  public
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
    property AvoidReturnToEditMode: boolean read FAvoidReturnToEditMode write FAvoidReturnToEditMode;

    property DatasetState: TDataSetState read FDatasetState;
    property ManageFilters: boolean read FManageFilters write FManageFilters;

    procedure DoAssignGrid(Sender: TObject); override;
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

    procedure Append; override;
    procedure Delete; override;
    procedure Post; override;
    procedure Cancel; override;
    procedure ForcedCancel; virtual;
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

    procedure MoveBy(RecordCount: integer); virtual;
    procedure MoveTo(RecordNumber: integer); virtual;
    procedure SyncToCurrent(Dataset: TDataset = nil); virtual;
    function ProcessCaption(str: string): string; override;

  published
    property SyncDataset: boolean read FSyncDataset write SetSyncDataset;
    property DataFields: TDataFieldsCollection read FDataFields write SetDataFields;
    property EditStyle: TEditStyle read FEditStyle write SetEditStyle;
    property Dataset: TDataset read FDataset write SetDataset;
    property PageRecordCount: integer read FPageRecordCount write FPageRecordCount default 10;
    property OnLockDataset: TDatasetEvent read FOnLockDataset write FOnLockDataset;
    property OnUnlockDataset: TDatasetEvent read FOnUnlockDataset write FOnUnlockDataset;
    property OnPostingGridCell: TPostingGridCellEvent read FOnPostingGridCell write FOnPostingGridCell;
    property OnBeforeAppend: TNotifyDatasetEvent read FOnBeforeAppend write FOnBeforeAppend;
    property OnBeforeEdit: TNotifyDatasetEvent read FOnBeforeEdit write FOnBeforeEdit;
    property OnBeforeDelete: TNotifyDatasetEvent read FOnBeforeDelete write FOnBeforeDelete;
    property OnAfterAppend: TNotifyDatasetEvent read FOnAfterAppend write FOnAfterAppend;
    property OnAfterEdit: TNotifyDatasetEvent read FOnAfterEdit write FOnAfterEdit;
    property OnAfterDelete: TNotifyDatasetEvent read FOnAfterDelete write FOnAfterDelete;
    property OnAfterPost: TNotifyDatasetEvent read FOnAfterPost write FOnAfterPost;
    property OnAfterCancel: TNotifyDatasetEvent read FOnAfterCancel write FOnAfterCancel;
    property OnBeforePost: TNotifyDatasetEvent read FOnBeforePost write FOnBeforePost;
    property OnBeforeCancel: TNotifyDatasetEvent read FOnBeforeCancel write FOnBeforeCancel;
    property OnCheckPosition: TNeedsRepositioningEvent read FOnCheckPosition write FOnCheckPosition;
    property OnRepositionDataset: TRepositionDatasetEvent read FOnRepositionDataset write FOnRepositionDataset;
    property OnBeforeRefreshData: TNotifyDatasetEvent read FOnBeforeRefreshData write FOnBeforeRefreshData;
    property DefaultStateChange: TDefaultStateChange read FDefaultStateChange write FDefaultStateChange;
    property OnOverrideRecordNumber: TRecordNumberEvent read FOnOverrideRecordNumber write FOnOverrideRecordNumber;
  end;

implementation

uses ArcIWCustomGrid, IWApplication;

{$IFDEF VER130}

function TryStrToInt(value: string; var i: integer): boolean;
begin
  result := True;
  try
    i := StrToInt(value);
  except
    result := false;
  end;
end;

function TryStrToBool(const S: string; out Value: Boolean): Boolean;
  function CompareWith(const aArray: array of string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Low(aArray) to High(aArray) do
      if AnsiSameText(S, aArray[I]) then
      begin
        Result := True;
        Break;
      end;
  end;
var
  LResult: integer;
begin
  Result := TryStrToInt(S, LResult);
  if Result then
    Value := LResult <> 0
  else
  begin
    Result := CompareWith(['True']);
    if Result then
      Value := True
    else
    begin
      Result := CompareWith(['False']);
      if Result then
        Value := False;
    end;
  end;
end;
function StrToBool(const S: string): Boolean;
begin
  if not TryStrToBool(S, Result) then
    raise Exception.Create('Invalid boolean value');
end;

{$ENDIF}

{ TArcIWStringGridDatasetContent }

constructor TArcIWStringGridDatasetContent.Create(AOwner: TComponent);
begin
  inherited;
  FEditors := TList.Create;
  FDataset := nil;
  FPageRecordCount := 10;
  SelectedDataRow := -1;
  TopVisibleRow := 0;
  FDataFields := TDataFieldsCollection.Create(Self);
  FDataFields.Content := Self;
  FDatasetState := dsBrowse;
end;

destructor TArcIWStringGridDatasetContent.Destroy;
begin
  ClearEditorList(True);
  FreeAndNil(FDataFields);
  FreeAndNil(FEditors);

  inherited;
end;

procedure TArcIWStringGridDatasetContent.Notification(AComponent: TComponent; Operation: TOperation);
var
  i: integer;
begin
  if (Operation = opRemove) then
  begin
    if (AComponent = GridNoRaise) then
      ClearEditorList(True);
  end;
  inherited;
  if (Operation = opRemove) then
  begin
    if (AComponent = FDataset) then
      FDataset := nil;
    if FDataFields <> nil then
      for i := 0 to FDataFields.Count - 1 do
        //Check FDataFields[i] is nil
        //comment by peter 2005/05/05
        if Assigned(FDataFields[i]) then
        //
        begin
          if AComponent = FDataFields[i].FControl then
            FDataFields[i].FControl := nil;
          if AComponent = FDataFields[i].FFilterControl then
            FDataFields[i].FFilterControl := nil;
          if AComponent = FDataFields[i].Lookup.Dataset then
            FDataFields[i].Lookup.Dataset := nil;
        end;
  end;
end;

function TArcIWStringGridDatasetContent.LockDataset: TDataset;
begin
  result := FDataset;
  if Assigned(FOnLockDataset) then
    FOnLockDataset(Self, Result);

  if not Assigned(Result) then
    raise Exception.Create('Cannot find an available dataset.');

  try
    if not Result.Active then
      Result.Open;
  except
    UnlockDataset(result);
    raise;
  end;
end;

procedure TArcIWStringGridDatasetContent.UnlockDataset(ADataset: TDataset);
begin
  if Assigned(FOnUnlockDataset) then
    FOnUnlockDataset(Self, ADataset)
end;

procedure TArcIWStringGridDatasetContent.Append;
var
  ds: TDataset;
  bNoMove : boolean;
begin
  bNoMove := False;
  //comment by peter 2005/04/29
  if (EditStyle = esAutoEdit) then
  begin
    if FDatasetState = dsInsert then
    begin
      FAvoidReturnToEditMode := True;
      Post;
      //Refresh;

      //exit;
    end;

    FOldSuppressRowClick := Grid.SuppressSelRowClick;
    Grid.SuppressSelRowClick := True;
  end;
  //

  if (EditStyle = esAutoEdit) then
  begin
    EnsureBrowseMode;
  end;

  TestReadWrite;

  if not bNoMove then
    Last;

  if (EditStyle = esAutoEdit) then
  begin
    EnsureBrowseMode;
  end;

  ChangeDatasetState(dsInsert);

  Grid.AddRow;

  Grid.DeselectAll;
  if Grid.RowCount > PageRecordCount then begin
    TopVisibleRow := TopVisibleRow + PageRecordCount;
    Grid.RowCount := Grid.RowCount - PageRecordCount;
  end;

  Grid.SelectedRow[Grid.RowCount - 1] := True;
  SelectedDataRow := TopVisibleRow + Grid.RowCount - 1;

  if FSyncDataset then
  begin
    ds := LockDataset;
    try
      if Assigned(FOnBeforeAppend) then
        FOnBeforeAppend(Self, ds);
      ds.Append;
      if Assigned(FOnAfterAppend) then
        FOnAfterAppend(Self, ds);
    finally
      UnlockDataset(ds);
    end;
  end;

  RenderEditorsForRow(Grid.RowCount - 1);

  //comment peter 2005/05/17
  if Assigned(GridNoRaise) then
    Grid.Focus(SelectedDataRow-TopVisibleRow);
end;

procedure TArcIWStringGridDatasetContent.Cancel;
var
  ds: TDataset;
  iCnt: integer;
begin
  FOkToRefresh := False;
  try
    TestEditMode;

    case DatasetState of
      dsInsert: Grid.DeleteRow(Grid.RowCount - 1, True);
      dsEdit: CacheToGrid(-1, SelectedDataRow - TopVisibleRow);
    end;

    ds := LockDataset;
    try
      if FSyncDataset then
      begin
        if Assigned(FOnBeforeCancel) then
          FOnBeforeCancel(Self, ds);
        ds.Cancel;
        if Assigned(FOnAfterCancel) then
          FOnAfterCancel(Self, ds);
      end;
      FixRecordCount(ds);
      iCnt := ds.RecordCount;
    finally
      UnlockDataset(ds);
    end;

    ChangeDatasetState(dsBrowse);

    if Grid.RowCount = 0 then
      SelectedDataRow := -1
    else
      SelectedDataRow := SelectedDataRow;

    if SelectedDataRow > iCnt - 1 then
    begin
      // this if will be true when an insert was cancelled.
      SelectedDataRow := iCnt - 1;
      if (not FAvoidReturnToEditMode) and (EditStyle = esAutoEdit) then
      begin
        Grid.DeselectAll;
        DoRowClick(Grid, SelectedDataRow - TopVisibleRow);
      end;
    end else if (not FAvoidReturnToEditMode) and (EditStyle = esAutoEdit) then
      Edit;

    FAvoidReturnToEditMode := False;
  finally
    FOkToRefresh := True;
  end;
end;

procedure TArcIWStringGridDatasetContent.Delete;
var
  ds: TDataset;
  iSel: integer;
begin
  if (DatasetState = dsInsert) then
  begin
    Cancel;
    exit;
  end;

  //roll back these code
  //comment by peter 2005/05/05
  if (EditStyle = esAutoEdit) and (DatasetState in [dsEdit, dsInsert]) then
  begin
    EnsureBrowseMode;
  end;
  //

  TestReadWrite;
  TestBrowseMode;

  ds := LockDataset;
  try
    MoveToCurrent(ds);
    if Assigned(FOnBeforeDelete) then
      FOnBeforeDelete(Self, ds);
    ds.Delete;
    if Assigned(FOnAfterDelete) then
      FOnAfterDelete(Self, ds);
  finally
    UnlockDataset(ds);
  end;

  SelectedDataRow := -1;

  ChangeDatasetState(dsBrowse);

  ResetGridData;
  SelectedDataRow := -1;
end;

procedure TArcIWStringGridDatasetContent.DoAfterRenderHTML(
  Sender: TObject);
begin

end;

procedure TArcIWStringGridDatasetContent.DoAssignGrid(Sender: TObject);
begin
  if Sender = nil then
    ClearEditorList(True);
  inherited;
end;

procedure TArcIWStringGridDatasetContent.DoBeforeRenderHTML(
  Sender: TObject; AContext: TIWBaseHTMLComponentContext);
begin
  LastKnownBrowser := AContext.Browser;
end;

procedure TArcIWStringGridDatasetContent.DoCellClick(Sender: TObject;
  const Col, Row: Integer; var Data: string);
begin

end;

procedure TArcIWStringGridDatasetContent.DoCellData(Sender: TObject; x,
  y: Integer; var sValue: string);
begin

end;

procedure TArcIWStringGridDatasetContent.DoClickCaption(Sender: TObject;
  const Col: Integer);
begin

end;

function TArcIWStringGridDatasetContent.DoNeedOnClickEvent(
  Sender: TObject): Boolean;
begin
  Result := True;
end;

procedure TArcIWStringGridDatasetContent.DoPopulateRow(Sender: TObject; const y: Integer; slValues: TStrings);
begin

end;

procedure TArcIWStringGridDatasetContent.DoReloadData(Sender: TObject);
begin
  if Active and (not (csLoading in ComponentState)) then
    ResetGridData;
end;

procedure TArcIWStringGridDatasetContent.DoRender(Sender: TObject);
begin

end;

procedure TArcIWStringGridDatasetContent.DoRetrieveObject(Sender: TObject; x, y: Integer; var ctrl: TControl);
begin
  if Active then
  begin
    if (High(Objects) >= x) and (High(Objects[x]) >= y) then
      ctrl := TControl(Objects[x, y]);
  end;
end;

procedure TArcIWStringGridDatasetContent.DoRowClick(Sender: TObject; const Row: Integer);
begin
  if Active then
  begin
    //roll back code
    //comment by peter 2005/05/05
    EnsureBrowseMode;
    Grid.DeselectAll;
    Grid.SelectedRow[Row] := True;
    SelectedDataRow := TopVisibleRow + Row;
    if FSyncDataset then
      MoveToCurrent;
  end;
end;

procedure TArcIWStringGridDatasetContent.DoSelectCell(Sender: TObject; const x, y: Integer; Value: Boolean);
begin
end;

procedure TArcIWStringGridDatasetContent.DoSelectCol(Sender: TObject; const x: Integer; Value: Boolean);
begin

end;

procedure TArcIWStringGridDatasetContent.DoSelectRow(Sender: TObject; const y: Integer; Value: Boolean);
begin
  if Active then
  begin
    //roll back code
    //comment by peter 2005/05/05
    if not (DatasetState in [dsInsert]) then
    //
    begin
      EnsureBrowseMode;
      SelectedDataRow := TopVisibleRow + y;
      if FSyncDataset then
        MoveToCurrent;
      if (EditStyle = esAutoEdit) then
      begin
        if (DatasetState in [dsInsert, dsEdit]) then
          Cancel;
        Edit;
      end;
    end;
  end;
end;

procedure TArcIWStringGridDatasetContent.FirstPage;
var
  iSel: integer;
begin
  EnsureBrowseMode;

  iSel := SelectedDataRow - TopVisibleRow;
  TopVisibleRow := 0;
  if SelectedDataRow >= 0 then
    SelectedDataRow := iSel;

  ResetGridData;
end;

procedure TArcIWStringGridDatasetContent.LastPage;
var
  ds: TDataset;
  iSel: integer;
begin
  EnsureBrowseMode;

  if Grid.RowCount = 0 then
    exit;

  iSel := SelectedDataRow - TopVisibleRow;

  ds := LockDataset;
  try
    FixRecordCount(ds);

    while TopVisibleRow < (ds.RecordCount - PageRecordCount) do
      inc(TopVisibleRow, PageRecordCount);

    if SelectedDataRow >= 0 then
    begin
      if TopVisibleRow + iSel < ds.RecordCount then
        SelectedDataRow := TopVisibleRow + iSel
      else
        SelectedDataRow := ds.RecordCount - 1;
    end;
  finally
    UnlockDataset(ds);
  end;

  ResetGridData;
end;

procedure TArcIWStringGridDatasetContent.NextPage;
var
  ds: TDataset;
  iSel: integer;
begin
  EnsureBrowseMode;

  iSel := SelectedDataRow - TopVisibleRow;

  ds := LockDataset;
  try
    inc(TopVisibleRow, PageRecordCount);
    FixRecordCount(ds);
    if TopVisibleRow > ds.RecordCount then
      dec(TopVisibleRow, PageRecordCount);

    if SelectedDataRow >= 0 then
    begin
      if TopVisibleRow + iSel < ds.RecordCount then
        SelectedDataRow := TopVisibleRow + iSel
      else
        SelectedDataRow := ds.RecordCount - 1;
    end;
  finally
    UnlockDataset(ds);
  end;
  ResetGridData;
end;

procedure TArcIWStringGridDatasetContent.Post;
  function RetrieveValue(fld: TDataFieldItem; obj: TObject; defValue: string): string;
  var
    o: TObject;
  begin
    Result := '';
    case fld.DataType of
      ftBlob, ftMemo:
        begin
          case GetPropInfo(obj, fld.ControlProperty)^.PropType^^.Kind of
            tkClass:
              begin
                o := GetObjectProp(obj, fld.ControlProperty);
                if o is TStrings then
                  Result := TStrings(o).Text;
              end
          else
            Result := GetStrProp(obj, fld.ControlProperty);
          end;
        end;
      ftBoolean:
        begin
          if GetPropValue(obj, fld.ControlProperty) then
          begin
            if fld.FDataBoolTrue = '' then
            begin
              if (fld.FBoolType = '0') or (fld.FBoolType = '1') then
                Result := '1'
              else
                if (fld.FBoolType = 'T') or (fld.FBoolType = 'F') then
                  Result := 'T'
                else
                  if (fld.FBoolType = 'Y') or (fld.FBoolType = 'N') then
                    Result := 'Y'
                  else
                    if (fld.FBoolType = 'True') or (fld.FBoolType = 'False') then
                      Result := 'True';
            end else
              Result := fld.FDataBoolTrue;
          end else
          begin
            if fld.FDataBoolTrue = '' then
            begin
              if (fld.FBoolType = '0') or (fld.FBoolType = '1') then
                Result := '0'
              else
                if (fld.FBoolType = 'T') or (fld.FBoolType = 'F') then
                  Result := 'F'
                else
                  if (fld.FBoolType = 'Y') or (fld.FBoolType = 'N') then
                    Result := 'N'
                  else
                    if (fld.FBoolType = 'True') or (fld.FBoolType = 'False') then
                      Result := 'False';
            end else
              Result := fld.FDataBoolFalse;
          end;
        end;
    else
      Result := GetStrProp(obj, fld.ControlProperty);
    end;
    if Result = '' then
      Result := defValue;
  end;
  function RetrieveLookupValue(dt: TFieldType; obj: TObject; Lookup: TDataLookupSettings; defValue: string; var IsNull: boolean): string;
  var
    o: TObject;
    idx: integer;
  begin
    IsNull := False;
    o := GetObjectProp(obj, Lookup.ControlProperty);
    if not (o is TStrings) then
      raise Exception.Create('Lookup ControlProperty is not a TStrings descendant.');

    if Lookup.UseDataset then
    begin
      if Lookup.KeyField = '' then
        raise Exception.Create('Lookup KeyField has not been assigned.');
      if Lookup.DataField = '' then
        raise Exception.Create('Lookup DataField has not been assigned.');
      if Lookup.IndexProperty = '' then
        raise Exception.Create('Lookup IndexProperty has not been assigned.');

      idx := GetPropValue(obj, Lookup.IndexProperty);
      if idx < 0 then
      begin
        IsNull := True;
        Result := '';
      end else
        Result := Lookup.KeyValues[idx];
    end else
    begin
      idx := GetPropValue(obj, Lookup.IndexProperty);
      if idx < 0 then
      begin
        IsNull := True;
        Result := '';
      end else
        Result := TStrings(o)[idx];
    end;
  end;
var
  ds: TDataset;
  x, idx: integer;
  bmp: TPicture;
  o: TObject;
  sValue: string;
  bIsNull, bIsPicture, bContinue: boolean;
  ppi : PPropInfo;
begin
  FOkToRefresh := False;
  try
    //comment by peter 2005/04/29
    if (EditStyle = esAutoEdit) then
    begin
      Grid.SuppressSelRowClick := FOldSuppressRowClick;
    end;
    //

    TestEditMode;

    ds := LockDataset;
    try
      if not (ds.State in [dsInsert, dsEdit]) then
      begin
        case FDatasetState of
          dsInsert:
            begin
              if Assigned(FOnBeforeAppend) then
                FOnBeforeAppend(Self, ds);
              ds.Append;
              if Assigned(FOnAfterAppend) then
                FOnAfterAppend(Self, ds);
            end;
          dsEdit:
            begin
              MoveToCurrent(ds);
              if Assigned(FOnBeforeEdit) then
                FOnBeforeEdit(Self, ds);
              ds.Edit;
              if Assigned(FOnAfterEdit) then
                FOnAfterEdit(Self, ds);
            end;
        end;
      end;
      bIsPicture := False;
      for x := 0 to DataFields.Count - 1 do
      begin
        if DataFields[x].ReadOnly then continue;
        if DataFields[x].columnIndex < 0 then continue;
        bIsNull := False;
        if DataFields[x].Lookup.ControlProperty = '' then
        begin
          if DataFields[x].ControlProperty = '' then continue;
          bIsPicture := False;
          ppi := GetPropInfo(Objects[DataFields[x].columnIndex, SelectedDataRow - TopVisibleRow], DataFields[x].ControlProperty);
          if ppi <> nil then          
            case ppi^.PropType^^.Kind of
              tkClass:
                begin
                  o := GetObjectProp(Objects[DataFields[x].columnIndex, SelectedDataRow - TopVisibleRow], DataFields[x].ControlProperty);
                  if o is TPicture then
                    bIsPicture := True;
                end;
            end;
          if not bIsPicture then
            sValue := RetrieveValue(DataFields[x], Objects[DataFields[x].columnIndex, SelectedDataRow - TopVisibleRow], Cells[DataFields[x].columnIndex, SelectedDataRow - TopVisibleRow])
        end else
          sValue := RetrieveLookupValue(DataFields[x].DataType, Objects[DataFields[x].columnIndex, SelectedDataRow - TopVisibleRow], DataFields[x].Lookup, Cells[DataFields[x].columnIndex, SelectedDataRow - TopVisibleRow], bIsNull);

        bContinue := True;
        if Assigned(FOnPostingGridCell) then
          FOnPostingGridCell(Self, ds, x, SelectedDataRow - TopVisibleRow, sValue, bContinue);
        if bContinue then
        begin
          if bIsPicture then
          begin
            bmp := TPicture.Create;
            try
              bmp.LoadFromFile(Cells[DataFields[x].columnIndex, SelectedDataRow - TopVisibleRow]);
              ds.FieldByName(DataFields[x].DataField).Assign(bmp);
            finally
              bmp.Free;
            end;
          end else
          begin
            if not bIsNull then
            begin
              case DataFields[x].DataType of
                ftBCD, ftBytes, ftVarBytes, ftBlob, ftMemo,
                  ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary,
                  ftCursor, ftADT, ftArray, ftReference, ftDataSet,
                  ftOraBlob, ftOraClob, ftInterface, ftIDispatch, ftGuid
                  {$IFNDEF VER130},ftFMTBcd{$ENDIF}:
                  begin
                    if ds.FieldByName(DataFields[x].DataField).AsString <> sValue then
                      ds.FieldByName(DataFields[x].DataField).AsString := sValue;
                  end;
                //set value when ftBoolean
                //comment by peter 2005/04/28
                ftBoolean:
                  begin
                    case ds.FieldByName(DataFields[x].DataField).DataType of
                      ftBoolean:
                        begin
                          if ds.FieldByName(DataFields[x].DataField).AsBoolean <> StrToBool(sValue) then
                            ds.FieldByName(DataFields[x].DataField).AsBoolean := StrToBool(sValue);
                        end;
                      ftInteger, ftSmallint, ftWord, ftLargeint:
                        begin
                          if not StrToBool(sValue) then
                          begin
                            if ds.FieldByName(DataFields[x].DataField).AsInteger <> 0 then
                              ds.FieldByName(DataFields[x].DataField).AsInteger := 0;
                          end
                          else
                          begin
                            if ds.FieldByName(DataFields[x].DataField).AsInteger = 0 then
                              ds.FieldByName(DataFields[x].DataField).AsInteger := 1;
                          end;
                        end;
                      ftString, ftWideString:
                        begin
                          if ds.FieldByName(DataFields[x].DataField).Text <> sValue then
                            ds.FieldByName(DataFields[x].DataField).Text := sValue;
                        end;
                    else
                      ds.FieldByName(DataFields[x].DataField).Text := sValue;
                    end;
                  end;
                //
              else
                if ds.FieldByName(DataFields[x].DataField).Text <> sValue then
                  ds.FieldByName(DataFields[x].DataField).Text := sValue;
              end;
            end else
              ds.FieldByName(DataFields[x].DataField).Clear;
          end;
        end;

        if (DataFields[x].columnIndex >= 0) then
        begin
          if (DataFields[x].Lookup.DataField <> '') then
          begin
            idx := FDataFields[x].Lookup.KeyValues.IndexOf(sValue);
            if idx >= 0 then
              Grid.Cells[FDataFields[x].ColumnIndex, SelectedDataRow - TopVisibleRow] := FDataFields[x].Lookup.DisplayValues[idx]
            else
              Grid.Cells[FDataFields[x].ColumnIndex, SelectedDataRow - TopVisibleRow] := '';
          end else
          begin
            Grid.Cells[FDataFields[x].ColumnIndex, SelectedDataRow - TopVisibleRow] := sValue;
          end;
          Cells[FDataFields[x].ColumnIndex, SelectedDataRow - TopVisibleRow] := sValue;
        end;
      end;
      if Assigned(FOnBeforePost) then
        FOnBeforePost(Self, ds);

      try
        ds.Post;
      except
        try
          ds.Cancel;
        except
        end;
        raise;
      end;

      if Assigned(FOnAfterPost) then
        FOnAfterPost(Self, ds);
    finally
      UnlockDataset(ds);
    end;
    if FEditStyle <> esAutoEdit then
    begin
      if FDatasetState = dsInsert then
      begin
        ChangeDatasetState(dsBrowse);
        ResetGridData;
        Last;
      end else
      begin
        ChangeDatasetState(dsBrowse);
        ResetGridData;
      end;
    end else
    begin
      ChangeDatasetState(dsBrowse);
      ResetGridData;
    end;
    SelectedDataRow := SelectedDataRow;
    if (not FAvoidReturnToEditMode) and (EditStyle = esAutoEdit) then
      Edit;
    FAvoidReturnToEditMode := False;
  finally
    FOkToRefresh := True;
  end;
end;

procedure TArcIWStringGridDatasetContent.PriorPage;
var
  iSel: integer;
begin
  EnsureBrowseMode;
  iSel := SelectedDataRow - TopVisibleRow;
  dec(TopVisibleRow, PageRecordCount);
  if TopVisibleRow < 0 then
    TopVisibleRow := 0;
  if SelectedDataRow >= 0 then
    SelectedDataRow := TopVisibleRow + iSel;
  ResetGridData;
end;

procedure TArcIWStringGridDatasetContent.SetDataset(const Value: TDataset);
begin
  FDataset := Value;
  if FDataset = nil then
    FSyncDataset := False;
  if (not (csDesigning in ComponentState)) and
    (not (csLoading in ComponentState)) and
    Active then
    ResetGridData;
end;

procedure TArcIWStringGridDatasetContent.TestReadWrite;
begin
  if FEditStyle = esReadOnly then
    raise EContentStateError.Create('Cannot edit a readonly dataset.');
end;

procedure TArcIWStringGridDatasetContent.EnsureBrowseMode;
begin
  if (FDatasetState in [dsEdit, dsInsert]) then
  begin
    case FDefaultStateChange of
      scSave:
        begin
          FAvoidReturnToEditMode := True;
          Post;
        end;
      scCancel:
        begin
          FAvoidReturnToEditMode := True;
          Cancel;
        end;
      scRaiseError: TestBrowseMode;
    end;
  end;
end;

procedure TArcIWStringGridDatasetContent.TestBrowseMode;
begin
  if DatasetState <> dsBrowse then
    raise EContentStateError.Create('Dataset not in Browse mode');
end;

procedure TArcIWStringGridDatasetContent.TestEditMode;
begin
  if not (DatasetState in [dsInsert, dsEdit]) then
    raise EContentStateError.Create('Dataset not in Edit or Insert mode');
end;

procedure TArcIWStringGridDatasetContent.ResetGridData;
  function ValueFromField(Dataset: TDataset; x, y: integer): string;
  var
    fld: TField;
  begin
    fld := Dataset.Fields.FindField(DataFields[x].DataField);
    if fld <> nil then
    begin
      case fld.DataType of
        ftBCD, ftBytes, ftVarBytes, ftBlob, ftMemo,
          ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary,
          ftCursor, ftADT, ftArray, ftReference, ftDataSet,
          ftOraBlob, ftOraClob, ftInterface, ftIDispatch, ftGuid
          {$IFNDEF VER130}, ftFMTBcd{$ENDIF}:
          Result := fld.AsString
      else
        if SelectedDataRow - TopVisibleRow = y then
          Result := fld.Text
        else
          Result := fld.DisplayText;
      end;
    end else
      Result := '&nbsp;';
  end;
var
  x, y: integer;
  ds: TDataset;
  di: TDataFieldItem;
  bFindColumns: boolean;
  sc: TArcGridStringColumn;
  bmp: TPicture;
  idx: integer;
  sVal: string;
  obj: TObject;
  ctrl: TIWBaseControl;
  b: boolean;
  FilteredRecordCount: integer;
begin
  if (not Active) or (csDesigning in ComponentState) then
    exit;

  EnsureBrowseMode;
//  -- Problem redrawing after a delete.

  for x := 0 to FDataFields.Count - 1 do
  begin
    if FDataFields[x].Lookup.DataField <> '' then
    begin
      ds := FDataFields[x].Lookup.LockDataset;
      try
        FDataFields[x].Lookup.KeyValues.Clear;
        FDataFields[x].Lookup.DisplayValues.Clear;
        ds.First;
        while not ds.Eof do
        begin
          FDataFields[x].Lookup.KeyValues.Add(ds.FieldByName(FDataFields[x].Lookup.FKeyField).AsString);
          FDataFields[x].Lookup.DisplayValues.Add(ds.FieldByName(FDataFields[x].Lookup.FDataField).DisplayText);
          ds.Next;
        end;
      finally
        FDataFields[x].Lookup.UnlockDataset(ds);
      end;
    end else
      if FDataFields[x].Lookup.ControlProperty <> '' then
      begin
        ctrl := FDataFields[x].Control;
        b := ctrl = nil;
        if Assigned(FDataFields[x].OnCreateControl) then
          FDataFields[x].OnCreateControl(Self, FDataFields[x], ctrl, b);
        if ctrl <> nil then
        begin
          obj := GetObjectProp(ctrl, FDataFields[x].Lookup.ControlProperty);
          if (obj is TStrings) then
            FDataFields[x].Lookup.DisplayValues.Assign(TStrings(obj));
          if b then
            ctrl.Free;
        end;
      end;
  end;
  ds := LockDataset;
  try
    if ManageFilters then
    begin
      if FFilterString <> '' then
      begin
        try
          ds.FilterOptions := [foCaseInsensitive];
        except
        end;
        ds.Filter := FFilterString;
        ds.Filtered := true;
      end else begin
        ds.Filter := '';
        ds.Filtered := False;
      end;
    end;

    if Assigned(FOnBeforeRefreshData) then
      FOnBeforeRefreshData(Self, ds);

    FixRecordCount(ds);

    while (TopVisibleRow > ds.RecordCount - 1) and (TopVisibleRow > 0) do
    begin
      Dec(TopVisibleRow, PageRecordCount);
    end;

    if SelectedDataRow > ds.RecordCount - 1 then
      SelectedDataRow := ds.RecordCount - 1;

    bFindColumns := False;
    if FDataFields.Count = 0 then
    begin
      bFindColumns := True;
      while FDataFields.Count < ds.Fields.Count do
      begin
        di := TDataFieldItem(FDataFields.Add);
        di.DataField := ds.Fields[FDataFields.Count - 1].FieldName;
        di.ColumnIndex := -1;
        di.KeyField := ds.Fields[FDataFields.Count - 1].Lookup;
      end;
    end;

    if Grid.Columns.Count = 0 then
    begin
      while Grid.Columns.Count < FDataFields.Count do
      begin
        sc := Grid.Columns.Add;
        sc.Caption := FDataFields[Grid.Columns.Count - 1].DataField;
        FDataFields[Grid.Columns.Count - 1].ColumnIndex := sc.Index;
      end;
      bFindColumns := False;
    end;

    if bFindColumns then
    begin
      for x := 0 to FDataFields.Count - 1 do
      begin
        di := FDataFields[x];
        for y := 0 to Grid.Columns.Count - 1 do
        begin
          if Grid.Columns[y].Caption = di.DataField then
          begin
            di.ColumnIndex := y;
            break;
          end;
        end;
      end;
    end;
    //fix Grid.RowCount wrong when DataSet is filtered.
    //comment by peter 2005/05/12
    {if ds.RecordCount - TopVisibleRow > FPageRecordCount then
      Grid.RowCount := FPageRecordCount
    else
      Grid.RowCount := ds.RecordCount - TopVisibleRow;}
    if ds.Filtered then
    begin
      //get RecordCount when filter
      FilteredRecordCount := 0;
      ds.First;
      while not ds.Eof do
      begin
        Inc(FilteredRecordCount);
        ds.Next;
      end;

      if FilteredRecordCount - TopVisibleRow > FPageRecordCount then
        Grid.RowCount := FPageRecordCount
      else
        Grid.RowCount := FilteredRecordCount - TopVisibleRow;
    end
    else
    begin
      if ds.RecordCount - TopVisibleRow > FPageRecordCount then
        Grid.RowCount := FPageRecordCount
      else
        Grid.RowCount := ds.RecordCount - TopVisibleRow;
    end;


    ds.First;
    ds.MoveBy(TopVisibleRow);
    y := 0;
    while (y < PageRecordCount) and (not ds.Eof) and (y < Grid.RowCount) do
    begin
      for x := 0 to DataFields.Count - 1 do
      begin
        if (DataFields[x].columnIndex < 0) then
          continue;
        if (DataFields[x].DataType = ftGraphic) and (Dataset.FieldByName(DataFields[x].DataField) is TBlobField) then
        begin
          bmp := TPicture.Create;
          try
            try
              if TBlobField(Dataset.FieldByName(DataFields[x].DataField)).BlobSize > 0 then
              begin
                bmp.Assign(TBlobField(Dataset.FieldByName(DataFields[x].DataField)));
                Cells[DataFields[x].ColumnIndex, y] := GGetWebApplicationThreadVar.UserCacheDir + Self.Name + '_' + IntToStr(x) + 'x' + IntToStr(y) + FDataFields[x].TmpFileExt;
                bmp.SaveToFile(Cells[DataFields[x].ColumnIndex, y]);
              end;
            except
              Cells[DataFields[x].ColumnIndex, y] := '';
              // probably shouldn't eat this error, but we should figure out what to do.
            end;
          finally
            bmp.Free;
          end;
        end else
          Cells[DataFields[x].ColumnIndex, y] := ValueFromField(ds, x, y);
        if (DataFields[x].Lookup.DataField <> '') then
        begin
          sVal := Cells[DataFields[x].ColumnIndex, y];
          idx := FDataFields[x].Lookup.KeyValues.IndexOf(sVal);
          if idx >= 0 then
            Grid.Cells[DataFields[x].ColumnIndex, y] := FDataFields[x].Lookup.DisplayValues[idx]
          else
            Grid.Cells[DataFields[x].ColumnIndex, y] := '';
        end else
          Grid.Cells[DataFields[x].ColumnIndex, y] := Cells[DataFields[x].ColumnIndex, y];
      end;
      if ((SelectedDataRow - TopVisibleRow) = y) and (not Grid.SelectedRow[y]) then
      begin
        Grid.DeselectAll;
        Grid.SelectedRow[y] := True;
      end;
      RenderEditorsForRow(y);
      ds.Next;
      inc(y);
    end;
    if FSyncDataset then
      MoveToCurrent(ds);
  finally
    UnlockDataset(ds);
  end;
  UpdateNavButtonState;
end;

procedure TArcIWStringGridDatasetContent.First;
begin
  EnsureBrowseMode;

  TopVisibleRow := 0;
  SelectedDataRow := 0;

  ResetGridData;
  Grid.DeselectAll;
  Grid.SelectedRow[0] := True;
end;

procedure TArcIWStringGridDatasetContent.Last;
var
  ds: TDataset;
begin
  EnsureBrowseMode;

  if Grid.RowCount = 0 then
    exit;

  LastPage;
  ds := LockDataset;
  try
    FixRecordCount(ds);
    SelectedDataRow := ds.RecordCount - 1;
  finally
    UnlockDataset(ds);
  end;
  ResetGridData;
  Grid.DeselectAll;
  Grid.SelectedRow[SelectedDataRow - TopVisibleRow] := True;
end;

procedure TArcIWStringGridDatasetContent.Next;
var
  ds: TDataset;
  iCnt: integer;
begin
  EnsureBrowseMode;

  ds := LockDataset;
  try
    FixRecordCount(ds);
    iCnt := ds.RecordCount;
  finally
    UnlockDataset(ds);
  end;

  if (SelectedDataRow < iCnt) and (SelectedDataRow = TopVisibleRow + PageRecordCount - 1) then
  begin
    NextPage;
    FSelectedDataRow := TopVisibleRow;
  end else
    inc(FSelectedDataRow);

  ds := LockDataset;
  try
    FixRecordCount(ds);
    if SelectedDataRow > ds.RecordCount - 1 then
      SelectedDataRow := ds.RecordCount - 1;
  finally
    UnlockDataset(ds);
  end;
  ResetGridData;
  Grid.DeselectAll;
  Grid.SelectedRow[SelectedDataRow - TopVisibleRow] := True;
end;

procedure TArcIWStringGridDatasetContent.Prior;
begin
  EnsureBrowseMode;

  if (SelectedDataRow >= 0) and (SelectedDataRow = TopVisibleRow) then
  begin
    PriorPage;
    SelectedDataRow := TopVisibleRow + PageRecordCount - 1;
  end else
    dec(FSelectedDataRow);
  if SelectedDataRow < 0 then
    SelectedDataRow := 0;

  ResetGridData;
  Grid.DeselectAll;
  Grid.SelectedRow[SelectedDataRow - TopVisibleRow] := True;
end;

procedure TArcIWStringGridDatasetContent.MoveBy(RecordCount: integer);
var
  ds: TDataset;
  iSel: integer;
begin
  TestBrowseMode;

  iSel := SelectedDataRow + RecordCount;
  if iSel < 0 then
    iSel := 0;
  ds := LockDataset;
  try
    FixRecordCount(ds);
    if iSel > ds.RecordCount then
      iSel := ds.RecordCount;
  finally
    UnlockDataset(ds);
  end;
  while SelectedDataRow < TopVisibleRow + PageRecordCount - 1 do
    NextPage;
  while SelectedDataRow > TopVisibleRow + PageRecordCount - 1 do
    PriorPage;
  if SelectedDataRow >= 0 then
    SelectedDataRow := iSel;

  Grid.DeselectAll;
  Grid.SelectedRow[SelectedDataRow - TopVisibleRow] := True;
  ResetGridData;
end;

procedure TArcIWStringGridDatasetContent.MoveTo(RecordNumber: integer);
var
  ds: TDataset;
  iSel: integer;
begin
  TestBrowseMode;

  iSel := RecordNumber;
  if iSel < 0 then
    iSel := 0;
  ds := LockDataset;
  try
    FixRecordCount(ds);
    if iSel > ds.RecordCount then
      iSel := ds.RecordCount;
  finally
    UnlockDataset(ds);
  end;
  while SelectedDataRow < TopVisibleRow + PageRecordCount - 1 do
    NextPage;
  while SelectedDataRow > TopVisibleRow + PageRecordCount - 1 do
    PriorPage;

  if SelectedDataRow >= 0 then
    SelectedDataRow := iSel;

  Grid.DeselectAll;
  Grid.SelectedRow[SelectedDataRow - TopVisibleRow] := True;
  ResetGridData;
end;

procedure TArcIWStringGridDatasetContent.ChangeDatasetState(
  state: TDatasetState);
begin
  FDatasetState := state;
  case FDatasetState of
    dsBrowse: ClearEditorList;
  end;
  UpdateNavButtonState;
end;

procedure TArcIWStringGridDatasetContent.DoRenameColumn(Sender: TObject;
  OldName, NewName: string);
var
  idx: integer;
begin
  idx := DataFields.IndexOf(OldName);
  if idx >= 0 then
  begin
    DataFields[idx].InternalRename := True;
    DataFields[idx].Column := NewName;
  end;
end;

procedure TArcIWStringGridDatasetContent.Edit;
var
  ds: TDataset;
begin
  if (Grid.RowCount = 0) or (SelectedDataRow < 0) then
    exit;

  //comment by peter 2005/04/29
  if (EditStyle = esAutoEdit) then
  begin
    FOldSuppressRowClick := Grid.SuppressSelRowClick;
    Grid.SuppressSelRowClick := True;
  end;
  //

  TestBrowseMode;


  if FSyncDataset then
  begin
    ds := LockDataset;
    try
      if Assigned(FOnBeforeEdit) then
        FOnBeforeEdit(Self, ds);
      ds.Edit;
      if Assigned(FOnAfterEdit) then
        FOnAfterEdit(Self, ds);
    finally
      UnlockDataset(ds);
    end;
  end;

  ChangeDatasetState(dsEdit);
  RenderEditorsForRow(SelectedDataRow - TopVisibleRow);

  //comment peter 2005/05/17
  if Assigned(GridNoRaise) then
    Grid.Focus(SelectedDataRow-TopVisibleRow);
end;

procedure TArcIWStringGridDatasetContent.RenderEditorsForRow(row: integer);
  procedure AssignDataToControl(ctrl: TControl; fld: TDataFieldItem; value: string);
    function TryArcStrToBool(value: string; var b: boolean): boolean;
    begin
      Result := TryStrToBool(value, b);
      if not result then
      begin
        if value <> '' then
        begin
          if CharInSet(value[1],['T', 't']) then
          begin
            b := True;
            Result := True;
          end else
            if CharInSet(value[1],['F', 'f']) then
            begin
              b := False;
              Result := True;
            end;
        end;
      end;
    end;
  var
    o: TObject;
    ppi: PPropInfo;
    b: boolean;
    i: integer;
  begin
    ppi := GetPropInfo(ctrl, fld.ControlProperty);
    if ppi = nil then
      exit;
    case ppi^.PropType^^.Kind of
      tkUnknown, tkSet,
        tkMethod, tkArray, tkRecord,
        tkInterface, tkDynArray:
        raise Exception.Create('Unsupported property type.');
      //when content field type is ftBoolean
      //comment by peter 2005/04/28
      tkEnumeration:
        begin
          // corrected handling of boolean types. -jds 2005/05/12
          if fld.DataType = ftBoolean then
          begin
            if fld.FDataBoolTrue = '' then
            begin
              if TryArcStrToBool(value, b) then
              begin
                fld.FBoolType := value;
                SetPropValue(ctrl, fld.ControlProperty, b);
              end else
              begin
                if TryStrToInt(value, i) then
                begin
                  if i <> 0 then
                    SetPropValue(ctrl, fld.ControlProperty, true)
                  else
                    SetPropValue(ctrl, fld.ControlProperty, false);
                  fld.FBoolType := value;
                end else
                begin
                  setPropValue(ctrl, fld.ControlProperty, false);
                  fld.FBoolType := '0';
                end;
              end;
            end else
              if UpperCase(value) = Uppercase(fld.FDataBoolTrue) then
                SetPropValue(ctrl, fld.ControlProperty, true)
              else
                SetPropValue(ctrl, fld.ControlProperty, false);
          end else
            SetPropValue(ctrl, fld.ControlProperty, value);
        end;
      //
      tkClass:
        begin
          o := GetObjectProp(ctrl, fld.ControlProperty);
          if o is TStrings then
            TStrings(o).Text := value
          else if o is TPicture then
          begin
            if FileExists(Value) then
              TPicture(o).LoadFromFile(Value);
          end;
        end;
    else
      SetStrProp(ctrl, fld.ControlProperty, value);
    end;
  end;

  procedure AssignLookupDataToControl(ctrl: TControl; Lookup: TDataLookupSettings; value: string);
  var
    o: TObject;
    sl: TStrings;
  begin
    if ctrl = nil then
      exit;
    o := GetObjectProp(ctrl, Lookup.ControlProperty);
    if not (o is TStrings) then
      raise Exception.Create('Lookup ControlProperty is not a TStrings descendant.');
    sl := TStrings(o);

    if Lookup.UseDataset then
    begin
      if Lookup.KeyField = '' then
        raise Exception.Create('Lookup KeyField has not been assigned.');
      if Lookup.DataField = '' then
        raise Exception.Create('Lookup DataField has not been assigned.');
      if Lookup.IndexProperty = '' then
        raise Exception.Create('Lookup IndexProperty has not been assigned.');
      sl.Assign(Lookup.DisplayValues);

      SetPropValue(ctrl, Lookup.IndexProperty, Lookup.KeyValues.IndexOf(value))
    end else
      SetPropValue(ctrl, Lookup.IndexProperty, sl.IndexOf(value))
  end;

var
  x: integer;
  ctrl: TIWBaseControl;
begin

  for x := 0 to FDataFields.Count - 1 do
  begin
    if (FDataFields[x].ColumnIndex >= 0) and ((((SelectedDataRow -
      TopVisibleRow) = row) and (FDatasetState in [dsEdit, dsInsert])) or
      (FDataFields[x].AlwaysRenderControl and
      (Objects[FDataFields[x].ColumnIndex, Row] = nil))) then
    begin
      if Assigned(Objects[FDataFields[x].ColumnIndex, Row]) then begin
          // remove old one
        FEditors.Remove(Objects[FDataFields[x].ColumnIndex, Row]);
        Objects[FDataFields[x].ColumnIndex, Row].Free;
      end;
      FDataFields[x].BuildEditor(FEditors, row, ctrl);
      Objects[FDataFields[x].ColumnIndex, Row] := ctrl;

      if FDataFields[x].ControlProperty <> '' then

        AssignDataToControl(ctrl, FDataFields[x], Cells[FDataFields[x].ColumnIndex, Row]);
      if FDataFields[x].Lookup.ControlProperty <> '' then

        AssignLookupDataToControl(ctrl, FDataFields[x].Lookup, Cells[FDataFields[x].ColumnIndex, Row]);
    end;
  end;

end;

procedure TArcIWStringGridDatasetContent.ClearEditorList(All: boolean = false);
var
  x, y: integer;
  lsSaves: TList;
begin
  lsSaves := TList.Create;
  try
    if not (csDestroying in ComponentState) then
      for x := Low(Objects) to High(Objects) do
      begin
        for y := Low(Objects[x]) to High(Objects[x]) do
        begin
          if (FDataFields.FieldFromColumnIndex(x) <> nil) and
             ((not FDataFields.FieldFromColumnIndex(x).AlwaysRenderControl) or All) and
             (FDataFields.FieldFromColumnIndex(x).Control <> Objects[x, y]) then
          begin
            if (Objects[x, y] <> nil) and (Objects[x, y] is TComponent) then
              if Assigned(GridNoRaise) then
                Grid.IWRemoveComponent(TComponent(Objects[x, y]));
            Objects[x, y] := nil;
          end else
          begin
            if Assigned(Objects[x, y]) then
            begin
              if (FDataFields.FieldFromColumnIndex(x) <> nil) and (not
                FDataFields.FieldFromColumnIndex(x).AlwaysRenderControl) then
              begin
                TIWBaseControl(Objects[x, y]).Parent := nil;
                Objects[x, y] := nil;
              end else
              begin
                lsSaves.Add(Objects[x, y]);
                FEditors.Delete(FEditors.IndexOf(Objects[x, y]));
              end;
            end;
          end;
        end;
      end;
    for x := 0 to FEditors.Count - 1 do
    begin
      if (TObject(FEditors[x]) is TControl) then
        TControl(FEditors[x]).Parent := nil;
      TObject(FEditors[x]).Free;
    end;
    FEditors.Clear;

    {$IFNDEF VER130}
    FEditors.Assign(lsSaves);
    {$ELSE}
    for x := 0 to lsSaves.Count-1 do
      FEditors.Add(lsSaves[x]);
    {$ENDIF}
  finally
    lsSaves.Free;
  end;
end;

function TArcIWStringGridDatasetContent.GetIsFirstPage: boolean;
begin
  Result := TopVisibleRow <= 0;
end;

function TArcIWStringGridDatasetContent.GetIsFirstRecord: boolean;
begin
  Result := SelectedDataRow <= 0;
end;

function TArcIWStringGridDatasetContent.GetIsLastPage: boolean;
var
  ds: TDataset;
begin
  ds := LockDataset;
  try
    FixRecordCount(ds);
    Result := (((ds.RecordCount - 1) div PageRecordCount) * PageRecordCount = TopVisibleRow)
    //or ((ds.RecordCount div PageRecordCount)*PageRecordCount = ds.RecordCount);
  finally
    UnlockDataset(ds);
  end;
end;

function TArcIWStringGridDatasetContent.GetIsLastRecord: boolean;
var
  ds: TDataset;
begin
  ds := LockDataset;
  try
    FixRecordCount(ds);
    Result := SelectedDataRow = ds.RecordCount - 1;
  finally
    UnlockDataset(ds);
  end;
end;

procedure TArcIWStringGridDatasetContent.UpdateNavButtonState;
begin
  if not Assigned(FGrid) then
    exit;

  if (FDatasetState <> dsBrowse) and (EditStyle <> esAutoEdit) then
  begin
    Grid.CaptionButtons.PriorPage.Enabled := False;
    Grid.CaptionButtons.Prior.Enabled := False;
    Grid.CaptionButtons.NextPage.Enabled := False;
    Grid.CaptionButtons.Next.Enabled := False;
    Grid.CaptionButtons.FirstPage.Enabled := False;
    Grid.CaptionButtons.First.Enabled := False;
    Grid.CaptionButtons.LastPage.Enabled := False;
    Grid.CaptionButtons.Last.Enabled := False;
  end else
    inherited;

  Grid.CaptionButtons.Save.Enabled := (DatasetState in [dsEdit, dsInsert]);
  Grid.CaptionButtons.Cancel.Enabled := (DatasetState in [dsEdit, dsInsert]);
  Grid.CaptionButtons.New.Enabled := ((DatasetState = dsBrowse) and (EditStyle <> esReadOnly)) or (EditStyle = esAutoEdit);
  Grid.CaptionButtons.Edit.Enabled := (DatasetState = dsBrowse) and (EditStyle = esExplicitEdit) and (SelectedDataRow >= 0);
  Grid.CaptionButtons.Refresh.Enabled := ((DatasetState = dsBrowse)) or (EditStyle = esAutoEdit);
  Grid.CaptionButtons.Delete.Enabled := (((DatasetState = dsBrowse) and (EditStyle <> esReadOnly)) or (EditStyle = esAutoEdit)) and (SelectedDataRow >= 0);
end;

function TArcIWStringGridDatasetContent.ProcessCaption(str: string): string;
  function PageNumber: string;
  begin
    Result := IntToStr((TopVisibleRow div PageRecordCount) + 1);
  end;
  function PageCount: string;
  var
    ds: TDataset;
  begin
    ds := LockDataset;
    try
      FixRecordCount(ds);
      if ds.RecordCount < PageRecordCount then
        Result := '1'
      else begin
        //Result := IntToStr((ds.RecordCount div PageRecordCount));
        if ds.RecordCount = TopVisibleRow then
          Result := PageNumber // adding new record and it's on a new page
        else
          Result := IntToStr(((ds.RecordCount - 1) div PageRecordCount) + 1);
      end;

    finally
      UnlockDataset(ds);
    end;
  end;
begin
  Result := ReplaceText(str, '<#>', PageNumber);
  Result := ReplaceText(Result, '{#}', PageCount);
end;

procedure TArcIWStringGridDatasetContent.SetSyncDataset(
  const Value: boolean);
begin
  if FSyncDataset = Value then
    exit;

  if csLoading in ComponentState then
    FSyncDataset := Value
  else
    FSyncDataset := Value and Assigned(FDataset);
end;

procedure TArcIWStringGridDatasetContent.MoveToCurrent(ds: TDataset = nil);
  function NeedsRepositioning(ds: TDataset): boolean;
  var
    i: integer;
    bHasKey: boolean;
  begin
    if not Assigned(FOnCheckPosition) then
    begin
      Result := SelectedDataRow < 0;
      if not Result then
      begin
        bHasKey := False;
        for i := 0 to FDataFields.Count - 1 do
        begin
          if FDataFields[i].KeyField then
          begin
            bHasKey := True;
            Result := ds.FieldByName(FDataFields[i].DataField).Text <> Cells[FDataFields[i].ColumnIndex, SelectedDataRow - TopVisibleRow];
            if Result then
              break;
          end;
        end;
        if not bHasKey then
        begin
          raise Exception.Create('No DatasetContent fields have been marked as KeyFields.  Cannot determine current dataset position.');
        end;
      end;
    end else
      FOnCheckPosition(Self, ds, SelectedDataRow, CopyRow(Grid.Columns.Count, SelectedDataRow - TopVisibleRow), Result);
  end;

var
  bLocked: boolean;
  sKeys: string;
  vValues: variant;
  bHandled: boolean;
begin
  if SelectedDataRow < 0 then
    exit;
  bLocked := False;
  if ds = nil then
    bLocked := True;

  if bLocked then
    ds := LockDataset;
  try
    if NeedsRepositioning(ds) then
    begin
      bHandled := False;
      if Assigned(FOnRepositionDataset) then
        FOnRepositionDataset(Self, ds, SelectedDataRow, CopyRow(Grid.Columns.Count, SelectedDataRow - TopVisibleRow), bHandled);
      if not bHandled then
      begin
        ds.First;
        if Dataset = nil then
        begin
          sKeys := FDataFields.ListKeyFields;
          vValues := FDataFields.ListKeyValues(SelectedDataRow - TopVisibleRow);
          if not ds.Locate(sKeys, vValues, []) then
            raise Exception.Create('Cannot locate key field.  Perhaps the record was removed by another user. (' + sKeys + ')');
        end else
        begin
          ds.MoveBy(SelectedDataRow);
        end;
      end;
    end;
  finally
    if bLocked then
      UnlockDataset(ds);
  end;
end;

procedure TArcIWStringGridDatasetContent.FixRecordCount(ds: TDataset);
var
  bLocked: boolean;
begin
  if FDatasetState in [dsInsert, dsEdit] then
    exit;

  bLocked := False;
  if ds = nil then
    bLocked := True;

  if bLocked then
    ds := LockDataset;
  try
    if ds.RecordCount <= 1 then
    begin
      ds.Last;
      ds.First;
    end;
  finally
    if bLocked then
      UnlockDataset(ds);
  end;
end;

procedure TArcIWStringGridDatasetContent.Refresh;
begin
  If FOkToRefresh then
  begin
    EnsureBrowseMode;

    ResetGridData;

    if (not FAvoidReturnToEditMode) and (EditStyle = esAutoEdit) then
      Edit;
    FAvoidReturnToEditMode := False;
  end;
end;

procedure TArcIWStringGridDatasetContent.SetEditStyle(
  const Value: TEditStyle);
begin
  if FEditStyle <> Value then
  begin
    FEditStyle := Value;
    if (not (csDesigning in ComponentState)) and
      (not (csLoading in ComponentState)) then
    begin
      ResetGridData;

    end;

    if (not (csLoading in ComponentState)) and Assigned(GridNoRaise) then
    begin
      if (EditStyle = esReadOnly) then
      begin
        Grid.CaptionButtons.Save.Visible := False;
        Grid.CaptionButtons.Cancel.Visible := False;
        Grid.CaptionButtons.New.Visible := False;
        Grid.CaptionButtons.Edit.Visible := False;
        Grid.CaptionButtons.Delete.Visible := False;
      end else
      begin
        Grid.CaptionButtons.Save.Visible := True;
        Grid.CaptionButtons.Cancel.Visible := True;
        Grid.CaptionButtons.New.Visible := True;
        Grid.CaptionButtons.Edit.Visible := True;
        Grid.CaptionButtons.Delete.Visible := True;
      end;
    end;
  end;
end;

function TArcIWStringGridDatasetContent.GetIsNextRecord: boolean;
begin
  Result := IsLastRecord;
  if not Result then
    Result := SelectedDataRow < 0;
end;

function TArcIWStringGridDatasetContent.GetIsPriorRecord: boolean;
begin
  Result := IsFirstRecord;
  if not Result then
    Result := SelectedDataRow < 0;
end;

procedure TArcIWStringGridDatasetContent.SetActive(const Value: Boolean);
begin
  inherited;
  if Active and (not (csDesigning in ComponentState)) then
    ResetGridData;
end;

procedure TArcIWStringGridDatasetContent.CacheToGrid(Col, Row: integer);
  procedure CopyRow(Row: integer);
  var
    x: integer;
    iVal : integer;
  begin
    if (Row > Grid.RowCount-1)  then
      exit;

    if Col < 0 then
      for x := 0 to FDataFields.Count - 1 do
      begin
        if FDataFields[x].ColumnIndex < 0 then
          continue;
        if FDataFields[x].Lookup.DataField <> '' then
        begin
          iVal := FDataFields[x].Lookup.KeyValues.IndexOf(Cells[FDataFields[x].ColumnIndex, Row]);
          if iVal >= 0 then
            Grid.Cells[FDataFields[x].ColumnIndex, Row] := FDataFields[x].Lookup.DisplayValues[ iVal ]
        else
            Grid.Cells[FDataFields[x].ColumnIndex, Row] := '';
        end else
          Grid.Cells[FDataFields[x].ColumnIndex, Row] := Cells[FDataFields[x].ColumnIndex, Row];
      end
    else
      Grid.Cells[Col, Row] := Cells[Col, Row];
  end;
var
  y: integer;
begin
  if Row < 0 then
  begin
    for y := 0 to Grid.RowCount - 1 do
      CopyRow(y);
  end else
    CopyRow(Row);

end;

function TArcIWStringGridDatasetContent.RecNo(ds: TDataset): integer;
begin
  Result := ds.RecNo;
  if Assigned(FOnOverrideRecordNumber) then
    FOnOverrideRecordNumber(Self, ds, Result);
end;

procedure TArcIWStringGridDatasetContent.SyncToCurrent(Dataset: TDataset = nil);
var
  ds: TDataset;
begin
  if Dataset = nil then
    ds := LockDataset
  else
    ds := Dataset;
  try
    SelectedDataRow := RecNo(ds) - 1;
  finally
    if DataSet = nil then
      UnlockDataset(ds);
  end;
  TopVisibleRow := 0;
  while SelectedDataRow > (TopVisibleRow + (PageRecordCount - 1)) do
    inc(TopVisibleRow, PageRecordCount);
  Refresh;
end;

procedure TArcIWStringGridDatasetContent.SetSelectedDataRow(
  const Value: Integer);
var
  ctrl: TIWBaseControl;
  x, y: integer;
  intf: IIWBaseComponent;
begin
  if (not (DatasetState in [dsEdit, dsInsert])) then
  begin
    y := SelectedDataRow - TopVisibleRow;
    if Assigned(GridNoRaise) and (y < Grid.RowCount) and (y >= 0) then
    begin
      for x := low(Objects) to High(Objects) do
      begin
        ctrl := TIWBaseControl(Objects[x, y]);
        if ctrl <> nil then
        begin
          if Supports(ctrl, IIWBaseComponent, intf) then
            if intf.SupportsInput then
              ctrl.Enabled := False;
        end;
      end;
    end;
  end;
  inherited;
  if (not (DatasetState in [dsEdit, dsInsert])) then
  begin
    y := SelectedDataRow - TopVisibleRow;
    if Assigned(GridNoRaise) and (y < Grid.RowCount) and (y >= 0) then
    begin
      for x := low(Objects) to High(Objects) do
      begin
        ctrl := TIWBaseControl(Objects[x, y]);
        if ctrl <> nil then
        begin
          if Supports(ctrl, IIWBaseComponent, intf) then
            if intf.SupportsInput then
              ctrl.Enabled := DatasetState in [dsEdit, dsInsert];
        end;
      end;
    end;
  end;
end;

procedure TArcIWStringGridDatasetContent.DoRetrieveCBObject(
  Sender: TObject; x: Integer; var ctrl: TControl);
var
  fld: TDataFieldItem;
  i: integer;
begin
  if Active then
  begin
    ctrl := nil;
    fld := FDataFields.FieldFromColumnIndex(x);
    if fld <> nil then
    begin
      if fld.FFilterControl = nil then
      begin
        case fld.FilterStyle of
          fsText:
            begin
              ctrl := TIWEdit.Create(nil);
              TIWEdit(ctrl).OnHTMLTag := MaskTagStyle;
              TIWEdit(ctrl).ScriptEvents.HookEvent('OnChange', 'SubmitClickConfirm("' + Grid.HTMLName + '","@C' + IntToStr(x) + '",false,"");');
              TIWEdit(ctrl).ScriptEvents.HookEvent('OnKeyPress', Grid.HTMLName + _SubmitOnEnterKey + '(event,"' + Grid.HTMLName + '","@C' + IntToStr(x) + '",false,"");');
              TIWEdit(ctrl).ZIndex := Grid.ZIndex + 100;
              ctrl.Name := Grid.HTMLName + 'xx' + Name + 'xx' + IntToStr(x);
              TIWEdit(ctrl).Text := '';
              if Grid.ColumnsWidthsType = cwtPixel then
                TIWEdit(ctrl).Width := Grid.Columns[fld.ColumnIndex].Width;
              if TIWEdit(ctrl).Width = 0 then
                if fld.MaxLength = 0 then
                  TIWEdit(ctrl).RenderSize := False
                else
                  TIWEdit(ctrl).Width := fld.MaxLength * 18;
              if ctrl.Width > 150 then
                ctrl.Width := 150;
            end;
          fsCombo:
            begin
              ctrl := TIWComboBox.Create(nil);

              TIWComboBox(ctrl).NoSelectionText := fld.FilterAllString;
              TIWComboBox(ctrl).RequireSelection := False;
              TIWComboBox(ctrl).OnHTMLTag := MaskTagStyle;
              TIWComboBox(ctrl).AutoHideOnMenuActivation := True;
              if Grid.ColumnsWidthsType = cwtPixel then
                TIWComboBox(ctrl).Width := Grid.Columns[fld.ColumnIndex].Width
              else
                TIWComboBox(ctrl).RenderSize := False;

              if not Assigned(fld.FOnGetFilterItems) then
              begin
                TIWComboBox(ctrl).Items.Duplicates := dupIgnore;
                TIWComboBox(ctrl).Items.Sorted := true;
                for i := 0 to Grid.Columns[x].Values.Count - 1 do
                begin
                  TIWComboBox(ctrl).Items.Add(Grid.Columns[x].Values[i]);
                end;
              end else
                fld.FOnGetFilterItems(Self, fld, TIWComboBox(ctrl).Items);
              TIWComboBox(ctrl).ScriptEvents.HookEvent('OnChange', 'SubmitClickConfirm("' + Grid.HTMLName + '","@C' + IntToStr(x) + '",false,"");');
              TIWComboBox(ctrl).ZIndex := Grid.ZIndex + 100;
              ctrl.Name := Grid.HTMLName + 'xx' + Name + 'xx' + IntToStr(x);
            end;
        end;
        fld.FilterControl := TIWBaseControl(ctrl);
      end else
      begin
        ctrl := fld.FFilterControl;
      end;
    end;
  end;
end;

procedure TArcIWStringGridDatasetContent.DoSubmit(Sender: TObject; const AValue: string);
  procedure RebuildFilters;
  var
    x: integer;
    sVal: string;
  begin
    ManageFilters := True;
    FFilterString := '';
    for x := 0 to FDataFields.Count - 1 do
    begin
      case FDataFields[x].FilterStyle of
        fsCombo:
          begin
            if TIWComboBox(FDataFields[x].FFilterControl).ItemIndex >= 0 then
            begin
              if FDataFields[x].Lookup.KeyValues.Count > 0 then
                sVal := FDataFields[x].Lookup.KeyValues[FDataFields[x].Lookup.DisplayValues.IndexOf(TIWComboBox(FDataFields[x].FFilterControl).Items[TIWComboBox(FDataFields[x].FFilterControl).ItemIndex])]
              else
                sVal := TIWComboBox(FDataFields[x].FFilterControl).Items[TIWComboBox(FDataFields[x].FFilterControl).ItemIndex];

              case FDataFields[x].DataType of
                ftString, ftMemo, ftFmtMemo, ftDate,
                  ftTime, ftDateTime, ftWideString, ftUnknown, ftFixedChar, ftVariant:
                  begin
                    FFilterString := FFilterString + ' and ' + FDataFields[x].DataField + '=''' + sVal + '''';
                  end;
                ftInteger, ftWord, ftBoolean,
                  ftFloat, ftCurrency, ftBCD, {$IFNDEF VER130}ftFMTBcd,{$ENDIF}
                  ftAutoInc, ftLargeint:
                  begin
                    FFilterString := FFilterString + ' and ' + FDataFields[x].DataField + '=' + sVal;
                  end;
              end;
            end;
          end;
        fsText:
          begin
            if TIWEdit(FDataFields[x].FFilterControl).Text <> '' then
            begin
              case FDataFields[x].DataType of
                ftString, ftMemo, ftFmtMemo, ftDate,
                  ftTime, ftDateTime, ftWideString, ftUnknown,
                  ftFixedChar, ftVariant:
                  begin
                    FFilterString := FFilterString + ' and ' + FDataFields[x].DataField + '=''' + TIWEdit(FDataFields[x].FFilterControl).Text + '*''';
                  end;
                ftInteger, ftWord, ftBoolean,
                  ftFloat, ftCurrency, ftBCD, {$IFNDEF VER130}ftFMTBcd,{$ENDIF}
                  ftAutoInc, ftLargeint:
                  begin
                    FFilterString := FFilterString + ' and ' + FDataFields[x].DataField + '=' + TIWEdit(FDataFields[x].FFilterControl).Text;
                  end;
              end;
            end;
          end;
      end;
    end;
    if FFilterString <> '' then
      FFilterString := Copy(FFilterString, 6, high(integer));
  end;
var
  x: integer;
  fld: TDataFieldItem;
  sVal: string;
begin
  x := StrToIntDef(Copy(AValue, 3, High(integer)), -1);
  if x >= 0 then
  begin
    fld := FDataFields.FieldFromColumnIndex(x);
    if Assigned(fld.OnChangeFilterText) then
    begin
      if fld.FFilterControl is TIWComboBox then
      begin
        if TIWComboBox(fld.FFilterControl).ItemIndex >= 0 then
          sVal := TIWComboBox(fld.FFilterControl).Items[TIWComboBox(fld.FFilterControl).ItemIndex]
        else
          sVal := '';
      end else
        if fld.FFilterControl is TIWEdit then
        begin
          sVal := TIWEdit(fld.FFilterControl).Text;
        end else
          raise Exception.Create('Unknown Filter Control Type.');
      fld.OnChangeFilterText(Self, fld, sVal);
      if fld.FFilterControl is TIWComboBox then
      begin
        TIWComboBox(fld.FFilterControl).ItemIndex := TIWComboBox(fld.FFilterControl).Items.IndexOf(sVal);
      end else
        if fld.FFilterControl is TIWEdit then
        begin
          TIWEdit(fld.FFilterControl).Text := sVal;
        end else
          raise Exception.Create('Unknown Filter Control Type.');
    end else
      RebuildFilters;
    ResetGridData;
  end;
end;

procedure TArcIWStringGridDatasetContent.MaskTagStyle(ASender: TObject;
  ATag: TIWHTMLTag);
var
  sStyle: string;
begin
//  ATag.AddStringParam('style','border: none;background-color: '+ColorToString(Grid.StyleControlBar.BackgroundColor)+';'+Grid.StyleControlBar.Font.FontToStringStyle(LastKnownBrowser));
  sStyle := 'background-color: ' + ColorToRGBString(Grid.StyleControlBar.BackgroundColor) + ';' + Grid.StyleControlBar.Font.FontToStringStyle(LastKnownBrowser);
  ATag.AddStringParam('style', sStyle);
end;


procedure TArcIWStringGridDatasetContent.Loaded;
begin
  inherited;
  FOkToRefresh := True;
end;

procedure TArcIWStringGridDatasetContent.ForcedCancel;
begin
  FAvoidReturnToEditMode := True;
  try
    Cancel;
  finally
    FAvoidReturnToEditMode := False;
  end;
end;

procedure TArcIWStringGridDatasetContent.SetDataFields(
  const Value: TDataFieldsCollection);
begin
  FDataFields.Assign(Value);
end;

{ TDataFieldItem }

procedure TDataFieldItem.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TDataFieldItem(Dest).UseDefaultEditor := UseDefaultEditor;
  TDataFieldItem(Dest).DataType := DataType;
  TDataFieldItem(Dest).KeyField := KeyField;
  TDataFieldItem(Dest).Column := Column;
  TDataFieldItem(Dest).ColumnIndex := ColumnIndex;
  TDataFieldItem(Dest).DataField := DataField;
  TDataFieldItem(Dest).Control := Control;
  TDataFieldItem(Dest).OnCreateControl := OnCreateControl;
  TDataFieldItem(Dest).AlwaysRenderControl := AlwaysRenderControl;
  TDataFieldItem(Dest).ReadOnly := ReadOnly;
  TDataFieldItem(Dest).FilterStyle := FilterStyle;
  TDataFieldItem(Dest).OnGetFilterItems := OnGetFilterItems;
  TDataFieldItem(Dest).OnChangeFilterText := OnChangeFilterText;
end;

procedure TDataFieldItem.BuildEditor(List: TList; Row: integer; var Ctrl: TIWBaseControl);
var
  bFreeControl: boolean;
  intf: IIWBaseComponent;
  intfc : IIWBaseControl;
begin
  ctrl := nil;
  if (FReadOnly and (not AlwaysRenderControl)) then
    exit;
  if FControl = nil then
  begin
    if FUseDefaultEditor then
    begin
      case FDataType of
        ftGraphic:
          begin
            ctrl := TIWImage.Create(nil);
            TIWImage(ctrl).AutoSize := False;
            TIWImage(ctrl).RenderSize := True;
            ctrl.Height := 24;
            ctrl.Width := 100;
            ControlProperty := 'Picture';
            TIWImage(ctrl).Hint := FHint;
          end;
        ftBoolean:
          begin
            ctrl := TIWCheckBox.Create(nil);
            ctrl.Caption := '';
            ControlProperty := 'Checked';
            TIWCheckBox(ctrl).Hint := FHint;
            TIWCheckBox(ctrl).Editable := True;
          end;
        ftUnknown, ftString, ftWideString:
          begin
            if FLookup.DataField <> '' then
            begin
              ctrl := TIWComboBox.Create(nil);
              TIWComboBox(ctrl).AutoHideOnMenuActivation := True;
              ControlProperty := 'Text';
              FLookup.ControlProperty := 'Items';
              ctrl.Width := 200;
              TIWComboBox(ctrl).Hint := FHint;
            end else
            begin
              ctrl := TIWEdit.Create(nil);
              ctrl.Width := 200;
              ControlProperty := 'Text';
              TIWEdit(ctrl).MaxLength := FMaxLength;
              TIWEdit(ctrl).PasswordPrompt := FPasswordPrompt;
              TIWEdit(ctrl).Hint := FHint;
              if FMaxLength > 0 then
                TIWEdit(ctrl).Width := FMaxLength * 18;
              if ctrl.Width > 150 then
                ctrl.Width := 150;
            end;
          end;
        ftSmallint, ftInteger, ftWord,
          ftFloat, ftCurrency, ftLargeint:
          begin
            if FLookup.DataField <> '' then
            begin
              ctrl := TIWComboBox.Create(nil);
              TIWComboBox(ctrl).AutoHideOnMenuActivation := True;
              ControlProperty := 'Text';
              FLookup.ControlProperty := 'Items';
              ctrl.Width := 75;
              TIWComboBox(ctrl).Hint := FHint;
            end else
            begin
              ctrl := TIWEdit.Create(nil);
              ControlProperty := 'Text';
              ctrl.Width := 75;
              TIWEdit(ctrl).MaxLength := FMaxLength;
              TIWEdit(ctrl).PasswordPrompt := FPasswordPrompt;
              TIWEdit(ctrl).Hint := FHint;
              if FMaxLength > 0 then
                TIWEdit(ctrl).Width := FMaxLength * 18;
              if ctrl.Width > 150 then
                ctrl.Width := 150;
            end;
          end;
        ftDate, ftTime:
          begin
            if FLookup.DataField <> '' then
            begin
              ctrl := TIWComboBox.Create(nil);
              TIWComboBox(ctrl).AutoHideOnMenuActivation := True;
              ControlProperty := 'Text';
              FLookup.ControlProperty := 'Items';
              ctrl.Width := 75;
              TIWComboBox(ctrl).Hint := FHint;
            end else
            begin
              ctrl := TIWEdit.Create(nil);
              ControlProperty := 'Text';
              ctrl.Width := 75;
              TIWEdit(ctrl).MaxLength := FMaxLength;
              TIWEdit(ctrl).PasswordPrompt := FPasswordPrompt;
              TIWEdit(ctrl).Hint := FHint;
              if FMaxLength > 0 then
                TIWEdit(ctrl).Width := FMaxLength * 18;
              if ctrl.Width > 150 then
                ctrl.Width := 150;
            end;
          end;
        ftDateTime:
          begin
            if FLookup.DataField <> '' then
            begin
              ctrl := TIWComboBox.Create(nil);
              TIWComboBox(ctrl).AutoHideOnMenuActivation := True;
              ControlProperty := 'Text';
              FLookup.ControlProperty := 'Items';
              ctrl.Width := 150;
              TIWComboBox(ctrl).Hint := FHint;
            end else
            begin
              ctrl := TIWEdit.Create(nil);
              ControlProperty := 'Text';
              ctrl.Width := 150;
              TIWEdit(ctrl).MaxLength := FMaxLength;
              TIWEdit(ctrl).PasswordPrompt := FPasswordPrompt;
              TIWEdit(ctrl).Hint := FHint;
              if FMaxLength > 0 then
                TIWEdit(ctrl).Width := FMaxLength * 18;
              if ctrl.Width > 150 then
                ctrl.Width := 150;
            end;
          end;
        ftMemo, ftFmtMemo:
          begin
            ctrl := TIWMemo.Create(nil);
            ControlProperty := 'Lines';
            ctrl.Width := 200;
            TIWMemo(ctrl).Hint := FHint;
          end;
      end;
    end;
    bFreeControl := True;
    if (not (TDataFieldsCollection(Collection).Content.DatasetState in [dsEdit, dsInsert])) or
      (Row <> TDataFieldsCollection(Collection).Content.SelectedDataRow - TDataFieldsCollection(Collection).Content.TopVisibleRow) then
    begin
      if Supports(ctrl, IIWBaseComponent, intf) then
        if intf.SupportsInput then
          ctrl.Enabled := False;
    end;

    if Assigned(FOnCreateControl) then
    begin
      FOnCreateControl(TDataFieldsCollection(Collection).Content, Self, ctrl, bFreeControl);
    end;
    if bFreeControl and (ctrl <> nil) then
      List.Add(ctrl);
  end else
    if FAlwaysRenderControl then
    begin
      if (not (TDataFieldsCollection(Collection).Content.DatasetState in [dsEdit, dsInsert])) or
        (Row <> TDataFieldsCollection(Collection).Content.SelectedDataRow - TDataFieldsCollection(Collection).Content.TopVisibleRow) then
      begin
        if Supports(ctrl, IIWBaseComponent, intf) then
          if intf.SupportsInput then
            ctrl.Enabled := False;
      end;
      ctrl := CloneControl(FControl, Row);
      //fix FOnCreateControl not be fired.
      //comment by peter 2005/05/10
      if Assigned(FOnCreateControl) then
      begin
        FOnCreateControl(TDataFieldsCollection(Collection).Content, Self, ctrl, bFreeControl);
      end;
      List.Add(ctrl);
    end else
    begin
      ctrl := FControl;
    end;
  if Assigned(ctrl) then
  begin
    if ctrl.Name <> TDataFieldsCollection(Collection).Content.Name + 'zzz' + IntToStr(Self.Index) + 'x' + IntToStr(Row) then
    begin
      ctrl.Name := TDataFieldsCollection(Collection).Content.Name + 'zzz' + IntToStr(Self.Index) + 'x' + IntToStr(Row);
      //clearing the default caption property after the name property is set when there have no user set caption
      //comment by peter 2005/05/04
      if (ctrl.Name = ctrl.Caption) then
        ctrl.Caption := '';
      //
    end;
    //check Grid set before access it.
    //comment by peter 2005/05/05
    if Assigned(TDataFieldsCollection(Collection).Content.GridNoRaise) then
      if ctrl is TIWBaseControl then
        TIWBaseControlHack(ctrl).ParentChanging(ctrl.Parent, TDataFieldsCollection(Collection).Content.Grid);
    //
    //  ctrl.Parent := TWinControl(TDataFieldsCollection(Collection).Content.Grid);

    if FUpdateControlTag then
      ctrl.Tag := Row;
  end;
end;

function TDataFieldItem.CloneControl(ctrl: TIWBaseControl; Row: integer): TIWBaseControl;
  procedure CopyProperties(ctrlFrom, ctrlTo: TObject);
  var
    ppl: PPropList;
    i, iCnt: integer;
    oFrom, oTo: TObject;
  begin
    if ctrlFrom.ClassType <> ctrlTo.ClassType then
      raise Exception.Create('Cannot copy from ' + ctrlFrom.ClassName + ' to ' + ctrlTo.ClassName);
    if ctrlFrom is TStrings then
    begin
      TStrings(ctrlTo).Assign(TStrings(ctrlFrom));
      exit;
    end;
    iCnt := GetPropList(ctrlFrom, ppl);
    for i := 0 to iCnt - 1 do
    begin
      if (ppl^[i]^.PropType^^.Name = 'Name') or
        (ppl^[i]^.PropType^^.Name = 'Left') or
        (ppl^[i]^.PropType^^.Name = 'Top') or
        (ppl^[i]^.PropType^^.Name = 'Width') or
        (ppl^[i]^.PropType^^.Name = 'Height') then
        continue;

      case ppl^[i]^.PropType^^.Kind of
        tkClass:
          begin
            oFrom := GetObjectProp(ctrlFrom, ppl^[i]);
            oTo := GetObjectProp(ctrlTo, ppl^[i]^.Name);
            CopyProperties(oFrom, oTo);
          end;
        tkMethod:
          begin
            SetMethodProp(ctrlTo, ppl^[i]^.Name, GetMethodProp(ctrlFrom, ppl^[i]));
          end;
      else SetPropValue(ctrlTo, ppl^[i]^.Name, GetPropValue(ctrlFrom, ppl^[i]^.Name, False));
      end;
    end;
  end;
begin
  Result := TIWBaseControl(TComponentClass(ctrl.ClassType).Create(nil));
  CopyProperties(ctrl, Result);
end;

constructor TDataFieldItem.Create(Collection: TCollection);
begin
  inherited;
  FColumnIndex := -1;
  FKeyField := False;
  FDataType := ftUnknown;
  FLookup := TDataLookupSettings.Create(Self);
  FFilterAllString := '(all)';
  FDataBoolTrue := '';
  FDataBoolFalse := '';
  FBoolType := '1';
end;

destructor TDataFieldItem.Destroy;
begin
  if FFilterControl <> nil then
  begin
    FFIlterControl.RemoveFreeNotification(TDataFieldsCollection(Collection).Content);
    if FFilterControl.Owner = nil then
    begin
      FFilterControl.Free;
    end;
    FFilterControl := nil;
  end;
  FLookup.Free;
  inherited;
end;

function TDataFieldItem.GetColumnIndex: integer;
begin
  Result := -1;
  //Content maybe not set Grid property. So access Content.Grid will cause AV
  //comment by peter 2005/05/05
  if (FColumn <> '')
    and (not (csWriting in TDataFieldsCollection(Collection).Content.ComponentState))
    and Assigned(TDataFieldsCollection(Collection).Content.GridNoRaise) then
    Result := TDataFieldsCollection(Collection).Content.Grid.Columns.IndexOf(FColumn);

  if Result < 0 then
    Result := FColumnIndex;
end;

function TDataFieldItem.GetDisplayName: string;
begin
  Result := FDataField + ' <' + FColumn + '>';
end;

procedure TDataFieldItem.SetColumn(const Value: string);
var
  idx: integer;
begin
  if FColumn = Value then
    exit;

  FColumn := Value;

  if Assigned(TDataFieldsCollection(Collection).Content.GridNoRaise) and (not InternalRename) then
  begin
    idx := TDataFieldsCollection(Collection).Content.Grid.Columns.IndexOf(FColumn);
    if idx >= 0 then
      TDataFieldsCollection(Collection).Content.Grid.Columns[idx].Caption := Value;
  end;

  InternalRename := False;
end;

procedure TDataFieldItem.SetControl(const Value: TIWBaseControl);
begin
  if FControl = Value then
    exit;
  if (Value = nil) and (FControl <> nil) then
    FControl.RemoveFreeNotification(TDataFieldsCollection(Collection).FContent);
  FControl := Value;
  if (FControl <> nil) then
    FControl.FreeNotification(TDataFieldsCollection(Collection).FContent);
end;

procedure TDataFieldItem.SetFilterControl(const Value: TIWBaseControl);
begin
  if FFilterControl = Value then
    exit;
  if (Value = nil) and (FFilterControl <> nil) then
    FFilterControl.RemoveFreeNotification(TDataFieldsCollection(Collection).FContent);
  FFilterControl := Value;
  if (FFilterControl <> nil) then
    FFilterControl.FreeNotification(TDataFieldsCollection(Collection).FContent);
end;

procedure TDataFieldItem.SetFilterStyle(const Value: TFilterStyle);
begin
  if (FFilterAllString = '') or (FFilterAllString = '(all)') then
    case Value of
      fsText: FFilterAllString := '';
      fsCombo: FFilterAllString := '(all)';
    end;
  FFilterStyle := Value;
end;

procedure TDataFieldItem.SetTmpFileExt(const Value: string);
begin
  FTmpFileExt := Value;
  if (FTmpFileExt <> '') and (FTmpFileExt[1] <> '.') then
    FTmpFileExt := '.' + FTmpFileExt;
end;

{ TDataFieldsCollection }

function TDataFieldsCollection.Add: TDataFieldItem;
begin
  Result := TDataFieldItem(inherited Add);
end;

constructor TDataFieldsCollection.Create(AOwner: TArcIWStringGridDatasetContent);
begin
  inherited Create(TDataFieldItem);
  FContent := AOwner;
end;

function TDataFieldsCollection.GetFields(idx: integer): TDataFieldItem;
begin
  Result := TDataFieldItem(Items[idx]);
end;

function TDataFieldsCollection.GetOwner: TPersistent;
begin
  Result := FContent;
end;

function TDataFieldsCollection.HasField(fieldname: string): boolean;
var
  i: integer;
  sField: string;
begin
  Result := False;
  sField := Uppercase(Fieldname);
  for i := 0 to count - 1 do
    if uppercase(Fields[i].DataField) = sField then
    begin
      result := True;
      break;
    end;
end;

function TDataFieldsCollection.IndexOf(fieldname: string): integer;
var
  i: integer;
  sField: string;
begin
  Result := -1;
  sField := Uppercase(Fieldname);
  for i := 0 to count - 1 do
    if uppercase(Fields[i].DataField) = sField then
    begin
      result := i;
      break;
    end;
end;

function TDataFieldsCollection.CountKeyFields: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
  begin
    if Fields[i].KeyField then
      inc(Result);
  end;
end;

function TDataFieldsCollection.ListKeyFields: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
  begin
    if Fields[i].KeyField then
      Result := Result + ';' + Fields[i].DataField;
  end;
  if Result <> '' then
    System.Delete(Result, 1, 1);
end;

function TDataFieldsCollection.ListKeyValues(Row: integer): Variant;
  function ConvInt(str: string): Variant;
  begin
    if str = '' then
      result := varEmpty
    else
      {$IFNDEF VER130}
      result := StrToInt64(Str);
      {$ELSE}
      result := StrToInt(Str);
      {$ENDIF}
  end;
  function ConvFloat(str: string): Variant;
  begin
    if str = '' then
      result := varEmpty
    else
      result := StrToFloat(Str);
  end;
  function ConvDateTime(str: string): Variant;
  begin
    if str = '' then
      result := varEmpty
    else
      result := Str; // Maybe convert something here?
  end;
var
  i, x: integer;
  iKeyCnt: integer;
  v: variant;
begin
  iKeyCnt := CountKeyFields;
  if iKeyCnt > 1 then
    Result := VarArrayCreate([0, CountKeyFields - 1], varVariant);

  x := 0;
  for i := 0 to Count - 1 do
  begin
    if Fields[i].KeyField then
    begin
      case Fields[i].DataType of
        ftInteger, ftWord, ftBCD, ftAutoInc, ftLargeint:
          begin
            v := ConvInt(Content.Cells[Fields[i].ColumnIndex, Row]);
          end;
        ftDate, ftDateTime, ftTime:
          begin
            v := ConvDateTime(Content.Cells[Fields[i].ColumnIndex, Row]);
          end;
        ftFloat, ftCurrency:
          begin
            v := ConvFloat(Content.Cells[Fields[i].ColumnIndex, Row]);
          end;
      else
        v := Content.Cells[Fields[i].ColumnIndex, Row];
      end;

      if iKeyCnt > 1 then
        Result[x] := v
      else
        Result := v;

      inc(x);
    end;
  end;
end;

procedure TDataFieldsCollection.SetFields(idx: integer; const Value: TDataFieldItem);
begin
  Items[idx].Assign(Value);
end;

function TDataFieldsCollection.FieldFromColumnIndex(idx: integer): TDataFieldItem;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count -1 do
    if Fields[i].ColumnIndex = idx then
    begin
      Result := Fields[i];
      break;
    end;
end;

{ TDataLookupSettings }

constructor TDataLookupSettings.Create(FI: TDataFieldItem);
begin
  inherited Create;
  FFieldItem := FI;
  FKeyValues := TStringList.Create;
  FDisplayValues := TStringList.Create;
  FIndexProperty := 'ItemIndex';
end;

destructor TDataLookupSettings.Destroy;
begin
  FDisplayValues.Free;
  FKeyValues.Free;
  inherited;
end;

function TDataLookupSettings.LockDataset: TDataset;
begin
  result := FLookupDataset;
  if Assigned(FFieldItem.FOnLockLookupDataset) then
    FFieldItem.OnLockLookupDataset(Self, Result);

  if not Assigned(Result) then
    raise Exception.Create('Cannot find an available Lookup dataset.');
  if not Result.Active then
    Result.Open;
end;

procedure TDataLookupSettings.SetLookupDataset(const Value: TDataset);
begin
  if FLookupDataset = Value then
    exit;
  if (Value = nil) and (FLookupDataset <> nil) then
    FLookupDataset.RemoveFreeNotification(TDataFieldsCollection(FFieldItem.Collection).FContent);
  FLookupDataset := Value;
  if (FLookupDataset <> nil) then
    FLookupDataset.FreeNotification(TDataFieldsCollection(FFieldItem.Collection).FContent);
end;

procedure TDataLookupSettings.UnlockDataset(ADataset: TDataset);
begin
  if Assigned(FFieldItem.FOnUnlockLookupDataset) then
    FFieldItem.OnUnlockLookupDataset(Self, ADataset)
end;

end.

