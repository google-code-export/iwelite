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

unit ArcIWStringGridINIContent;

interface

uses SysUtils, {$IFNDEF VER130}Variants, StrUtils, {$ENDIF}Classes, ArcIWStringGridContent, ArcIWStringGrid, db, controls,
  IWBaseControl,IWCompCheckbox, IWCompEdit, IWCompMemo, IWCompButton, IWExtCtrls, IWRenderContext,
  IniFiles, Graphics, TypInfo, ArcFastStrings, IWCompListbox, ArcIWGridCommon;

type
  TArcIWStringGridINIContent = class;
  TValueItem = class;
  TSectionItem = class;
  TAlterValueEvent = procedure(Sender : TObject; Item : TValueItem; var Value : String) of object;
  TEditorOverrideEvent = procedure(Sender : TObject; Item : TValueItem; var Control : TIWBaseControl) of object;
  TEditorConfigureEvent = procedure(Sender : TObject; Item : TValueITem; Control : TIWBaseControl) of object;
  TINICreateControlEvent = procedure(Sender : TObject; ValueItem : TValueItem; var Control : TIWBaseControl; var FreeControl : boolean) of object;
  TLoadLookupItemsEvent = procedure(Sender : TObject; ValueItem : TValueItem; Items : TStrings) of object;
  TFilenameNeededEvent = procedure(Sender : TObject; var aFilename : TFilename) of object;

  TValueItem = class(TCollectionItem)
  private
    FControl : TIWBaseControl;
    FValue : string;
    FValueName: string;
    FDataType: TFieldType;
    FOnWriteValue: TAlterValueEvent;
    FOnReadValue: TAlterValueEvent;
    FEditable: boolean;
    FVisible: boolean;
    FOnEditorOverride: TEditorOverrideEvent;
    FOnEditorConfigure: TEditorConfigureEvent;
    FOnCreateControl: TINICreateControlEvent;
    FUseDefaultEditor: boolean;
    FControlProperty: TControlProperty;
    FLookupItems: TStrings;
    FOnLoadLookupItems: TLoadLookupItemsEvent;
    FLookupControlProperty: TControlProperty;
    FUpdateControlTag: boolean;
    FPasswordPrompt: boolean;
    function GetValue: string;
    procedure SetValue(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: String; override;
    procedure BuildEditor(List : TList; Row : integer; var Ctrl : TIWBaseControl); virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property ValueName : string read FValueName write FValueName;
    property Value : string read GetValue write SetValue;
    property Editable : boolean read FEditable write FEditable default true;
    property Visible : boolean read FVisible write FVisible default true;
    property DataType : TFieldType read FDataType write FDataType;
    property OnReadValue : TAlterValueEvent read FOnReadValue write FOnReadValue;
    property OnWriteValue : TAlterValueEvent read FOnWriteValue write FOnWriteValue;
    property OnEditorOverride : TEditorOverrideEvent read FOnEditorOverride write FOnEditorOverride;
    property OnEditorConfigure : TEditorConfigureEvent read FOnEditorConfigure write FOnEditorConfigure;
    property OnCreateControl : TINICreateControlEvent read FOnCreateControl write FOnCreateControl;
    property UseDefaultEditor : boolean read FUseDefaultEditor write FUseDefaultEditor default True;
    property OnLoadLookupItems : TLoadLookupItemsEvent read FOnLoadLookupItems write FOnLoadLookupItems;
    property ControlProperty : TControlProperty read FControlProperty write FControlProperty;
    property LookupControlProperty : TControlProperty read FLookupControlProperty write FLookupControlProperty;
    property LookupItems : TStrings read FLookupItems write FLookupItems;
    property UpdateControlTag : boolean read FUpdateControlTag write FUpdateControlTag;
    property PasswordPrompt : boolean read FPasswordPrompt write FPasswordPrompt;
  end;

  TValuesCollection = class(TCollection)
  private
    FOwner : TPersistent;
    FContent: TArcIWStringGridINIContent;
    function GetValues(idx: integer): TValueItem;
    procedure SetValues(idx: integer; const Value: TValueItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner : TArcIWStringGridINIContent; Item : TSectionItem); reintroduce; virtual;
    property Values[idx : integer] : TValueItem read GetValues write SetValues; default;
    function HasValue(Valuename : string) : boolean;
    function IndexOf(Valuename : string) : integer;
    function Add : TValueItem; virtual;
    procedure RenderValues(Grid : TArcIWStringGrid; var Row : integer); virtual;
    procedure PostValues(Grid : TArcIWStringGrid; var Row : integer); virtual;
    function CountRows : integer; virtual;
    property Content : TArcIWStringGridINIContent read FContent;
  end;

  TSectionItem = class(TCollectionItem)
  private
    FRow : integer;
    FSectionName: string;
    FValues: TValuesCollection;
    FVisible: boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: String; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Row : integer read FRow;
  published
    property SectionName : string read FSectionName write FSectionName;
    property Values : TValuesCollection read FValues write FValues;
    property Visible : boolean read FVisible write FVisible;
  end;

  TSectionItemsCollection = class(TCollection)
  private
    FContent: TArcIWStringGridINIContent;
    function GetSections(idx: integer): TSectionItem;
    procedure SetSections(idx: integer; const Value: TSectionItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner : TArcIWStringGridINIContent); reintroduce; virtual;
    property Sections[idx : integer] : TSectionItem read GetSections write SetSections; default;
    function HasSection(SectionName : string) : boolean;
    function IndexOf(SectionName : string) : integer;
    function Add : TSectionItem; virtual;
    property Content : TArcIWStringGridINIContent read FContent write FContent;
    procedure ReadValues(ini : TIniFile);
    procedure WriteValues(ini : TIniFile);
    procedure Rebuild(ini : TIniFile);
    procedure RenderSections(Grid : TArcIWStringGrid); virtual;
    procedure PostSections(Grid : TArcIWStringGrid); virtual;
    function CountRows : integer; virtual;
  end;

  TArcIWStringGridINIContent = class(TArcIWStringGridContent)
  private
    FEditors : TList;
    FSinglePage: boolean;
    FFilename: TFilename;
    FSections: TSectionItemsCollection;
    FSelectedSection : TSectionItem;
    FColumnName: string;
    FColumnValue: string;
    FColumnNameIndex : integer;
    FColumnValueIndex : integer;
    FInternalRename: boolean;
    FReadOnly: boolean;
    FOnFilenameNeeded: TFilenameNeededEvent;
    procedure SetColumnName(const Value: string);
    procedure SetColumnValue(const Value: string);
    function GetColumnNameIndex: integer;
    function GetColumnValueIndex: integer;
    procedure SetSinglePage(const Value: boolean);
    procedure SetSelectedSection(const Value: TSectionItem);
  protected
    property InternalRename : boolean read FInternalRename write FInternalRename;
    procedure ClearEditorList; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ResetGridData; virtual;
    function GetIsFirstPage: boolean; override;
    function GetIsFirstRecord: boolean; override;
    function GetIsLastPage: boolean; override;
    function GetIsLastRecord: boolean; override;
    function GetIsNextRecord: boolean; override;
    function GetIsPriorRecord: boolean; override;
    procedure UpdateNavButtonState; override;
    property SelectedSection : TSectionItem read FSelectedSection write SetSelectedSection;
    procedure Loaded; override;
    procedure SetActive(const Value: Boolean); override;
    function GetFirstPage:TSectionItem;
    function GetLastPage:TSectionItem;
    function GetPriorPage:TSectionItem;
    function GetNextPage:TSectionItem;
  public
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;

    procedure StoreEditChanges; virtual;
    procedure DoAfterRenderHTML(Sender: TObject); override;
    procedure DoBeforeRenderHTML(Sender: TObject; AContext: TIWBaseHTMLComponentContext); override;
    procedure DoCellClick(Sender: TObject; const Col: Integer; const Row: Integer; var Data: String); override;
    procedure DoCellData(Sender: TObject; x: Integer; y: Integer; var sValue: String); override;
    procedure DoClickCaption(Sender: TObject; const Col: Integer); override;
    function DoNeedOnClickEvent(Sender: TObject): Boolean; override;
    function DoNeedStyleOverride(Sender: TObject): Boolean; override;
    procedure DoPopulateRow(Sender: TObject; const y: Integer; slValues: TStrings); override;
    procedure DoOverrideCellStyle(Sender : TObject; const col : integer; const Row : Integer; Style : TArcGridStyle); override;
    procedure DoReloadData(Sender: TObject); override;
    procedure DoRender(Sender: TObject); override;
    procedure DoRetrieveObject(Sender: TObject; x: Integer; y: Integer; var ctrl: TControl); override;
    procedure DoRowClick(Sender: TObject; const Row: Integer); override;
    procedure DoSelectCell(Sender: TObject; const x: Integer; const y: Integer; Value: Boolean); override;
    procedure DoSelectCol(Sender: TObject; const x: Integer; Value: Boolean); override;
    procedure DoSelectRow(Sender: TObject; const y: Integer; Value: Boolean); override;
    procedure DoRenameColumn(Sender : TObject; OldName, NewName : String); override;

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

    function ProcessCaption(str : string) : string; override;
    procedure LoadIniFile(aFilename : string = ''; RebuildStructure : boolean = false); virtual;
    procedure SaveIniFile(aFilename : string = ''); virtual;

  published
    property ReadOnly : boolean read FReadOnly write FReadOnly;
    property Sections : TSectionItemsCollection read FSections write FSections;
    property Filename : TFilename read FFilename write FFilename;
    property SinglePage : boolean read FSinglePage write SetSinglePage;
    property ColumnName : string read FColumnName write SetColumnName;
    property ColumnValue : string read FColumnValue write SetColumnValue;
    property ColumnNameIndex : integer read GetColumnNameIndex write FColumnNameIndex;
    property ColumnValueIndex : integer read GetColumnValueIndex write FColumnValueIndex;
    property OnFilenameNeeded : TFilenameNeededEvent read FOnFilenameNeeded write FOnFilenameNeeded;
  end;

implementation

uses ArcIWCustomGrid, IWApplication;

{ TArcIWStringGridINIContent }

constructor TArcIWStringGridINIContent.Create(AOwner: TComponent);
begin
  inherited;
  FEditors := TList.Create;
  SelectedDataRow := -1;
  FSections := TSectionItemsCollection.Create(Self);
end;

destructor TArcIWStringGridINIContent.Destroy;
begin
  ClearEditorList;
  inherited;
  FreeAndNil(FSections);
  FreeAndNil(FEditors);
end;

procedure TArcIWStringGridINIContent.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
end;

procedure TArcIWStringGridINIContent.Append;
begin
  // Not supported
end;

procedure TArcIWStringGridINIContent.Cancel;
begin
  LoadIniFile;
  ResetGridData;
end;

procedure TArcIWStringGridINIContent.Delete;
begin
  // Not supported
end;

procedure TArcIWStringGridINIContent.DoAfterRenderHTML(
  Sender: TObject);
begin

end;

procedure TArcIWStringGridINIContent.DoBeforeRenderHTML(Sender: TObject; AContext: TIWBaseHTMLComponentContext);
begin
end;

procedure TArcIWStringGridINIContent.DoCellClick(Sender: TObject;
  const Col, Row: Integer; var Data: String);
begin

end;

procedure TArcIWStringGridINIContent.DoCellData(Sender: TObject; x,
  y: Integer; var sValue: String);
begin

end;

procedure TArcIWStringGridINIContent.DoClickCaption(Sender: TObject;
  const Col: Integer);
begin

end;

function TArcIWStringGridINIContent.DoNeedOnClickEvent(
  Sender: TObject): Boolean;
begin
  Result := False;
end;

procedure TArcIWStringGridINIContent.DoPopulateRow(Sender: TObject; const y: Integer; slValues: TStrings);
begin

end;

procedure TArcIWStringGridINIContent.DoReloadData(Sender: TObject);
begin
  if Active then
    ResetGridData;
end;

procedure TArcIWStringGridINIContent.DoRender(Sender: TObject);
begin

end;

procedure TArcIWStringGridINIContent.DoRetrieveObject(Sender: TObject; x, y: Integer; var ctrl: TControl);
begin
  if Active then
    ctrl := TControl(Objects[x,y]);
end;

procedure TArcIWStringGridINIContent.DoRowClick(Sender: TObject; const Row: Integer);
begin
end;

procedure TArcIWStringGridINIContent.DoSelectCell(Sender: TObject; const x, y: Integer; Value: Boolean);
begin

end;

procedure TArcIWStringGridINIContent.DoSelectCol(Sender: TObject; const x: Integer; Value: Boolean);
begin

end;

procedure TArcIWStringGridINIContent.DoSelectRow(Sender: TObject; const y: Integer; Value: Boolean);
begin
end;

function TArcIWStringGridINIContent.GetFirstPage:TSectionItem;
var
  A: integer;
begin
  Result := nil;
  for a:=0 to Sections.Count-1 do begin
    if Sections[a].Visible then begin
      Result := Sections[a];
      Break;
    end;
  end;
end;

function TArcIWStringGridINIContent.GetLastPage:TSectionItem;
var
  A: integer;
begin
  Result := nil;
  for A := Sections.Count-1 downto 0 do begin
    if Sections[a].Visible then begin
      Result := Sections[a];
      Break;
    end;
  end;
end;

function TArcIWStringGridINIContent.GetNextPage:TSectionItem;
var
  A: integer;
  LStart: integer;
begin
  Result := SelectedSection;
  LStart := 0;
  if Assigned(Result) then
    LStart := Result.Index+1;

  for a:=LStart to Sections.Count-1 do begin
    if Sections[a].Visible then begin
      Result := Sections[a];
      Break;
    end;
  end;
end;

function TArcIWStringGridINIContent.GetPriorPage:TSectionItem;
var
  A: integer;
  LStart: integer;
begin
  Result := SelectedSection;
  LStart := Sections.Count-1;
  if Assigned(Result) then
    LStart := Result.Index-1;

  for A := LStart downto 0 do begin
    if Sections[a].Visible then begin
      Result := Sections[a];
      Break;
    end;
  end;
end;

procedure TArcIWStringGridINIContent.FirstPage;
begin
  StoreEditChanges;
  SelectedSection := GetFirstPage;
  ResetGridData;
end;

procedure TArcIWStringGridINIContent.LastPage;
begin
  StoreEditChanges;
  SelectedSection := GetLastPage;
  ResetGridData;
end;

procedure TArcIWStringGridINIContent.NextPage;
begin
  StoreEditChanges;
  SelectedSection := GetNextPage;
  ResetGridData;
end;

procedure TArcIWStringGridINIContent.Post;
begin
  StoreEditChanges;
  SaveIniFile;
end;

procedure TArcIWStringGridINIContent.PriorPage;
begin
  StoreEditChanges;
  SelectedSection := GetPriorPage;
  ResetGridData;
end;

procedure TArcIWStringGridINIContent.ResetGridData;
var
  iRow : integer;
begin
  if (not FSinglePage) and (SelectedSection = nil) then
  begin
    if Sections.Count > 0 then
      SelectedSection := GetFirstPage
    else
      exit;
  end;

   while Grid.IWComponentsCount > 0 do begin
     with Grid.Component[0] do begin
       Grid.IWRemoveComponent(Grid.Component[0]);
       Free;
     end;
   end;
     
  if FSinglePage then
  begin
    Grid.RowCount := Sections.CountRows;
    Sections.RenderSections(Grid);
  end else
  begin
    Grid.RowCount := SelectedSection.Values.CountRows;
    iRow := 0;
    SelectedSection.Values.RenderValues(Grid,iRow);
  end;
  UpdateNavButtonState;
end;

procedure TArcIWStringGridINIContent.First;
begin
  // Not Supported;
end;

procedure TArcIWStringGridINIContent.Last;
begin
  // Not Supported;
end;

procedure TArcIWStringGridINIContent.Next;
begin
  // Not Supported;
end;

procedure TArcIWStringGridINIContent.Prior;
begin
  // Not Supported;
end;

procedure TArcIWStringGridINIContent.DoRenameColumn(Sender: TObject;
  OldName, NewName: String);
begin
  InternalRename := True;
  if OldName = FColumnName then
    ColumnName := NewName
  else if OldName = FColumnValue then
    ColumnValue := NewName
  else
    InternalRename := False;
end;

procedure TArcIWStringGridINIContent.Edit;
begin
  // Not Supported;
end;

procedure TArcIWStringGridINIContent.ClearEditorList;
var
  x, y : integer;
begin
  for x := Low(Objects) to High(Objects) do
  begin
    for y := Low(Objects[x]) to High(Objects[x]) do
    begin
        if Assigned(Objects[x,y]) then
          Objects[x,y].Free;
        Objects[x,y] := nil;
    end;
  end;
  FEditors.Clear;
end;

function TArcIWStringGridINIContent.GetIsFirstPage: boolean;
begin
  Result := (SelectedSection = nil) or ((Sections.Count > 0) and (SelectedSection = Sections[0]))
end;

function TArcIWStringGridINIContent.GetIsFirstRecord: boolean;
begin
  Result := True;
end;

function TArcIWStringGridINIContent.GetIsLastPage: boolean;
begin
  Result := (SelectedSection = nil) or ((Sections.Count > 0) and (SelectedSection = Sections[Sections.Count-1]))
end;

function TArcIWStringGridINIContent.GetIsLastRecord: boolean;
begin
  Result := True;
end;

procedure TArcIWStringGridINIContent.UpdateNavButtonState;
begin
  if not Assigned(FGrid) then
    exit;

  Grid.CaptionButtons.PriorPage.Enabled := (not IsFirstPage) and (not SinglePage);
  Grid.CaptionButtons.NextPage.Enabled := (not IsLastPage) and (not SinglePage);
  Grid.CaptionButtons.FirstPage.Enabled := (not IsFirstPage) and (not SinglePage);
  Grid.CaptionButtons.LastPage.Enabled := (not IsLastPage) and (not SinglePage);
  Grid.CaptionButtons.Prior.Enabled := False;
  Grid.CaptionButtons.Next.Enabled := False;
  Grid.CaptionButtons.First.Enabled := False;
  Grid.CaptionButtons.Last.Enabled := False;

  Grid.CaptionButtons.Save.Enabled := True;
  Grid.CaptionButtons.Cancel.Enabled := True;
  Grid.CaptionButtons.New.Enabled := False;
  Grid.CaptionButtons.Edit.Enabled := False;
  Grid.CaptionButtons.Refresh.Enabled := True;
  Grid.CaptionButtons.Delete.Enabled := False;
end;

function TArcIWStringGridINIContent.ProcessCaption(str: string): string;
begin
  if SelectedSection <> nil then
    Result := FastReplace(str,'<SECTION>',SelectedSection.SectionName, false)
  else
    Result := FastReplace(str,'<SECTION>','All', false);
  Result := FastReplace(Result,'<FILENAME>',ExtractFilename(FFilename), false);
  Result := FastReplace(Result,'<FILEPATH>',FFilename, false);
end;

procedure TArcIWStringGridINIContent.Refresh;
begin
  LoadIniFile;
  ResetGridData;
end;

function TArcIWStringGridINIContent.GetIsNextRecord: boolean;
begin
  Result := False;
end;

function TArcIWStringGridINIContent.GetIsPriorRecord: boolean;
begin
  Result := False;
end;

procedure TArcIWStringGridINIContent.SetColumnName(const Value: string);
var
  idx : integer;
begin
  if FColumnName = Value then
    exit;
  if Assigned(GridNoRaise) and (not InternalRename) then
  begin
    idx := Grid.Columns.IndexOf(FColumnName);
    if idx >= 0 then
      Grid.Columns[idx].Caption := Value;
  end;
  FColumnName := Value;

  InternalRename := False;
end;

procedure TArcIWStringGridINIContent.SetColumnValue(const Value: string);
var
  idx : integer;
begin
  if FColumnValue = Value then
    exit;
  if Assigned(GridNoRaise) and (not InternalRename) then
  begin
    idx := Grid.Columns.IndexOf(FColumnValue);
    if idx >= 0 then
      Grid.Columns[idx].Caption := Value;
  end;
  FColumnValue := Value;

  InternalRename := False;
end;

procedure TArcIWStringGridINIContent.LoadIniFile(aFilename: string=''; RebuildStructure : boolean = false);
var
  ini : TIniFile;
begin
  if aFilename <> '' then
    FFilename := aFilename;
  ini := TIniFile.Create(FFilename);
  try
    if RebuildStructure then
      Sections.Rebuild(ini);
    Sections.ReadValues(ini);
  finally
    ini.Free;
  end;
end;

procedure TArcIWStringGridINIContent.SaveIniFile(aFilename: string='');
var
  ini : TIniFile;
begin
  if aFilename <> '' then
    FFilename := aFilename;
  ini := TIniFile.Create(FFilename);
  try
    Sections.WriteValues(ini);
  finally
    ini.Free;
  end;
end;

function TArcIWStringGridINIContent.GetColumnNameIndex: integer;
begin
  Result := -1;
  if (FColumnName <> '') and (not (csWriting in ComponentState)) then
    Result := Grid.Columns.IndexOf(FColumnName);
  if Result < 0 then
    Result := FColumnNameIndex;
end;

function TArcIWStringGridINIContent.GetColumnValueIndex: integer;
begin
  Result := -1;
  if (FColumnValue <> '') and (not (csWriting in ComponentState)) then
    Result := Grid.Columns.IndexOf(FColumnValue);
  if Result < 0 then
    Result := FColumnValueIndex;
end;

function TArcIWStringGridINIContent.DoNeedStyleOverride(
  Sender: TObject): Boolean;
begin
  Result := True;
end;

procedure TArcIWStringGridINIContent.DoOverrideCellStyle(Sender: TObject;
  const col, Row: Integer; Style: TArcGridStyle);
var
  i : integer;
begin
  if FSinglePage then
  begin
    for i := 0 to Sections.Count -1 do
      if Row = Sections[i].Row then
      begin
        Style.Assign(Grid.StyleSelected);
        break;
      end;
  end;
end;

procedure TArcIWStringGridINIContent.StoreEditChanges;
var
  iRow : integer;
begin
  if FSinglePage then
  begin
    Sections.PostSections(Grid);
  end else
  begin
    iRow := 0;
    SelectedSection.Values.PostValues(Grid,iRow);
  end;
end;

procedure TArcIWStringGridINIContent.SetSinglePage(const Value: boolean);
begin
  FSinglePage := Value;
  if FSinglePage then
    SelectedSection := nil;
end;

procedure TArcIWStringGridINIContent.SetSelectedSection(
  const Value: TSectionItem);
begin
  if Value = nil then
    ClearEditorList;
  FSelectedSection := Value;
end;

procedure TArcIWStringGridINIContent.Loaded;
begin
  inherited;
  if Active then
  begin
    if Assigned(FOnFilenameNeeded) then
      FOnFilenameNeeded(Self,FFilename);
    Refresh;
  end;
end;

procedure TArcIWStringGridINIContent.SetActive(const Value: Boolean);
begin
  inherited;
  if (not (csLoading in ComponentState)) and (not (csDesigning in ComponentState)) and Active then
  begin
    if Assigned(FOnFilenameNeeded) then
      FOnFilenameNeeded(Self,FFilename);
    Refresh;
  end;

end;

{ TValueItem }

procedure TValueItem.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TValueItem(Dest).FValueName         := FValueName;
  TValueItem(Dest).FValue             := FValue;
  TValueItem(Dest).FEditable          := FEditable;
  TValueItem(Dest).FVisible           := FVisible;
  TValueItem(Dest).FDataType          := FDataType;
  TValueItem(Dest).FOnReadValue       := FOnReadValue;
  TValueItem(Dest).FOnWriteValue      := FOnWriteValue;
  TValueItem(Dest).FOnEditorOverride  := FOnEditorOverride;
  TValueItem(Dest).FOnEditorConfigure := FOnEditorConfigure;
  TValueItem(Dest).FOnCreateControl       :=  FOnCreateControl;
  TValueItem(Dest).FUseDefaultEditor      :=  FUseDefaultEditor;
  TValueItem(Dest).FOnLoadLookupItems     :=  FOnLoadLookupItems;
  TValueItem(Dest).FControlProperty       :=  FControlProperty;
  TValueItem(Dest).FLookupControlProperty :=  FLookupControlProperty;
  TValueItem(Dest).FLookupItems           :=  FLookupItems;
  TValueItem(Dest).FUpdateControlTag      :=  FUpdateControlTag;
end;

constructor TValueItem.Create(Collection: TCollection);
begin
  inherited;
  FVisible := True;
  FEditable := True;
  FDataType := ftUnknown;
  FUseDefaultEditor := True;
  FValue := '';
  FLookupItems := TStringList.Create;
end;

destructor TValueItem.Destroy;
begin
  FLookupItems.Free;
  inherited;
end;

procedure TValueItem.BuildEditor(List: TList; Row: integer;
  var Ctrl: TIWBaseControl);
var
  bFreeControl : boolean;
begin
  ctrl := nil;
  if (not FEditable) then
    exit;

  if FUseDefaultEditor then
  begin
    case FDataType of
      ftBoolean:
        begin
          ctrl := TIWCheckBox.Create(nil);
          ctrl.Caption := '';
          ControlProperty := 'Checked';
        end;
      ftSmallint, ftInteger, ftWord,
      ftFloat, ftCurrency, ftLargeint:
        begin
          if FLookupItems.Count <> 0 then
          begin
            ctrl := TIWComboBox.Create(nil);
            ControlProperty := 'Text';
            LookupControlProperty := 'Items';
            ctrl.Width := 75;
          end else
          begin
            ctrl := TIWEdit.Create(nil);
            ControlProperty := 'Text';
            TIWEdit(ctrl).PasswordPrompt := FPasswordPrompt;
            ctrl.Width := 75;
          end;
        end;
      ftDate, ftTime:
        begin
          if FLookupItems.Count <> 0 then
          begin
            ctrl := TIWComboBox.Create(nil);
            ControlProperty := 'Text';
            LookupControlProperty := 'Items';
            ctrl.Width := 75;
          end else
          begin
            ctrl := TIWEdit.Create(nil);
            ControlProperty := 'Text';
            TIWEdit(ctrl).PasswordPrompt := FPasswordPrompt;
            ctrl.Width := 75;
          end;
        end;
      ftDateTime:
        begin
          if FLookupItems.Count <> 0 then
          begin
            ctrl := TIWComboBox.Create(nil);
            ControlProperty := 'Text';
            LookupControlProperty := 'Items';
            ctrl.Width := 150;
          end else
          begin
            ctrl := TIWEdit.Create(nil);
            ControlProperty := 'Text';
            TIWEdit(ctrl).PasswordPrompt := FPasswordPrompt;
            ctrl.Width := 150;
          end;
        end;
      ftMemo, ftFmtMemo:
        begin
          ctrl := TIWMemo.Create(nil);
          ControlProperty := 'Items';
          ctrl.Width := 200;
        end;
      else
        begin
          if FLookupItems.Count <> 0 then
          begin
            ctrl := TIWComboBox.Create(nil);
            ControlProperty := 'Text';
            LookupControlProperty := 'Items';
            ctrl.Width := 200;
          end else
          begin
            ctrl := TIWEdit.Create(nil);
            ctrl.Width := 200;
            TIWEdit(ctrl).PasswordPrompt := FPasswordPrompt;
            ControlProperty := 'Text';
          end;
        end;
    end;
  end;
  bFreeControl := True;
  if Assigned(FOnCreateControl) then
  begin
    FOnCreateControl(TValuesCollection(Collection).Content, Self, ctrl, bFreeControl);
  end;

  FControl := Ctrl;  // For Reference in Post;

  if Assigned(ctrl) then
  begin
    if bFreeControl then
      List.Add(ctrl);
    ctrl.Name := TValuesCollection(Collection).Content.Name+'zzz'+IntToStr(Row);
    TValuesCollection(Collection).Content.Grid.IWAddComponent(ctrl);
    if FUpdateControlTag then
      ctrl.Tag := Row;
  end;
end;

function TValueItem.GetDisplayName: String;
begin
  if FValueName = '' then
    Result := '(unnamed)'
  else
    Result := FValueName;
end;

function TValueItem.GetValue: string;
begin
  Result := FValue;
  if Assigned(FOnReadValue) then
    FOnReadValue(TValuesCollection(Collection).Content,Self,Result);
end;

procedure TValueItem.SetValue(const Value: string);
var
  sValue : string;
begin
  sValue := Value;
  if Assigned(FOnWriteValue) then
    FOnWriteValue(TValuesCollection(Collection).Content,Self,sValue);
  FValue := sValue;
end;


{ TSectionItem }

procedure TSectionItem.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TSectionItem(Dest).FSectionName  := FSectionName;
  TSectionItem(Dest).FValues       := FValues;
  TSectionItem(Dest).FVisible      := FVisible;
end;

constructor TSectionItem.Create(Collection: TCollection);
begin
  inherited;
  FValues := TValuesCollection.Create(TSectionItemsCollection(Collection).Content, self);
end;

destructor TSectionItem.Destroy;
begin
  FValues.Free;
  inherited;
end;

function TSectionItem.GetDisplayName: String;
begin
  if FSectionName = '' then
    Result := '(unnamed)'
  else
    Result := FSectionName;
end;

{ TSectionItemsCollection }

function TSectionItemsCollection.Add: TSectionItem;
begin
  Result := TSectionItem(inherited Add);
end;

function TSectionItemsCollection.CountRows: integer;
var
  i : integer;
begin
  Result := 0;
  for i := 0 to Count-1 do
    if Sections[i].Visible then
    begin
      inc(Result);
      inc(Result,Sections[i].Values.CountRows);
    end;
end;

constructor TSectionItemsCollection.Create(
  AOwner: TArcIWStringGridINIContent);
begin
  inherited Create(TSectionItem);
  FContent := AOwner;
end;

function TSectionItemsCollection.GetOwner: TPersistent;
begin
  Result := Content;
end;

function TSectionItemsCollection.GetSections(idx: integer): TSectionItem;
begin
  Result := TSectionItem(Items[idx]);
end;

function TSectionItemsCollection.HasSection(SectionName: string): boolean;
begin
  Result := IndexOf(SectionName)>=0;
end;

function TSectionItemsCollection.IndexOf(SectionName: string): integer;
var
  idx : integer;
begin
  Result := -1;
  for idx := 0 to Count-1 do
  begin
    if Uppercase(Sections[idx].SectionName)=Uppercase(SectionName) then
    begin
      Result := idx;
      break;
    end;
  end;
end;

procedure TSectionItemsCollection.PostSections(Grid: TArcIWStringGrid);
var
  i, iRow : integer;
begin
  iRow := 0;
  for i := 0 to Count-1 do
    if Sections[i].Visible then begin
      inc(iRow);
      Sections[i].Values.PostValues(Grid,iRow);
    end;
end;

procedure TSectionItemsCollection.ReadValues(ini: TIniFile);
var
  iVal, iSec : integer;
begin
  for iSec := 0 to Count-1 do
  begin
    for iVal := 0 to Sections[iSec].Values.Count -1 do
    begin
      case Sections[iSec].Values[iVal].DataType of
        ftSmallint, ftInteger, ftWord, ftBCD, ftAutoInc, ftLargeint:
          begin
            Sections[iSec].Values[iVal].Value :=
              IntToStr(ini.ReadInteger(Sections[iSec].SectionName,Sections[iSec].Values[iVal].ValueName,0));
          end;
        ftDate, ftDateTime{$IFNDEF VER130}, ftTimeStamp{$ENDIF}:
          begin
            Sections[iSec].Values[iVal].Value :=
              DateTimeToStr(ini.ReadDateTime(Sections[iSec].SectionName,Sections[iSec].Values[iVal].ValueName,0));
          end;
        ftTime:
          begin
            Sections[iSec].Values[iVal].Value :=
              TimeToStr(ini.ReadDateTime(Sections[iSec].SectionName,Sections[iSec].Values[iVal].ValueName,0));
          end;
        else
          begin
            Sections[iSec].Values[iVal].Value :=
              ini.ReadString(Sections[iSec].SectionName,Sections[iSec].Values[iVal].ValueName,'');
          end;
      end;
    end;
  end;
end;

procedure TSectionItemsCollection.Rebuild(ini: TIniFile);
var
  slSections, slValues : TStringList;
  iSec, iVal : integer;
  si : TSectionItem;
  vi : TValueItem;
begin
  Clear;
  slSections := TStringList.Create;
  slValues := TStringList.Create;
  try
    ini.ReadSections(slSections);
    for iSec := 0 to slSections.Count-1 do
    begin
      si := TSectionItem.Create(Self);
      si.SectionName := slSections[iSec];
      si.Visible := True;
      slValues.Clear;
      ini.ReadSection(si.SectionName, slValues);
      for iVal := 0 to slValues.Count-1 do
      begin
        vi := TValueItem.Create(si.Values);
        vi.ValueName := slValues[iVal];
        vi.Value := ini.ReadString(si.SectionName,slValues[iVal],'');
        vi.DataType := ftUnknown;
        vi.Editable := True;
        vi.Visible := True;
      end;
    end;
  finally
    slSections.Free;
    slValues.Free;
  end;
end;

procedure TSectionItemsCollection.RenderSections(Grid: TArcIWStringGrid);
var
  i, iRow : integer;
begin
  iRow := 0;
  for i := 0 to Count-1 do
    if Sections[i].Visible then begin
      if Content.ColumnNameIndex >= 0 then
        Grid.Cells[Content.ColumnNameIndex, iRow] := Sections[i].SectionName;
      if Content.ColumnValueIndex >= 0 then
        Grid.Cells[Content.ColumnValueIndex, iRow] := '';
      Sections[i].FRow := iRow;
      inc(iRow);
      Sections[i].Values.RenderValues(Grid,iRow);
    end;
end;

procedure TSectionItemsCollection.SetSections(idx: integer;
  const Value: TSectionItem);
begin
  Items[idx] := Value;
end;

procedure TSectionItemsCollection.WriteValues(ini: TIniFile);
var
  iVal, iSec : integer;
begin
  for iSec := 0 to Count-1 do
  begin
    for iVal := 0 to Sections[iSec].Values.Count -1 do
    begin
      case Sections[iSec].Values[iVal].DataType of
        ftSmallint, ftInteger, ftWord, ftBCD, ftAutoInc, ftLargeint:
          begin
            ini.WriteInteger(Sections[iSec].SectionName,Sections[iSec].Values[iVal].ValueName,StrToInt(Sections[iSec].Values[iVal].Value));
          end;
        ftDate, ftDateTime{$IFNDEF VER130}, ftTimeStamp{$ENDIF}:
          begin
            ini.WriteDateTime(Sections[iSec].SectionName,Sections[iSec].Values[iVal].ValueName,StrToDateTime(Sections[iSec].Values[iVal].Value));
          end;
        ftTime:
          begin
            ini.WriteTime(Sections[iSec].SectionName,Sections[iSec].Values[iVal].ValueName,StrToTime(Sections[iSec].Values[iVal].Value));
          end;
        else
          begin
            ini.WriteString(Sections[iSec].SectionName,Sections[iSec].Values[iVal].ValueName,Sections[iSec].Values[iVal].Value);
          end;
      end;
    end;
  end;
end;

{ TValuesCollection }

function TValuesCollection.Add: TValueItem;
begin
  Result := TValueItem(inherited Add);
end;

function TValuesCollection.CountRows: integer;
var
  i : integer;
begin
  Result := 0;
  for i := 0 to Count-1 do
    if Values[i].Visible then
      inc(Result);
end;

constructor TValuesCollection.Create(AOwner: TArcIWStringGridINIContent; Item : TSectionItem);
begin
  inherited Create(TValueItem);
  FContent := AOwner;
  FOwner := Item;
end;

function TValuesCollection.GetOwner: TPersistent;
begin
  Result := FOwner;//FContent;
end;

function TValuesCollection.GetValues(idx: integer): TValueItem;
begin
  Result := TValueItem(Items[idx]);
end;

function TValuesCollection.HasValue(Valuename: string): boolean;
begin
  Result := IndexOf(Valuename)>=0;
end;

function TValuesCollection.IndexOf(Valuename: string): integer;
var
  idx : integer;
begin
  Result := -1;
  for idx := 0 to Count-1 do
  begin
    if Uppercase(Values[idx].ValueName)=Uppercase(Valuename) then
    begin
      Result := idx;
      break;
    end;
  end;
end;

procedure TValuesCollection.PostValues(Grid: TArcIWStringGrid;
  var Row: integer);
  function RetrieveValue(dt : TFieldType; obj : TObject; prop, defValue : string) : string;
  begin
    Result := '';
    case dt of
      ftBlob, ftMemo: exit;
      else Result := GetStrProp(obj,prop);
    end;
    if Result = '' then
      Result := defValue;
  end;
var
  i : integer;
begin
  for i := 0 to Count-1 do
    if Values[i].Visible then
    begin
      Values[i].Value := RetrieveValue(Values[i].DataType, Values[i].FControl, Values[i].ControlProperty, Values[i].Value);
      Grid.Cells[Content.ColumnValueIndex,Row] := Values[i].Value;
    end;
end;

procedure TValuesCollection.RenderValues(Grid: TArcIWStringGrid; var Row: integer);
  procedure AssignDataToControl(ctrl : TControl; prop, value : string);
  var
    o : TObject;
    ppi : PPropInfo;
  begin
    ppi := GetPropInfo(ctrl,prop);
    if ppi = nil then
      exit;
    case ppi^.PropType^^.Kind of
      tkUnknown, tkEnumeration, tkSet,
      tkMethod, tkArray, tkRecord,
      tkInterface, tkDynArray: raise Exception.Create('Unsupported property type.');
      tkClass:
        begin
          o := GetObjectProp(ctrl,prop);
          if o is TStrings then
            TStrings(o).Text := value
          else if o is TPicture then
          begin
            TPicture(o).LoadFromFile(Value);
          end;
        end;
      else
        SetStrProp(ctrl,prop,value);
    end;
  end;
  procedure AssignLookupDataToControl(ctrl : TControl; vi : TValueItem; value : string);
  var
    o : TObject;
    sl : TStrings;
  begin
    o := GetObjectProp(ctrl,vi.LookupControlProperty);
    if not (o is TStrings) then
      raise Exception.Create('Lookup ControlProperty is not a TStrings descendant.');
    sl := TStrings(o);

    sl.Assign(vi.LookupItems);
    if assigned(vi.FOnLoadLookupItems) then
      vi.OnLoadLookupItems(Content,vi,sl);
  end;
var
  i : integer;
  ctrl : TIWBaseControl;
begin
  for i := 0 to Count-1 do
    if Values[i].Visible then
    begin
      Grid.Cells[Content.ColumnNameIndex,Row] := Values[i].ValueName;
      Grid.Cells[Content.ColumnValueIndex,Row] := Values[i].Value;

      Values[i].BuildEditor(Content.FEditors, Row, ctrl);
      //Grid.Objects[Content.ColumnValueIndex,Row] := ctrl;
      Content.Objects[Content.ColumnValueIndex,Row] := ctrl;

      if Values[i].ControlProperty <> '' then
        AssignDataToControl(ctrl,Values[i].ControlProperty,Grid.Cells[Content.ColumnValueIndex,Row]);
      if Values[i].LookupControlProperty <> '' then
        AssignLookupDataToControl(ctrl,Values[i],Grid.Cells[Content.ColumnValueIndex,Row]);

      inc(Row);
    end;
end;

procedure TValuesCollection.SetValues(idx: integer;
  const Value: TValueItem);
begin
  Items[idx] := Value;
end;

end.
