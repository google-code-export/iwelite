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

unit ArcIWStringGrid;

interface

{$I IntraWebVersion.inc}
{$I Eval.inc}

uses
  SysUtils, Classes, Controls, Graphics, IWHTMLTag, IWControl, IWColor, IWTypes,
  {$IFNDEF VER130}Types,{$ENDIF}
  {$IFDEF INTRAWEB72}IWVCLBaseControl, IWBaseControl, IWBaseHTMLControl, IWRenderContext, IWContainer,
  IWBaseRenderContext, IWBaseHTMLInterfaces, IWHTML40Interfaces, IWFileReference, IWBaseInterfaces,
  IWVCLComponent, IWContainerLayout, IWLayoutMgrForm, IWXMLTag,{$ENDIF}
  IWTemplateProcessorHTML, DB, IWApplication, ArcIWGridCommon, ArcIWCustomGrid, IWServer, IWForm, ArcCommon;

type
  TArcGridStringColumn = class;

  //TRolloverType = (rtNone, rtRow, rtCell, rtCellClickable);
  TArcPopulateRowEvent = procedure(Sender: TObject; const Row: integer; ColData: TStringList) of object;
  TArcCellDataEvent = procedure(Sender: TObject; const Col, Row: integer; var Data: string) of object;

  TCaptionClickEvent = procedure(Sender: TObject; Column: TArcGridStringColumn) of object;
  TColumnClickEvent = procedure(Sender: TObject; Column: TArcGridStringColumn; const Row: Integer; var Data: string) of object;
  TArcCellClickableEvent = procedure(Sender: TObject; Column: TArcGridStringColumn; const Row: integer; var IsClickable : boolean) of object;
  TRowClickEvent = procedure(Sender: TObject; const Row: Integer) of object;

  //comment by peter 2005/05/18
  TOnCellHintEvent = procedure(Sender: TObject; Column: TArcGridStringColumn; const Row: integer; var Hint: string) of object;
  //

{$IFDEF INTRAWEB72}
  TArcGetControlEvent = procedure(Sender: TObject; Column: TArcGridStringColumn; const Row: Integer; var Control: TControl) of object;
  TArcGetCBControlEvent = procedure(Sender: TObject; Column: TArcGridStringColumn; var Control: TControl) of object;
{$ENDIF}

  {$IFDEF VER130}
  TStringDynArray = array of string;
  {$ENDIF}

  TIWCustomControlCracker = class(TIWCustomControl);

  TArcOverrideStyle = procedure(Sender: TObject; Column: TArcGridStringColumn; const Row: Integer; Style: TArcGridStyle; var Updated: boolean) of object;
  TArcOverrideShow = procedure(Sender : TObject; Column: TArcGridStringColumn; const Row: Integer; var Show : boolean) of object;

  TArcIWStringGrid = class;

  TArcGridColumns = class(TOwnedCollection)
  private
    FContent: TArcIWStringGridContentBase;
    FGrid: TArcIWStringGrid;
    function GetColumnState: string;
    procedure SetColumnState(const Value: string);
    function GetItems(idx: integer): TArcGridStringColumn;
    procedure SetItems(idx: integer; const Value: TArcGridStringColumn);
  public
    property Items[idx: integer]: TArcGridStringColumn read GetItems write SetItems; default;
    function Add: TArcGridStringColumn;
    function HasColumn(ColumnCaption: string): boolean; virtual;
    function IndexOf(ColumnCaption: string): integer; virtual;
    property Content: TArcIWStringGridContentBase read FContent write FContent;
    property Grid: TArcIWStringGrid read FGrid write FGrid;
    property ColumnState : string read GetColumnState write SetColumnState;
  end;

  TColumnsWriter = class(TComponent)
  private
    FColumns: TArcGridColumns;
  public
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
    procedure SetColumns(const Value: TArcGridColumns);
  published
    property Columns : TArcGridColumns read FColumns write SetColumns;
  end;

  TControlSizingType = (csDesigned, csFullWidth, csFullHeight, csFull);
  TArcGridStringColumn = class(TCollectionItem)
  private
    FSelected: TBits;
    FOverrideStyle: boolean;
    FCaption: string;
    FValues: TStrings;
    FAlignment: TAlignment;
    FStyleDetailAlt: TArcGridStyle;
    FStyleControlBar: TArcGridStyle;
    FStyleHeader: TArcGridStyle;
    FStyleDetail: TArcGridStyle;
    FWidth: integer;
    FVisible: boolean;
    FOnClick: TColumnClickEvent;
    FOnClickCaption: TCaptionClickEvent;
    FValuesHaveLinks: boolean;
    FClickEventAsLinks: boolean;
    FLinkTarget: string;
    FCaptionIndent: integer;
    FIndent: integer;
    FStyleSelected: TArcGridStyle;
    //FUseMinimumWidth: boolean;
{$IFDEF INTRAWEB72}
    FAutoRetrieveObject: boolean;
    FCaptionIcon: TIWFileReference;
    FCaptionIconAlignment: TArcIconAlign;
    FIcon: TIWFileReference;
    FIconAlignment: TArcIconAlign;
    FControlAlignment: TArcControlAlign;
    FOnRetrieveControl: TArcGetControlEvent;
    FOnRetrieveCBControl: TArcGetCBControlEvent;
    FControlSizing: TControlSizingType;
    FOnCellClickable : TArcCellClickableEvent;

    FHint: string;
    FWrapText: boolean;
    FShowHint: boolean;

{$ENDIF}
    procedure SetValues(const Value: TStrings);
    procedure SetCaption(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function PaddingString(WantDetail: boolean; DefaultPadding: integer): string; virtual;
    property Selected: TBits read FSelected write FSelected;
    procedure MoveTo(AIndex : integer);
  published
    property Alignment: TAlignment read FAlignment write FAlignment;
    property CaptionIndent: integer read FCaptionIndent write FCaptionIndent;
    property Indent: integer read FIndent write FIndent;
    property OverrideStyle: boolean read FOverrideStyle write FOverrideStyle;
    property StyleControlBar: TArcGridStyle read FStyleControlBar write FStyleControlBar;
    property StyleHeader: TArcGridStyle read FStyleHeader write FStyleHeader;
    property StyleDetail: TArcGridStyle read FStyleDetail write FStyleDetail;
    property StyleDetailAlt: TArcGridStyle read FStyleDetailAlt write FStyleDetailAlt;
    property StyleSelected: TArcGridStyle read FStyleSelected write FStyleSelected;
    property Caption: string read FCaption write SetCaption;
    property Values: TStrings read FValues write SetValues;
    property Width: integer read FWidth write FWidth;
    property Visible: boolean read FVisible write FVisible;
    property OnClickCaption: TCaptionClickEvent read FOnClickCaption write FOnClickCaption;
    property OnClick: TColumnClickEvent read FOnClick write FOnClick;
    property OnRenderClick: TColumnClickEvent read FOnClick write FOnClick;
    property ValuesHaveLinks: boolean read FValuesHaveLinks write FValuesHaveLinks;
    property ClickEventAsLinks: boolean read FClickEventAsLinks write FClickEventAsLinks;
    property LinkTarget: string read FLinkTarget write FLinkTarget;
    property WrapText: boolean read FWrapText write FWrapText default true;
    //property UseMinimumWidth: boolean read FUseMinimumWidth write FUseMinimumWidth default false;

    //comment by peter 2005/05/17
    property Hint: string read FHint write FHint;
    property ShowHint: boolean read FShowHint write FShowHint default false;
{$IFDEF INTRAWEB72}
    property AutoRetrieveObject: boolean read FAutoRetrieveObject write FAutoRetrieveObject;
    property CaptionIcon: TIWFileReference read FCaptionIcon write FCaptionIcon;
    property CaptionIconAlignment: TArcIconAlign read FCaptionIconAlignment write FCaptionIconAlignment;
    property Icon: TIWFileReference read FIcon write FIcon;
    property IconAlignment: TArcIconAlign read FIconAlignment write FIconAlignment;
    property OnCellClickable : TArcCellClickableEvent read FOnCellClickable write FOnCellClickable;
    property OnRetrieveControl: TArcGetControlEvent read FOnRetrieveControl write FOnRetrieveControl;
    property OnRetrieveCBControl: TArcGetCBControlEvent read FOnRetrieveCBControl write FOnRetrieveCBControl;
    property ControlAlignment: TArcControlAlign read FControlAlignment write FControlAlignment;
    property ControlSizing: TControlSizingType read FControlSizing write FControlSizing;
{$ENDIF}
  end;

  TCaptionButton = class(TPersistent)
  private
    FDefaultIcon: string;
    FDefaultIconDisabled: string;
    FVisible: boolean;
    FHint: string;
    FAlignment: TAlignment;
    FIcon: TIWFileReference;
    FOrder: integer;
    FPadAfter: integer;
    FButtonType: TCaptionButtonType;
    FIconWidth: integer;
    FIconHeight: integer;
    FEnabled: boolean;
    FIconDisabled: TIWFileReference;
    FConfirm: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property ButtonType: TCaptionButtonType read FButtonType;
    constructor Create(aButtonType: TCaptionButtonType; DefaultIcon, DefaultIconDisabled: string); reintroduce;
    destructor Destroy; override;
    function IconLocation(URLBase: string): string;
  published
    property Enabled: boolean read FEnabled write FEnabled;
    property Visible: boolean read FVisible write FVisible;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property Icon: TIWFileReference read FIcon write FIcon;
    property IconDisabled: TIWFileReference read FIconDisabled write FIconDisabled;
    property Hint: string read FHint write FHint;
    property Order: integer read FOrder write FOrder;
    property PadAfter: integer read FPadAfter write FPadAfter;
    property IconWidth: integer read FIconWidth write FIconWidth default 16;
    property IconHeight: integer read FIconHeight write FIconHeight default 16;
    property Confirm: string read FConfirm write FConfirm;
  end;

  TCaptionButtons = class(TPersistent)
  private
    FRefresh: TCaptionButton;
    FFirst: TCaptionButton;
    FDelete: TCaptionButton;
    FFirstPage: TCaptionButton;
    FNext: TCaptionButton;
    FNextPage: TCaptionButton;
    FEdit: TCaptionButton;
    FCancel: TCaptionButton;
    FPriorPage: TCaptionButton;
    FLast: TCaptionButton;
    FLastPage: TCaptionButton;
    FPrior: TCaptionButton;
    FNew: TCaptionButton;
    FSave: TCaptionButton;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure RenderButtons(HTMLName: string; AContext: TIWBaseHTMLComponentContext; LeftTag, CenterTag, RightTag: TIWHTMLTag);
    procedure HideNavButtons;
    procedure ShowNavButtons;
    procedure HideDefaultNavButtons;
    procedure ShowDefaultNavButtons;
    procedure HideEditButtons;
    procedure ShowEditButtons;
    procedure HideBrowseModeButtons;
    procedure ShowBrowseModeButtons;
    procedure HideEditModeButtons;
    procedure ShowEditModeButtons;
  published
    property PriorPage: TCaptionButton read FPriorPage write FPriorPage;
    property NextPage: TCaptionButton read FNextPage write FNextPage;
    property FirstPage: TCaptionButton read FFirstPage write FFirstPage;
    property LastPage: TCaptionButton read FLastPage write FLastPage;

    property First: TCaptionButton read FFirst write FFirst;
    property Prior: TCaptionButton read FPrior write FPrior;
    property Next: TCaptionButton read FNext write FNext;
    property Last: TCaptionButton read FLast write FLast;

    property Refresh: TCaptionButton read FRefresh write FRefresh;
    property Edit: TCaptionButton read FEdit write FEdit;
    property New: TCaptionButton read FNew write FNew;
    property Delete: TCaptionButton read FDelete write FDelete;
    property Save: TCaptionButton read FSave write FSave;
    property Cancel: TCaptionButton read FCancel write FCancel;
  end;

  TColumnWidthType = (cwtPixel, cwtPercent {, cwtPoint, cwtPica, cwtInch, cwtMM, cwtCM});

  TCaptionButtonClickEvent = procedure(Sender: TObject; ButtonType: TCaptionButtonType) of object;

  TArcIWStringGrid = class(TArcIWCustomGrid{$IFDEF INTRAWEB72}, IIWBaseContainer, IIWHTML40Container, IIWBaseComponent{$ENDIF})
  private
{$IFDEF INTRAWEB72}
    FContainerImplementation: TIWContainerImplementation;
    FOnRender: TNotifyEvent;
{$ENDIF}
    FOnCellData: TArcCellDataEvent;
    FColumns: TArcGridColumns;
    FOnPopulateRow: TArcPopulateRowEvent;
    FOnRowClick: TRowClickEvent;
    FDetailVisible: boolean;
    FAutoHeight: boolean;
    FRowHeight: integer;
    FStyleSelected: TArcGridStyle;
    FOnOverrideCellStyle: TArcOverrideStyle;
    FOnOverrideCellShowIcon : TArcOverrideShow;
    FOnOverrideCellShowControl : TArcOverrideShow;
    FAllowEmptyCells: boolean;
    FScrollbars: TArcScrollbarStyle;
    FSuppressSelRowClick: boolean;
    FColumnWidthType: TColumnWidthType;
    FContent: TArcIWStringGridContentBase;
    FShowButtonBar: boolean;
    FShowControlBar: boolean;
    FCaption: string;
    FCaptionButtons: TCaptionButtons;
    FStyleButtonBar: TArcGridStyle;
    FScrollToSelectedCell: boolean;
    FScrollToSelectedRow: boolean;
    FOnCaptionButtonClick: TCaptionButtonClickEvent;
    FButtonBarHeight: integer;
    FStyleControlBar: TArcGridStyle;
    FControlBarAboveHeader: boolean;
    FWebApplication : TIWApplication;
    //FRollover : TRolloverType;
    //FStyleRollover : TArcGridStyle;

    FRowHeaderHeight: integer;

    //comment by peter 2005/05/16
    FIsFocus: boolean;
    FFocusRow: integer;

    //comment by peter 2005/05/18
    FOnCellHint: TOnCellHintEvent;
    function CWT: string;
  protected
    procedure InitControl; override;
    procedure Dispose(ADispose: Boolean); override;
    function GetFirstSelectedRow: integer;
    procedure SetRowCount(const Value: Integer); override;
    procedure SetContent(const Value: TArcIWStringGridContentBase);
    procedure Submit(const AValue: string); override;
    function SupportsInput: Boolean; override;
    function GetCell(x, y: integer): string; virtual;
    procedure SetCell(x, y: integer; Value: string); virtual;
    function GetObject(x, y: integer): TObject; virtual;
    procedure SetObject(x, y: integer; Value: TObject); virtual;
    function GetSelected(x, y: integer): boolean; virtual;
    procedure SetSelected(x, y: integer; Value: boolean); virtual;
    function GetSelectedRow(y: integer): boolean; virtual;
    procedure SetSelectedRow(y: integer; Value: boolean); virtual;
    function GetSelectedCol(x: integer): boolean; virtual;
    procedure SetSelectedCol(x: integer; Value: boolean); virtual;
    procedure SetColumns(const AValue : TArcGridColumns); virtual;

    procedure EnsureXYValues(x: integer; y: integer = -1); virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RenderAsyncComponents(AContext: TIWContainerContext; APageContext: TIWBasePageContext);

    procedure ContentReload; virtual;
    procedure ContentClickCaption(const Col: integer); virtual;
    procedure ContentOverrideCellStyle(const Col: integer; const Row: Integer; Style: TArcGridStyle); virtual;
    procedure ContentRowClick(const Row: integer); virtual;
    procedure ContentCellClick(const Col, Row: integer; var Data: string); virtual;
    procedure ContentDoRender; virtual;
    procedure ContentSelect(const x, y: integer; Value: boolean); virtual;
    procedure ContentSelectRow(const y: integer; Value: boolean); virtual;
    procedure ContentSelectCol(const x: integer; Value: boolean); virtual;
    procedure ContentBeforeRenderHTML(AContext: TIWBaseHTMLComponentContext); virtual;
    procedure ContentPopulateRow(const y: integer; slValues: TStrings); virtual;
    function ContentNeedOnClickEvent: boolean; virtual;
    function ContentNeedScript : string;
    procedure ContentCellData(x, y: integer; var sValue: string); virtual;
    procedure ContentRetrieveObject(x, y: integer; var ctrl: TControl); virtual;
    procedure ContentRetrieveCBObject(x: integer; var ctrl: TControl); virtual;
    procedure ContentAfterRenderHTML; virtual;
    procedure ContentAssignSession(ASession : TIWApplication);

    procedure SetRowHeaderHeight(const Value: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromDelimitedFile(AFilename: string; ADelimiter: char = ',';
      AQuote: char = '"'; AAutoFields: boolean = true; AReadHeaders: boolean = true;
      AValuesAreLinks: boolean = false; SkipRows: integer = 0); virtual;
    procedure LoadFromStringList(Strings: TStrings; {$IFNDEF VER130}ADelimiter: char = ',';
      AQuote: char = '"'; {$ENDIF}AAutoFields: boolean = true; AReadHeaders: boolean = true;
      AValuesAreLinks: boolean = false; SkipRows: integer = 0); virtual;
    procedure LoadFromDataset(ADataset: TDataset); overload;
    procedure LoadFromDataset(ADataset: TDataset; const AFieldNames: array of string;
      AAutoFields: boolean = true; AStartRow: integer = 0; ACount: integer = -1); overload;
    procedure LoadFromDataset(ADataset: TDataset; const AFieldNames: TStringDynArray;
      AAutoFields: boolean = true; AStartRow: integer = 0; ACount: integer = -1); overload;
    procedure Invalidate; override;
    function SupportsSubmit: Boolean;
    function RenderAsyncComponent(AContext: TIWBaseComponentContext): TIWXMLTag; override;
    property Cells[x, y: integer]: string read GetCell write SetCell;
    property Objects[x, y: integer]: TObject read GetObject write SetObject;
    property Selected[x, y: integer]: boolean read GetSelected write SetSelected;
    property SelectedRow[y: integer]: boolean read GetSelectedRow write SetSelectedRow;
    property SelectedCol[x: integer]: boolean read GetSelectedCol write SetSelectedCol;
    procedure SelectAll; virtual;
    procedure DeselectAll; virtual;

    procedure AddRow; virtual;
    procedure DeleteRow(idx: integer; FreeObjects: boolean = False); virtual;
    procedure InsertRow(idx: integer); virtual;
    procedure RenderScripts(AComponentContext: TIWBaseHTMLComponentContext); override;

{$IFNDEF INTRAWEB72}
    function RenderHTML: TIWHTMLTag; override;
{$ELSE}
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;

    function CheckComponentForRender(AComponent: TComponent): Boolean;
    function ContainerName: string;
    function ContainerPrefix: string;
    procedure DoRender;
    function FindComponentByName(AControl: string): TComponent;
    procedure IWAddComponent(AComponent: TComponent);
    function IWFindComponent(AComponent: TComponent): Integer;
    procedure IWRemoveComponent(AComponent: TComponent);
    function InitContainerContext(AWebApplication: TIWApplication): TIWContainerContext;
    function InterfaceInstance: TComponent;
    procedure RenderComponents(AContainerContext: TIWContainerContext;
      APageContext: TIWBasePageContext);
    function get_Component(AIndex: Integer): TComponent;
    function get_ContainerContext: TIWContainerContext;
    function get_IWComponentsCount: Integer;
    procedure set_ContainerContext(const AContainerContext: TIWContainerContext);
    function GenerateControlPositions: string;
    procedure SetActiveControl(AControl: IIWHTML40Control);
    function get_Height: Integer;
    function get_LayoutMgr: TIWContainerLayout;
    function get_ShowHint: Boolean;
    function get_Width: Integer;
    procedure set_LayoutMgr(Value: TIWContainerLayout);
    procedure set_ShowHint(Value: Boolean);
    property ContainerContext: TIWContainerContext read get_ContainerContext write set_ContainerContext;
    property LayoutMgr: TIWContainerLayout read get_LayoutMgr write set_LayoutMgr;
    property Component[AIndex: Integer]: TComponent read get_Component;
    property IWComponentsCount: Integer read get_IWComponentsCount;
{$ENDIF}
    property FirstSelectedRow: integer read GetFirstSelectedRow;
    function FindValue(const Value: string; var Col, Row: integer): boolean;
    function FindValueInCol(const Value: string; Col: integer): integer;
    function FindValueInRow(const Value: string; Row: integer): integer;
    procedure SelectRowWithValue(const Value: string; Col: integer; DeselectOthers: boolean = True; SelectAll: boolean = False);
    procedure DeselectRowWithValue(const Value: string; Col: integer; SelectOthers: boolean = False; DeselectAll: boolean = False);
    procedure ExportGridStyle(const Filename: string);
    procedure ImportGridStyle(const Filename: string);
    function ContainerClassname: String;

    //comment by peter 2005/05/16
    procedure Focus(Row: integer);
  published
    property AutoColumnWidth;
    property AutoRowHeight;
    property StyleHeader;
    property StyleDetail;
    property StyleDetailAlt;
    property StyleTable;
    property UseAltStyles;
    property RowCount;
    property ShowHeaderRow;
    //property StaticHeader;
    property StyleButtonBar: TArcGridStyle read FStyleButtonBar write FStyleButtonBar;
    property StyleControlBar: TArcGridStyle read FStyleControlBar write FStyleControlBar;
    property StyleSelected: TArcGridStyle read FStyleSelected write FStyleSelected;
    property ScrollToSelectedCell: boolean read FScrollToSelectedCell write FScrollToSelectedCell;
    property ScrollToSelectedRow: boolean read FScrollToSelectedRow write FScrollToSelectedRow;
    property ShowButtonBar: boolean read FShowButtonBar write FShowButtonBar;
    property ShowControlBar: boolean read FShowControlBar write FShowControlBar;
    property AllowEmptyCells: boolean read FAllowEmptyCells write FAllowEmptyCells;
    property Scrollbars: TArcScrollbarStyle read FScrollbars write FScrollbars;
    property AutoHeight: boolean read FAutoHeight write FAutoHeight;
    property Columns: TArcGridColumns read FColumns write SetColumns;
    property OnPopulateRow: TArcPopulateRowEvent read FOnPopulateRow write FOnPopulateRow;
    property OnCellData: TArcCellDataEvent read FOnCellData write FOnCellData;
    property OnRowClick: TRowClickEvent read FOnRowClick write FOnRowClick;
    property OnCaptionButtonClick: TCaptionButtonClickEvent read FOnCaptionButtonClick write FOnCaptionButtonClick;
    property DetailVisible: boolean read FDetailVisible write FDetailVisible;
    property RowHeight: integer read FRowHeight write FRowHeight;
    property OnOverrideCellStyle: TArcOverrideStyle read FOnOverrideCellStyle write FOnOverrideCellStyle;
    property OnOverrideCellShowIcon : TArcOverrideShow read FOnOverrideCellShowIcon write FOnOverrideCellShowIcon;
    property OnOverrideCellShowControl : TArcOverrideShow read FOnOverrideCellShowControl write FOnOverrideCellShowControl;
{$IFDEF INTRAWEB72}
    property OnRender: TNotifyEvent read FOnRender write FOnRender;
{$ENDIF}
    property ColumnsWidthsType: TColumnWidthType read FColumnWidthType write FColumnWidthType;
    property SuppressSelRowClick: boolean read FSuppressSelRowClick write FSuppressSelRowClick;
    property Content: TArcIWStringGridContentBase read FContent write SetContent;
    property Caption: string read FCaption write FCaption;
    property CaptionButtons: TCaptionButtons read FCaptionButtons write FCaptionButtons;
    property ButtonBarHeight: integer read FButtonBarHeight write FButtonBarHeight default 24;
    property ControlBarAboveHeader: boolean read FControlBarAboveHeader write FControlBarAboveHeader;
    //property Rollover : TRolloverType read FRollover write FRollover;
    //property StyleRollover : TArcGridStyle read FStyleRollover write FStyleRollover;

    //Add RowHeaderHeight property
    //comment by peter 2005/04/26
    property RowHeaderHeight: integer read FRowHeaderHeight write SetRowHeaderHeight;
    //

    //comment by peter 2005/05/18
    property OnCellHint: TOnCellHintEvent read FOnCellHint write FOnCellHint;
  end;

  TStyleExportFile = class(TComponent)
  private
    FAutoColumnWidth: boolean;
    FShowHeaderRow: boolean;
    FAllowEmptyCells: boolean;
    FSuppressSelRowClick: boolean;
    FScrollToSelectedRow: boolean;
    FShowControlBar: boolean;
    FAutoHeight: boolean;
    FScrollToSelectedCell: boolean;
    FUseAltStyles: boolean;
    FControlBarAboveHeader: boolean;
    FShowButtonBar: boolean;
    FAutoRowHeight: boolean;
    FRowHeight: integer;
    FButtonBarHeight: integer;
    FCaption: string;
    FStyleTable: TArcGridStyle;
    FStyleButtonBar: TArcGridStyle;
    FStyleDetail: TArcGridStyle;
    FStyleHeader: TArcGridStyle;
    FStyleSelected: TArcGridStyle;
    FStyleDetailAlt: TArcGridStyle;
    FStyleControlBar: TArcGridStyle;
    FScrollbars: TArcScrollbarStyle;
    FCaptionButtons: TCaptionButtons;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyStyleTo(Grid: TArcIWStringGrid);
    procedure ApplyStyleFrom(Grid: TArcIWStringGrid);
  published
    property StyleHeader: TArcGridStyle read FStyleHeader write FStyleHeader;
    property StyleDetail: TArcGridStyle read FStyleDetail write FStyleDetail;
    property StyleDetailAlt: TArcGridStyle read FStyleDetailAlt write FStyleDetailAlt;
    property StyleTable: TArcGridStyle read FStyleTable write FStyleTable;
    property StyleButtonBar: TArcGridStyle read FStyleButtonBar write FStyleButtonBar;
    property StyleControlBar: TArcGridStyle read FStyleControlBar write FStyleControlBar;
    property StyleSelected: TArcGridStyle read FStyleSelected write FStyleSelected;
    property CaptionButtons: TCaptionButtons read FCaptionButtons write FCaptionButtons;

    property AutoColumnWidth: boolean read FAutoColumnWidth write FAutoColumnWidth;
    property AutoRowHeight: boolean read FAutoRowHeight write FAutoRowHeight;
    property UseAltStyles: boolean read FUseAltStyles write FUseAltStyles;
    property ShowHeaderRow: boolean read FShowHeaderRow write FShowHeaderRow;
    property ScrollToSelectedCell: boolean read FScrollToSelectedCell write FScrollToSelectedCell;
    property ScrollToSelectedRow: boolean read FScrollToSelectedRow write FScrollToSelectedRow;
    property ShowButtonBar: boolean read FShowButtonBar write FShowButtonBar;
    property ShowControlBar: boolean read FShowControlBar write FShowControlBar;
    property AllowEmptyCells: boolean read FAllowEmptyCells write FAllowEmptyCells;
    property Scrollbars: TArcScrollbarStyle read FScrollbars write FScrollbars;
    property AutoHeight: boolean read FAutoHeight write FAutoHeight;
    property RowHeight: integer read FRowHeight write FRowHeight;
    property SuppressSelRowClick: boolean read FSuppressSelRowClick write FSuppressSelRowClick;
    property Caption: string read FCaption write FCaption;
    property ButtonBarHeight: integer read FButtonBarHeight write FButtonBarHeight;
    property ControlBarAboveHeader: boolean read FControlBarAboveHeader write FControlBarAboveHeader;
  end;

function _DoCaptionButtonClick: string;
function _btnbar: string;
function _btnbarbtn: string;
function _rowCB: string;
function _colCB: string;
function _rowhead: string;
function _DoCaptionClick: string;
function _colhead: string;
function _rowalt: string;
function _row: string;
function _DoColumnClick: string;
function _selected: string;
function _colalt: string;
function _col: string;
function _SelectedRow: string;
function _ReturnStyle: string;
function _SubmitOnEnterKey: string;
function _tbl: string;


implementation

uses IWUtils, IWGlobal, {$IFNDEF VER130}StrUtils, Math, MaskUtils, {$ENDIF}
  IWBaseForm, uSMCommon;

function _DoCaptionButtonClick: string;
begin
  if GServerController.DebugHTML then
    result := '_DoCaptionButtonClick'
  else
    result := '_1';
end;

function _btnbar: string;
begin
  if GServerController.DebugHTML then
    result := '_btnbar'
  else
    result := '_a';
end;

function _btnbarbtn: string;
begin
  if GServerController.DebugHTML then
    result := '_btnbarbtn'
  else
    result := '_b';
end;

function _rowCB: string;
begin
  if GServerController.DebugHTML then
    result := '_rowCB'
  else
    result := '_c';
end;

function _colCB: string;
begin
  if GServerController.DebugHTML then
    result := '_colCB'
  else
    result := '_d';
end;

function _rowhead: string;
begin
  if GServerController.DebugHTML then
    result := '_rowhead'
  else
    result := '_e';
end;

function _DoCaptionClick: string;
begin
  if GServerController.DebugHTML then
    result := '_DoCaptionClick'
  else
    result := '_f';
end;

function _colhead: string;
begin
  if GServerController.DebugHTML then
    result := '_colhead'
  else
    result := '_g';
end;

function _rowalt: string;
begin
  if GServerController.DebugHTML then
    result := '_rowalt'
  else
    result := '_h';
end;

function _row: string;
begin
  if GServerController.DebugHTML then
    result := '_row'
  else
    result := '_i';
end;

function _DoColumnClick: string;
begin
  if GServerController.DebugHTML then
    result := '_DoColumnClick'
  else
    result := '_j';
end;

function _selected: string;
begin
  if GServerController.DebugHTML then
    result := '_selected'
  else
    result := '_k';
end;

function _colalt: string;
begin
  if GServerController.DebugHTML then
    result := '_colalt'
  else
    result := '_l';
end;

function _col: string;
begin
  if GServerController.DebugHTML then
    result := '_col'
  else
    result := '_m';
end;

function _SelectedRow: string;
begin
  if GServerController.DebugHTML then
    result := '_SelectedRow'
  else
    result := '_n';
end;

function _ReturnStyle: string;
begin
  if GServerController.DebugHTML then
    result := '_ReturnStyle'
  else
    result := '_o';
end;

function _SubmitOnEnterKey: string;
begin
  if GServerController.DebugHTML then
    result := '_SubmitOnEnterKey'
  else
    result := '_p';
end;

function _tbl: string;
begin
  if GServerController.DebugHTML then
    result := '_tbl'
  else
    result := '_q';
end;


{ TCaptionButton }

procedure TCaptionButton.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TCaptionButton(Dest).Enabled := Enabled;
  TCaptionButton(Dest).Visible := Visible;
  TCaptionButton(Dest).Alignment := Alignment;
  TCaptionButton(Dest).Icon.Assign(Icon);
  TCaptionButton(Dest).IconDisabled.Assign(IconDisabled);
  TCaptionButton(Dest).Hint := Hint;
  TCaptionButton(Dest).Order := Order;
  TCaptionButton(Dest).PadAfter := PadAfter;
  TCaptionButton(Dest).IconWidth := IconWidth;
  TCaptionButton(Dest).IconHeight := IconHeight;
end;

constructor TCaptionButton.Create(aButtonType: TCaptionButtonType; DefaultIcon, DefaultIconDisabled: string);
begin
  inherited Create;
  FDefaultIcon := DefaultIcon;
  FDefaultIconDisabled := DefaultIconDisabled;
  FButtonType := aButtonType;
  FIcon := TIWFileReference.Create;
  FIconDisabled := TIWFileReference.Create;
  FIconWidth := 16;
  FIconHeight := 16;
end;

destructor TCaptionButton.Destroy;
begin
  FIcon.Free;
  FIconDisabled.Free;
  inherited;
end;

function TCaptionButton.IconLocation(URLBase: string): string;
begin
  if FEnabled then
  begin
    Result := FIcon.Location(URLBase);
    if Result = '' then
      Result := URLBase + FDefaultIcon;
  end else
  begin
    Result := FIconDisabled.Location(URLBase);
    if Result = '' then
      Result := URLBase + FDefaultIconDisabled;
  end;
end;


{ TCaptionButtons }

procedure TCaptionButtons.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TCaptionButtons(Dest).PriorPage.Assign(PriorPage);
  TCaptionButtons(Dest).NextPage.Assign(NextPage);
  TCaptionButtons(Dest).FirstPage.Assign(FirstPage);
  TCaptionButtons(Dest).LastPage.Assign(LastPage);

  TCaptionButtons(Dest).First.Assign(First);
  TCaptionButtons(Dest).Prior.Assign(Prior);
  TCaptionButtons(Dest).Next.Assign(Next);
  TCaptionButtons(Dest).Last.Assign(Last);

  TCaptionButtons(Dest).Refresh.Assign(Refresh);
  TCaptionButtons(Dest).Edit.Assign(Edit);
  TCaptionButtons(Dest).New.Assign(New);
  TCaptionButtons(Dest).Delete.Assign(Delete);
  TCaptionButtons(Dest).Save.Assign(Save);
  TCaptionButtons(Dest).Cancel.Assign(Cancel);
end;

constructor TCaptionButtons.Create;
begin
  FFirstPage := TCaptionButton.Create(cbtFirstPage, '/gfx/ArcGridFirstPage.png', '/gfx/ArcGridFirstPageDisabled.png');
  FFirstPage.Enabled := True;
  FFirstPage.Visible := False;
  FFirstPage.Hint := 'First Page';
  FFirstPage.Order := 0;
  FFirstPage.Alignment := taLeftJustify;

  FPriorPage := TCaptionButton.Create(cbtPriorPage, '/gfx/ArcGridPriorPage.png', '/gfx/ArcGridPriorPageDisabled.png');
  FPriorPage.Enabled := True;
  FPriorPage.Visible := True;
  FPriorPage.Hint := 'Prior Page';
  FPriorPage.Order := 1;
  FPriorPage.Alignment := taLeftJustify;

  FFirst := TCaptionButton.Create(cbtFirst, '/gfx/ArcGridFirst.png', '/gfx/ArcGridFirstDisabled.png');
  FFirst.Enabled := True;
  FFirst.Visible := False;
  FFirst.Hint := 'First Record';
  FFirst.Order := 2;
  FFirst.Alignment := taLeftJustify;

  FPrior := TCaptionButton.Create(cbtPrior, '/gfx/ArcGridPrior.png', '/gfx/ArcGridPriorDisabled.png');
  FPrior.Enabled := True;
  FPrior.Visible := True;
  FPrior.Hint := 'Prior Record';
  FPrior.Order := 3;
  FPrior.Alignment := taLeftJustify;

  FNext := TCaptionButton.Create(cbtNext, '/gfx/ArcGridNext.png', '/gfx/ArcGridNextDisabled.png');
  FNext.Enabled := True;
  FNext.Visible := True;
  FNext.Hint := 'Next Record';
  FNext.Order := 4;
  FNext.Alignment := taLeftJustify;

  FLast := TCaptionButton.Create(cbtLast, '/gfx/ArcGridLast.png', '/gfx/ArcGridLastDisabled.png');
  FLast.Enabled := True;
  FLast.Visible := False;
  FLast.Hint := 'Last Record';
  FLast.Order := 5;
  FLast.Alignment := taLeftJustify;

  FNextPage := TCaptionButton.Create(cbtNextPage, '/gfx/ArcGridNextPage.png', '/gfx/ArcGridNextPageDisabled.png');
  FNextPage.Enabled := True;
  FNextPage.Visible := True;
  FNextPage.Hint := 'Next Page';
  FNextPage.Order := 6;
  FNextPage.Alignment := taLeftJustify;

  FLastPage := TCaptionButton.Create(cbtLastPage, '/gfx/ArcGridLastPage.png', '/gfx/ArcGridLastPageDisabled.png');
  FLastPage.Enabled := True;
  FLastPage.Visible := False;
  FLastPage.Hint := 'Last Page';
  FLastPage.Order := 7;
  FLastPage.Alignment := taLeftJustify;

  FRefresh := TCaptionButton.Create(cbtRefresh, '/gfx/ArcGridRefresh.png', '/gfx/ArcGridRefreshDisabled.png');
  FRefresh.Enabled := True;
  FRefresh.Visible := True;
  FRefresh.Hint := 'Refresh Grid';
  FRefresh.Order := 8;
  FRefresh.Alignment := taRightJustify;

  FNew := TCaptionButton.Create(cbtNew, '/gfx/ArcGridAppend.png', '/gfx/ArcGridAppendDisabled.png');
  FNew.Enabled := True;
  FNew.Visible := True;
  FNew.Hint := 'New Record';
  FNew.Order := 9;
  FNew.Alignment := taRightJustify;

  FEdit := TCaptionButton.Create(cbtEdit, '/gfx/ArcGridEdit.png', '/gfx/ArcGridEditDisabled.png');
  FEdit.Enabled := True;
  FEdit.Visible := True;
  FEdit.Hint := 'Edit Record';
  FEdit.Order := 10;
  FEdit.Alignment := taRightJustify;

  FDelete := TCaptionButton.Create(cbtDelete, '/gfx/ArcGridDelete.png', '/gfx/ArcGridDeleteDisabled.png');
  FDelete.Enabled := True;
  FDelete.Visible := True;
  FDelete.Hint := 'Delete Record';
  FDelete.Order := 11;
  FDelete.Alignment := taRightJustify;

  FSave := TCaptionButton.Create(cbtSave, '/gfx/ArcGridSave.png', '/gfx/ArcGridSaveDisabled.png');
  FSave.Enabled := False;
  FSave.Visible := True;
  FSave.Hint := 'Save Record';
  FSave.Order := 12;
  FSave.Alignment := taRightJustify;

  FCancel := TCaptionButton.Create(cbtCancel, '/gfx/ArcGridCancel.png', '/gfx/ArcGridCancelDisabled.png');
  FCancel.Enabled := False;
  FCancel.Visible := True;
  FCancel.Hint := 'Cancel Record';
  FCancel.Order := 13;
  FCancel.Alignment := taRightJustify;
end;

destructor TCaptionButtons.Destroy;
begin
  FRefresh.Free;
  FFirst.Free;
  FDelete.Free;
  FFirstPage.Free;
  FNext.Free;
  FNextPage.Free;
  FEdit.Free;
  FCancel.Free;
  FPriorPage.Free;
  FLast.Free;
  FLastPage.Free;
  FPrior.Free;
  FNew.Free;
  FSave.Free;
  inherited;
end;

function ReorderButtons(Item1, Item2: pointer): integer;
begin
  Result := TCaptionButton(Item1).Order - TCaptionButton(Item2).Order;
end;

procedure TCaptionButtons.HideBrowseModeButtons;
begin
  FNew.Visible := False;
  FEdit.Visible := False;
  FDelete.Visible := False;
  FRefresh.Visible := False;
end;

procedure TCaptionButtons.HideDefaultNavButtons;
begin
  FNextPage.Visible := False;
  FNext.Visible := False;
  FPriorPage.Visible := False;
  FPrior.Visible := False;
end;

procedure TCaptionButtons.HideEditButtons;
begin
  FNew.Visible := False;
  FEdit.Visible := False;
  FDelete.Visible := False;
  FRefresh.Visible := False;
  FSave.Visible := False;
  FCancel.Visible := False;
end;

procedure TCaptionButtons.HideEditModeButtons;
begin
  FSave.Visible := False;
  FCancel.Visible := False;
end;

procedure TCaptionButtons.HideNavButtons;
begin
  FNextPage.Visible := False;
  FNext.Visible := False;
  FPriorPage.Visible := False;
  FPrior.Visible := False;
  FFirst.Visible := False;
  FFirstPage.Visible := False;
  FLast.Visible := False;
  FLastPage.Visible := False;
end;

procedure TCaptionButtons.RenderButtons(HTMLName: string; AContext: TIWBaseHTMLComponentContext; LeftTag, CenterTag, RightTag: TIWHTMLTag);
  procedure RenderButton(AContext: TIWBaseHTMLComponentContext; Button: TCaptionButton; LeftTag, CenterTag, RightTag: TIWHTMLTag; var iLeft, iRight: integer);
  var
    tag: TIWHTMLTag;
    sLoc, sStyle: string;
  begin
    if Button.Visible then
    begin
      tag := nil;
      sLoc := Button.IconLocation(AContext.PageContext.WebApplication.InternalURLBase);
      if BrowserIsIE(AContext.Browser) and (lowercase(ExtractFileExt(sLoc)) = '.png') then
      begin
        case Button.Alignment of
          taLeftJustify: tag := LeftTag.Contents.AddTag('span');
          taCenter: tag := CenterTag.Contents.AddTag('span');
          taRightJustify: tag := RightTag.Contents.AddTag('span');
        end;

        tag.AddStringParam('title', Button.Hint);
        //add 'px' char
        //comment by peter 2005/05/11
        //sStyle := 'height:' + IntToStr(Button.IconHeight) + ';width:' + IntToStr(Button.IconWidth) + ';padding-right:' + IntToStr(Button.PadAfter) + ';';
        sStyle := 'height:' + IntToStr(Button.IconHeight) + 'px;width:' + IntToStr(Button.IconWidth) + 'px;padding-right:' + IntToStr(Button.PadAfter) + ';';
        sStyle := sStyle + 'filter:progid:DXImageTransform.Microsoft.AlphaImageLoader(enabled=true,src=''' + sLoc + ''',sizingMethod=''scale'');';
      end
      else
      begin
        case Button.Alignment of
          taLeftJustify: tag := LeftTag.Contents.AddTag('img');
          taCenter: tag := CenterTag.Contents.AddTag('img');
          taRightJustify: tag := RightTag.Contents.AddTag('img');
        end;
        tag.AddStringParam('title', Button.Hint);
        tag.AddStringParam('src', sLoc);
        tag.AddIntegerParam('border', 0);
        //add 'px' char
        //comment by peter 2005/05/11
        //tag.AddIntegerParam('width', Button.IconWidth);
        //tag.AddIntegerParam('height', Button.IconHeight);
        sStyle := 'height:' + IntToStr(Button.IconHeight) + 'px;width:' + IntToStr(Button.IconWidth) + 'px;';

        sStyle := sStyle + 'padding-right:' + IntToStr(Button.PadAfter) + ';';
      end;
      if Button.Enabled then
      begin
        tag.AddStringParam('style', sStyle + 'cursor:hand;');
        tag.AddStringParam('onClick', HTMLName + _DoCaptionButtonClick + '(' + IntToStr(Integer(Button.ButtonType)) + ',''' + Button.Confirm + ''');');
      end else
        tag.AddStringParam('style', sStyle);

      case Button.Alignment of
        taLeftJustify: inc(iLeft, Button.IconWidth + Button.PadAfter);
        taRightJustify: inc(iRight, Button.IconWidth + Button.PadAfter);
      end;
    end;
  end;
var
  ls: TList;
  i, iLeft, iRight: integer;
begin
  ls := TList.Create;
  try
    ls.Add(FRefresh);
    ls.Add(FFirst);
    ls.Add(FDelete);
    ls.Add(FFirstPage);
    ls.Add(FNext);
    ls.Add(FNextPage);
    ls.Add(FEdit);
    ls.Add(FCancel);
    ls.Add(FPriorPage);
    ls.Add(FLast);
    ls.Add(FLastPage);
    ls.Add(FPrior);
    ls.Add(FNew);
    ls.Add(FSave);
    ls.Sort(ReorderButtons);

    iLeft := 0;
    iRight := 0;
    for i := 0 to ls.Count - 1 do
      RenderButton(AContext, TCaptionButton(ls[i]), LeftTag, CenterTag, RightTag, iLeft, iRight);

    if iLeft > 0 then
      LeftTag.AddIntegerParam('width', iLeft)
    else
      LeftTag.AddStringParam('width', '*');
    if iRight > 0 then
      RightTag.AddIntegerParam('Width', iRight)
    else
      RightTag.AddStringParam('width', '*');

    if LeftTag.Contents.Count = 0 then
      LeftTag.Contents.AddText('&nbsp;');
    if RightTag.Contents.Count = 0 then
      RightTag.Contents.AddText('&nbsp;');

    CenterTag.AddStringParam('width', '*');
  finally
    ls.Free;
  end;
end;

procedure TCaptionButtons.ShowBrowseModeButtons;
begin
  FNew.Visible := True;
  FEdit.Visible := True;
  FDelete.Visible := True;
  FRefresh.Visible := True;
end;

procedure TCaptionButtons.ShowDefaultNavButtons;
begin
  FNextPage.Visible := True;
  FNext.Visible := True;
  FPriorPage.Visible := True;
  FPrior.Visible := True;
end;

procedure TCaptionButtons.ShowEditButtons;
begin
  FNew.Visible := True;
  FEdit.Visible := True;
  FDelete.Visible := True;
  FRefresh.Visible := True;
  FSave.Visible := True;
  FCancel.Visible := True;
end;

procedure TCaptionButtons.ShowEditModeButtons;
begin
  FSave.Visible := True;
  FCancel.Visible := True;
end;

procedure TCaptionButtons.ShowNavButtons;
begin
  FNextPage.Visible := True;
  FNext.Visible := True;
  FPriorPage.Visible := True;
  FPrior.Visible := True;
  FFirst.Visible := True;
  FFirstPage.Visible := True;
  FLast.Visible := True;
  FLastPage.Visible := True;
end;

{ TArcIWStringGrid }

constructor TArcIWStringGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TArcIWStringGrid.Destroy;
begin
  inherited;
end;

procedure TArcIWStringGrid.InitControl;
begin
  FContainerImplementation := TIWContainerImplementation.Create(Self);
  inherited InitControl;
  FButtonBarHeight := 24;
{$IFDEF INTRAWEB72}
  SupportsPartial := True;
{$ENDIF}
  FColumns := TArcGridColumns.Create(Self, TArcGridStringColumn);
  FColumns.Grid := self;
  FCaptionButtons := TCaptionButtons.Create;
  FDetailVisible := True;
  FAutoHeight := False;
  FRowHeight := 0;
  FContent := nil;
  FScrollbars := scrBoth;

  FStyleButtonBar := TArcGridStyle.Create(False, True, False, False);
  FStyleButtonBar.BackgroundColor := $00800000;
  FStyleButtonBar.BorderStyle.Style := brdSolid;
  FStyleButtonBar.BorderStyle.Width := 1;
  FStyleButtonBar.BorderStyle.Color := $00000000;
  FStyleButtonBar.Font.Color := clWebWhite;
  FStyleButtonBar.Font.Style := [fsBold];
  FStyleButtonBar.Font.FontName := 'Arial';
  FStyleButtonBar.Font.Size := 10;
  FStyleButtonBar.Padding := 0;

  FStyleControlBar := TArcGridStyle.Create(False, True, False, True);
  FStyleControlBar.TextAlign := taCenter;
  FStyleControlBar.TextVertAlign := vaMiddle;
  FStyleControlBar.BackgroundColor := $00800000;
  FStyleControlBar.BorderStyle.Style := brdSolid;
  FStyleControlBar.BorderStyle.Width := 1;
  FStyleControlBar.BorderStyle.Color := clWebBlack;
  FStyleControlBar.Font.Color := clWebWhite;
  FStyleControlBar.Font.Style := [fsBold];
  FStyleControlBar.Font.FontName := 'Arial';
  FStyleControlBar.Font.Size := 9;
  FStyleControlBar.Padding := 0;

  FStyleSelected := TArcGridStyle.Create(False, True, False, True);
  FStyleSelected.BackgroundColor := $00600000;
  FStyleSelected.BorderStyle.Style := brdSolid;
  FStyleSelected.BorderStyle.Width := 1;
  FStyleSelected.BorderStyle.Color := clWebBlack;
  FStyleSelected.Font.Color := clWebWhite;
  FStyleSelected.Font.Style := [fsBold];
  FStyleSelected.Font.FontName := 'Arial';
  FStyleSelected.Font.Size := 10;
  FStyleSelected.Padding := 2;
  //FRollover := rtNone;
  //FStyleRollover := TArcGridStyle.Create(False,True,False,True);

  FRowHeaderHeight := 0;

  //comment by peter 2005/05/16
  FIsFocus := false;
  FFocusRow := -1;
  //comment by peter 2005/05/18
  FOnCellHint := nil;

end;

procedure TArcIWStringGrid.Dispose(ADispose: Boolean);
begin
  //FStyleRollover.Free;
  if Assigned(FContent) then
    FContent.RemoveFreeNotification(Self);
  FColumns.Free;
  FCaptionButtons.Free;
  FStyleControlBar.Free;
  FStyleButtonBar.Free;
  FStyleSelected.Free;
  inherited Dispose(ADispose);
  FContainerImplementation.Free;
end;

procedure TArcIWStringGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FContent) then
    FContent := nil;
end;

procedure TArcIWStringGrid.Loaded;
var
  x: integer;
begin
  inherited;
  if FColumns.Count > 0 then
    for x := 0 to FColumns.Count - 1 do
      EnsureXYValues(x);
  try
    if Assigned(FContent) then
      ContentReload;
  except
  end;
end;

procedure TArcIWStringGrid.ExportGridStyle(const Filename: string);
var
  fs: TFileStream;
  ms: TMemoryStream;
  sf: TStyleExportFile;
begin
  fs := TFileStream.Create(Filename, fmOpenWrite or fmCreate);
  ms := TMemoryStream.Create;
  try
    sf := TStyleExportFile.Create(nil);
    try
      sf.ApplyStyleFrom(Self);
      ms.WriteComponent(sf);
      ms.Position := 0;
      ObjectBinaryToText(ms, fs);
    finally
      sf.Free;
    end;
  finally
    ms.Free;
    fs.Free;
  end;
end;

function TArcIWStringGrid.ContainerClassname: String;
begin
  Result := ClassName;
end;

procedure TArcIWStringGrid.ImportGridStyle(const Filename: string);
var
  fs: TFileStream;
  ms: TMemoryStream;
  sf: TStyleExportFile;
begin
  fs := TFileStream.Create(Filename, fmOpenRead);
  ms := TMemoryStream.Create;
  try
    sf := TStyleExportFile.Create(nil);
    try
      ObjectTextToBinary(fs, ms);
      ms.Position := 0;
      ms.ReadComponent(sf);
      sf.ApplyStyleTo(Self);
    finally
      sf.Free;
    end;
  finally
    fs.Free;
    ms.Free;
  end;
end;

//comment by peter 2005/05/16

procedure TArcIWStringGrid.Focus(Row: integer);
begin
  if not SelectedRow[Row] then
  begin
    DeselectAll;
    SelectedRow[Row] := True;
  end;
  FIsFocus := true;
  FFocusRow := Row;
end;


procedure TArcIWStringGrid.ContentReload;
begin
  if (not (csDesigning in ComponentState)) and Assigned(FContent) then
    FContent.DoReloadData(Self);
end;

procedure TArcIWStringGrid.ContentOverrideCellStyle(const Col: integer; const Row: Integer; Style: TArcGridStyle);
begin
  if (not (csDesigning in ComponentState)) and Assigned(FContent) then
    FContent.DoOverrideCelLStyle(Self, col, Row, Style);
end;

procedure TArcIWStringGrid.ContentClickCaption(const Col: integer);
begin
  if (not (csDesigning in ComponentState)) and Assigned(FContent) then
    FContent.DoClickCaption(Self, col);
end;

procedure TArcIWStringGrid.ContentRowClick(const Row: integer);
begin
  if (not (csDesigning in ComponentState)) and Assigned(FContent) then
    FContent.DoRowClick(Self, Row);
end;

procedure TArcIWStringGrid.ContentCellClick(const Col, Row: integer; var Data: string);
begin
  if (not (csDesigning in ComponentState)) and Assigned(FContent) then
    FContent.DoCellClick(Self, Col, Row, Data);
end;

procedure TArcIWStringGrid.ContentDoRender;
begin
  if (not (csDesigning in ComponentState)) and Assigned(FContent) then
    FContent.DoRender(Self);
end;

procedure TArcIWStringGrid.ContentSelect(const x, y: integer; Value: boolean);
begin
  if (not (csDesigning in ComponentState)) and Assigned(FContent) then
    FContent.DoSelectCell(Self, x, y, Value);
end;

procedure TArcIWStringGrid.ContentSelectRow(const y: integer; Value: boolean);
begin
  if (not (csDesigning in ComponentState)) and Assigned(FContent) then
    FContent.DoSelectRow(Self, y, Value);
end;

procedure TArcIWStringGrid.ContentSelectCol(const x: integer; Value: boolean);
begin
  if (not (csDesigning in ComponentState)) and Assigned(FContent) then
    FContent.DoSelectCol(Self, x, Value);
end;

procedure TArcIWStringGrid.ContentBeforeRenderHTML(AContext: TIWBaseHTMLComponentContext);
begin
  if (not (csDesigning in ComponentState)) and Assigned(FContent) then
    FContent.DoBeforeRenderHTML(Self, AContext);
end;

procedure TArcIWStringGrid.ContentPopulateRow(const y: integer; slValues: TStrings);
begin
  if (not (csDesigning in ComponentState)) and Assigned(FContent) then
    FContent.DoPopulateRow(Self, y, slValues);
end;

function TArcIWStringGrid.ContentNeedScript : string;
begin
  Result := '';
  if Assigned(FContent) then
    Result := FContent.DoNeedScript;
end;

function TArcIWStringGrid.ContentNeedOnClickEvent: boolean;
begin
  Result := False;
  if (not (csDesigning in ComponentState)) and Assigned(FContent) then
    Result := FContent.DoNeedOnClickEvent(Self);
end;

procedure TArcIWStringGrid.ContentAssignSession(ASession : TIWApplication);
begin
  if Assigned(FContent) then
    FContent.DoAssignSession(ASession);
end;

procedure TArcIWStringGrid.ContentCellData(x, y: integer; var sValue: string);
begin
  if (not (csDesigning in ComponentState)) and Assigned(FContent) then
    FContent.DoCellData(Self, x, y, sValue);
end;

procedure TArcIWStringGrid.ContentRetrieveCBObject(x: integer; var ctrl: TControl);
begin
  if (not (csDesigning in ComponentState)) and Assigned(FContent) then
    FContent.DoRetrieveCBObject(Self, x, ctrl);
end;

procedure TArcIWStringGrid.ContentRetrieveObject(x, y: integer; var ctrl: TControl);
begin
  if (not (csDesigning in ComponentState)) and Assigned(FContent) then
    FContent.DoRetrieveObject(Self, x, y, ctrl);
end;

procedure TArcIWStringGrid.ContentAfterRenderHTML;
begin
  if (not (csDesigning in ComponentState)) and Assigned(FContent) then
    FContent.DoAfterRenderHTML(Self);
end;

procedure TArcIWStringGrid.AddRow;
begin
  RowCount := RowCount + 1;
end;

function TArcIWStringGrid.FindValue(const Value: string; var Col, Row: integer): boolean;
var
  x, y: integer;
begin
  Result := False;
  Col := -1;
  Row := -1;
  for x := 0 to FColumns.Count - 1 do
  begin
    for y := 0 to RowCount - 1 do
    begin
      if Cells[x, y] = Value then
      begin
        Col := x;
        Row := y;
        result := True;
        break;
      end;
    end;
  end;
end;

function TArcIWStringGrid.FindValueInCol(const Value: string; Col: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to RowCount - 1 do
  begin
    if Cells[Col, i] = Value then
    begin
      result := i;
      break;
    end;
  end;
end;

function TArcIWStringGrid.FindValueInRow(const Value: string; Row: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FColumns.Count - 1 do
  begin
    if Cells[i, Row] = Value then
    begin
      result := i;
      break;
    end;
  end;
end;

procedure TArcIWStringGrid.SelectRowWithValue(const Value: string; Col: integer; DeselectOthers: boolean = True; SelectAll: boolean = False);
var
  i: integer;
begin
  for i := 0 to RowCount - 1 do
  begin
    if Cells[Col, i] = Value then
    begin
      SelectedRow[i] := True;
      if not SelectAll then
        break;
    end else
      if DeselectOthers then
        SelectedRow[i] := False;
  end;
end;

procedure TArcIWStringGrid.DeselectRowWithValue(const Value: string; Col: integer; SelectOthers: boolean = False; DeselectAll: boolean = False);
var
  i: integer;
begin
  for i := 0 to RowCount - 1 do
  begin
    if Cells[Col, i] = Value then
    begin
      SelectedRow[i] := False;
      if not DeselectAll then
        break;
    end else
      if SelectOthers then
        SelectedRow[i] := True;
  end;
end;

procedure TArcIWStringGrid.DeleteRow(idx: integer; FreeObjects: boolean = False);
var
  i: integer;
begin
  if (idx < 0) or (idx > RowCount - 1) then
    raise EListError.Create('List Index Out of Bounds');
  for i := 0 to FColumns.Count - 1 do
  begin
    if FreeObjects then
    begin
      if (Objects[i, idx] <> nil) and (Objects[i, idx] is TComponent) then
        IWRemoveComponent(TComponent(Objects[i, idx]));
      Objects[i, idx].Free;
    end;
    FColumns[i].Values.Delete(idx);
  end;
  RowCount := RowCount - 1;
end;

procedure TArcIWStringGrid.InsertRow(idx: integer);
var
  i: integer;
begin
  if (idx < 0) or (idx > RowCount - 1) then
    raise EListError.Create('List Index Out of Bounds');
  for i := 0 to FColumns.Count - 1 do
  begin
    FColumns[i].Values.Insert(idx, '');
  end;
  RowCount := RowCount + 1;
end;

procedure TArcIWStringGrid.EnsureXYValues(x: integer; y: integer = -1);
var
  bResized: boolean;
begin
  if y < 0 then y := FRowCount;
  if (x < 0) or (x > FColumns.Count - 1) then
    raise EListError.Create('X is out of bounds');
  if (y < 0) or (y > RowCount) then
    raise EListError.Create('Y is out of bounds');

  bResized := False;
  while FColumns[x].Values.Count <= y do
  begin
    bResized := True;
    FColumns[x].Values.Add('');
  end;
  FColumns[x].Selected.Size := RowCount;

  if bResized and Assigned(Content) then
    Content.DoResizeGrid(Self, FColumns.Count, FRowCount);
end;

function TArcIWStringGrid.GetCell(x, y: integer): string;
begin
  EnsureXYValues(x, y);
  Result := FColumns[x].Values[y];
end;

procedure TArcIWStringGrid.SetCell(x, y: integer; Value: string);
begin
  EnsureXYValues(x, y);
  FColumns[x].Values[y] := Value;
  Invalidate;
end;

procedure TArcIWStringGrid.SetRowHeaderHeight(const Value: integer);
begin
  FRowHeaderHeight := Value;
end;

function TArcIWStringGrid.GetObject(x, y: integer): TObject;
begin
  EnsureXYValues(x, y);
  Result := FColumns[x].Values.Objects[y];
end;

procedure TArcIWStringGrid.SetObject(x, y: integer; Value: TObject);
begin
  EnsureXYValues(x, y);
  FColumns[x].Values.Objects[y] := Value;
  Invalidate;
end;

procedure TArcIWStringGrid.Submit(const AValue: string);
var
  sType: string;
  iRow, iCol: integer;
  sData: string;
  bt: TCaptionButtonType;
begin
  sType := Copy(AValue, 1, 1);
  if sType = 'h' then
  begin
    Invalidate;
    iCol := StrToIntDef(Copy(AValue, 3, High(Integer)), -1);
    if (iCol < 0) or (iCol > FColumns.Count - 1) then
      raise Exception.Create('Invalid Column Number');
    if Assigned(TArcGridStringColumn(FColumns.Items[iCol]).OnClickCaption) then
      TArcGridStringColumn(FColumns.Items[iCol]).OnClickCaption(Self, TArcGridStringColumn(FColumns.Items[iCol]));
    ContentClickCaption(iCol);
  end else
    if sType = 'c' then
    begin
      Invalidate;
      sType := Copy(AValue, 3, High(Integer));
      iRow := StrToIntDef(Copy(sType, 1, Pos('x', sType) - 1), -1);
      iCol := StrToIntDef(Copy(sType, Pos('x', sType) + 1, high(integer)), -1);

      if (iCol < 0) or (iCol > FColumns.Count - 1) then
        raise Exception.Create('Invalid Column Number');

      if (iRow < 0) or (iRow > FRowCount - 1) then
        raise Exception.Create('Invalid Row Number');

      ContentRowClick(iRow);

      if Assigned(FOnRowClick) then
      begin
        FOnRowClick(Self, iRow);
      end;

      sData := '';
      if iRow < TArcGridStringColumn(FColumns.Items[iCol]).Values.Count then
        sData := TArcGridStringColumn(FColumns.Items[iCol]).Values[iRow];

      ContentCellClick(iCol, iRow, sData);

      if Assigned(TArcGridStringColumn(FColumns.Items[iCol]).OnClick) then
        TArcGridStringColumn(FColumns.Items[iCol]).OnClick(Self, TArcGridStringColumn(FColumns.Items[iCol]), iRow, sData);

      while TArcGridStringColumn(FColumns.Items[iCol]).Values.Count < RowCount do
        TArcGridStringColumn(FColumns.Items[iCol]).Values.Add('');
      if iRow < TArcGridStringColumn(FColumns.Items[iCol]).Values.Count then
        TArcGridStringColumn(FColumns.Items[iCol]).Values[iRow] := sData;
    end else
      if sType = 'b' then
      begin
        Invalidate;
        bt := TCaptionButtonType(StrToInt(Copy(AValue, 2, High(Integer))));

        if Assigned(FOnCaptionButtonClick) then
          FOnCaptionButtonClick(Self, bt);

        if Assigned(FContent) then
        begin
          case bt of
            cbtFirstPage: FContent.FirstPage;
            cbtPriorPage: FContent.PriorPage;
            cbtNextPage: FContent.NextPage;
            cbtLastPage: FContent.LastPage;
            cbtFirst: FContent.First;
            cbtPrior: FContent.Prior;
            cbtNext: FContent.Next;
            cbtLast: FContent.Last;
            cbtNew: FContent.Append;
            cbtEdit: FContent.Edit;
            cbtDelete: FContent.Delete;
            cbtSave: FContent.Post;
            cbtCancel: FContent.Cancel;
            cbtRefresh: FContent.Refresh;
          end;
        end;

      end else
        if sType = '@' then
        begin
          if Assigned(FContent) then
            FContent.DoSubmit(Self, AValue)
          else
            raise Exception.Create('Unknown Column Click Value');
        end else
          raise Exception.Create('Unknown Column Click Value');
end;

function TArcIWStringGrid.SupportsInput: Boolean;
begin
  Result := True;
end;

procedure TArcIWStringGrid.SetContent(const Value: TArcIWStringGridContentBase);
begin
  if Value <> nil then
    Value.DoAssignGrid(Self)
  else
    if FContent <> nil then
      FContent.DoAssignGrid(nil);
  FContent := Value;
  FColumns.Content := Value;
  if FContent <> nil then
  begin
    FContent.FreeNotification(Self);
    if not (csDesigning in ComponentState) then
    begin
      if (FWebApplication = nil) and (Owner is TIWForm) then
        FWebApplication := TIWForm(Owner).WebApplication;
      ContentAssignSession(FWebApplication);
    end;
  end;
end;

procedure TArcIWStringGrid.SetRowCount(const Value: Integer);
var
  i: integer;
begin
  inherited SetRowCount(Value);
  for i := 0 to FColumns.Count - 1 do
  begin
    FColumns[i].Selected.Size := Value;
    while FColumns[i].Values.Count > FRowCount do
    begin
      FColumns[i].Selected[FColumns[i].Values.Count - 1] := False;
      FColumns[i].Values.Delete(FColumns[i].Values.Count - 1);
    end;
    while FColumns[i].Values.Count < FRowCount do
    begin
      FColumns[i].Values.Add('');
      FColumns[i].Selected[FColumns[i].Values.Count - 1] := False;
    end;
  end;
  if Assigned(Content) then
    Content.DoResizeGrid(Self, FColumns.Count, Value);
end;

procedure TArcIWStringGrid.LoadFromDataset(ADataset: TDataset);
var
  sa: TStringDynArray;
begin
  SetLength(sa, 0);
  LoadFromDataset(ADataSet, sa);
end;

procedure TArcIWStringGrid.LoadFromDataset(ADataset: TDataset; const AFieldNames: array of string;
  AAutoFields: boolean = true; AStartRow: integer = 0; ACount: integer = -1);
var
  i: integer;
  sa: TStringDynArray;
begin
  setLength(sa, High(AFieldNames) - Low(AFieldNames) + 1);
  for i := Low(AFieldNames) to High(AFieldNames) do
    sa[i] := AFieldNames[i];

  LoadFromDataset(ADataset, sa, AAutoFields, AStartRow, ACount);
end;

procedure TArcIWStringGrid.LoadFromDataset(ADataset: TDataset; const AFieldNames: TStringDynArray;
  AAutoFields: boolean = true; AStartRow: integer = 0; ACount: integer = -1);
var
  x, iCnt: integer;
  s : string;
  item: TCollectionItem;
begin
//  if AAutoFields then


  if not ADataSet.Active then
    ADataSet.Open
  else
    ADataSet.First;

  if AStartRow > 0 then
    ADataSet.MoveBy(AStartRow);


  if AAutoFields then
  begin
    while FColumns.Count < ADataset.Fields.Count do
    begin
      item := FColumns.Add;
      item.Assign(FColumns.Items[0]);
    end;
    while FColumns.Count > ADataset.Fields.Count do
      FColumns.Items[FColumns.Count - 1].Free;
  end;

  RowCount := 0;
  iCnt := 0;
  while not ADataset.Eof do
  begin
    RowCount := iCnt + 1;
    if Length(AFieldNames)=0 then
    begin
      for x := 0 to ADataset.Fields.Count - 1 do
      begin
        if x >= FColumns.Count then
          break;

        while TArcGridStringColumn(FColumns.Items[x]).Values.Count < RowCount do
          TArcGridStringColumn(FColumns.Items[x]).Values.Add('');

        if (not (ADataset.Fields[x] is TStringField)) and
          (not (ADataset.Fields[x] is TMemoField)) then
          TArcGridStringColumn(FColumns.Items[x]).Values[iCnt] := ADataset.Fields[x].DisplayText
        else
          TArcGridStringColumn(FColumns.Items[x]).Values[iCnt] := ADataset.Fields[x].AsString;
      end;
    end else
    begin
      for x := Low(AFieldNames) to High(AFieldNames) do
      begin
        s := AFieldNames[x];
        if x >= FColumns.Count then
          break;

        if s <> '' then
        begin
          while TArcGridStringColumn(FColumns.Items[x]).Values.Count < RowCount do
            TArcGridStringColumn(FColumns.Items[x]).Values.Add('');

          if (not (ADataset.FieldByName(s) is TStringField)) and
            (not (ADataset.FieldByName(s) is TMemoField)) then
            TArcGridStringColumn(FColumns.Items[x]).Values[iCnt] := ADataset.FieldByName(s).DisplayText
          else
            TArcGridStringColumn(FColumns.Items[x]).Values[iCnt] := ADataset.FieldByName(s).AsString;
        end;
      end;
    end;

    inc(iCnt);
    if (ACount >= 0) and (iCnt > ACount) then
      break;
    ADataSet.Next;
  end;
end;


procedure TArcIWStringGrid.LoadFromStringList(Strings: TStrings; {$IFNDEF VER130}ADelimiter: char = ',';
  AQuote: char = '"'; {$ENDIF}AAutoFields: boolean = true; AReadHeaders: boolean = true;
  AValuesAreLinks: boolean = false; SkipRows: integer = 0);
var
  slCols: TStringList;
  iRowCount, x, y: integer;
  item: TCollectionItem;
begin
  slCols := TStringList.Create;
  try
    {$IFNDEF OLDSTRINGLIST}
    slCols.Delimiter := ADelimiter;
    slCols.QuoteChar := AQuote;
    {$ENDIF}

    iRowCount := Strings.Count;
    if AReadHeaders then
      dec(iRowCount);

    dec(iRowCount, SkipRows);
    RowCount := iRowCount;
    for y := SkipRows to Strings.Count - 1 do
    begin
      {$IFNDEF OLDSTRINGLIST}
      slCols.DelimitedText := Strings[y];
      {$ELSE}
      slCols.CommaText := Strings[y];
      {$ENDIF}
      if AAutoFields then
      begin
        while FColumns.Count < slCols.Count do
        begin
          item := FColumns.Add;
          item.Assign(FColumns.Items[0]);
        end;
        while FColumns.Count > slCols.Count do
          FColumns.Items[FColumns.Count - 1].Free;
      end;

      for x := 0 to slCols.Count - 1 do
      begin
        if x >= FColumns.Count then
          break;
        if (y = SkipRows) and AReadHeaders then
        begin
          TArcGridStringColumn(FColumns.Items[x]).Caption := slCols[x]
        end else
        begin
          while TArcGridStringColumn(FColumns.Items[x]).Values.Count < iRowCount do
            TArcGridStringColumn(FColumns.Items[x]).Values.Add('');

          if AReadHeaders then
            TArcGridStringColumn(FColumns.Items[x]).Values[y - SkipRows - 1] := slCols[x]
          else
            TArcGridStringColumn(FColumns.Items[x]).Values[y - SkipRows] := slCols[x];

          if AValuesAreLinks and (Pos('=',slCols[x]) > 0) then
            TArcGridStringColumn(FColumns.Items[x]).FValuesHaveLinks := True;
        end;
      end;
    end;
  finally
    slCols.Free;
  end;
end;

procedure TArcIWStringGrid.LoadFromDelimitedFile(AFilename: string; ADelimiter: char = ',';
  AQuote: char = '"'; AAutoFields: boolean = true; AReadHeaders: boolean = true;
  AValuesAreLinks: boolean = false; SkipRows: integer = 0);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFilename);
    LoadFromStringList(sl, {$IFNDEF VER130}ADelimiter, AQuote, {$ENDIF}AAutoFields, AReadHeaders, AValuesAreLinks, SkipRows);
  finally
    sl.Free;
  end;
end;

procedure TArcIWStringGrid.Invalidate;
begin
  inherited Invalidate;
end;

procedure TArcIWStringGrid.SelectAll;
var
  i: integer;
begin
  for i := 0 to FColumns.Count - 1 do
    SelectedCol[i] := True;
end;

procedure TArcIWStringGrid.DeselectAll;
var
  i: integer;
begin
  for i := 0 to FColumns.Count - 1 do
    SelectedCol[i] := False;
end;

{$IFDEF INTRAWEB72}

function TArcIWStringGrid.CheckComponentForRender(AComponent: TComponent): Boolean;
begin
  if SupportsInterface(Parent, IIWHTML40Form) then begin
    {$IFDEF INTRAWEB110}
    result := False;
    {$ELSE}
    result := HTML40FormInterface(Parent).PageContext.UpdateMode = umPartial;
    {$ENDIF}
  end else begin
    if SupportsInterface(Parent, IIWHTML40Container) then begin
      result := HTML40ContainerInterface(Parent).CheckComponentForRender(AComponent);
    end else begin
      result := false;
    end;
  end;
end;

function TArcIWStringGrid.ContainerName: string;
begin
  Result := FContainerImplementation.ContainerName;
end;

function TArcIWStringGrid.ContainerPrefix: string;
begin
  if SupportsInterface(Parent, IIWBaseContainer) then begin
    result := BaseContainerInterface(Parent).ContainerPrefix;
  end else begin
    result := UpperCase(Name);
  end;
end;

procedure TArcIWStringGrid.DoRender;
var
  i: Integer;
begin
  ContentDoRender;
  if Assigned(OnRender) then
  begin
    OnRender(Self);
  end;
  for i := 0 to ComponentCount - 1 do begin
    if SupportsInterface(Components[i], IIWBaseContainer) then begin
      BaseContainerInterface(Components[i]).DoRender;
    end;
  end;
end;

function TArcIWStringGrid.FindComponentByName(AControl: string): TComponent;
var
  x: integer;
  y: integer;
  ctrl : TControl;
begin
  result := FContainerImplementation.FindComponentByName(AControl);
  if result = nil then
  begin
    for x := 0 to FColumns.Count - 1 do
      for y := 0 to RowCount - 1 do
      begin
        ctrl := nil;
        if FColumns[x].AutoRetrieveObject then
          ctrl := TControl(Objects[x, y]);

        if Assigned(Content) then
          Content.DoRetrieveObject(Self, x, y, ctrl);
        if Assigned(FColumns[x].FOnRetrieveControl) then
          FColumns[x].FOnRetrieveControl(Self, FColumns[x], y, ctrl);

        if (ctrl <> nil) and (ctrl is TControl) then
        begin
          if ((ctrl is TComponent) and (AnsiSameText(TComponent(ctrl).Name, AControl))) or
            ((ctrl is TIWCustomControl) and (TIWCustomControlCracker(ctrl).IsForThisControl(AControl))) then
          begin
            Result := ctrl;
            exit;
          end;
        end;
      end;
  end;
end;

procedure TArcIWStringGrid.IWAddComponent(AComponent: TComponent);
begin
  FContainerImplementation.AddComponent(AComponent);
end;

function TArcIWStringGrid.IWFindComponent(AComponent: TComponent): Integer;
begin
  result := FContainerImplementation.FindComponent(AComponent);
end;

procedure TArcIWStringGrid.IWRemoveComponent(AComponent: TComponent);
begin
  if not (csDestroying in ComponentState) then
    FContainerImplementation.RemoveComponent(AComponent);
end;

function TArcIWStringGrid.InitContainerContext(AWebApplication: TIWApplication): TIWContainerContext;
begin
  if not (csDesigning in ComponentState) then
  begin
    FWebApplication := AWebApplication;
    ContentAssignSession(AWebApplication);
  end;
  result := ContainerContext;
  ContainerContext.LayoutManager := TIWContainerLayout.Create(Self);
  TIWContainerLayout(ContainerContext.LayoutManager).SetContainer(Self);
end;

function TArcIWStringGrid.InterfaceInstance: TComponent;
begin
  result := Self;
end;

procedure TArcIWStringGrid.RenderComponents(AContainerContext: TIWContainerContext; APageContext: TIWBasePageContext);
Var
  i: integer;
  LContainerContext: IWBaseRenderContext.TIWContainerContext;
  LIWContainer: IIWBaseContainer;
begin
  // Process controls to add Partial update SPAN tags.
  // Componenets are rendered during RenderHTML inside the coresponding cells
  with TIWContainerLayout(ContainerContext.LayoutManager) do begin
    ProcessControls(ContainerContext, TIWBaseHTMLPageContext(APageContext));

    // Becouse HTML Tags are already embeded into table tag we should clear the HTML pointer inside container context.
    with ContainerContext do begin
      for i := 0 to ComponentsCount - 1 do begin
        if SupportsInterface(ComponentsList[i], IIWBaseContainer) then begin
          LIWContainer := BaseContainerInterface(ComponentsList[i]);
          LContainerContext := LIWContainer.InitContainerContext(APageContext.WebApplication);
          if Assigned(LContainerContext) then begin
            LContainerContext.UpdateTree := {CheckUpdateTree(LComponent) or }AContainerContext.UpdateTree;
            LIWContainer.RenderComponents(LContainerContext, APageContext);
          end;
        end;

        TIWBaseHTMLComponentContext(ComponentContext[BaseHTMLComponentInterface(ComponentsList[i]).HTMLName]).HTMLTag := nil;
      end;
    end;

    // SetContainer(nil);
    Free;
  end;
end;

function TArcIWStringGrid.get_Component(AIndex: Integer): TComponent;
begin
  result := FContainerImplementation.GetComponent(AIndex);
end;

function TArcIWStringGrid.get_ContainerContext: TIWContainerContext;
begin
  result := FContainerImplementation.ContainerContext;
end;

function TArcIWStringGrid.get_IWComponentsCount: Integer;
begin
  result := FContainerImplementation.get_IWComponentsCount;
end;

procedure TArcIWStringGrid.set_ContainerContext(const AContainerContext: TIWContainerContext);
begin
  if Assigned(ContainerContext) then begin
    ContainerContext.Free;
  end;
  FContainerImplementation.set_ContainerContext(AContainerContext);
end;

function TArcIWStringGrid.GenerateControlPositions: string;
begin
  result := '';
end;

procedure TArcIWStringGrid.SetActiveControl(AControl: IIWHTML40Control);
begin
  //
end;

function TArcIWStringGrid.get_Height: Integer;
begin
  if not FAutoHeight then
  begin
    Result := Height;
  end else
  begin
    if FDetailVisible then
      Result := FStyleHeader.MinHeight(FRowHeight) * (FRowCount + 1)
    else
      Result := FStyleHeader.MinHeight(FRowHeight);
  end;
end;

function TArcIWStringGrid.get_LayoutMgr: TIWContainerLayout;
begin
  result := nil;
end;

function TArcIWStringGrid.get_ShowHint: Boolean;
begin
  Result := ShowHint;
end;

function TArcIWStringGrid.get_Width: Integer;
begin
  Result := Width;
end;

procedure TArcIWStringGrid.set_LayoutMgr(Value: TIWContainerLayout);
begin
  //
end;

procedure TArcIWStringGrid.set_ShowHint(Value: Boolean);
begin
  ShowHint := Value;
end;

{$ENDIF}

function TArcIWStringGrid.SupportsSubmit: Boolean;
begin
  Result := True;
end;

function TArcIWStringGrid.GetSelected(x, y: integer): boolean;
begin
  EnsureXYValues(x, y);
  Result := FColumns[x].Selected[y];
end;

procedure TArcIWStringGrid.SetSelected(x, y: integer; Value: boolean);
begin
  EnsureXYValues(x, y);
  ContentSelect(x, y, Value);
  FColumns[x].Selected[y] := Value;
end;

function TArcIWStringGrid.GetSelectedRow(y: integer): boolean;
var
  x: integer;
begin
  if (y < 0) or (y >= FRowCount) then
    raise EListError.Create('y is out of range.');
  Result := True;
  for x := 0 to FColumns.Count - 1 do
  begin
    EnsureXYValues(x, y);
    if not FColumns[x].Selected[y] then
    begin
      Result := False;
      break;
    end;
  end;
end;

procedure TArcIWStringGrid.SetSelectedRow(y: integer; Value: boolean);
var
  x: integer;
begin
  //check y >= FRowCount when FRowCount>0
  //comment by peter 2005/04/27
  if (y < 0) or (y >= FRowCount) then
    //((FRowCount > 0) and (y >= FRowCount)) then
    raise EListError.Create('y is out of range.');
  //

  for x := 0 to FColumns.Count - 1 do
  begin
    EnsureXYValues(x, y);
    FColumns[x].Selected[y] := Value;
  end;
  ContentSelectRow(y, Value);
end;

function TArcIWStringGrid.GetSelectedCol(x: integer): boolean;
var
  y: integer;
begin
  if (x < 0) or (x >= FColumns.Count) then
    raise EListError.Create('x is out of range.');
  EnsureXYValues(x);
  Result := True;
  for y := 0 to FRowCount - 1 do
  begin
    if not FColumns[x].Selected[y] then
    begin
      Result := False;
      break;
    end;
  end;
end;

procedure TArcIWStringGrid.SetColumns(const AValue : TArcGridColumns);
begin
  FColumns.Assign(AValue);
end;

procedure TArcIWStringGrid.SetSelectedCol(x: integer; Value: boolean);
var
  y: integer;
begin
  if (x < 0) or (x >= FColumns.Count) then
    raise EListError.Create('x is out of range.');
  EnsureXYValues(x);

  ContentSelectCol(x, Value);
  for y := 0 to FRowCount - 1 do
  begin
    FColumns[x].Selected[y] := Value;
  end;
end;

function TArcIWStringGrid.GetFirstSelectedRow: integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to RowCount - 1 do
  begin
    if SelectedRow[i] then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TArcIWStringGrid.CWT: string;
begin
  case FColumnWidthType of
    cwtPixel: Result := 'px';
    cwtPercent: Result := '%';
    {cwtPoint:   Result := 'pt';
    cwtPica:    Result := 'pc';
    cwtInch:    Result := 'in';
    cwtMM:      Result := 'mm';
    cwtCM:      Result := 'cm';}
  end;
end;

function TArcIWStringGrid.RenderAsyncComponent(
  AContext: TIWBaseComponentContext): TIWXMLTag;
begin
  // Todo?
end;

procedure TArcIWStringGrid.RenderAsyncComponents(AContext: TIWContainerContext;
  APageContext: TIWBasePageContext);
begin
  // Todo?
end;

function TArcIWStringGrid.RenderHTML{$IFNDEF INTRAWEB72}: TIWHTMLTag; {$ELSE}(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; {$ENDIF}
  function IsNotTemplate(str : string; alt : string='') : string;
  begin
    {$IFDEF INTRAWEB72}
      if AContext.ContainerContext.LayoutManager is TIWLayoutMgrForm then
        Result := str
      else
        Result := alt;
    {$ELSE}
      Result := str;
    {$ENDIF}
  end;
  function ValueFromIndex(sl : TStrings; y : integer) : string;
  var
    iPos : integer;
  begin
    {$IFDEF VER150}
      result := sl.ValueFromIndex[y];
    {$ELSE}
      iPos := Pos('=',sl[y]);
      if iPos > 0 then
        result := Copy(sl[y],iPos+1,High(Integer))
      else
        result := '';
    {$ENDIF}
  end;
  function NeedsOverride: boolean;
  begin
    result := Assigned(FOnOverrideCellStyle) or (Assigned(FContent) and FContent.DoNeedStyleOverride(Self));
  end;
  function ShouldRenderLink(idx, Row : integer): boolean;
  begin
    result := TArcGridStringColumn(FColumns.Items[idx]).ClickEventAsLinks and
      Assigned(TArcGridStringColumn(FColumns.Items[idx]).OnClick);

    if Result and Assigned(TArcGridStringColumn(FColumns.Items[idx]).FOnCellClickable) then
      TArcGridStringColumn(FColumns.Items[idx]).FOnCellClickable(Self, FColumns[idx], Row, Result);
  end;
{$IFDEF INTRAWEB72}
  procedure RenderControl(Tag: TIWHTMLTag; ctrl: TControl; FullWidth, FullHeight : boolean);
    function FixupStyle(str: string): string;
    var
      iPos: integer;
    begin
      Result := IsNotTemplate(ReplaceStr(str, ':absolute;', ':static;'),'relative');

      iPos := FastPosNoCase(Result, ':static',1);
      if iPos = 0 then
        Result := 'position:static;'+Result;

      iPos := FastPosNoCase(Result, 'left:', 1);
      if iPos > 0 then
        Result := Copy(Result, 1, iPos - 1) + Copy(Result, PosEx(';', Result, iPos + 5) + 1, High(Integer));

      iPos := FastPosNoCase(Result, 'left :', 1);
      if iPos > 0 then
        Result := Copy(Result, 1, iPos - 1) + Copy(Result, PosEx(';', Result, iPos + 5) + 1, High(Integer));

      iPos := FastPosNoCase(Result, 'top:', 1);
      if iPos > 0 then
        Result := Copy(Result, 1, iPos - 1) + Copy(Result, PosEx( ';',Result, iPos + 4) + 1, High(Integer));

      iPos := FastPosNoCase(Result, 'top :', 1);
      if iPos > 0 then
        Result := Copy(Result, 1, iPos - 1) + Copy(Result, PosEx( ';',Result, iPos + 4) + 1, High(Integer));
      if FullWidth then
      begin
        iPos := FastPosNoCase(Result, 'width :', 1);
        if iPos > 0 then
          Result := Copy(Result, 1, iPos - 1) + 'width :100%;' + Copy(Result, PosEx(';', Result, iPos + 7) + 1, High(Integer))
        else
          Result := Result + 'width: 100%;';
      end;
      if FullHeight then
      begin
        iPos := FastPosNoCase(Result, 'height :', 1);
        if iPos > 0 then
          Result := Copy(Result, 1, iPos - 1) + 'height :100%;' + Copy(Result, PosEx(';', Result, iPos + 7) + 1, High(Integer))
        else
          Result := Result + 'height: 100%;';
      end;
    end;
  var
    ctrli: IIWBaseControl;
    ctrlh: IIWHTML40Component;
    ctrlContext: TIWComponent40Context;
    cntrContext: TIWContainerContext;
    Result: TIWHTMLTag;
    s: string;
  begin
    if (ctrl <> nil) then
    begin

      ctrli := BaseControlInterface(ctrl);
      ctrlh := HTML40ComponentInterface(ctrl);

      if ctrli = nil then
        raise Exception.Create('The assigned cell control is not an Intraweb control.');
      if ctrlh = nil then
        raise Exception.Create('The assigned cell control is not an Intraweb 4.0 control.');
    end;

    if ctrl.Parent <> TWinControl(Self) then ctrli.ParentChanging(nil,Self);
		try
		
		    cntrContext := HTML40ContainerInterface(Self).ContainerContext;
		    //cntrContext := AContext.ContainerContext;
		
		    //ctrl.Parent := TWinControl(Self);
		    //ctrli.ParentChanging(ctrl.Parent, Self);
		
		    ctrl.Visible := true;
		    ctrlContext := TIWComponent40Context.Create(ctrl, cntrContext, AContext.PageContext);
		
		    Result := HTML40ControlInterface(ctrl).RenderHTML(ctrlContext);

        {$IFDEF INTRAWEB90}
          if HTML40ControlInterface(ctrl).Css <> '' then
          begin
            Result.AddStringParam('CLASS', HTML40ControlInterface(ctrl).Css);
          end else
            Result.AddStringParam('CLASS', ctrlh.RenderCSSClass(AContext));

        {$ELSE}
          if BaseHTMLControlInterface(ctrl).get_WebFont.CSSStyle <> '' then
          begin
            Result.AddStringParam('CLASS', BaseHTMLControlInterface(ctrl).get_WebFont.CSSStyle);
          end else
            Result.AddStringParam('CLASS', ctrlh.RenderCSSClass(AContext));
        {$ENDIF}

		    Result.AddParmsList(BaseComponentInterface(ctrl).get_ExtraTagParams);
		    ctrlContext.HTMLTag := Result;
		
		    if Assigned(Result) then
		    begin
		      HTML40ControlInterface(ctrl).DoHTMLTag(Result);
		
		      s := ctrlh.RenderStyle(ctrlContext);
		      if Result.Params.Values['STYLE'] <> '' then
		      begin
		        s := s + ' ' + Result.Params.Values['STYLE'] + ';';
		      end;
		      s := FixupStyle(s);
		
		      if Result.Params.Values['ID'] = '' then
		      begin
		        Result.AddStringParam('ID', BaseHTMLControlInterface(ctrl).HTMLName);
		      end;
		      if Result.Params.Values['NAME'] = '' then
		      begin
		        Result.AddStringParam('NAME', BaseHTMLControlInterface(ctrl).HTMLName);
		      end;
		
		      Result.AddStringParam('STYLE', s);
		
		      if SupportsInterface(ctrl, IIWHTML40Component) then begin
		        HTML40ComponentInterface(ctrl).RenderScripts(ctrlContext);
		      end;
		    end;
		    cntrContext.AddComponent(ctrlContext);
		
		    DoRefreshControl := DoRefreshControl or HTML40ComponentInterface(ctrl).DoRefreshControl;
		    if DoRefreshControl then
		    begin
		      Invalidate;
		    end;
		    if Assigned(Result) then
		    begin
		      Tag.Contents.AddTagAsObject(Result);
		      ctrlContext.HTMLTag := nil;
		    end;
		    //ctrl.Parent := nil;
		    //ctrli.ParentChanging(ctrl.Parent, nil);
		
		    // This is a temporary workaround to fix problems when using editable grid controls in templates.
		    if cntrContext.LayoutManager is TIWTemplateProcessorHTML then
		      with AContext.PageContext.FormTag.Contents.AddTag('INPUT') do
		      begin
		        AddStringParam('TYPE', 'HIDDEN');
		        if ctrl is TIWCustomControl then
		          AddStringParam('NAME', TIWCustomControl(ctrl).HTMLName)
		        else
		          AddStringParam('NAME', Uppercase(ctrl.name));
		      end;
		finally
    	if ctrl.Parent <> TWinControl(Self) then ctrli.ParentChanging(self,ctrl.Parent);
		end;
  end;
  function NeedStyleDetail: boolean;
  begin
    {$IFDEF INTRAWEB110}
    result := False;
    {$ELSE}
    result := (AContext.Browser = brIE) and AContext.WebApplication.IsPartialUpdate;
    {$ENDIF}
  end;
{$ENDIF}
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
var
  tagHead, tagBody, tagTmp, tagLeft, tagCenter, tagRight, tagRow, tagCol, tagStyle, tagScript, tagTable: TIWHTMLTag;
  i, iTableWidth, iWidthTotal: integer;
  s, sStyle, sRowHeight, sValue, sScript: string;
  sCSS, sCSSTbl, sCSSRowCB, sCSSColCB, sCSSRowH, sCSSColH, sCSSRow, sCSSCol, sCSSRowAlt, sCSSColAlt, sCSSSel, sCSSBtnB, sCSSBtnC: string;
  slValues: TStringList;
  ctrl: TControl;
  bUpdated: boolean;
  style, styleFrom: TArcGridStyle;
  procedure DoRenderButtonBar;
  begin
    if FShowButtonBar then
    begin
      tagHead.AddStringParam('id', HTMLName + _btnbar);
      tagHead.AddStringParam('class', HTMLName + _btnbar);

      // Create Button Bar
      tagTmp := tagHead.Contents.AddTag('table');
      tagTmp.AddStringParam('width', '100%');
  //    tagTmp.AddStringParam('style','padding:0;margin:0;');
      tagRow := tagTmp.Contents.AddTag('tr');
      tagRow.AddStringParam('style', 'padding:0;margin:0;');
  //    tagRow.AddStringParam('class',HTMLName+_btnbar);

      tagCol := tagRow.Contents.AddTag('td');
      tagCol.AddStringParam('width', '100%');

      tagTmp := tagCol.Contents.AddTag('table');
      tagTmp.AddStringParam('width', '100%');
  //    tagTmp.AddStringParam('class',HTMLName+_btnbar);
      tagTmp := tagTmp.Contents.AddTag('tr');
      tagTmp.AddStringParam('class', HTMLName + _btnbarbtn);

      tagLeft := tagTmp.Contents.AddTag('td');
      tagLeft.AddStringParam('width', '100%');
      tagLeft.AddStringParam('class', HTMLName + _btnbarbtn);
      tagLeft.AddStringParam('align', 'left');
      tagLeft.AddStringParam('valign', 'top');

      tagCenter := tagTmp.Contents.AddTag('td');
      tagCenter.AddStringParam('class', HTMLName + _btnbarbtn);
      case FStyleButtonBar.TextAlign of
        taLeftJustify: tagCenter.AddStringParam('align', 'left');
        taRightJustify: tagCenter.AddStringParam('align', 'right');
        taCenter: tagCenter.AddStringParam('align', 'center');
      end;
      case FStyleButtonBar.TextVertAlign of
        vaTop: tagCenter.AddStringParam('valign', 'top');
        vaMiddle: tagCenter.AddStringParam('valign', 'middle');
        vaBottom: tagCenter.AddStringParam('valign', 'bottom');
      end;

      tagRight := tagTmp.Contents.AddTag('td');
      tagRight.AddStringParam('class', HTMLName + _btnbarbtn);
      tagRight.AddStringParam('align', 'right');
      tagRight.AddStringParam('valign', 'top');

      FCaptionButtons.RenderButtons(HTMLName, AContext, tagLeft, tagCenter, tagRight);

      if FCaption <> '' then
      begin
        if Assigned(FContent) then
        begin
          tagCenter.Contents.AddText(FContent.ProcessCaption(FCaption))
        end else
        begin
          tagCenter.Contents.AddText(FCaption);
        end
      end else
        tagCenter.Contents.AddText('&nbsp;');
    end;
  end;
  procedure DoRenderControlBar;
  var
    i: integer;
  begin
    if FShowControlBar then
    begin
      // Create Header Row
      tagRow := tagTable.Contents.AddTag('tr');
      tagRow.AddStringParam('class', HTMLName + _rowCB);

      for i := 0 to FColumns.Count - 1 do
      begin
        if FColumns[i].Visible then
        begin
          tagCol := tagRow.Contents.AddTag('td');

          //if not FColumns[i].UseMinimumWidth then
          //begin
            if FColumnWidthType = cwtPercent then
            begin
              if FColumns[i].Width >= 0 then
                tagCol.AddStringParam('width', IntToStr(FColumns[i].Width) + CWT);
              //tagCol.AddStringParam('width', '*');
            end;
          //end else
          //  tagCol.AddStringParam('width', '0px');
          sStyle := FColumns[i].PaddingString(False, FStyleControlBar.Padding);

          tagCol.AddStringParam('class', HTMLName + _colCB);

          if FColumns[i].OverrideStyle then
          begin
            sStyle := sStyle + AlignmentToStr(FColumns[i].FStyleControlBar.TextAlign);
            sStyle := sStyle + FColumns[i].StyleControlBar.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF});
          end else
            sStyle := sStyle + AlignmentToStr(FStyleControlBar.TextAlign);

          //if not FColumns[i].UseMinimumWidth then
          //begin
            if not FAutoColumnWidth then
              if FColumnWidthType <> cwtPercent then
                sStyle := sStyle + 'width: ' + IntToStr(FColumns[i].Width) + CWT + ';';
          //end else
          //  sStyle := sStyle + 'width: 0px;';

          tagCol.AddStringParam('style', sStyle);

          ctrl := nil;
          ContentRetrieveCBObject(i, ctrl);
          if Assigned(FColumns[i].OnRetrieveCBControl) then
            FColumns[i].OnRetrieveCBControl(Self, FColumns[i], ctrl);

          if ctrl <> nil then
            RenderControl(tagCol, ctrl, True, True)
          else
            tagCol.Contents.AddText('&nbsp;');
        end;
      end;
    end;
  end;
  procedure DoRenderHeader;
  var
    i: integer;
    sURL : string;
  begin
    if FShowHeaderRow then
    begin
      // Create Header Row
      tagRow := tagTable.Contents.AddTag('tr');
      tagRow.AddStringParam('class', HTMLName + _rowhead);

{$IFDEF INTRAWEB72}
      //if NeedStyleDetail then
        //tagRow.AddStringParam('style',sCSSRowH);
{$ENDIF}

      for i := 0 to FColumns.Count - 1 do
      begin
        if FColumns[i].Visible then
        begin
          {if StaticHeader then
            tagCol := tagRow.Contents.AddTag('th')
          else}
          tagCol := tagRow.Contents.AddTag('td');

          //comment by peter 2005/05/17
          if FColumns[i].ShowHint then
            tagCol.AddStringParam('title', FColumns[i].Hint);

          if not FColumns[i].WrapText then
            tagCol.AddBoolParam('nowrap', True);

          //Add RowHeaderHeight property
          //comment by peter 2005/04/26
          if RowHeaderHeight > 0 then
            tagCol.AddStringParam('height', IntToStr(RowHeaderHeight) + 'px');
          //

          //if not FColumns[i].UseMinimumWidth then
          //begin
            if FColumnWidthType = cwtPercent then
            begin
              if FColumns[i].Width >= 0 then
                tagCol.AddStringParam('width', IntToStr(FColumns[i].Width) + CWT);
              //tagCol.AddStringParam('width', '*');
            end;
          //end else
          //  tagCol.AddStringParam('width', '0px');

          sStyle := FColumns[i].PaddingString(False, FStyleHeader.Padding);

          if Assigned(FColumns[i].FOnClickCaption) then
          begin
            tagCol.AddStringParam('onClick', 'IWTop().' + HTMLName + _DoCaptionClick + '(''' + IntToStr(i) + ''');');
{$IFDEF INTRAWEB51}
            if WebApplication.Browser = brIE then
              sStyle := sStyle + 'cursor: hand;'
            else
              sStyle := sStyle + 'cursor: pointer;';
{$ELSE}
            if BrowserIsIE(AContext.Browser) then
              sStyle := sStyle + 'cursor: hand;'
            else
              sStyle := sStyle + 'cursor: pointer;';
{$ENDIF}
          end;
          tagCol.AddStringParam('class', HTMLName + _colhead);

          if FColumns[i].OverrideStyle then
          begin
            sStyle := sStyle + AlignmentToStr(FColumns[i].FStyleHeader.TextAlign);
            sStyle := sStyle + FColumns[i].StyleHeader.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF});
          end else
            sStyle := sStyle + AlignmentToStr(FStyleHeader.TextAlign);

          if not FAutoColumnWidth then
            if FColumnWidthType <> cwtPercent then
              sStyle := sStyle + 'width: ' + IntToStr(FColumns[i].Width) + CWT + ';';

{$IFDEF INTRAWEB72}
          //if NeedStyleDetail then
            //sStyle := sCSSColH+sStyle;
{$ENDIF}

          tagCol.AddStringParam('style', sStyle);
{$IFNDEF INTRAWEB51}
          if (not FColumns[i].CaptionIcon.Empty) and (FColumns[i].CaptionIconAlignment = iaLeft) then
          begin
            sURL := FColumns[i].CaptionIcon.Location(AContext.WebApplication.InternalURLBase);
            if BrowserIsIE(AContext.Browser) and (Pos('.png',sURL)>0) then
            begin
              tagCol.Contents.AddText('<span width=16 height=16 style="position:static;width:16px;height:16px;float:none;filter:progid:DXImageTransform.Microsoft.AlphaImageLoader(src='''+
                sURL+''', sizingMethod=''scale'');"></span>');
            end else
              tagCol.Contents.AddText('<img src="' + sURL + '">');
          end;
{$ENDIF}
          tagCol.Contents.AddText(FColumns[i].Caption);
{$IFNDEF INTRAWEB51}
          if (not FColumns[i].CaptionIcon.Empty) and (FColumns[i].CaptionIconAlignment = iaRight) then
          begin
            sURL := FColumns[i].CaptionIcon.Location(AContext.WebApplication.InternalURLBase);
            if BrowserIsIE(AContext.Browser) and (Pos('.png',sURL)>0) then
            begin
              tagCol.Contents.AddText('<span width=16 height=16 style="position:static;width:16px;height:16px;float:none;filter:progid:DXImageTransform.Microsoft.AlphaImageLoader(src='''+
                sURL+''', sizingMethod=''scale'');"></span>');
            end else
              tagCol.Contents.AddText('<img src="' + sURL + '">');
          end;
{$ENDIF}
        end;
      end;
    end;
  end;
  procedure DoRenderDetail;
  var
    bShow : boolean;
    y, i: integer;
    //comment by peter 2005/05/18
    s: string;
    bOnClick: Boolean;
  begin
    if FDetailVisible then
    begin
      slValues := TStringList.Create;
      try
        while slValues.Count < FColumns.Count do
          slValues.Add('');

        // Create Rows
        for y := 0 to FRowCount - 1 do
        begin
          ContentPopulateRow(y, slValues);
          if Assigned(FOnPopulateRow) then
          begin
            for i := 0 to FColumns.Count - 1 do
              slValues[i] := '';
            FOnPopulateRow(Self, y, slValues);
          end;

          tagRow := tagTable.Contents.AddTag('tr');
          if FUseAltStyles then
          begin
            if (y mod 2) <> 0 then
            begin
              tagRow.AddStringParam('class', HTMLName + _rowalt);
{$IFDEF INTRAWEB72}
              if NeedStyleDetail then
                tagRow.AddStringParam('style', sCSSRowAlt);
{$ENDIF}
            end else
            begin
              tagRow.AddStringParam('class', HTMLName + _row);
{$IFDEF INTRAWEB72}
              if NeedStyleDetail then
                tagRow.AddStringParam('style', sCSSRow);
{$ENDIF}
            end;
          end else
          begin
            tagRow.AddStringParam('class', HTMLName + _row);
{$IFDEF INTRAWEB72}
            if NeedStyleDetail then
              tagRow.AddStringParam('style', sCSSRow);
{$ENDIF}
          end;
          {if (FRollover = rtRow) then
          begin
            tagRow.AddStringParam('onMouseOver','this.className=''rollover''');//DoRolloverOn(this);');
            tagRow.AddStringParam('onMouseOut','this.className='''+HTMLName+_row+''');//HTMLName+_DoRolloverOff+'(this);');
          end;}

          for i := 0 to FColumns.Count - 1 do
          begin
            if FColumns[i].Visible then
            begin
              tagCol := tagRow.Contents.AddTag('td');

              //comment by peter 2005/05/18
              if FColumns[i].ShowHint then
              begin
                s := FColumns[i].Hint;
                if Assigned(FOnCellHint) then
                  FOnCellHint(self, FColumns[i], y, s);
                tagCol.AddStringParam('title', s);
              end;

              if not FColumns[i].WrapText then
                tagCol.AddBoolParam('nowrap', True);

              //if not FColumns[i].UseMinimumWidth then
              //begin
                if FColumnWidthType = cwtPercent then
                  tagCol.AddStringParam('width', IntToStr(FColumns[i].Width) + CWT);
              //end else
              //    tagCol.AddStringParam('width', '0px');
              sStyle := FColumns[i].PaddingString(True, FStyleDetail.Padding);


              bOnClick := Assigned(FOnRowClick) or ( (not ShouldRenderLink(i, y)) and (Assigned(FColumns[i].FOnClick) or ContentNeedOnClickEvent));
              if (bOnClick) and Assigned(FColumns[i].OnCellClickable) then
                FColumns[i].OnCellClickable(Self, FColumns[i], y, bOnClick);

              if bOnClick then
              begin
                if (not FSuppressSelRowClick) or (FSuppressSelRowClick and (not SelectedRow[y])) then
                begin
                  tagCol.AddStringParam('onClick', 'IWTop().' + HTMLName + _DoColumnClick + '(''' + IntToStr(y) + ''',''' + IntToStr(i) + ''');');
                end;
{$IFDEF INTRAWEB51}
                if WebApplication.Browser = brIE then
                  sStyle := sStyle + 'cursor:hand;'
                else
                  sStyle := sStyle + 'cursor:pointer;';
{$ELSE}
                if BrowserIsIE(AContext.Browser) then
                  sStyle := sStyle + 'cursor:hand;'
                else
                  sStyle := sStyle + 'cursor:pointer;';
{$ENDIF}
              end;

              {if (FRollover = rtCell) or ((FRollover = rtCellClickable) and (Assigned(FColumns[i].FOnClick))) then
              begin
                tagCol.AddStringParam('onMouseOver',HTMLName+_DoRolloverOn+'(this);');
                tagCol.AddStringParam('onMouseOut',HTMLName+_DoRolloverOff+'(this);');
              end;}

              sStyle := sStyle + AlignmentToStr(FColumns[i].Alignment);

              style := nil;
              styleFrom := nil;
              bUpdated := false;

              if NeedsOverride then
              begin
                if Selected[i, y] then
                  styleFrom := FColumns[i].StyleSelected
                else
                  if FColumns[i].OverrideStyle then
                    if FUseAltStyles then
                      if (y mod 2) <> 0 then
                        styleFrom := FColumns[i].StyleDetailAlt
                      else
                        styleFrom := FColumns[i].StyleDetail
                    else
                      styleFrom := FColumns[i].StyleDetail
                  else
                    if FUseAltStyles then
                      if (y mod 2) <> 0 then
                        styleFrom := StyleDetailAlt
                      else
                        styleFrom := StyleDetail
                    else
                      styleFrom := StyleDetail;
                style := TArcGridStyle.Create((brsTop in styleFrom.BorderStyle.Sides),
                  (brsBottom in styleFrom.BorderStyle.Sides),
                  (brsLeft in styleFrom.BorderStyle.Sides),
                  (brsRight in styleFrom.BorderStyle.Sides));
              end;
              try
                if NeedsOverride then
                begin
                  style.Assign(styleFrom);
                  if Assigned(FContent) and FContent.DoNeedStyleOverride(Self) then
                  begin
                    ContentOverrideCellStyle(i, y, style);
                    bUpdated := True;
                  end;
                  if Assigned(FOnOverrideCellStyle) then
                    FOnOverrideCellStyle(Self, FColumns[i], y, style, bUpdated);
                end;

                if not bUpdated then
                begin
                  if FColumns[i].OverrideStyle then
                  begin
                    if Selected[i, y] then
                      sStyle := sStyle + FColumns[i].StyleSelected.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF})
                    else
                      if FUseAltStyles then
                        if (y mod 2) <> 0 then
                          sStyle := sStyle + FColumns[i].StyleDetailAlt.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF})
                        else
                          sStyle := sStyle + FColumns[i].StyleDetail.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF})
                      else
                        sStyle := sStyle + FColumns[i].StyleDetail.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF});
                  end;
                end else
                begin
                  sStyle := sStyle + style.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF});
                end;
              finally
                style.free; // free will first check to see if style is nil.  won't then AV.
              end;

              //sStyle := sStyle+'clip:rect(0px,0px,'+IntToStr(FColumns[i].Width)+CWT+','+IntToStr(FRowHeight)+CWT+');';
              tagCol.AddStringParam('style', sStyle);

              if Selected[i, y] then
              begin
                tagCol.AddStringParam('class', HTMLName + _selected);
                if FScrollToSelectedCell and (BrowserIsIE(AContext.Browser) or BrowserIsNetscape7(AContext.Browser)) then
                begin
                  tagCol.AddStringParam('id', HTMLName + _selected + '_' + IntToStr(i) + '_' + IntToStr(y));
                  {$IFDEF INTRAWEB110}
                  if False then
                  {$ELSE}
                  if AContext.PageContext.WebApplication.IsPartialUpdate then
                  {$ENDIF}
                    TIWPageContext40(AContext.PageContext).AddToUpdateInitProc('IWTop().document.getElementById("' + HTMLName + _selected + '_' + IntToStr(i) + '_' + IntToStr(y) + '").scrollIntoView(false)')
                  else
                    TIWPageContext40(AContext.PageContext).AddToInitProc('IWTop().document.getElementById("' + HTMLName + _selected + '_' + IntToStr(i) + '_' + IntToStr(y) + '").scrollIntoView(false)');
                end;
              end else
                if FUseAltStyles then
                  if (y mod 2) <> 0 then
                    tagCol.AddStringParam('class', HTMLName + _colalt)
                  else
                    tagCol.AddStringParam('class', HTMLName + _col)
                else
                  tagCol.AddStringParam('class', HTMLName + _col);


              if not Assigned(FOnPopulateRow) then
              begin
                sValue := '';
                if y < FColumns[i].Values.Count then
                begin
                  if FColumns[i].ValuesHaveLinks then
                    sValue := '<a ' + IfThen(FColumns[i].LinkTarget <> '', 'target="' + FColumns[i].LinkTarget + '"') + ' href="' + ValueFromIndex(FColumns[i].Values, y) + '">' + FColumns[i].Values.Names[y] + '</a>'
                  else
                    sValue := IfThen(ShouldRenderLink(i, y), '<a href="javascript:IWTop().' + HTMLName + _DoColumnClick + '(''' + IntToStr(y) + ''',''' + IntToStr(i) + ''');">') + FColumns[i].Values[y] + IfThen(ShouldRenderLink(i, y), '</a>');
                end;
              end else
                sValue := slValues[i];

              ContentCellData(i, y, sValue);
              if Assigned(FOnCellData) then
                FOnCellData(Self, i, y, sValue);
              if (sValue = '') or (sValue = '<a href="#"></a>') then
                sValue := '&nbsp;';

              tagCol.AddStringParam('style', sStyle);

              // Render Cell Contents
{$IFDEF INTRAWEB72}
              ctrl := nil;
              if FColumns[i].AutoRetrieveObject then
              begin
                if FColumns[i].Values.Objects[y] is TControl then
                  ctrl := TControl(FColumns[i].Values.Objects[y]);
              end;
              ContentRetrieveObject(i, y, ctrl);
              if Assigned(FColumns[i].OnRetrieveControl) then
                FColumns[i].OnRetrieveControl(Self, FColumns[i], y, ctrl);
{$ENDIF}

{$IFNDEF INTRAWEB51}
              // Render Icon if iaLeft
              if (not FColumns[i].Icon.Empty) and (FColumns[i].IconAlignment = iaLeft) then
              begin
                bShow := True;
                if Assigned(FOnOverrideCellShowIcon) then
                  FOnOverrideCellShowIcon(Self,FColumns[i],y,bShow);
                if bShow then
                  tagCol.Contents.AddText('<img src="' + FColumns[i].Icon.Location(AContext.WebApplication.InternalURLBase) + '">');
              end;
{$ENDIF}

              // Render Control if iaLeft
{$IFDEF INTRAWEB72}
              if (ctrl <> nil) and (FColumns[i].ControlAlignment in [caNoText, caLeft]) then
              begin
                bShow := True;
                if Assigned(FOnOverrideCellShowControl) then
                  FOnOverrideCellShowControl(Self,FColumns[i],y,bShow);
                if bShow then
                  RenderControl(tagCol, ctrl, FColumns[i].FControlSizing in [csFull, csFullWidth], FColumns[i].FControlSizing in [csFull, csFullHeight]);
              end;
{$ENDIF}

              if (ctrl = nil) or ((ctrl <> nil) and (FColumns[i].ControlAlignment <> caNoText)) then
              begin
                // Render Cell Contents
                if (sValue = '') and (not FAllowEmptyCells) then
                  sValue := '&nbsp;';
                tagCol.Contents.AddText(sValue);//'<div style="width:100%;overflow:hidden;">'+sValue+'</div>');
              end;

              // Render Control if iaRight
{$IFDEF INTRAWEB72}
              if (ctrl <> nil) and (FColumns[i].ControlAlignment = caRight) then
              begin
                bShow := True;
                if Assigned(FOnOverrideCellShowControl) then
                  FOnOverrideCellShowControl(Self,FColumns[i],y,bShow);
                if bShow then
                  RenderControl(tagCol, ctrl, FColumns[i].FControlSizing in [csFull, csFullWidth], FColumns[i].FControlSizing in [csFull, csFullHeight]);
              end;
{$ENDIF}

{$IFNDEF INTRAWEB51}
              // Render Icon if iaRight
              if (not FColumns[i].Icon.Empty) and (FColumns[i].IconAlignment = iaRight) then
              begin
                bShow := True;
                if Assigned(FOnOverrideCellShowIcon) then
                  FOnOverrideCellShowIcon(Self,FColumns[i],y,bShow);
                if bShow then
                  tagCol.Contents.AddText('<img src="' + FColumns[i].Icon.Location(AContext.WebApplication.InternalURLBase) + '">');
              end;
              //comment by peter 2005/05/16
              if (ctrl <> nil) and (FIsFocus) and (y = FFocusRow) then
              begin
                FIsFocus := false;
                TIWPageContext40(AContext.PageContext).AddToInitProc('IWTop().FindElem("' + BaseHTMLControlInterface(ctrl).HTMLName + '").focus();');
              end;
{$ENDIF}
            end;

          end;
          if SelectedRow[y] then
          begin
            if FScrollToSelectedRow and (BrowserIsIE(AContext.Browser) or BrowserIsNetscape7(AContext.Browser)) then
            begin
              tagRow.AddStringParam('id', HTMLName + _SelectedRow + '_' + IntToStr(y));
              {$IFDEF INTRAWEB110}
              if False then
              {$ELSE}
              if AContext.PageContext.WebApplication.IsPartialUpdate then
              {$ENDIF}
                TIWPageContext40(AContext.PageContext).AddToUpdateInitProc('IWTop().document.getElementById("' + HTMLName + _SelectedRow + '_' + IntToStr(y) + '").scrollIntoView(false)')
              else
                TIWPageContext40(AContext.PageContext).AddToInitProc('IWTop().document.getElementById("' + HTMLName + _SelectedRow + '_' + IntToStr(y) + '").scrollIntoView(false)');
            end;
          end;
        end;
      finally
        slValues.Free;
      end;
    end;
  end;
{var
  tag : TIWHTMLTag;}
var
  bShow : boolean;
  p : TControl;
begin
  ContentBeforeRenderHTML(AContext);
  ContainerContext := IWBaseRenderContext.TIWContainerContext.Create(AContext.WebApplication);

  Result := TIWHTMLTag.CreateTag('span');

  s := 'width:' + IntToStr(Width) + 'px;';
  if not FAutoHeight then
  begin
    s := s + 'height:' + IntToStr(Height) + 'px;';
  end else
  begin
    if FDetailVisible then
      s := s + 'height:' +
        IntToStr(
          IfThen(FShowButtonBar, FStyleButtonBar.MinHeight(FButtonBarHeight),0)+
          IfThen(FShowControlBar, FStyleControlBar.MinHeight(FRowHeight),0)+
          IfThen(FShowHeaderRow, FStyleHeader.MinHeight(FRowHeaderHeight),0)+
          (FStyleDetail.MinHeight(FRowHeight) * (FRowCount))
        ) + 'px;'
    else
      s := s + 'height:' +
        IntToStr(
          IfThen(FShowButtonBar, FStyleButtonBar.MinHeight(FButtonBarHeight),0)+
          IfThen(FShowControlBar, FStyleControlBar.MinHeight(FRowHeight),0)+
          IfThen(FShowHeaderRow, FStyleHeader.MinHeight(FRowHeaderHeight),0)
        ) + 'px;';
  end;

  //always set position:absolute
  //comment by peter 2005/04/27
  s := s + IsNotTemplate('position:absolute;','position:relative');
  //

  Result.AddStringParam('style', s);

  if FShowButtonBar then
  begin
    tagHead := Result.Contents.AddTag('span');
    tagHead.AddStringParam('style', IsNotTemplate('position:absolute;','position:relative')+'top:0px;left:0px;width:100%;height:' + IntToStr(FButtonBarHeight) + 'px;padding:0;margin:0;');
    if not FAutoHeight then
    begin
      tagBody := Result.Contents.AddTag('span');
      tagBody.AddStringParam('style', IsNotTemplate('position:absolute;','position:relative')+'top:' + IntToStr(FButtonBarHeight + 1) + 'px;width:100%;height:' + IfThen(AutoHeight, '100%',IntToStr(Height - FButtonBarHeight - 1) + 'px;') + RenderScrollbarStyle);
    end else
      tagBody := Result;
  end else
  begin
    tagHead := nil;
    if not FAutoHeight then
    begin
      tagBody := Result.Contents.AddTag('span');
      tagBody.AddStringParam('style', IsNotTemplate('position:absolute;','position:relative')+'top:0px;width:100%;height:100%;' + RenderScrollbarStyle);
    end else
      tagBody := Result;
  end;
  tagScript := AContext.PageContext.BodyTag.Contents.AddTag('script');
  //tagScript := Result.Contents.AddTag('script');

  sScript := DebugEOL + 'var ' + HTMLName + _ReturnStyle + ' = "' + HTMLName + _col + '";' + DebugEOL +
    'function ' + HTMLName + _SubmitOnEnterKey + '(e, Param1, Param2, Param3, Param4) {' + DebugEOL +
    '  var c;' + DebugEOL +
    '  c=e.keyCode;' + DebugEOL +
    '  if (c==13) { SubmitClickConfirm(Param1, Param2, Param3, Param4); return false;} else { return true; }' + DebugEOL +
    '}' + DebugEOL +
    'function ' + HTMLName + _DoCaptionClick + '(col) {' + DebugEOL +
    //'  alert("hx"+col);'+DebugEOL+
  '  SubmitClickConfirm("' + HTMLName + '","hx"+col,false,"");' + DebugEOL +
    '}' + DebugEOL +
    'function ' + HTMLName + _DoColumnClick + '(row,col) {' + DebugEOL +
    //'  alert("cy"+row+"x"+col);'+DebugEOL+
  '  SubmitClickConfirm("' + HTMLName + '","cy"+row+"x"+col,false,"");' + DebugEOL +
    '}' + DebugEOL;
  if FShowButtonBar then
    sScript := sScript +
      'function ' + HTMLName + _DoCaptionButtonClick + '(ButtonType,ConfirmString) {' + DebugEOL +
      '  if (ConfirmString != "") {' + DebugEOL +
      '    if (!confirm(ConfirmString)) { return false; }' + DebugEOL +
      '  }' + DebugEOL +
      '  SubmitClickConfirm("' + HTMLName + '","b"+ButtonType,false,"");' + DebugEOL +
      '}' + DebugEOL;

  (*if FRollover <> rtNone then
  begin
    sScript := sScript +
      'function '+HTMLName+_DoRolloverOn+'(obj) {'+DebugEOL+
      '  '+HTMLName+'_ReturnStyle=obj.className;'+DebugEOL+
      '  obj.className="'+HTMLName+_rollover+'";'+DebugEOL+
      '}'+DebugEOL+
      'function '+HTMLName+_DoRolloverOff+'(obj) {'+DebugEOL+
      '  obj.className='+HTMLName+_ReturnStyle+';'+DebugEOL+
      '}'+DebugEOL;
  end;*)
  //comment peter 2005/05/06
  (*sScript := sScript +
    'function Jmwu()' +
    '{' +
    'alert("Jmwu");' +
    '}' + DebugEOL;
  //*)
  sScript := sScript+ContentNeedScript;
  tagScript.Contents.AddText(sScript);

//  tagStyle := Result.Contents.AddTag('style');
  tagStyle := AContext.PageContext.BodyTag.Contents.AddTag('style');

  iTableWidth := width;
  if BrowserIsIE({$IFDEF INTRAWEB72}AContext.Browser{$ELSE}WebApplication.Browser{$ENDIF}) and (not (FStyleTable.BorderStyle.Style in [brdNone, brdHidden])) then
    iTableWidth := width - (FStyleTable.BorderStyle.Width * 2);

  iWidthTotal := 0;
  for i := 0 to FColumns.Count - 1 do
    if FColumns[i].Visible then
      inc(iWidthTotal, FColumns[i].Width);
  if iWidthTotal = 0 then
    iWidthTotal := iTableWidth;
  if not FAutoColumnWidth then
    iTableWidth := iWidthTotal;

  sRowHeight := IfThen((FRowHeight > 0) and not FAutoRowHeight, 'height: ' + IntToStr(FRowHeight) + 'px;');
  sCSS := FStyleTable.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF}) + 'overflow:hidden;'+IsNotTemplate('','width:'+IntToStr(Width)+';height:'+IntTostr(Height));
  //if RowHeaderHeight is > 0 and RowCount = 0, then do not set the table style to height:100%
  //comment by peter 2005/05/11
  if (FRowHeaderHeight > 0) and (FRowCount = 0) then
    sCSSTbl := IsNotTemplate('position:absolute;','position:relative')+'left:0px;'+IfThen(FAutoHeight and ShowButtonBar and (FButtonBarHeight > 0),'top:'+IntToStr(FButtonBarHeight)+'px;','top:0px;') + IfThen(FColumnWidthType = cwtPixel, 'width:' + IntToStr(iTableWidth) + 'px;', '')
  else
    sCSSTbl := IsNotTemplate('position:absolute;','position:relative')+'left:0px;'+IfThen(FAutoHeight and ShowButtonBar and (FButtonBarHeight > 0),'top:'+IntToStr(FButtonBarHeight)+'px;','top:0px;') + IfThen(FColumnWidthType = cwtPixel, 'width:' + IntToStr(iTableWidth) + 'px;', 'width:100%;') + IfThen(FAutoRowHeight, 'height:100%;');

  //add head row height
  //comment by peter 2005/05/11
  if FRowHeaderHeight > 0 then
    sCSSRowH := 'background-color:' + ColorToRGBString(FStyleHeader.BackgroundColor) + ';height:' + IntToStr(FRowHeaderHeight) + 'px;'
  else
    sCSSRowH := 'background-color:' + ColorToRGBString(FStyleHeader.BackgroundColor) + ';' + sRowHeight;

  if FRowHeaderHeight > 0 then
    sCSSColH := FStyleHeader.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF}) + ';height:' + IntToStr(FRowHeaderHeight) + 'px;'
  else
    sCSSColH := FStyleHeader.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF}) + sRowHeight;


  sCSSRowCB := 'background-color:' + ColorToRGBString(FStyleControlBar.BackgroundColor) + ';' + sRowHeight;
  sCSSColCB := FStyleControlBar.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF}) + sRowHeight;
  sCSSRow := 'background-color:' + ColorToRGBString(FStyleDetail.BackgroundColor) + ';' + sRowHeight;
  sCSSCol := FStyleDetail.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF}) + sRowHeight;;
  sCSSRowAlt := 'background-color:' + ColorToRGBString(FStyleDetailAlt.BackgroundColor) + ';' + sRowHeight;
  sCSSColAlt := FStyleDetailAlt.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF}) + sRowHeight;;
  sCSSSel := FStyleSelected.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF}) + sRowHeight;
  sCSSBtnB := 'height:' + IntToStr(FButtonBarHeight) + 'px;width:100%;background-color:' + ColorToRGBString(FStyleButtonBar.BackgroundColor) + ';' + sRowHeight + FStyleButtonBar.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF});

  if FShowButtonBar then
  begin
    style := TArcGridStyle.Create(False, False, False, False);
    try
      style.Assign(FStyleButtonBar);
      style.BorderStyle.Style := brdNone;
      style.BorderStyle.Width := 0;
      sCSSBtnC := style.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF}) + sRowHeight;
    finally
      FreeAndNil(style);
    end;
  end else
    sCSSBtnC := '';

  tagStyle.Contents.AddText(DebugEOL +
    '  .' + HTMLName + 'CSS {' + sCSS + '} ' + DebugEOL +
    '  .' + HTMLName + _tbl + ' {' + sCSSTbl + '}' + DebugEOL +
    '  .' + HTMLName + _btnbar + ' {' + sCSSBtnB + '} ' + DebugEOL +
    '  .' + HTMLName + _btnbarbtn + ' {' + sCSSBtnC + '} ' + DebugEOL +
    '  .' + HTMLName + _rowhead + ' { ' + sCSSRowH + '} ' + DebugEOL +
    '  .' + HTMLName + _colhead + ' {' + sCSSColH + '} ' + DebugEOL +
    '  .' + HTMLName + _row + ' { ' + sCSSRow + '} ' + DebugEOL +
    '  .' + HTMLName + _col + ' {' + sCSSCol + '} ' + DebugEOL +
    '  .' + HTMLName + _rowCB + ' { ' + sCSSRowCB + '} ' + DebugEOL +
    '  .' + HTMLName + _colCB + ' {' + sCSSColCB + '} ' + DebugEOL +
    '  .' + HTMLName + _selected + ' {' + sCSSSel + '} ' + DebugEOL +
    '  .' + HTMLName + _rowalt + ' { ' + sCSSRowAlt + '} ' + DebugEOL +
    '  .' + HTMLName + _colalt + ' {' + sCSSColAlt + '} ' + DebugEOL
    //'  .'+HTMLName+'_row.rollover {'+FStyleRollover.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF})+'} '+DebugEOL
    );

  tagTable := tagBody.Contents.AddTag('table');
  tagTable.AddStringParam('id', HTMLName + _tbl);
  tagTable.AddStringParam('class', HTMLName + _tbl);
  tagTable.AddIntegerParam('border', 0);
  tagTable.AddIntegerParam('cellpadding', 0);
  tagTable.AddIntegerParam('cellspacing', 0);
  tagTable.AddStringParam('summary', '');

{$IFDEF INTRAWEB72}
  if NeedStyleDetail then
    tagTable.AddStringParam('style', sCSS);
{$ENDIF}

  DoRenderButtonBar;
  if FControlBarAboveHeader then
  begin
    DoRenderControlBar;
    DoRenderHeader;
  end else
  begin
    DoRenderHeader;
    DoRenderControlBar;
  end;
  DoRenderDetail;

  p := self;
  repeat
    bShow := p.Visible;
    p := p.Parent;
  until (not bShow) or (p = nil);

  
  if FAutoHeight and bShow then
  begin
    s := 'document.getElementById('''+HTMLName+''').style.height=IWTop().FindElem('''+HTMLName+_tbl+''').clientHeight';

    if FShowButtonBar then
      s := s+'+IWTop().FindElem('''+HTMLName+_btnbar+''').clientHeight;';

    {$IFDEF INTRAWEB110}
    if False then
    {$ELSE}
    if AContext.PageContext.WebApplication.IsPartialUpdate then
    {$ENDIF}
      TIWPageContext40(AContext.PageContext).AddToUpdateInitProc(s)
    else
      TIWPageContext40(AContext.PageContext).AddToInitProc(s);
  end;


  ContentAfterRenderHTML;
end;

procedure TArcIWStringGrid.RenderScripts(AComponentContext: TIWBaseHTMLComponentContext);
begin
  inherited RenderScripts(AComponentContext);
  if AComponentContext.PageContext is TIWPageContext40 then begin
    if StyleTable.BorderStyle.Color <> clNone then begin
      TIWPageContext40(AComponentContext.PageContext).AddToIWCLInitProc(
        'if (' +HTMLName + 'IWCL!=null){'+HTMLName + 'IWCL.BorderWidthPixels=' +inttostr(StyleTable.BorderStyle.Width)+ ';}'
      );
    end;
  end;
end;


{ TArcGridStringColumn }

procedure TArcGridStringColumn.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TArcGridStringColumn(Dest).Caption := Caption;
  TArcGridStringColumn(Dest).FAlignment := FAlignment;
  TArcGridStringColumn(Dest).FOverrideStyle := FOverrideStyle;
  TArcGridStringColumn(Dest).FStyleHeader.Assign(FStyleHeader);
  TArcGridStringColumn(Dest).FStyleControlBar.Assign(FStyleControlBar);
  TArcGridStringColumn(Dest).FStyleDetail.Assign(FStyleDetail);
  TArcGridStringColumn(Dest).FStyleDetailAlt.Assign(FStyleDetailAlt);
  TArcGridStringColumn(Dest).FStyleSelected.Assign(FStyleSelected);
  TArcGridStringColumn(Dest).FCaption := FCaption;
  TArcGridStringColumn(Dest).FValues.Assign(FValues);
  TArcGridStringColumn(Dest).FWidth := FWidth;
  TArcGridStringColumn(Dest).FVisible := FVisible;
  TArcGridStringColumn(Dest).FOnClickCaption := FOnClickCaption;
  TArcGridStringColumn(Dest).FOnClick := FOnClick;
  TArcGridStringColumn(Dest).FValuesHaveLinks := FValuesHaveLinks;
  TArcGridStringColumn(Dest).FClickEventAsLinks := FClickEventAsLinks;
  TArcGridStringColumn(Dest).FLinkTarget := FLinkTarget;

  TArcGridStringColumn(Dest).FCaptionIndent := FCaptionIndent;
  TArcGridStringColumn(Dest).FIndent := FIndent;
  TArcGridStringColumn(Dest).FWrapText := FWrapText;
  TArcGridStringColumn(Dest).FHint := FHint;
  TArcGridStringColumn(Dest).FShowHint := FShowHint;
  //TArcGridStringColumn(Dest).FUseMinimumWidth := FUseMinimumWidth;

{$IFDEF INTRAWEB72}
  TArcGridStringColumn(Dest).AutoRetrieveObject := FAutoRetrieveObject;
  TArcGridStringColumn(Dest).FCaptionIcon.Assign(FCaptionIcon);
  TArcGridStringColumn(Dest).FCaptionIconAlignment := FCaptionIconAlignment;
  TArcGridStringColumn(Dest).FIcon.Assign(FIcon);
  TArcGridStringColumn(Dest).FIconAlignment := FIconAlignment;
  TArcGridStringColumn(Dest).FControlAlignment := FControlAlignment;
  TArcGridStringColumn(Dest).FOnRetrieveControl := FOnRetrieveControl;
  TArcGridStringColumn(Dest).FOnRetrieveCBControl := FOnRetrieveCBControl;
  TArcGridStringColumn(Dest).FControlSizing := FControlSizing;
{$ENDIF}
end;

constructor TArcGridStringColumn.Create(Collection: TCollection);
begin
  inherited;
  FWrapText := True;
  FVisible := True;
  FStyleHeader := TArcGridStyle.Create(False, True, False, True);
  FStyleDetail := TArcGridStyle.Create(False, True, False, True);
  FStyleDetailAlt := TArcGridStyle.Create(False, True, False, True);
  FStyleSelected := TArcGridStyle.Create(False, True, False, True);
  FValues := TStringList.Create; //TArcGridStringList.Create;
  FSelected := TBits.Create;

  FAlignment := taLeftJustify;

{$IFNDEF INTRAWEB51}
  FCaptionIcon := TIWFileReference.Create;
  FIcon := TIWFileReference.Create;
{$ENDIF}

  FStyleControlBar := TArcGridStyle.Create(False, True, False, True);
  FStyleControlBar.TextAlign := taCenter;
  FStyleControlBar.TextVertAlign := vaMiddle;
  FStyleControlBar.BackgroundColor := $00800000;
  FStyleControlBar.BorderStyle.Style := brdSolid;
  FStyleControlBar.BorderStyle.Width := 1;
  FStyleControlBar.BorderStyle.Color := clWebBlack;
  FStyleControlBar.Font.Color := clWebWhite;
  FStyleControlBar.Font.Style := [fsBold];
  FStyleControlBar.Font.FontName := 'Arial';
  FStyleControlBar.Font.Size := 9;
  FStyleControlBar.Padding := 0;

  FStyleHeader.BackgroundColor := $00800000;
  FStyleHeader.BorderStyle.Style := brdSolid;
  FStyleHeader.BorderStyle.Width := 1;
  FStyleHeader.BorderStyle.Color := clWebBlack;
  FStyleHeader.Font.Color := clWebWhite;
  FStyleHeader.Font.Style := [fsBold];
  FStyleHeader.Font.FontName := 'Arial';
  FStyleHeader.Font.Size := 10;
  FStyleHeader.Padding := 2;

  FStyleDetail.BackgroundColor := clWebWHITE;
  FStyleDetail.BorderStyle.Style := brdSolid;
  FStyleDetail.BorderStyle.Width := 1;
  FStyleDetail.BorderStyle.Color := clWebBlack;
  FStyleDetail.Font.Color := clWebBlack;
  FStyleDetail.Font.Style := [];
  FStyleDetail.Font.FontName := 'Arial';
  FStyleDetail.Font.Size := 10;
  FStyleDetail.Padding := 2;

  FStyleDetailAlt.BackgroundColor := $00FFCECE;
  FStyleDetailAlt.BorderStyle.Style := brdSolid;
  FStyleDetailAlt.BorderStyle.Width := 1;
  FStyleDetailAlt.BorderStyle.Color := clWebBlack;
  FStyleDetailAlt.Font.Color := clWebBlack;
  FStyleDetailAlt.Font.Style := [];
  FStyleDetailAlt.Font.FontName := 'Arial';
  FStyleDetailAlt.Font.Size := 10;
  FStyleDetailAlt.Padding := 2;

  FStyleSelected.BackgroundColor := $00600000;
  FStyleSelected.BorderStyle.Style := brdSolid;
  FStyleSelected.BorderStyle.Width := 1;
  FStyleSelected.BorderStyle.Color := clWebBlack;
  FStyleSelected.Font.Color := clWebWhite;
  FStyleSelected.Font.Style := [fsBold];
  FStyleSelected.Font.FontName := 'Arial';
  FStyleSelected.Font.Size := 10;
  FStyleSelected.Padding := 2;
end;

destructor TArcGridStringColumn.Destroy;
begin
{$IFNDEF INTRAWEB51}
  FCaptionIcon.Free;
  FIcon.Free;
{$ENDIF}
  FSelected.Free;
  FStyleHeader.Free;
  FStyleControlBar.Free;
  FStyleDetail.Free;
  FStyleDetailAlt.Free;
  FStyleSelected.Free;
  FValues.Free;
  inherited;
end;

function TArcGridStringColumn.GetDisplayName: string;
begin
  if Caption = '' then
    Result := '(unnamed)'
  else
    Result := Caption;
end;

procedure TArcGridStringColumn.MoveTo(AIndex: integer);
begin
  if Self.Index <> AIndex then
  begin
    Self.Index := AIndex;
  end;
end;

function TArcGridStringColumn.PaddingString(WantDetail: boolean; DefaultPadding: integer): string;
begin
  Result := 'padding: ' + IntToStr(DefaultPadding) + 'px;';
  if WantDetail then
  begin
    if FAlignment = taLeftJustify then
      Result := Result + 'padding-left: ' + IntToStr(DefaultPadding + FIndent) + 'px;'
    else if FAlignment = taRightJustify then
      Result := Result + 'padding-right: ' + IntToStr(DefaultPadding + FIndent) + 'px;'
  end else
  begin
    if FStyleControlBar.TextAlign = taLeftJustify then
      Result := Result + 'padding-left: ' + IntToStr(DefaultPadding + FCaptionIndent) + 'px;'
    else if FAlignment = taRightJustify then
      Result := Result + 'padding-right: ' + IntToStr(DefaultPadding + FCaptionIndent) + 'px;'
  end;
end;

procedure TArcGridStringColumn.SetCaption(const Value: string);
begin
  if FCaption = Value then
    exit;

  if Assigned(TArcGridColumns(Collection).Content) then
    TArcGridColumns(Collection).Content.DoRenameColumn(TArcGridColumns(Collection).Grid, FCaption, Value);
  FCaption := Value;
end;

procedure TArcGridStringColumn.SetValues(const Value: TStrings);
begin
  FValues.Assign(Value);
end;

{ TArcGridColumns }

function TArcGridColumns.Add: TArcGridStringColumn;
begin
  Result := TArcGridStringColumn(inherited Add);
  Grid.EnsureXYValues(Count-1,Grid.RowCount-1);
end;

function TArcGridColumns.GetColumnState: string;
var
  ms : TMemoryStream;
  ss : TStringStream;
  cmp : TColumnsWriter;
  i: Integer;
begin
  ms := TMemoryStream.Create;
  ss := TStringStream.Create(Result);
  try
    cmp := TColumnsWriter.Create(nil);
    try
      cmp.Columns.Assign(Self);
      {for i := 0 to cmp.Columns.Count - 1 do
        cmp.Columns[i].Values.Clear;}
      ms.WriteComponent(cmp);
      ms.Position := 0;
      ObjectBinarytoText(ms, ss);
      Result := ss.DataString;
    finally
      cmp.Free;
    end;
  finally
    ss.Free;
    ms.Free;
  end;
end;

function TArcGridColumns.GetItems(idx: integer): TArcGridStringColumn;
begin
  Result := TArcGridStringColumn(inherited Items[idx]);
end;

function TArcGridColumns.HasColumn(ColumnCaption: string): boolean;
var
  i: integer;
  sCap: string;
begin
  sCap := Uppercase(ColumnCaption);
  Result := False;
  for i := 0 to Count - 1 do
    if Uppercase(Items[i].Caption) = sCap then
    begin
      Result := True;
      break;
    end;
end;

function TArcGridColumns.IndexOf(ColumnCaption: string): integer;
var
  i: integer;
  sCap: string;
begin
  Result := -1;
  sCap := Uppercase(ColumnCaption);
  for i := 0 to Count - 1 do
  begin
    if Uppercase(Items[i].Caption) = sCap then
    begin
      Result := i;
      break;
    end;
  end;
end;

procedure TArcGridColumns.SetColumnState(const Value: string);
var
  ms : TMemoryStream;
  ss : TStringStream;
  cmp : TColumnsWriter;
begin
  ms := TMemoryStream.Create;
  ss := TStringStream.Create(Value);
  try
    cmp := TColumnsWriter.Create(nil);
    try
      ObjectTextToBinary(ss, ms);
      ms.Position := 0;
      ms.ReadComponent(cmp);
      Self.Assign(cmp.Columns);
    finally
      cmp.Free;
    end;
  finally
    ss.Free;
    ms.Free;
  end;
end;

procedure TArcGridColumns.SetItems(idx: integer; const Value: TArcGridStringColumn);
begin
  inherited Items[idx] := Value;
end;

{ TStyleExportFile }

procedure TStyleExportFile.ApplyStyleFrom(Grid: TArcIWStringGrid);
begin
  StyleHeader.Assign(Grid.StyleHeader);
  StyleDetail.Assign(Grid.StyleDetail);
  StyleDetailAlt.Assign(Grid.StyleDetailAlt);
  StyleTable.Assign(Grid.StyleTable);
  StyleButtonBar.Assign(Grid.StyleButtonBar);
  StyleControlBar.Assign(Grid.StyleControlBar);
  StyleSelected.Assign(Grid.StyleSelected);
  CaptionButtons.Assign(Grid.CaptionButtons);

  AutoColumnWidth := Grid.AutoColumnWidth;
  AutoRowHeight := Grid.AutoRowHeight;
  UseAltStyles := Grid.UseAltStyles;
  ShowHeaderRow := Grid.ShowHeaderRow;
  ScrollToSelectedCell := Grid.ScrollToSelectedCell;
  ScrollToSelectedRow := Grid.ScrollToSelectedRow;
  ShowButtonBar := Grid.ShowButtonBar;
  ShowControlBar := Grid.ShowControlBar;
  AllowEmptyCells := Grid.AllowEmptyCells;
  Scrollbars := Grid.Scrollbars;
  AutoHeight := Grid.AutoHeight;
  RowHeight := Grid.RowHeight;
  SuppressSelRowClick := Grid.SuppressSelRowClick;
  Caption := Grid.Caption;
  ButtonBarHeight := Grid.ButtonBarHeight;
  ControlBarAboveHeader := Grid.ControlBarAboveHeader;
end;

procedure TStyleExportFile.ApplyStyleTo(Grid: TArcIWStringGrid);
begin
  Grid.StyleHeader.Assign(StyleHeader);
  Grid.StyleDetail.Assign(StyleDetail);
  Grid.StyleDetailAlt.Assign(StyleDetailAlt);
  Grid.StyleTable.Assign(StyleTable);
  Grid.StyleButtonBar.Assign(StyleButtonBar);
  Grid.StyleControlBar.Assign(StyleControlBar);
  Grid.StyleSelected.Assign(StyleSelected);
  Grid.CaptionButtons.Assign(CaptionButtons);

  Grid.AutoColumnWidth := AutoColumnWidth;
  Grid.AutoRowHeight := AutoRowHeight;
  Grid.UseAltStyles := UseAltStyles;
  Grid.ShowHeaderRow := ShowHeaderRow;
  Grid.ScrollToSelectedCell := ScrollToSelectedCell;
  Grid.ScrollToSelectedRow := ScrollToSelectedRow;
  Grid.ShowButtonBar := ShowButtonBar;
  Grid.ShowControlBar := ShowControlBar;
  Grid.AllowEmptyCells := AllowEmptyCells;
  Grid.Scrollbars := Scrollbars;
  Grid.AutoHeight := AutoHeight;
  Grid.RowHeight := RowHeight;
  Grid.SuppressSelRowClick := SuppressSelRowClick;
  Grid.Caption := Caption;
  Grid.ButtonBarHeight := ButtonBarHeight;
  Grid.ControlBarAboveHeader := ControlBarAboveHeader;
end;

constructor TStyleExportFile.Create(AOwner: TComponent);
begin
  inherited;
  FStyleTable := TArcGridStyle.Create(False, False, False, False);
  FStyleButtonBar := TArcGridStyle.Create(False, False, False, False);
  FStyleDetail := TArcGridStyle.Create(False, False, False, False);
  FStyleHeader := TArcGridStyle.Create(False, False, False, False);
  FStyleSelected := TArcGridStyle.Create(False, False, False, False);
  FStyleDetailAlt := TArcGridStyle.Create(False, False, False, False);
  FStyleControlBar := TArcGridStyle.Create(False, False, False, False);
  FCaptionButtons := TCaptionButtons.Create;
end;

destructor TStyleExportFile.Destroy;
begin
  FStyleTable.Free;
  FStyleButtonBar.Free;
  FStyleDetail.Free;
  FStyleHeader.Free;
  FStyleSelected.Free;
  FStyleDetailAlt.Free;
  FStyleControlBar.Free;
  FCaptionButtons.Free;
  inherited;
end;

{ TColumnsWriter }

constructor TColumnsWriter.Create(AOwner: TComponent);
begin
  inherited;
  FColumns := TArcGridColumns.Create(self,TArcGridStringColumn);
end;

destructor TColumnsWriter.Destroy;
begin
  FColumns.Free;
  inherited;
end;

procedure TColumnsWriter.SetColumns(const Value: TArcGridColumns);
begin
  FColumns.Assign(Value);
end;

initialization
  TIWServer.AddInternalFile('IW_GFX_ArcGridFirstPage', '/gfx/ArcGridFirstPage.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridLastPage', '/gfx/ArcGridLastPage.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridFirst', '/gfx/ArcGridFirst.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridLast', '/gfx/ArcGridLast.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridRefresh', '/gfx/ArcGridRefresh.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridPriorPage', '/gfx/ArcGridPriorPage.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridPrior', '/gfx/ArcGridPrior.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridNext', '/gfx/ArcGridNext.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridNextPage', '/gfx/ArcGridNextPage.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridAppend', '/gfx/ArcGridAppend.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridEdit', '/gfx/ArcGridEdit.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridDelete', '/gfx/ArcGridDelete.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridSave', '/gfx/ArcGridSave.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridCancel', '/gfx/ArcGridCancel.png');

  TIWServer.AddInternalFile('IW_GFX_ArcGridFirstPageDisabled', '/gfx/ArcGridFirstPageDisabled.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridLastPageDisabled', '/gfx/ArcGridLastPageDisabled.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridFirstDisabled', '/gfx/ArcGridFirstDisabled.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridLastDisabled', '/gfx/ArcGridLastDisabled.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridRefreshDisabled', '/gfx/ArcGridRefreshDisabled.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridPriorPageDisabled', '/gfx/ArcGridPriorPageDisabled.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridPriorDisabled', '/gfx/ArcGridPriorDisabled.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridNextDisabled', '/gfx/ArcGridNextDisabled.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridNextPageDisabled', '/gfx/ArcGridNextPageDisabled.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridAppendDisabled', '/gfx/ArcGridAppendDisabled.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridEditDisabled', '/gfx/ArcGridEditDisabled.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridDeleteDisabled', '/gfx/ArcGridDeleteDisabled.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridSaveDisabled', '/gfx/ArcGridSaveDisabled.png');
  TIWServer.AddInternalFile('IW_GFX_ArcGridCancelDisabled', '/gfx/ArcGridCancelDisabled.png');
end.

