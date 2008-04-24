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

unit ArcIWStringGridTreeContent;

interface

uses SysUtils, {$IFNDEF VER130}StrUtils, Variants, {$ENDIF}
  Classes, ArcIWStringGridContent, ArcIWStringGrid, controls,
  IWBaseControl, IWCompCheckbox, IWCompEdit, IWCompMemo, IWCompButton, IWExtCtrls,
  Graphics, TypInfo, ArcFastStrings, IWCompListbox, IWBaseInterfaces,
  IWRenderContext, IWHTMLTag, IWTypes, IWColor, db, IWFileReference, IWServer,
  ArcIWGridCommon;

type
  TArcIWStringGridTreeContent = class;
  TArcIWTreeItems = class;
  TArcIWTreeItem = class;

  TArcIWTreeItemEvent = procedure(Sender : TObject; Item : TArcIWTreeItem) of object;
  TArcIWTreeItem = class(TCollectionItem)
  private
    FTag: integer;
    FItems: TArcIWTreeItems;
    FValue: Variant;
    FCaption: string;
    FOnClick: TArcIWTreeItemEvent;
    FIcon: TArcIWGraphic;
    FLink: TIWFileReference;
    FIconMinus: TArcIWGraphic;
    FIconPlus: TArcIWGraphic;
    FHidePlusMinus: boolean;
    FCollapsed: boolean;
    FEnabled: boolean;
    FIconPadding: integer;
    FHint: string;
    FTarget: string;
    FRow: integer;
    FColumn: integer;
    FOnExpandCollapse: TArcIWTreeItemEvent;
    FOnRowChange: TArcIWTreeItemEvent;
    function GetLevel: integer;
    function GetParent: TArcIWTreeItem;
    procedure SetCollapsed(const Value: boolean);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function URLPlusIcon(Attributes : string) : String;
    function URLMinusIcon(Attributes : string) : String;
    function URLIcon(Attributes : string) : string;
    function FindLinkID : string;
    procedure MakeVisible;
    property Column : integer read FColumn;
    property Row : integer read FRow;
    property Level : integer read GetLevel;
    property Parent : TArcIWTreeItem read GetParent;
  published
    property Caption : string read FCaption write FCaption;
    property Value : Variant read FValue write FValue;
    property Tag : integer read FTag write FTag;
    property Items : TArcIWTreeItems read FItems write FItems;
    property Icon : TArcIWGraphic read FIcon write FIcon;
    property IconPlus : TArcIWGraphic read FIconPlus write FIconPlus;
    property IconMinus : TArcIWGraphic read FIconMinus write FIconMinus;
    property IconPadding : integer read FIconPadding write FIconPadding;
    property Link : TIWFileReference read FLink write FLink;
    property OnClick : TArcIWTreeItemEvent read FOnClick write FOnClick;
    property OnExpandCollapse : TArcIWTreeItemEvent read FOnExpandCollapse write FOnExpandCollapse;
    property OnRowChange : TArcIWTreeItemEvent read FOnRowChange write FOnRowChange;
    property HidePlusMinus : boolean read FHidePlusMinus write FHidePlusMinus;
    property Collapsed : boolean read FCollapsed write SetCollapsed;
    property Enabled : boolean read FEnabled write FEnabled;
    property Hint : string read FHint write FHint;
    property Target : string read FTarget write FTarget;
  end;

  TArcIWTreeItems = class(TOwnedCollection)
  private
    FItemOwner : TArcIWTreeItem;
    FContent: TArcIWStringGridTreeContent;
    function GetItem(idx: integer): TArcIWTreeItem;
    procedure SetItem(idx: integer; const Value: TArcIWTreeItem);
  protected
    function GetOwner: TPersistent; override;
  public
    function Add : TArcIWTreeItem;
    property Items[idx : integer] : TArcIWTreeItem read GetItem write SetItem; default;
    constructor Create(AOwner: TArcIWStringGridTreeContent; AItemOwner : TArcIWTreeItem); reintroduce; virtual;
  end;

  TArcIWListItemsEvent = procedure(Sender : TObject; ParentItem : TArcIWTreeItem; Items : TArcIWTreeItems) of object;


  TArcIWStringGridTreeContent = class(TArcIWStringGridContent)
  private
    FOnListItems: TArcIWListItemsEvent;
    FItems: TArcIWTreeItems;
    FTreeColumnIndex : integer;
    ItemList : TStringList;
    function GetTreeColumnIndex: integer;
    procedure SetTreeColumnIndex(const Value: integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ResetGridData; virtual;
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
    procedure RecursivelyBuildItems(item : TArcIWTreeItem); virtual;
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

    function FindItemInRow(Row : integer) : TArcIWTreeItem;
    function ProcessCaption(str: string): string; override;
  published
    property TreeColumnIndex : integer read GetTreeColumnIndex write SetTreeColumnIndex;
    property Items : TArcIWTreeItems read FItems write FItems;
    property OnListItems : TArcIWListItemsEvent read FOnListItems write FOnListItems;
  end;

implementation

uses IWUtils;



{ TArcIWStringGridTreeContent }

procedure TArcIWStringGridTreeContent.Append;
begin
  inherited;
  raise Exception.Create('This method does not apply to TreeContent');
end;

procedure TArcIWStringGridTreeContent.CacheToGrid(Col, Row: integer);
begin
  inherited;

end;

procedure TArcIWStringGridTreeContent.Cancel;
begin
  inherited;
  raise Exception.Create('This method does not apply to TreeContent');
end;

constructor TArcIWStringGridTreeContent.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TArcIWTreeItems.Create(Self, nil);
  ItemList := TStringList.Create;
end;

procedure TArcIWStringGridTreeContent.Delete;
begin
  inherited;
  raise Exception.Create('This method does not apply to TreeContent');
end;

destructor TArcIWStringGridTreeContent.Destroy;
begin
  FItems.Free;
  ItemList.Free;
  inherited;
end;

procedure TArcIWStringGridTreeContent.DoAfterRenderHTML(Sender: TObject);
begin
  inherited;

end;

procedure TArcIWStringGridTreeContent.DoBeforeRenderHTML(Sender: TObject;
  AContext: TIWBaseHTMLComponentContext);
begin
  inherited;

end;

procedure TArcIWStringGridTreeContent.DoCellClick(Sender: TObject;
  const Col, Row: Integer; var Data: string);
begin
  inherited;

end;

procedure TArcIWStringGridTreeContent.DoCellData(Sender: TObject; x,
  y: Integer; var sValue: string);
begin
  inherited;

end;

procedure TArcIWStringGridTreeContent.DoClickCaption(Sender: TObject;
  const Col: Integer);
begin
  inherited;

end;

function TArcIWStringGridTreeContent.DoNeedOnClickEvent(
  Sender: TObject): Boolean;
begin
  Result := False;
end;

function TArcIWStringGridTreeContent.DoNeedScript: string;
begin
  Result :=
    'function '+Name+'_PMClick(ItemID) {'+DebugEOL+
    '  SubmitClickConfirm("' + Grid.HTMLName + '","@."+ItemID,false,"");' + DebugEOL +
    '}'+DebugEOL+
    'function '+Name+'_Click(ItemID) {'+DebugEOL+
    '  SubmitClickConfirm("' + Grid.HTMLName + '","@,"+ItemID,false,"");' + DebugEOL +
    '}'+DebugEOL;
end;

procedure TArcIWStringGridTreeContent.DoPopulateRow(Sender: TObject;
  const y: Integer; slValues: TStrings);
begin
  inherited;

end;

procedure TArcIWStringGridTreeContent.DoReloadData(Sender: TObject);
begin
  inherited;

end;

procedure TArcIWStringGridTreeContent.DoRenameColumn(Sender: TObject;
  OldName, NewName: string);
begin
  inherited;

end;

procedure TArcIWStringGridTreeContent.DoRender(Sender: TObject);
  procedure RenderVisibleItems(AItem : TArcIWTreeItem; Col : integer; var Row : integer);
    function PadLevels(item : TArcIWTreeItem) : string;
    begin
      Result := 'padding-left: '+IntToStr(item.Level*(item.IconPlus.Width+item.IconPadding))+'px;';
    end;
    function RenderPlusMinus(item : TArcIWTreeItem) : string;
    begin
      if (not item.HidePlusMinus) and (item.Items.Count > 0) then
      begin
        if item.Collapsed then
          Result := item.URLPlusIcon('alt="+" style="margin-right: '+IntToStr(item.IconPadding)+'px;" onClick="return '+Name+'_PMClick('''+item.FindLinkID+''')"')
        else
          Result := item.URLMinusIcon('alt="-" style="margin-right: '+IntToStr(item.IconPadding)+'px;" onClick="return '+Name+'_PMClick('''+item.FindLinkID+''')"');
      end else
        Result := '';
    end;
    function RenderIcon(item : TArcIWTreeItem) : string;
    begin
      Result := item.URLIcon('style="margin-right: '+IntToStr(item.IconPadding)+'px;"');
    end;
    function RenderLink(item : TArcIWTreeItem) : string;
    var
      s : string;
    begin
      Result := '';
      s := item.Link.Location(WebApplication.InternalURLBase);
      if (s <> '') then
      begin
        Result := '<a href="'+s+'"';
        if (s <> '') and (item.Target <> '') then
          Result := Result+' Target="'+item.Target+'"';
        Result := Result+'>';
      end;
    end;
    function RenderLinkClose(item : TArcIWTreeItem) : string;
    var
      s : string;
    begin
      Result := '';
      s := item.Link.Location(WebApplication.InternalURLBase);
      if (s <> '') then
        Result := '</a>';
    end;
    function RenderClick(item : TArcIWTreeItem) : string;
    begin
      Result := '';
      if Assigned(item.OnClick) then
      begin
        Result := Result+'<span onClick="return '+Name+'_Click('''+item.FindLinkID+''');" style="cursor:pointer;">';
        if Grid.Columns[FTreeColumnIndex].ClickEventAsLinks then
          Result := Result+'<a href="#">';
      end;
    end;
    function RenderClickClose(item : TArcIWTreeItem) : string;
    begin
      Result := '';
      if Assigned(item.OnClick) then
      begin
        if Grid.Columns[FTreeColumnIndex].ClickEventAsLinks then
          Result := '</a>';
        Result := Result+'</span>';
      end;
    end;
    function RenderHint(item : TArcIWTreeItem) : string;
    begin
      if item.Hint <> '' then
        Result := 'title="'+item.Hint+'"'
      else
        Result := '';
    end;
  var
    i : integer;
    items : TArcIWTreeItems;
    CWT : string;
    sWidth: string;
  begin
    if AItem = nil then
      items := FItems
    else
      items := AItem.Items;

    for i := 0 to items.Count -1 do
    begin
      items[i].FColumn := Col;
      if items[i].FRow <> Row then
      begin
        items[i].FRow := Row;
        if Assigned(items[i].FOnRowChange) then
          items[i].OnRowChange(Self,Items[i]);
      end;

      if Grid.ColumnsWidthsType = cwtPixel then
        CWT := 'px'
      else if Grid.ColumnsWidthsType = cwtPercent then
        CWT := '%';

      if items[i].Column >= 0 then
        sWidth := IntToStr(Grid.Columns[items[i].Column].Width)+CWT
      else
        sWidth := '100%';


      Grid.Cells[Col,Row] :=
        '<div style="overflow:hidden;width:100%;max-width:100%;'+
        //'width:expression(alert(this.parentNode.clientWidth+'' ''+this.parentNode.parentNode.clientWidth));">'+//(this.parentNode.clientWidth > this.parentNode.parentNode.clientWidth-20) ? (this.parentNode.parentNode.clientWidth-20)+''px'' : ''auto'');">'+
        '"><span style="'+PadLevels(items[i])+'" '+RenderHint(items[i])+'>'+
         RenderPlusMinus(items[i])+
         RenderIcon(items[i])+RenderLink(items[i])+RenderClick(items[i])+
         items[i].Caption+
         RenderClickClose(items[i])+RenderLinkClose(items[i])+'</span></div>';
      Objects[Col,Row] := items[i];
      inc(Row);
      if not items[i].Collapsed then
      begin
        RenderVisibleItems(items[i], Col, Row);
      end;
    end;
  end;
  function CountVisibleItems(AItem : TArcIWTreeItem) : integer;
  var
    i : integer;
    items : TArcIWTreeItems;
  begin
    if AItem = nil then
      items := FItems
    else
      items := AItem.Items;

    result := items.Count;
    if (AItem = nil) or (not AItem.Collapsed) then
      for i := 0 to items.Count -1 do
      begin
        if not Items[i].Collapsed then
          result := Result + CountVisibleItems(Items[i]);
      end;
  end;
var
  y : integer;
begin
  Grid.RowCount := CountVisibleItems(nil);

  y := 0;
  if TreeColumnIndex >= 0 then
    RenderVisibleItems(nil, TreeColumnIndex, y);
end;

procedure TArcIWStringGridTreeContent.DoRetrieveCBObject(Sender: TObject;
  x: Integer; var ctrl: TControl);
begin
  inherited;

end;

procedure TArcIWStringGridTreeContent.DoRetrieveObject(Sender: TObject; x,
  y: Integer; var ctrl: TControl);
begin
  inherited;

end;

procedure TArcIWStringGridTreeContent.DoRowClick(Sender: TObject;
  const Row: Integer);
begin
  inherited;

end;

procedure TArcIWStringGridTreeContent.DoSelectCell(Sender: TObject;
  const x, y: Integer; Value: Boolean);
begin
  inherited;

end;

procedure TArcIWStringGridTreeContent.DoSelectCol(Sender: TObject;
  const x: Integer; Value: Boolean);
begin
  inherited;

end;

procedure TArcIWStringGridTreeContent.DoSelectRow(Sender: TObject;
  const y: Integer; Value: Boolean);
begin
  inherited;

end;

procedure TArcIWStringGridTreeContent.DoSubmit(Sender: TObject;
  const AValue: string);
var
  idx : integer;
  item : TArcIWTreeItem;
  s : string;
begin
  inherited;
  s := Copy(AValue,2,1);
  if s = '.' then
  begin
    idx := ItemList.IndexOf(Copy(AValue,3,High(Integer)));
    if idx >= 0 then
    begin
      item := TArcIWTreeItem(ItemList.Objects[idx]);
      item.MakeVisible;
      {if Assigned(FOnListItems) then
      begin
        RecursivelyBuildItems(item);
      end;}
      if item.Items.Count > 0 then
        item.Collapsed := not item.Collapsed;
      if Assigned(item.OnExpandCollapse) then
        item.OnExpandCollapse(Self,item);
    end;
  end else if s = ',' then
  begin
    idx := ItemList.IndexOf(Copy(AValue,3,High(Integer)));
    if idx >= 0 then
    begin
      item := TArcIWTreeItem(ItemList.Objects[idx]);
      if Assigned(item.OnClick) then
        item.OnClick(Self,item);
    end;
  end;
end;

procedure TArcIWStringGridTreeContent.Edit;
begin
  inherited;
  raise Exception.Create('This method does not apply to TreeContent');
end;

function TArcIWStringGridTreeContent.FindItemInRow(Row: integer): TArcIWTreeItem;
  function FindItemIn(List : TArcIWTreeItems; Row : integer) : TArcIWTreeItem;
  var
    i : integer;
begin
  Result := nil;
  for i := 0 to ItemList.Count-1 do
  begin
    Result := nil;
    for i := 0 to List.Count-1 do
    begin
      if List[i].Row = Row then
      begin
        Result := List[i];
        break;
      end;
      //if not List[i].Collapsed then
      //begin
        Result := FindItemIn(List[i].Items,Row);
        if Result <> nil then
          break;
//      end;
    end;
  end;
begin
  Result := FindItemIn(Items, Row);
end;

procedure TArcIWStringGridTreeContent.First;
begin
  inherited;
  raise Exception.Create('This method does not apply to TreeContent');
end;

procedure TArcIWStringGridTreeContent.FirstPage;
begin
  inherited;
  raise Exception.Create('This method does not apply to TreeContent');
end;

function TArcIWStringGridTreeContent.GetIsFirstPage: boolean;
begin
  Result := True;
end;

function TArcIWStringGridTreeContent.GetIsFirstRecord: boolean;
begin
  Result := True;
end;

function TArcIWStringGridTreeContent.GetIsLastPage: boolean;
begin
  Result := True;
end;

function TArcIWStringGridTreeContent.GetIsLastRecord: boolean;
begin
  Result := True;
end;

function TArcIWStringGridTreeContent.GetIsNextRecord: boolean;
begin
  Result := False;
end;

function TArcIWStringGridTreeContent.GetIsPriorRecord: boolean;
begin
  Result := False;
end;

function TArcIWStringGridTreeContent.GetTreeColumnIndex: integer;
begin
  Result := FTreeColumnIndex;
  if Assigned(GridNoRaise) then
  begin
    if Result >= Grid.Columns.Count then
      Result := Grid.Columns.Count-1;
    if (Result < 0) and (Grid.Columns.Count > 0) then
      Result := 0;
  end;
end;

procedure TArcIWStringGridTreeContent.Last;
begin
  inherited;
  raise Exception.Create('This method does not apply to TreeContent');
end;

procedure TArcIWStringGridTreeContent.LastPage;
begin
  inherited;
  raise Exception.Create('This method does not apply to TreeContent');
end;

procedure TArcIWStringGridTreeContent.Loaded;
begin
  inherited;
  if Active and (not (csDesigning in ComponentState)) then
    ResetGridData;
end;

procedure TArcIWStringGridTreeContent.Next;
begin
  inherited;
  raise Exception.Create('This method does not apply to TreeContent');
end;

procedure TArcIWStringGridTreeContent.NextPage;
begin
  inherited;
  raise Exception.Create('This method does not apply to TreeContent');
end;

procedure TArcIWStringGridTreeContent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

end;

procedure TArcIWStringGridTreeContent.Post;
begin
  inherited;
  raise Exception.Create('This method does not apply to TreeContent');
end;

procedure TArcIWStringGridTreeContent.Prior;
begin
  inherited;
  raise Exception.Create('This method does not apply to TreeContent');
end;

procedure TArcIWStringGridTreeContent.PriorPage;
begin
  inherited;
  raise Exception.Create('This method does not apply to TreeContent');
end;

function TArcIWStringGridTreeContent.ProcessCaption(str: string): string;
begin

end;

procedure TArcIWStringGridTreeContent.RecursivelyBuildItems(item: TArcIWTreeItem);
var
  i : integer;
begin
  FOnListItems(Self,item,Item.FItems);
  for i := 0 to Item.FItems.Count -1 do
    RecursivelyBuildItems(item.FItems[i]);
end;

procedure TArcIWStringGridTreeContent.Refresh;
begin
  inherited;
  ResetGridData;
end;

procedure TArcIWStringGridTreeContent.ResetGridData;
var
  i : integer;
begin
  if Assigned(FOnListItems) then
  begin
    FOnListItems(Self,nil,FItems);
    for i := 0 to FItems.Count -1 do
      RecursivelyBuildItems(FItems[i]);
  end;
  if (TreeColumnIndex >= 0) and (TreeColumnIndex < Grid.Columns.Count) then
    Grid.Columns[TreeColumnIndex].WrapText := False;
end;

procedure TArcIWStringGridTreeContent.SetActive(const Value: Boolean);
begin
  inherited;
  if Active and (not (csDesigning in ComponentState)) and (not (csLoading in ComponentState)) then
    ResetGridData;
end;

procedure TArcIWStringGridTreeContent.SetSelectedDataRow(
  const Value: Integer);
begin
  inherited;

end;

procedure TArcIWStringGridTreeContent.SetTreeColumnIndex(
  const Value: integer);
begin
  FTreeColumnIndex := Value;
end;

procedure TArcIWStringGridTreeContent.UpdateNavButtonState;
begin
  inherited;

end;

{ TArcIWTreeItems }

function TArcIWTreeItems.Add: TArcIWTreeItem;
begin
  result := TArcIWTreeItem(inherited Add);
end;

constructor TArcIWTreeItems.Create(AOwner: TArcIWStringGridTreeContent; AItemOwner : TArcIWTreeItem);
begin
  inherited Create(AOwner, TArcIWTreeItem);
  FContent := AOwner;
  FItemOwner := AItemOwner;
end;

function TArcIWTreeItems.GetItem(idx: integer): TArcIWTreeItem;
begin
  result := TArcIWTreeItem(inherited Items[idx]);
end;

function TArcIWTreeItems.GetOwner: TPersistent;
begin
  Result := FContent;
end;

procedure TArcIWTreeItems.SetItem(idx: integer;
  const Value: TArcIWTreeItem);
begin
  inherited Items[idx] := Value;
end;

{ TArcIWTreeItem }

constructor TArcIWTreeItem.Create(Collection: TCollection);
begin
  inherited;
  FItems := TArcIWTreeItems.Create(TArcIWTreeItems(Collection).FContent, Self);
  FIcon := TArcIWGraphic.Create('');
  FIcon.Width := 16;
  FIcon.Height := 16;
  FIconPlus := TArcIWGraphic.Create('/gfx/ArcTreePlus.gif');
  FIconPlus.Width := 9;
  FIconPlus.Height := 9;
  FIconMinus := TArcIWGraphic.Create('/gfx/ArcTreeMinus.gif');
  FIconMinus.Width := 9;
  FIconMinus.Height := 9;
  FLink := TIWFileReference.Create;
  FTarget := '_blank';
  FIconPadding := 4;
  FCollapsed := True;
  if not (csDesigning in TArcIWTreeItems(Collection).FContent.ComponentState) then
  begin
    TArcIWTreeItems(Collection).FContent.ItemList.AddObject(FindLinkID, Self);
  end;
end;

destructor TArcIWTreeItem.Destroy;
var
  idx : integer;
begin
  if not (csDesigning in TArcIWTreeItems(Collection).FContent.ComponentState) then
  begin
    idx := TArcIWTreeItems(Collection).FContent.ItemList.IndexOf(FindLinkID);
    TArcIWTreeItems(Collection).FContent.ItemList.Delete(idx);
  end;

  FIcon.Free;
  FIconPlus.Free;
  FIconMinus.Free;
  FLink.Free;
  FItems.Free;
  inherited;
end;

{.$o-}
function TArcIWTreeItem.FindLinkID: string;
var
  o : TArcIWTreeItem;
begin
  Result := '';
  o := Self;
  repeat
    Result := Result+'_'+IntToStr(o.ID);
    o := o.Parent;
  until o = nil;
end;
{.$o+}

function TArcIWTreeItem.GetDisplayName: string;
begin
  Result := Caption;
end;

function TArcIWTreeItem.GetLevel: integer;
var
  item : TArcIWTreeItem;
begin
  Result := 0;
  item := Self;
  while item.Parent <> nil do
  begin
    item := item.Parent;
    inc(Result);
  end;
end;

function TArcIWTreeItem.GetParent: TArcIWTreeItem;
begin
  Result := TArcIWTreeItems(Collection).FItemOwner;  
end;

procedure TArcIWTreeItem.MakeVisible;
var
  p : TArcIWTreeItem;
begin
  p := Parent;
  while p <> nil do
  begin
    p.Collapsed := False;
    p := p.Parent;
  end;
end;

procedure TArcIWTreeItem.SetCollapsed(const Value: boolean);
var
  i : integer;
begin
  FCollapsed := Value;
  if FCollapsed then
  begin
    for i := 0 to FItems.Count-1 do
    begin
      Items[i].FRow := -1;
    end;
  end;
end;

function TArcIWTreeItem.URLIcon(Attributes : string): string;
begin
  Result := FIcon.RenderTag(TArcIWTreeItems(Collection).FContent.WebApplication.InternalURLBase, Attributes);
end;

function TArcIWTreeItem.URLMinusIcon(Attributes : string): String;
begin
  Result := FIconMinus.RenderTag(TArcIWTreeItems(Collection).FContent.WebApplication.InternalURLBase, Attributes);
end;

function TArcIWTreeItem.URLPlusIcon(Attributes : string): String;
begin
  Result := FIconPlus.RenderTag(TArcIWTreeItems(Collection).FContent.WebApplication.InternalURLBase, Attributes);
end;

initialization
  TIWServer.AddInternalFile('IW_GFX_ARCTREEPLUS', '/gfx/ArcTreePlus.png');
  TIWServer.AddInternalFile('IW_GFX_ARCTREEMINUS', '/gfx/ArcTreeMinus.png');

end.
