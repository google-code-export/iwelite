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

unit ArcIWStringGridContent;

interface

uses SysUtils, Classes, Controls, ArcIWCustomGrid, ArcIWStringGrid, ArcIWGridCommon,
  IWApplication;

type
  TControlProperty = string;
  EContentStateError = class(Exception)
  end;

  TStringDynArray = array of string;
  TCellCache = array of array of string;
  TObjectCache = array of array of TObject;
  TArcIWStringGridContent = class(TArcIWStringGridContentBase)
  private
    FCells: TCellCache;
    FObjects: TObjectCache;
    FWebApplication : TIWApplication;
  protected
    FGrid: TArcIWStringGrid;
    property Objects: TObjectCache read FObjects write FObjects;
    property Cells: TCellCache read FCells write FCells;
    procedure SetGrid(const Value: TArcIWStringGrid); virtual;
    function GetGrid: TArcIWStringGrid; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property GridNoRaise: TArcIWStringGrid read FGrid;
    procedure UpdateNavButtonState; override;
    procedure AssignTo(Dest: TPersistent); override;
    function CopyRow(ColCount, Row: integer): TStringDynArray;
  public
    property WebApplication : TIWApplication read FWebApplication;
    function DoNeedStyleOverride(Sender: TObject): boolean; override;
    procedure GridToCache(Col: integer = -1; Row: integer = -1); virtual;
    procedure CacheToGrid(Col: integer = -1; Row: integer = -1); virtual;
    procedure DoResizeGrid(Sender: TObject; Cols, Rows: integer); override;
    procedure DoOverrideCellStyle(Sender: TObject; const col: integer; const Row: Integer; Style: TArcGridStyle); override;
    procedure DoAssignGrid(Sender: TObject); override;
    procedure DoAssignSession(Session : TIWApplication); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active;
    property Grid: TArcIWStringGrid read GetGrid write SetGrid stored False;
  end;

implementation

{ TArcIWStringGridContent }

procedure TArcIWStringGridContent.AssignTo(Dest: TPersistent);
begin
  inherited;
  //TArcIWStringGridContentBase(Dest).
end;

procedure TArcIWStringGridContent.CacheToGrid(Col: integer = -1; Row: integer = -1);
  procedure CopyRow(Row: integer);
  var
    x: integer;
  begin
    if Col < 0 then
      for x := 0 to Grid.Columns.Count - 1 do
        Grid.Cells[x, Row] := Cells[x, Row]
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

function TArcIWStringGridContent.CopyRow(ColCount, Row: integer): TStringDynArray;
var
  x: integer;
begin
  SetLength(Result, ColCount);
  for x := 0 to ColCount - 1 do
    Result[x] := Cells[x, Row];
end;

constructor TArcIWStringGridContent.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TArcIWStringGridContent.Destroy;
begin

  inherited;
end;

procedure TArcIWStringGridContent.DoAssignGrid(Sender: TObject);
begin
  if (Sender <> nil) and (not (Sender is TArcIWStringGrid)) then
    raise Exception.Create('Content providers may only be used with a TArcIWStringGrid or descendant.');
  FGrid := TArcIWStringGrid(Sender);
  if Assigned(FGrid) then
    FGrid.FreeNotification(Self);
end;

procedure TArcIWStringGridContent.DoAssignSession(Session: TIWApplication);
begin
  FWebApplication := Session;
end;

function TArcIWStringGridContent.DoNeedStyleOverride(
  Sender: TObject): boolean;
begin
  Result := False;
end;

procedure TArcIWStringGridContent.DoOverrideCellStyle(Sender: TObject;
  const col, Row: Integer; Style: TArcGridStyle);
begin

end;

procedure TArcIWStringGridContent.DoResizeGrid(Sender: TObject; Cols, Rows: integer);
var
  i: integer;
begin
  SetLength(FCells, Cols);
  SetLength(FObjects, Cols);
  for i := low(FCells) to High(FCells) do
  begin
    SetLength(FCells[i], Rows);
    SetLength(FObjects[i], Rows);
  end;
end;

function TArcIWStringGridContent.GetGrid: TArcIWStringGrid;
begin
  if (not (csDesigning in ComponentState)) //and
//     (not (csDestroying in ComponentState))
  then
  begin
    if FGrid = nil then
      raise Exception.Create('Content provider has no grid assigned.');
  end;
  Result := FGrid;
end;

procedure TArcIWStringGridContent.GridToCache(Col: integer = -1; Row: integer = -1);
  procedure CopyRow(Row: integer);
  var
    x: integer;
  begin
    if Col < 0 then
      for x := 0 to Grid.Columns.Count - 1 do
        Cells[x, Row] := Grid.Cells[x, Row]
    else
      Cells[Col, Row] := Grid.Cells[Col, Row];
  end;
var
  y: integer;
begin
  DoResizeGrid(Grid, Grid.Columns.Count, Grid.RowCount);
  if Row < 0 then
  begin
    for y := 0 to Grid.RowCount - 1 do
      CopyRow(y);
  end else
    CopyRow(Row);
end;

procedure TArcIWStringGridContent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (AComponent = FGrid) then
      FGrid := nil;
  end;
end;

procedure TArcIWStringGridContent.SetGrid(const Value: TArcIWStringGrid);
begin
  if Value = nil then
  begin
    FGrid.Content := nil;
  end else
    Value.Content := Self
end;

procedure TArcIWStringGridContent.UpdateNavButtonState;
begin
  inherited;
  if Assigned(FGrid) then
  begin
    Grid.CaptionButtons.FirstPage.Enabled := not IsFirstPage;
    Grid.CaptionButtons.PriorPage.Enabled := not IsFirstPage;
    Grid.CaptionButtons.Prior.Enabled := not IsPriorRecord;
    Grid.CaptionButtons.First.Enabled := not IsFirstRecord;
    Grid.CaptionButtons.Next.Enabled := not IsNextRecord;
    Grid.CaptionButtons.NextPage.Enabled := not IsLastPage;
    Grid.CaptionButtons.LastPage.Enabled := not IsLastPage;
    Grid.CaptionButtons.Last.Enabled := not IsLastRecord;
  end;
end;

end.

