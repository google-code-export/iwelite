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

unit ArcIWStringContainer;

interface

uses
  SysUtils, Classes;

type
  TArcStringItem = class(TCollectionItem)
  private
    FStringID: string;
    FValue: WideString;
    procedure SetStringID(const Value: string);
  published
    property StringID : string read FStringID write SetStringID;
    property Value : WideString read FValue write FValue;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  end;

  TArcIWStringContainer = class;

  TArcStringCollection = class(TCollection)
  private
    FOwner : TArcIWStringContainer;
    function GetStrings(idx: integer): TArcStringItem;
    procedure SetStrings(idx: integer; const Value: TArcStringItem);
  public
    constructor Create(AOwner : TArcIWStringContainer); virtual;
    destructor Destroy; override;

    function Add: TArcStringItem; virtual;
    function FindItemID(ID: Integer): TArcStringItem; virtual;
    function Insert(Index: Integer): TArcStringItem; virtual;
    property Strings[idx : integer] : TArcStringItem read GetStrings write SetStrings; default;
  end;

  TArcIWStringContainer = class(TComponent)
  private
    FStrings: TArcStringCollection;
  protected
    FStringIndex : TStringList;
    procedure RebuildIndex; virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function StringByID(ID : string) : WideString;
  published
    property Strings : TArcStringCollection read FStrings write FStrings;
  end;

implementation

{ TArcIWStringContainer }

constructor TArcIWStringContainer.Create(AOwner: TComponent);
begin
  inherited;
  FStringIndex := TStringList.Create;
  FStrings := TArcStringCollection.Create(Self);
  FStringIndex.Duplicates := dupIgnore;
  FStringIndex.Sorted := True;
end;

destructor TArcIWStringContainer.Destroy;
begin

  inherited;
end;

procedure TArcIWStringContainer.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    RebuildIndex;
end;

procedure TArcIWStringContainer.RebuildIndex;
var
  i : integer;
begin
  FStringIndex.Clear;
  for i := 0 to FStrings.Count-1 do
  begin
    FStringIndex.AddObject(FStrings[i].StringID,TObject(i));
  end;
end;

function TArcIWStringContainer.StringByID(ID: string): WideString;
begin
  Result := FStrings[Integer(FStringIndex.Objects[FStringIndex.IndexOf(ID)])].Value;
end;

{ TArcStringCollection }

function TArcStringCollection.Add: TArcStringItem;
begin
  Result := TArcStringItem(inherited Add);
end;

constructor TArcStringCollection.Create(AOwner: TArcIWStringContainer);
begin
  inherited Create(TArcStringItem);
  FOwner := AOwner;
end;

destructor TArcStringCollection.Destroy;
begin

  inherited;
end;

function TArcStringCollection.FindItemID(ID: Integer): TArcStringItem;
begin
  Result := TArcStringItem(inherited FindItemID(ID));
end;

function TArcStringCollection.GetStrings(idx: integer): TArcStringItem;
begin
  Result := TArcStringItem(Items[idx]);
end;

function TArcStringCollection.Insert(Index: Integer): TArcStringItem;
begin
  Result := TArcStringItem(inherited Insert(Index));
end;

procedure TArcStringCollection.SetStrings(idx: integer;
  const Value: TArcStringItem);
begin
  Items[idx] := Value;
end;

{ TArcStringItem }

procedure TArcStringItem.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TArcStringItem(Dest).FStringID := FStringID;
  TArcStringItem(Dest).FValue := FValue;
end;

procedure TArcStringItem.SetStringID(const Value: string);
begin
  if (not (csDesigning in TArcStringCollection(Collection).FOwner.ComponentState)) and
     (not (csLoading in TArcStringCollection(Collection).FOwner.ComponentState)) then
    raise Exception.Create('Cannot change string IDs at runtime.');

  FStringID := Value;
end;

end.
 
