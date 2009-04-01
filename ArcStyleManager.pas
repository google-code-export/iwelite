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

unit ArcStyleManager;

interface

{$I IntrawebVersion.inc}

uses
  SysUtils, Classes, Controls, iniFiles, TypInfo, ArcD5Fix, Graphics, IWColor
  {$ifdef DELPHIBDS},Contnrs{$ENDIF};

type
  TArcStyleManager = class;
  TArcStyleItem = class;

  {$ifdef DELPHIBDS}
  TArcObjectList = class(TObjectList);
  {$else}
  TArcObjectList = class(TList);
  {$ENDIF}

  TArcControlItem = class(TCollectionItem)
  private
    FControl: TControl;
    procedure SetControl(const Value: TControl);
  protected
    function GetDisplayName: string; override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Control : TControl read FControl write SetControl;
  end;

  TArcControlCollection = class(TOwnedCollection)
  private
    Component : TArcStyleManager;
    function GetItems(idx: integer): TArcControlItem;
    procedure SetItems(idx: integer; const Value: TArcControlItem);
  public
    procedure NilControl(Control : TControl);
    property Items[idx : integer] : TArcControlItem read GetItems write SetItems; default;
    function Add : TArcControlitem;
    procedure WriteCommonProperties(ini : TIniFile; Section : string);
    procedure ReadProperties(ini : TInifile; Section : string; AddObjects : TArcObjectList);
  end;

  TArcStyleAttachEvent = procedure(ASender : TObject; AStyle : TArcStyleItem; ObjectList : TArcObjectList) of object;

  TArcStyleItem = class(TCollectionItem)
  private
    FControls: TArcControlCollection;
    FStyleName: string;
    FTag: integer;
    FLocal: boolean;
    FOnAttachObjects : TArcStyleAttachEvent;
    procedure SetControls(const Value: TArcControlCollection);
  protected
    function GetDisplayName: string; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure WriteToIni(ini : TIniFile);
    procedure ReadFromIni(ini : TIniFile);
  published
    property StyleName : string read FStyleName write FStyleName;
    property Local : boolean read FLocal write FLocal default false;
    property Tag : integer read FTag write FTag default 0;
    property Controls : TArcControlCollection read FControls write SetControls;
    property OnAttachObjects : TArcStyleAttachEvent read FOnAttachObjects write FOnAttachObjects;
  end;

  TArcStyleCollection = class(TOwnedCollection)
  private
    Component : TArcStyleManager;
    function GetItems(idx: integer): TArcStyleItem;
    procedure SetItems(idx: integer; const Value: TArcStyleItem);
  public
    procedure NilControl(Control : TControl);
    property Items[idx : integer] : TArcStyleItem read GetItems write SetItems; default;
    function Add : TArcStyleItem;
    procedure SaveStyle(Filename : string);
    procedure LoadStyle(Filename : string);
  end;

  TArcStyleManager = class(TComponent)
  private
    FStyles: TArcStyleCollection;
    FStyleFile: TFilename;
    FAutoLoad: boolean;
    procedure SetStyles(const Value: TArcStyleCollection);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    procedure LoadStyles(Filename : TFilename = '');
    procedure SaveStyles(Filename : TFilename = '');

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoLoad : boolean read FAutoLoad write FAutoLoad default true;
    property Styles : TArcStyleCollection read FStyles write SetStyles;
    property StyleFile : TFilename read FStyleFile write FStyleFile;
  end;

  procedure AddComponentsOfType(Component : TComponent; List : TArcObjectList; Types : array of TClass);

implementation

procedure AddComponentsOfType(Component : TComponent; List : TArcObjectList;
  Types : array of TClass);
var
  i, j : integer;
begin
  for i := 0 to Component.ComponentCount-1 do
  begin
    for j := Low(Types) to High(Types) do
      if Component.Components[i] is Types[j] then
      begin
        List.Add(Component.Components[i]);
        break;
      end;
  end;
end;

{ TArcControlCollection }

function TArcControlCollection.GetItems(idx: integer): TArcControlItem;
begin
  result := TArcControlItem(inherited items[idx]);
end;

procedure TArcControlCollection.SetItems(idx: integer;
  const Value: TArcControlItem);
begin
  inherited Items[idx] := Value;
end;

function TArcControlCollection.Add: TArcControlitem;
begin
  Result := TArcControlItem(inherited Add);
end;

procedure TArcControlCollection.NilControl(Control: TControl);
var
  i : integer;
begin
  for i := 0 to Count-1 do
    if Items[i].FControl = Control then
      Items[i].FControl := nil;
end;

procedure TArcControlCollection.WriteCommonProperties(ini: TIniFile; Section: string);
  procedure EnumerateProperties(obj : TObject; Path : string);
  var
    i64 : Int64;
    ppl : PPropList;
    iProp, iPropCnt : integer;
    s, sName : string;
    o : TObject;
  begin
    if (obj = nil) then
      exit;
    if obj is TStrings then
    begin
      ini.WriteString(Section,Path+'Text',TStrings(obj).CommaText);
      exit;
    end;

    iPropCnt := GetPropList(obj,ppl);
    try
      for iProp := 0 to iPropCnt-1 do
      begin
        sName := ppl[iProp]^.Name;
        if (sName = 'Caption') or (sName = 'Text') or (sName = 'Name') then
          continue;
        case ppl[iProp].PropType^^.Kind of
          {$IFNDEF VER130}
          tkInteger, tkChar:
            ini.WriteInteger(Section,Path+sName,GetOrdProp(obj,ppl[iProp]));
          tkEnumeration:
            ini.WriteString(Section,Path+sName,GetEnumProp(obj,ppl[iProp]));
          tkSet:
            ini.WriteString(Section,Path+sName,GetSetProp(obj,ppl[iProp],True));
          {$ELSE}
          tkInteger, tkChar, tkEnumeration, tkSet:
            ini.WriteInteger(Section,Path+sName,GetOrdProp(obj,ppl[iProp]));
          {$ENDIF}
          tkFloat:
            ini.WriteFloat(Section,Path+sName,GetFloatProp(obj,ppl[iProp]));
          tkWChar, tkWString,
          tkLString, tkString:
            ini.WriteString(Section,Path+sName,GetStrProp(obj,ppl[iProp]));
          {$IFNDEF VER130}
          tkInt64:
            begin
              i64 := GetInt64Prop(obj,ppl[iProp]);
              ini.WriteInteger(Section,Path+sName,i64);
            end;
          {$ENDIF}
          tkVariant:
            begin
              s := GetVariantProp(obj,ppl[iProp]);
              ini.WriteString(Section,Path+sName,s);
            end;
          tkClass:
            begin
              o := GetObjectProp(obj,ppl[iProp]);
              if not (obj is TWinControl) then
                EnumerateProperties(o,Path+sName+'.');
            end;
          tkRecord, tkMethod:
            continue;
          else
            continue;
        end;
      end;
    finally
      FreeMem(ppl);
    end;
  end;
var
  ctrl : TControl;
begin
  if Count > 0 then
  begin
    ctrl := Items[0].FControl;
    if ctrl <> nil then
    begin
      EnumerateProperties(ctrl, '');
    end;
  end;
  // TODO: Write out only those properties that are common among all controls.
end;

procedure TArcControlCollection.ReadProperties(ini: TInifile; Section: string; AddObjects : TArcObjectList);
var
  lProcessed : TList;
  function EnumerateProperties(obj : TObject; Path : string) : string;
  var
    i64 : Int64;
    ppl : PPropList;
    iProp, iPropCnt : integer;
    s, sName : string;
    o : TObject;
  begin
    if (obj = nil) then
      exit;

    lProcessed.Add(obj);

    if obj is TStrings then
    begin
      TStrings(obj).CommaText := ini.ReadString(Section,Path+'Text',TStrings(obj).CommaText);
      exit;
    end;

    iPropCnt := GetPropList(obj,ppl);
    try
      for iProp := 0 to iPropCnt-1 do
      begin
        sName := ppl[iProp]^.Name;
        try
          if ppl[iProp]^.PropType^^.Name = 'TColor' then
          begin
            s := ini.ReadString(Section,Path+sName,ColorToString(GetOrdProp(obj,ppl[iProp])));
            if s <> '' then
              SetOrdProp(obj,ppl[iProp],StringToColor(s));
            continue;
          end;

          if ppl[iProp]^.PropType^^.Name = 'TIWColor' then
          begin
            s := ini.ReadString(Section,Path+sName,IWColorToString(GetOrdProp(obj,ppl[iProp])));
            if s <> '' then
              SetOrdProp(obj,ppl[iProp],StringToIWColor(s));
            continue;
          end;

          case ppl[iProp].PropType^^.Kind of
            {$IFNDEF VER130}
            tkInteger, tkChar:
              SetOrdProp(obj,ppl[iProp],ini.ReadInteger(Section,Path+sName,GetOrdProp(obj,ppl[iProp])));
            tkEnumeration:
              SetEnumProp(obj,ppl[iProp],ini.ReadString(Section,Path+sName,GetEnumProp(obj,ppl[iProp])));
            tkSet:
              SetSetProp(obj,ppl[iProp],ini.ReadString(Section,Path+sName,GetSetProp(obj,ppl[iProp],True)));
            {$ELSE}
            tkInteger, tkChar, tkEnumeration, tkSet:
              SetOrdProp(obj,ppl[iProp],ini.ReadInteger(Section,Path+sName,GetOrdProp(obj,ppl[iProp])));
            {$ENDIF}
            tkFloat:
              SetFloatProp(obj,ppl[iProp],ini.ReadFloat(Section,Path+sName,GetFloatProp(obj,ppl[iProp])));
            tkWChar, tkWString,
            tkLString, tkString:
              SetStrProp(obj,ppl[iProp],ini.ReadString(Section,Path+sName,GetStrProp(obj,ppl[iProp])));
            {$IFNDEF VER130}
            tkInt64:
              begin
                i64 := GetInt64Prop(obj,ppl[iProp]);
                SetInt64Prop(obj,ppl[iProp],ini.ReadInteger(Section,Path+sName,i64));
              end;
            {$ENDIF}
            tkVariant:
              begin
                s := GetVariantProp(obj,ppl[iProp]);
                SetStrProp(obj,ppl[iProp],ini.ReadString(Section,Path+sName,s));
              end;
            tkClass:
              begin
                o := GetObjectProp(obj,ppl[iProp]);
                if (lProcessed.IndexOf(o) < 0) and (not (o is TWinControl)) then
                  EnumerateProperties(o,Path+sName+'.');
              end;
            tkRecord, tkMethod:
              continue;
            else
              continue;
          end;
        except
        end;
      end;
    finally
      FreeMem(ppl);
    end;
  end;
var
  ctrl : TObject;
  i : integer;
begin
  lProcessed := TList.Create;
  try
    for i := 0 to Count -1 do
    begin
      ctrl := Items[i].FControl;
      if ctrl <> nil then
      begin
        EnumerateProperties(ctrl, '');
      end;
    end;
    for i := 0 to AddObjects.Count -1 do
    begin
      ctrl := AddObjects[i];
      if ctrl <> nil then
      begin
        EnumerateProperties(ctrl, '');
      end;
    end;
  finally
    lProcessed.Free;
  end;
end;

{ TArcStyleCollection }

function TArcStyleCollection.GetItems(idx: integer): TArcStyleItem;
begin
  Result := TArcStyleItem(inherited Items[idx]);
end;

function TArcStyleCollection.Add: TArcStyleItem;
begin
  Result := TArcStyleItem(inherited Add);
end;

procedure TArcStyleCollection.SetItems(idx: integer;
  const Value: TArcStyleItem);
begin
  inherited Items[idx] := Value;
end;

procedure TArcStyleCollection.NilControl(Control: TControl);
var
  i : integer;
begin
  for i := 0 to Count-1 do
    Items[i].Controls.NilControl(Control);
end;

procedure TArcStyleCollection.SaveStyle(Filename: string);
var
  i : integer;
  ini : TIniFile;
begin
  ini := TIniFile.Create(Filename);
  try
    for i := 0 to Count-1 do
      Items[i].WriteToIni(ini);
  finally
    ini.Free;
  end;
end;

procedure TArcStyleCollection.LoadStyle(Filename: string);
var
  i : integer;
  ini : TIniFile;
begin
  ini := TIniFile.Create(Filename);
  try
    for i := 0 to Count-1 do
      Items[i].ReadFromIni(ini);
  finally
    ini.Free;
  end;
end;

{ TArcStyleManager }

constructor TArcStyleManager.Create(AOwner: TComponent);
begin
  inherited;
  FAutoLoad := True;
  FStyles := TArcStyleCollection.Create(Self,TArcStyleItem);
  FStyles.Component := Self;
end;

destructor TArcStyleManager.Destroy;
begin
  FStyles.Free;
  inherited;
end;

procedure TArcStyleManager.Loaded;
begin
  inherited;
  if FAutoLoad and (not (csDesigning in ComponentState)) then
  begin
    LoadStyles;
  end;
end;

procedure TArcStyleManager.LoadStyles(Filename: TFilename = '');
begin
  if Filename = '' then
    Filename := FStyleFile;
  if Filename = '' then
    raise EPropertyError.Create('There has been no filename specified.');
  Styles.LoadStyle(Filename);
end;

procedure TArcStyleManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent is TControl) then
    Styles.NilControl(TControl(AComponent));
end;

procedure TArcStyleManager.SaveStyles(Filename: TFilename = '');
begin
  if Filename = '' then
    Filename := FStyleFile;
  if Filename = '' then
    raise EPropertyError.Create('There has been no filename specified.');
  Styles.SaveStyle(Filename);
end;

procedure TArcStyleManager.SetStyles(const Value: TArcStyleCollection);
begin
  FStyles.Assign(Value);
end;

{ TArcStyleItem }

constructor TArcStyleItem.Create(Collection: TCollection);
begin
  inherited;
  FControls := TArcControlCollection.Create(Self,TArcControlItem);
  FControls.Component := TArcStyleCollection(Collection).Component;
end;

destructor TArcStyleItem.Destroy;
begin
  FControls.Free;
  inherited;
end;

procedure TArcStyleItem.WriteToIni(ini: TIniFile);
var
  sSection : string;
begin
  if not FLocal then
    sSection := FStyleName
  else
    sSection := TArcStyleCollection(Collection).Component.Name+'.'+FStyleName;
  FControls.WriteCommonProperties(ini, sSection);
end;

procedure TArcStyleItem.ReadFromIni(ini: TIniFile);
var
  sSection : string;
  lst : TArcObjectList;
begin
  if not FLocal then
    sSection := FStyleName
  else
    sSection := TArcStyleCollection(Collection).Component.Name+'.'+FStyleName;
  lst := TArcObjectList.Create;
  try
    if Assigned(FOnAttachObjects) then
      FOnAttachObjects(TArcStyleCollection(Collection).Component,Self,lst);
    FControls.ReadProperties(ini, sSection, lst);
  finally
    lst.Free;
  end;
end;

function TArcStyleItem.GetDisplayName: string;
begin
  if FStyleName <> '' then
    Result := FStyleName
  else
    Result := '(unknown)';
end;

procedure TArcStyleItem.SetControls(const Value: TArcControlCollection);
begin
  FControls.Assign(Value);
end;

procedure TArcStyleItem.AssignTo(Dest: TPersistent);
begin
  if not (Dest is TArcStyleItem) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a TArcStyleItem.');

  TArcStyleItem(Dest).FStyleName := FStyleName;
  TArcStyleItem(Dest).FLocal := FLocal;
  TArcStyleItem(Dest).FTag := FTag;
  TArcStyleItem(Dest).FControls.Assign(FControls);
  TArcStyleItem(Dest).FOnAttachObjects := FOnAttachObjects;
end;

{ TArcControlItem }

procedure TArcControlItem.AssignTo(Dest: TPersistent);
begin
  if not (Dest is TArcControlItem) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a TArcControlItem.');
  TArcControlItem(Dest).FControl := FControl;
end;

function TArcControlItem.GetDisplayName: string;
begin
  if FControl <> nil then
    Result := FControl.Name
  else
    Result := '(none)';
end;

procedure TArcControlItem.SetControl(const Value: TControl);
begin
  if FControl <> nil then
    FControl.RemoveFreeNotification(TArcControlCollection(Collection).Component);
  if Value <> nil then
    Value.FreeNotification(TArcControlCollection(Collection).Component);
  FControl := Value;
end;

end.
