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

unit ArcIWLanguageTypes;

interface

uses SysUtils, Classes, Controls, IWAppForm, IWControl, ArcIWTranslatorBase,
  TypInfo, IWFileReference, Forms;

type
  TTransTableItem = class(TCollectionItem)
  private
    FLanguage: string;
    FLanguageCodes: TStringList;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property HTTPLanguageCodes : TStringList read FLanguageCodes write FLanguageCodes;
    property Language : string read FLanguage write FLanguage;
  end;

  TTranslationTable = class(TCollection)
  private
    function GetItems(idx: integer): TTransTableItem;
    procedure SetItems(idx: integer; const Value: TTransTableItem);
  public
    constructor Create; virtual;
    property Items[idx : integer] : TTransTableItem read GetItems write SetItems; default;
    function LanguageLookup(Code : string) : string;
  end;

  TTransLanguageItem = class(TCollectionItem)
  private
    FText: string;
    FLanguage: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
  published
    property Language : string read FLanguage write FLanguage;
    property Text     : string read FText     write FText;
  end;

  TTranslations = class(TCollection)
  private
    function GetItems(idx: integer): TTransLanguageItem;
    procedure SetItems(idx: integer; const Value: TTransLanguageItem);
  protected
  public
    constructor Create; virtual;
    property Items[idx : integer] : TTransLanguageItem read GetItems write SetItems; default;
    function IndexOf(str : string) : integer;
    function TextByLanguage(Language : string) : string;
  end;

  TLanguageItem = class(TCollectionItem)
  private
    FPropName: string;
    FControl: TComponent;
    FTranslations: TTranslations;
    FControlName: string;
    procedure SetControlName(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Control      : TComponent    read FControl      write FControl;
    property ControlName  : string        read FControlName  write SetControlName;
    property PropName     : string        read FPropName     write FPropName;
    property Translations : TTranslations read FTranslations write FTranslations;
  end;

  TControlTranslations = class(TCollection)
  private
    FForm: TIWAppForm;
    function GetItems(idx: integer): TLanguageItem;
    procedure SetItems(idx: integer; const Value: TLanguageItem);
  protected
    property Form : TIWAppForm read FForm write FForm;
  public
    constructor Create(aForm : TIWAppForm); virtual;
    property Items[idx : integer] : TLanguageItem read GetItems write SetItems; default;
    function HasControl(AControlName : string) : boolean;
    function ItemByName(AControlName : string) : TLanguageItem;
    procedure BuildComponent(AComponent : TComponent; ADefaultLanguage : string);
    procedure ImportFromXML(xml : string);
    function ExportToXML : string;
  end;

function Prop_AvoidName(aName : string) : boolean;
function Prop_AvoidClassName(aName : string) : boolean;
function Prop_AcceptedClassType(obj : TObject; pi : PPropInfo) : boolean;
function Prop_LookupPropertyValue(AComponent : TObject; PropName : string) : string;
procedure Prop_SetPropertyValue(AComponent : TObject; PropName, Value : string);

implementation

{$IFDEF VER130}
function Supports(const Instance: TObject; const IID: TGUID): Boolean; overload;
var
  LUnknown: IUnknown;
  Intf : IUnknown;
begin
  Result := (Instance <> nil) and
            ((Instance.GetInterface(IUnknown, LUnknown) and Supports(LUnknown, IID, Intf)) or
             Instance.GetInterface(IID, Intf));
end;

function _GetPropList5(TypeInfo: PTypeInfo; out PropList: PPropList): Integer;
begin
  Result := GetTypeData(TypeInfo)^.PropCount;
  if Result > 0 then
  begin
    GetMem(PropList, Result * SizeOf(Pointer));
    GetPropInfos(TypeInfo, PropList);
  end;
end;

function GetPropList5(AObject: TObject; out PropList: PPropList): Integer;
begin
  Result := _GetPropList5(PTypeInfo(AObject.ClassInfo), PropList);
end;
{$ENDIF}


function Prop_AvoidName(aName : string) : boolean;
begin
  result := (aName = 'HotKey') or (aName = 'HelpKeyword') or
            (aName = 'Name') or (aName = 'FriendlyName') or
            (aName = 'StringID') or (aName = 'DataField') or
            (aName = 'LookupDataField') or (aName = 'HiddenFields');
end;
function Prop_AvoidClassName(aName : string) : boolean;
begin
  result := (aName = 'ExtraHeader') or (aName = 'JavaScript') or
            (aName = 'ExtraTagParams') or (aName = 'Font');
end;

function Prop_AcceptedClassType(obj : TObject; pi : PPropInfo) : boolean;
var
  o : TObject;
begin
  result := (pi.PropType^^.Kind = tkClass);
  if result then
  begin
    try
      o := GetObjectProp(obj,pi.Name);
      Result := (o is TStrings) or
                (o is TCollection) or
                (o is TPersistent);
      if Result then
        Result := (not (o is TIWFileReference)) and (not (o is TControlScrollBar));
    except
      result := False;
    end;
  end;
end;

function Prop_LookupPropertyValue(AComponent : TObject; PropName : string) : string;
var
  i, iPos, idx : integer;
  o : TObject;
  s,sProp : string;
begin
  i := Pos('[',PropName);
  iPos := Pos('.',PropName);
  if ((i = 0) and (iPos > 0)) or ((i>0) and (iPos > 0) and (i > iPos)) then
  begin
    // Handle Persistent Properties
    sProp := PropName;
    o := AComponent;
    while iPos > 0 do
    begin
      s := Copy(sProp,1,iPos-1);
      o := GetObjectProp(o,s);
      sProp := Copy(sProp,iPos+1,High(Integer));
      iPos := Pos('.',sProp);
    end;
    Result := Prop_LookupPropertyValue(o,sProp);
  end else
  begin
    iPos := Pos('[',PropName);
    if iPos > 0 then
    begin
      // Handle Collection Indexes
      sProp := Copy(PropName, 1, iPos-1);
      o := GetObjectProp(AComponent,sProp);

      idx := StrToInt(Copy(PropName,iPos+1,Pos(']',PropName)-iPos-1));
      sProp := Copy(PropName, Pos(']',PropName)+2,High(Integer));

      Result := Prop_LookupPropertyValue(TCollection(o).Items[idx],sProp);
    end else
    begin
      // Handle String Properties
      if GetPropInfo(AComponent,PropName)^.PropType^^.Kind = tkClass then
      begin
        o := GetObjectProp(AComponent,PropName);
        if o is TStrings then
        	Result := TStrings(o).Text
        else
          Result := GetStrProp(AComponent,PropName);
      end else
      	Result := GetStrProp(AComponent,PropName);
    end;
  end;
end;

procedure Prop_SetPropertyValue(AComponent : TObject; PropName, Value : string);
var
  i, iPos, idx : integer;
  o : TObject;
  s,sProp : string;
begin
  i := Pos('[',PropName);
  iPos := Pos('.',PropName);
  if ((i = 0) and (iPos > 0)) or ((i>0) and (iPos > 0) and (i > iPos)) then
  begin
    // Handle Persistent Properties
    sProp := PropName;
    o := AComponent;
    while iPos > 0 do
    begin
      s := Copy(sProp,1,iPos-1);
      o := GetObjectProp(o,s);
      sProp := Copy(sProp,iPos+1,High(Integer));
      iPos := Pos('.',sProp);
    end;
    Prop_SetPropertyValue(o,sProp, Value);
  end else
  begin
    iPos := Pos('[',PropName);
    if iPos > 0 then
    begin
      // Handle Collection Indexes
      sProp := Copy(PropName, 1, iPos-1);
      o := GetObjectProp(AComponent,sProp);

      idx := StrToInt(Copy(PropName,iPos+1,Pos(']',PropName)-iPos-1));
      sProp := Copy(PropName, Pos(']',PropName)+2,High(Integer));

      Prop_SetPropertyValue(TCollection(o).Items[idx],sProp, Value);
    end else
    begin
      // Handle String Properties
      if GetPropInfo(AComponent,PropName)^.PropType^^.Kind = tkClass then
      begin
        o := GetObjectProp(AComponent,PropName);
        if o is TStrings then
          TStrings(o).Text := Value;
      end else
        SetStrProp(AComponent,PropName, Value);
    end;
  end;
end;

{ TLanguageItem }

procedure TLanguageItem.AssignTo(Dest: TPersistent);
begin
  if not (Dest is TLanguageItem) then raise Exception.Create('Cannot assign a TLanguageItem to a '+Dest.ClassName);
  TLanguageItem(Dest).PropName := FPropName;
  TLanguageItem(Dest).Control  := FControl;
  TLanguageItem(Dest).ControlName  := FControlName;
  TLanguageItem(Dest).Translations.Assign(FTranslations);

end;

constructor TLanguageItem.Create(Collection: TCollection);
begin
  inherited;
  FTranslations := TTranslations.Create;
end;

destructor TLanguageItem.Destroy;
begin
  FTranslations.Free;
  inherited;
end;

procedure TLanguageItem.SetControlName(const Value: string);
var
  i : integer;
begin
  FControlName := Value;
  if not Assigned(TControlTranslations(Collection).Form) then exit;
  for i := 0 to TControlTranslations(Collection).Form.ControlCount-1 do
  begin
    if TControlTranslations(Collection).Form.Controls[i].Name = Value then
    begin
      FControl := TControlTranslations(Collection).Form.Controls[i];
      break;
    end;
  end;
end;

{ TTranslations }

constructor TTranslations.Create;
begin
  inherited Create(TTransLanguageItem);
end;

function TTranslations.GetItems(idx: integer): TTransLanguageItem;
begin
  result := TTransLanguageItem(inherited Items[idx]);
end;

function TTranslations.IndexOf(str: string): integer;
var
  i : integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
  begin
    if Items[i].Language = str then
    begin
      Result := i;
      break;
    end;
  end;
end;

procedure TTranslations.SetItems(idx: integer;
  const Value: TTransLanguageItem);
begin
  inherited Items[idx] := Value;
end;

function TTranslations.TextByLanguage(Language: string): string;
var
  i : integer;
begin
  Result := '';
  for i := 0 to Self.Count -1 do
    if (Items[i].Language = Language) then
    begin
      Result := Items[i].Text;
      break;
    end;
end;

{ TControlTranslations }

procedure TControlTranslations.BuildComponent(AComponent: TComponent;
  ADefaultLanguage: string);
  procedure AddClassProperties(obj : TObject; pi : PPropInfo; list : TStrings; Prefix : string);
    procedure WriteStringProps(obj : TObject);
    begin
      list.Add(Prefix+pi.Name);
    end;
    procedure WriteCollection(obj : TObject;PropName : string);
    var
      i, y, iCnt : integer;
      item : TCollectionItem;
      ppl : PPropList;
      sPrefix : string;
    begin
      for y := 0 to TCollection(obj).Count-1 do
      begin
        item := TCollection(obj).Items[y];
        sPrefix := Prefix+PropName+'['+IntToStr(y)+'].';
        {$IFNDEF VER130}
        iCnt := GetPropList(item,ppl);
        {$ELSE}
        iCnt := GetPropList5(item,ppl);
        {$ENDIF}
        for i := 0 to iCnt-1 do
        begin
          if ppl[i].PropType^^.Kind in [tkString, tkLString, tkWString] then
          begin
            if not Prop_AvoidName(ppl[i].Name) then
              list.Add(sPrefix+ppl[i].Name);
          end else
          begin
            if Prop_AcceptedClassType(obj, ppl[i]) then
               begin
                  if not Prop_AvoidClassName(ppl[i].Name) then
                  begin
                    AddClassProperties(item, ppl[i], list, sPrefix)
                  end;
               end ;
          end;
        end;
      end;
    end;
    procedure WritePersistent(obj : TObject;PropName : string);
    var
      i, iCnt : integer;
      ppl : PPropList;
      sPrefix : string;
    begin
      sPrefix := Prefix+PropName+'.';

      {$IFNDEF VER130}
      iCnt := GetPropList(obj,ppl);
      {$ELSE}
      iCnt := GetPropList5(obj,ppl);
      {$ENDIF}
      for i := 0 to iCnt-1 do
      begin
        if ppl[i].PropType^^.Kind in [tkString, tkLString, tkWString] then
        begin
          if not Prop_AvoidName(ppl[i].Name) then
            list.Add(sPrefix+ppl[i].Name);
        end else
        begin
          if Prop_AcceptedClassType(obj, ppl[i]) then
          begin
             if not Prop_AvoidClassName(ppl[i].Name) then
               AddClassProperties(obj, ppl[i], list, sPrefix)
          end;
        end;
      end;
    end;
  var
    o : TObject;
  begin
    o := GetObjectProp(obj,pi.Name);
    if o is TStrings then
      WriteStringProps(o)
    else if o is TCollection then
      WriteCollection(o,pi.Name)
    else if o is TPersistent then
      WritePersistent(o,pi.Name); // Persistent must be handled last.
  end;
var
  sl : TStringList;
  ppl : PPropList;
  i, iCount : integer;
  li : TLanguageItem;
  ti : TTransLanguageItem;
  s : string;
begin
  sl := TStringList.Create;
  try
    {$IFNDEF VER130}
    iCount := GetPropList(AComponent,ppl);
    {$ELSE}
    iCount := GetPropList5(AComponent,ppl);
    {$ENDIF}
    for i := 0 to iCount-1 do
    begin
      if ppl[i].PropType^^.Kind in [tkString, tkLString, tkWString] then
      begin
        if not Prop_AvoidName(ppl[i].Name) then
          sl.Add(ppl[i].Name);
      end else
      begin
        if Prop_AcceptedClassType(AComponent, ppl[i]) then
        begin
          if not Prop_AvoidClassName(ppl[i].Name) then
            AddClassProperties(AComponent, ppl[i], sl,'')
        end ;//else showmessage(ppl[i].PropType^^.Name);
      end;
    end;

    for i := 0 to sl.Count-1 do
    begin
      li := TLanguageItem(Add);
      li.ControlName := AComponent.Name;
      li.Control := AComponent;
      li.PropName := sl[i];
      ti := TTransLanguageItem(li.Translations.Add);
      ti.Language := ADefaultLanguage;
      s := Prop_LookupPropertyValue(AComponent, sl[i]);
      ti.Text := s;
    end;
  finally
    sl.Free;
  end;
end;

constructor TControlTranslations.Create(aForm : TIWAppForm);
begin
  inherited Create(TLanguageItem);
  FForm := aForm; 
end;

function TControlTranslations.ExportToXML: string;
begin
// Future Functionality
end;

function TControlTranslations.GetItems(idx: integer): TLanguageItem;
begin
  Result := TLanguageItem(inherited Items[idx]);
end;

function TControlTranslations.HasControl(AControlName: string): boolean;
var
  li : TLanguageItem;
begin
  li := ItemByName(AControlName);
  Result := li <> nil;
end;

procedure TControlTranslations.ImportFromXML(xml: string);
begin
// Future Functionality
end;

function TControlTranslations.ItemByName(
  AControlName: string): TLanguageItem;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
  begin
    if Uppercase(Items[i].ControlName) = Uppercase(AControlName) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

procedure TControlTranslations.SetItems(idx: integer;
  const Value: TLanguageItem);
begin
  Items[idx] := Value;
end;

{ TTranslationTable }

constructor TTranslationTable.Create;
begin
  inherited Create(TTransTableItem);
end;

function TTranslationTable.GetItems(idx: integer): TTransTableItem;
begin
  result := TTransTableItem(inherited Items[idx]);
end;

function TTranslationTable.LanguageLookup(Code: string): string;
var
  i, idx : integer;
begin
  Result := '';
  for i := 0 to Count-1 do
  begin
    idx := Items[i].FLanguageCodes.IndexOf(Code);
    if idx >= 0 then
    begin
      Result := Items[i].Language;
      break;
    end;
  end;
end;

procedure TTranslationTable.SetItems(idx: integer;
  const Value: TTransTableItem);
begin
  inherited Items[idx] := Value;
end;

{ TTransTableItem }

procedure TTransTableItem.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TTransTableItem(Dest).FLanguage := FLanguage;
  TTransTableItem(Dest).FLanguageCodes.Assign(FLanguageCodes);
end;

constructor TTransTableItem.Create(Collection: TCollection);
begin
  inherited;
  FLanguageCodes := TStringList.Create;
end;

destructor TTransTableItem.Destroy;
begin
  FLanguageCodes.Free;
  inherited;
end;

{ TTransLanguageItem }

procedure TTransLanguageItem.AssignTo(Dest: TPersistent);
begin
  if not (Dest is TTransLanguageItem) then raise Exception.Create('Cannot assign a TTransLanguageItem to a '+Dest.ClassName);
  TTransLanguageItem(Dest).Language := FLanguage;
  TTransLanguageItem(Dest).Text     := FText;
end;

end.



