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

unit ArcIWCheckListBox;

interface

uses
  {$IFDEF VSNET}System.ComponentModel, System.Drawing, {$ENDIF}
  {$IFDEF VSNET}
  IWNetClasses,
  {$ELSE}
  SysUtils, Classes,
  {$IFDEF Linux}
  QTypes, IWCLXComponent, IWCLXClasses,
  {$ELSE}
  IWVCLComponent, IWVCLClasses,
  {$ENDIF}
  {$IFDEF Linux}QControls, {$ELSE}Controls, {$ENDIF}
  {$ENDIF}
  {$IFDEF Linux}QGraphics, {$ELSE}Graphics, {$ENDIF}
  IWTypes, IWHTMLTag, IWControl, IWScriptEvents, IWRenderContext, IWBaseInterfaces,
  IWColor, IWFileReference, IWGLobal, IWCompListbox, IWFont, IWForm, ArcD5Fix;

type
  TInvalidSelectionBehavior = (isSelectNone, isSelectAll);
  TArcIWCheckListBox = class(TIWCustomListbox, IIWMultiInputControl)
  private
    DebugEOL : String;
    FInvalidSelection: TInvalidSelectionBehavior;

    function GetSelectedObjectCommaText: string;
    procedure SetSelectedObjectCommaText(const Value: string);
    function GetSelectedCommaText: string;
    procedure SetSelectedCommaText(const Value: string);
    function GetSelCount: integer;
  protected
    procedure SetRequireSelection(const AValue: Boolean); override;
    procedure InitControl; override;
    procedure SetMultiSelect(const Value: boolean);
    procedure SetValue(const AValue: string); override;
    procedure SetValueByName(const AName, AValue: string); virtual;
    procedure GetInputControlNames(ANames: TStringList); override;
    function IsForThisControl(AName: string): Boolean; override;
  published
  public
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    property SelCount : integer read GetSelCount;
    property SelectedCommaText : string read GetSelectedCommaText write SetSelectedCommaText;
    property SelectedObjectCommaText : string read GetSelectedObjectCommaText write SetSelectedObjectCommaText;
    procedure SelectedToStringList(SelectedItems : TStringList);
    procedure StringListToSelected(SelectedItems : TStringList; ByObject : boolean = false);
  published
    property FriendlyName;
    property ItemIndex;
    property Items;
    property Sorted;
    property MultiSelect: boolean read FMultiSelect write SetMultiSelect;
    property InvalidSelection : TInvalidSelectionBehavior read FInvalidSelection write FInvalidSelection;
  end;

implementation

{ TArcIWCheckListBox }

procedure TArcIWCheckListBox.GetInputControlNames(ANames: TStringList);
var
  i: Integer;
begin
  inherited;
  if FMultiselect then
  begin
    ANames.Add(HTMLName+'_none');
    for i := 0 to Items.Count - 1 do
      ANames.Add(HTMLName+'_'+IntToStr(i));
  end else
  begin
    ANames.Add(HTMLName+'_ITEMS');
  end;
end;

function TArcIWCheckListBox.GetSelCount: integer;
var
  i : integer;
begin
  Result := 0;
  for i := 0 to Items.Count - 1 do
     if Selected[i] then
       inc(Result);
end;

function TArcIWCheckListBox.GetSelectedCommaText: string;
var
  sl : TStringList;
begin
  sl := TStringList.Create;
  try
    SelectedToStringList(sl);
    Result := sl.CommaText;
  finally
    sl.Free;
  end;
end;

function TArcIWCheckListBox.GetSelectedObjectCommaText: string;
var
  sl : TStringList;
  i: Integer;
begin
  Result := '';
  sl := TStringList.Create;
  try
    SelectedToStringList(sl);
    for i := 0 to sl.Count - 1 do
      Result := Result+','+IntToStr(Integer(sl.Objects[i]));
    Delete(Result,1,1);
  finally
    sl.Free;
  end;
end;

procedure TArcIWCheckListBox.InitControl;
begin
  inherited;

  if (GServerController <> nil) and GServerController.DebugHTML then
    DebugEOL := #13#10;
  FMultiSelect := True;
  Width := 121;
  Height := 121;
end;

function TArcIWCheckListBox.IsForThisControl(AName: string): Boolean;
var
  sCtrl : string;
begin
  sCtrl := Copy(AName,1,length(HTMLName)+1);
  result := (sCTRL = HTMLName) or (sCtrl = HTMLName+'_');
end;

function TArcIWCheckListBox.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
  function WidthHeight : string;
  begin
    if UseSize then
      result := 'Width:'+IntToStr(Width)+'px;Height:'+IntToStr(height)+'px;';
  end;
var
  i, iWidth: Integer;
  tag : TIWHTMLTag;
  sBoolNeg1, sBoolItem : string;
begin
  Result := TIWHTMLTag.CreateTag('div');
  if AContext.Browser = brIE then
    iWidth := 2
  else
    iWidth := 1;
  Result.AddStringParam('style',{'position:static;}'border-style:groove;border-width:'+IntToStr(iWidth)+'px;white-space:nowrap;overflow-x:hidden;overflow-y:auto;'+WidthHeight);

  tag := Result.Contents.AddTag('script');
  tag.AddStringParam('language','javascript');
  if FInvalidSelection = isSelectNone then
  begin
    sBoolNeg1 := 'true;';
    sBoolItem := 'false;';
  end else
  begin
    sBoolNeg1 := 'true;';
    sBoolItem := 'true;';
  end;

  if FMultiSelect then
    tag.Contents.AddText(
      'function '+HTMLName+'_clickItem(cmp) {' +DebugEOL+
      //'  var sVal = '''';'+DebugEOL+
      '  IWTop().FindElem('''+HTMLName+'_none'').checked = false;'+DebugEOL+
      '  for (var i=0;i<'+IntToStr(Items.Count)+';i++) {'+DebugEOL+
      '    if (IWTop().FindElem('''+HTMLName+'_''+i).checked) {'+DebugEOL+
      //'      sVal = sVal+i.toString()+'','';'+DebugEOL+
      '      IWTop().FindElem('''+HTMLName+'_''+i).value = ''on'';'+DebugEOL+
      '    } else {'+DebugEOL+
      '      IWTop().FindElem('''+HTMLName+'_''+i).value = ''off'';'+DebugEOL+
      '    }'+DebugEOL+
      '  }'+DebugEOL+
      //'  IWTop().FindElem('''+HTMLName+''').value = sVal;'+DebugEOL+
      '}'+DebugEOL+
      'function '+HTMLName+'_clear() {' +DebugEOL+
      '  IWTop().FindElem('''+HTMLName+'_none'').checked = '+sBoolNeg1+DebugEOL+
      '  for (var i=0;i<'+IntToStr(Items.Count)+';i++) {'+DebugEOL+
      '    IWTop().FindElem('''+HTMLName+'_''+i).checked = '+sBoolItem+DebugEOL+
      '  }'+DebugEOL+
      '  '+HTMLName+'_clickItem();'+DebugEOL+
      '}'+DebugEOL);

  if (FRequireSelection and (FItemIndex < 0)) or (not FRequireSelection)  then
  begin
    tag := Result.Contents.AddTag('input');

    if FMultiSelect then
    begin
      tag.AddStringParam('type', 'CHECKBOX');
      tag.AddStringParam('name',HTMLName+'_none');
      tag.AddStringParam('ID',HTMLName+'_none');
      if (SelCount < 0) and (FInvalidSelection=isSelectNone) then
        tag.AddBoolParam('checked',true);
    end else
    begin
      tag.AddStringParam('type', 'RADIO');
      tag.AddStringParam('name',HTMLName+'_ITEMS');
      tag.AddStringParam('ID',HTMLName+'_none');
      tag.AddStringParam('value',HTMLName+'_none');
      if (FItemIndex < 0) then
        tag.AddBoolParam('checked',true);
    end;

    if FMultiSelect then
      tag.AddStringParam('onClick','return '+HTMLName+'_clear();')
    else
      tag.AddStringParam('onClick','return FindElem('''+HTMLName+''').value = this.ID;');

    tag := Result.Contents.AddTag('span');
    tag.AddStringParam('style',WebFont.FontToStringStyle(AContext.Browser));
    tag.Contents.AddText(FNoSelectionText);

    Result.Contents.AddTag('br');
  end;

  for i := 0 to Items.Count - 1 do
  begin
    tag := Result.Contents.AddTag('input');

    if FMultiSelect then
    begin
      tag.AddStringParam('type', 'CHECKBOX');
      tag.AddStringParam('name',HTMLName+'_'+IntToStr(i));
      tag.AddStringParam('ID',HTMLName+'_'+IntToStr(i));
      if Selected[i] then
        tag.AddBoolParam('checked',True);
    end else
    begin
      tag.AddStringParam('type', 'RADIO');
      tag.AddStringParam('name',HTMLName+'_ITEMS');
      tag.AddStringParam('ID',HTMLName+'_'+IntToStr(i));
      tag.AddStringParam('value',HTMLName+'_'+IntToStr(i));

      if i = ItemIndex then
        tag.AddBoolParam('checked',True);
    end;

    if FMultiSelect then
      tag.AddStringParam('onClick','return '+HTMLName+'_clickItem(this);')
    else
      tag.AddStringParam('onClick','return FindElem('''+HTMLName+''').value = this.ID;');

    tag := Result.Contents.AddTag('span');
    tag.AddStringParam('style',Font.FontToStringStyle(AContext.Browser));

    if FItemsHaveValues then
      tag.Contents.AddText(Items.Names[i])
    else
      tag.Contents.AddText(Items[i]);

    Result.Contents.AddTag('br');
  end;
  
end;

procedure TArcIWCheckListBox.SelectedToStringList(SelectedItems: TStringList);
var
  i: Integer;
begin
  for i := 0 to Items.Count - 1 do
  begin
    if Selected[i] then
      SelectedItems.AddObject(Items[i],Items.Objects[i]);
  end;
end;

procedure TArcIWCheckListBox.SetMultiSelect(const Value: boolean);
begin
  if FMultiselect <> Value then
  begin
    FMultiSelect := Value;
    ResetSelection;
    Invalidate;
  end;
end;

procedure TArcIWCheckListBox.SetRequireSelection(const AValue: Boolean);
begin
  inherited;

end;

procedure TArcIWCheckListBox.SetSelectedCommaText(const Value: string);
var
  sl : TStringList;
begin
  sl := TStringList.Create;
  try
    sl.CommaText := Value; 
    StringListToSelected(sl);
  finally
    sl.Free;
  end;
end;

procedure TArcIWCheckListBox.SetSelectedObjectCommaText(const Value: string);
var
  sl : TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    sl.CommaText := Value;
    for i := 0 to sl.Count - 1 do
      sl.Objects[i] := TObject(StrToInt(sl[i]));
    StringListToSelected(sl,True);
  finally
    sl.Free;
  end;
end;

procedure TArcIWCheckListBox.SetValue(const AValue: string);
begin
  //
end;

procedure TArcIWCheckListBox.SetValueByName(const AName, AValue: string);
var
  sVal : string;
  i : integer;
begin
  if AValue = '' then
    exit;

  if FMultiselect then
  begin
    sVal := Copy(AName,pos('_',AName)+1,high(integer));

    if sVal <> 'none' then
    begin
      if TryStrToInt(sVal,i) then
      begin
        //FSelectedList.Add(TObject(i));
        if AValue = 'on' then
        begin
          //ItemIndex := i;
          Selected[i] := true;
        end else
          Selected[i] := false;
      end;
    end else
    begin
      if Uppercase(AValue) = 'on' then
      begin
        ResetSelection;
        //ItemIndex := -1;
      end;
    end;
  end else
  begin
    sVal := Copy(AValue,pos('_',AValue)+1,high(integer));
    if sVal = 'none' then
      ItemIndex := -1
    else
      if TryStrToInt(sVal,i) then
        ItemIndex := i;
  end;
end;

procedure TArcIWCheckListBox.StringListToSelected(SelectedItems: TStringList; ByObject : boolean = false);
var
  i: Integer;
begin
  ResetSelection;
  for i := 0 to Items.Count - 1 do
  begin
    if ByObject then
      Selected[i] := SelectedItems.IndexOfObject(Items.Objects[i]) >= 0
    else
      Selected[i] := SelectedItems.IndexOf(Items[i]) >= 0;
  end;
end;

end.
