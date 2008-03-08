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

unit ArcIWStringGridDesigners;

interface

{$I IntraWebVersion.inc}

uses Windows, Controls, Classes, SysUtils, ShellApi, Dialogs, ComCtrls, IWControl,
  IWForm, {$IFDEF VER130}Forms, DsgnIntf,{$ELSE}DesignIntf, DesignEditors,{$ENDIF} Menus
  {$IFDEF INTRAWEB60}, IWBaseControlInterface {$ENDIF}
  {$IFDEF INTRAWEB70}, IWBaseInterfaces {$ENDIF}
  , ArcIWStringGrid, ArcIWStringGridDatasetContent, ArcIWStringGridINIContent,
  ArcIWStringGridTreeContent, ColnEdit;

type
  TTreeContentComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

  TDatasetContentComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

  TINIContentComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

  TArcIWStringGridComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses DB, ClipBrd, iniFiles, TypInfo, ShlObj, ActiveX;

procedure Register;
begin
  RegisterComponentEditor(TArcIWStringGridDatasetContent, TDatasetContentComponentEditor);
  RegisterComponentEditor(TArcIWStringGrid, TArcIWStringGridComponentEditor);
  RegisterComponentEditor(TArcIWStringGridINIContent, TINIContentComponentEditor);
  RegisterComponentEditor(TArcIWStringGridTreeContent, TTreeContentComponentEditor);
end;

procedure StrResetLength(var S: AnsiString);
begin
  SetLength(S, StrLen(PChar(S)));
end;

function PidlToPath(IdList: PItemIdList): string;
begin
  SetLength(Result, MAX_PATH);
  if SHGetPathFromIdList(IdList, PChar(Result)) then
    StrResetLength(Result)
  else
    Result := '';
end;

function PidlFree(var IdList: PItemIdList): Boolean;
var
  Malloc: IMalloc;
begin
  Result := False;
  if IdList = nil then
    Result := True
  else
  begin
    if Succeeded(SHGetMalloc(Malloc)) and (Malloc.DidAlloc(IdList) > 0) then
    begin
      Malloc.Free(IdList);
      IdList := nil;
      Result := True;
    end;
  end;
end;

function GetSpecialFolderLocation(const Folder: Integer): string;
var
  FolderPidl: PItemIdList;
begin
  if Succeeded(SHGetSpecialFolderLocation(0, Folder, FolderPidl)) then
  begin
    Result := PidlToPath(FolderPidl);
    PidlFree(FolderPidl);
  end
  else
    Result := '';
end;

function GetPersonalFolder: string;
begin
  {$IFDEF UNIX}
  Result := GetEnvironmentVariable('HOME');
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS}
  Result := GetSpecialFolderLocation(CSIDL_PERSONAL);
  {$ENDIF MSWINDOWS}
end;

{ TDatasetContentComponentEditor }

procedure TDatasetContentComponentEditor.ExecuteVerb(Index: Integer);
  procedure DoLoadFieldsClip(cmp : TArcIWStringGridDatasetContent);
    procedure ProcessObject(sl : TStringList);
      function ClassTypeToDataType(sType : String) : TFieldType;
      begin
        Result := ftUnknown;
        if (sType = 'TStringField') then
          Result := ftString
        else if (sType = 'TWideStringField') then
          Result := ftWideString
        else if (sType = 'TNumericField') then
          Result := ftFloat
        else if (sType = 'TIntegerField') then
          Result := ftInteger
        else if (sType = 'TSmallintField') then
          Result := ftSmallint
        else if (sType = 'TLargeintField') then
          Result := ftLargeint
        else if (sType = 'TWordField') then
          Result := ftWord
        else if (sType = 'TAutoIncField') then
          Result := ftAutoInc
        else if (sType = 'TFloatField') then
          Result := ftFloat
        else if (sType = 'TCurrencyField') then
          Result := ftCurrency
        else if (sType = 'TBooleanField') then
          Result := ftBoolean
        else if (sType = 'TDateTimeField') then
          Result := ftDateTime
        {$IFNDEF VER130}
        else if (sType = 'TSQLTimeStampField') then
          Result := ftTimeStamp
        {$ENDIF}
        else if (sType = 'TDateField') then
          Result := ftDate
        else if (sType = 'TTimeField') then
          Result := ftTime
        else if (sType = 'TBinaryField') then
          Result := ftBlob
        else if (sType = 'TBytesField') then
          Result := ftBytes
        else if (sType = 'TVarBytesField') then
          Result := ftVarBytes
        else if (sType = 'TBCDField') then
          Result := ftBCD
        {$IFNDEF VER130}
        else if (sType = 'TFMTBCDField') then
          Result := ftFMTBcd
        {$ENDIF}
        else if (sType = 'TBlobField') then
          Result := ftBlob
        else if (sType = 'TMemoField') then
          Result := ftMemo
        else if (sType = 'TGraphicField') then
          Result := ftGraphic
        else if (sType = 'TObjectField') then
          Result := ftBlob
        else if (sType = 'TADTField') then
          Result := ftADT
        else if (sType = 'TArrayField') then
          Result := ftArray
        else if (sType = 'TDataSetField') then
          Result := ftDataSet
        else if (sType = 'TReferenceField') then
          Result := ftReference
        else if (sType = 'TVariantField') then
          Result := ftVariant
        else if (sType = 'TInterfaceField') then
          Result := ftInterface
        else if (sType = 'TIDispatchField') then
          Result := ftIDispatch
        else if (sType = 'TGuidField') then
          Result := ftGuid
        else if (sType = 'TAggregateField') then
          Result := ftBlob;
      end;
    var
      fi : TDataFieldItem;
      sType : String;
      slObj : TStringList;
    begin
      if sl.Count = 0 then
        exit;
      if System.Copy(sl[0],1,7)<>'object ' then
        raise Exception.Create('Delphi object not found in clipboard.');

      slObj := TStringList.Create;
      try
        sType := TrimLeft(System.Copy(sl[0],Pos(':',sl[0])+1,High(integer)));
        sl.delete(0);
        repeat
          slObj.Add(TrimLeft(StringReplace(sl[0],' = ','=',[])));
          sl.Delete(0);
        until (sl.Count = 0) or (sl[0]='end');
        if sl.Count > 0 then
          sl.Delete(0); // remove end

        if not cmp.DataFields.HasField(StringReplace(slObj.Values['FieldName'],'''','',[rfReplaceAll])) then
        begin
          fi := cmp.DataFields.Add;
          fi.KeyField  := (slObj.Values['Lookup'] <> '') or
                          (sType = 'TAutoIncField') or
                          (Pos('pfInKey',slObj.Values['ProviderFlags'])>0);
          fi.DataField := StringReplace(slObj.Values['FieldName'],'''','',[rfReplaceAll]);
          fi.DataType  := ClassTypeToDataType(sType);
          fi.ReadOnly  := slObj.Values['ReadOnly'] = 'true';
          fi.MaxLength := StrToIntDef(slObj.Values['Size'],0);
        end;
      finally
        slObj.Free;
      end;
    end;
  var
    sl : TStringList;
  begin
    if cmp.DataFields.Count > 0 then
    begin
      case (MessageDlg('Do you wish clear your fields before adding from the dataset?', mtConfirmation, [mbYes, mbNo], 0)) of
        mrYes: cmp.DataFields.Clear;
      end;
    end;

    sl := TStringList.Create;
    try
      sl.Text := Clipboard.AsText;
      while sl.Count > 0 do
        ProcessObject(sl);
    finally
      sl.Free;
    end;
  end;
  procedure DoLoadFields(cmp : TArcIWStringGridDatasetContent);
  var
    i : integer;
    fi : TDataFieldItem;
  begin
    if not Assigned(cmp.Dataset) then
      raise exception.Create('No dataset assigned.');
    if cmp.DataFields.Count > 0 then
    begin
      case (MessageDlg('Do you wish clear your fields before adding from the dataset?', mtConfirmation, [mbYes, mbNo], 0)) of
        mrYes: cmp.DataFields.Clear;
      end;
    end;
    for i := 0 to cmp.Dataset.Fields.Count-1 do
    begin
      if not cmp.DataFields.HasField(cmp.Dataset.Fields[i].FieldName) then
      begin
        fi := cmp.DataFields.Add;
        fi.KeyField  := cmp.Dataset.Fields[i].Lookup or
                        (cmp.Dataset.Fields[i].DataType = ftAutoInc) or
                        (pfInKey in cmp.Dataset.Fields[i].ProviderFlags);
        fi.DataField := cmp.Dataset.Fields[i].FieldName;
        fi.DataType  := cmp.Dataset.Fields[i].DataType;
        fi.ReadOnly  := cmp.Dataset.Fields[i].ReadOnly;
        fi.MaxLength := cmp.Dataset.Fields[i].Size;
      end;
    end;
  end;
  procedure DoLoadGridCols(cmp : TArcIWStringGridDatasetContent);
  var
    i : integer;
  begin
    if not Assigned(cmp.Grid) then
      raise Exception.Create('No Grid Assigned.');
    case (MessageDlg('Do you wish clear all columns before adding fields?', mtConfirmation, [mbYes, mbNo], 0)) of
      mrYes: cmp.Grid.Columns.Clear;
    end;
    for i := 0 to cmp.DataFields.Count-1 do
    begin
      if cmp.DataFields[i].Column <> '' then
      begin
        if not cmp.Grid.Columns.HasColumn(cmp.DataFields[i].Column) then
        begin
          cmp.Grid.Columns.Add.Caption := cmp.DataFields[i].Column;
          cmp.DataFields[i].ColumnIndex := cmp.Grid.Columns.IndexOf(cmp.DataFields[i].Column);
        end;
      end else
        if not cmp.Grid.Columns.HasColumn(cmp.DataFields[i].DataField) then
        begin
          cmp.DataFields[i].Column := cmp.DataFields[i].DataField;
          cmp.Grid.Columns.Add.Caption := cmp.DataFields[i].DataField;
          cmp.DataFields[i].ColumnIndex := cmp.Grid.Columns.IndexOf(cmp.DataFields[i].DataField);
        end;
    end;
  end;
begin
  inherited;
  case Index of
    0: ShowCollectionEditor(Designer,Component,TArcIWStringGridDatasetContent(Component).DataFields,'DataFields');
    1: DoLoadFields(TArcIWStringGridDatasetContent(Component));
    2: DoLoadFieldsClip(TArcIWStringGridDatasetContent(Component));
    3: DoLoadGridCols(TArcIWStringGridDatasetContent(Component));
  end;
end;

function TDatasetContentComponentEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := 'Edit Fields';
    1: Result := 'Load Fields From Dataset';
    2: Result := 'Load Fields From Clipboard';
    3: Result := 'Build Grid Columns from Fields';
  end;
end;

function TDatasetContentComponentEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

{ TArcIWStringGridComponentEditor }

procedure TArcIWStringGridComponentEditor.ExecuteVerb(Index: Integer);
  procedure ExportGridStyle(grid : TArcIWStringGrid);
  var
    dlg : TSaveDialog;
  begin
    dlg := TSaveDialog.Create(nil);
    try
      dlg.DefaultExt := '.ags';
      dlg.Filter := 'Arcana Grid Styles (*.ags)|*.ags|All Files (*.*)|*.*';
      dlg.FilterIndex := 0;
      dlg.InitialDir := GetPersonalFolder;
      dlg.Options := [ofOverwritePrompt, ofExtensionDifferent, ofPathMustExist, ofEnableSizing{$IFNDEF VER130}, ofForceShowHidden{$ENDIF}];
      dlg.Title := 'Select an export file';
      if dlg.Execute then
      begin
        grid.ExportGridStyle(dlg.FileName);
      end;
    finally
      dlg.Free;
    end;
  end;
  procedure ImportGridStyle(grid : TArcIWStringGrid);
  var
    dlg : TOpenDialog;
  begin
    dlg := TOpenDialog.Create(nil);
    try
      dlg.DefaultExt := '.ags';
      dlg.Filter := 'Arcana Grid Styles (*.ags)|*.ags|All Files (*.*)|*.*';
      dlg.FilterIndex := 0;
      dlg.InitialDir := GetPersonalFolder;
      dlg.Options := [ofExtensionDifferent, ofPathMustExist, ofFileMustExist, ofEnableSizing{$IFNDEF VER130}, ofForceShowHidden{$ENDIF}];
      dlg.Title := 'Select an file to import';
      if dlg.Execute then
      begin
        grid.ImportGridStyle(dlg.FileName);
      end;
    finally
      dlg.Free;
    end;
  end;
begin
  inherited;
  case Index of
    0: ShowCollectionEditor(Designer,Component,TArcIWStringGrid(Component).Columns,'Columns');
    1:;
    2: TArcIWStringGrid(Component).CaptionButtons.HideDefaultNavButtons;
    3: TArcIWStringGrid(Component).CaptionButtons.ShowDefaultNavButtons;
    4: TArcIWStringGrid(Component).CaptionButtons.HideNavButtons;
    5: TArcIWStringGrid(Component).CaptionButtons.ShowNavButtons;
    6:;
    7: TArcIWStringGrid(Component).CaptionButtons.HideEditButtons;
    8: TArcIWStringGrid(Component).CaptionButtons.ShowEditButtons;
    9:;
    10: TArcIWStringGrid(Component).CaptionButtons.HideEditModeButtons;
    11: TArcIWStringGrid(Component).CaptionButtons.ShowEditModeButtons;
    12: TArcIWStringGrid(Component).CaptionButtons.HideBrowseModeButtons;
    13: TArcIWStringGrid(Component).CaptionButtons.ShowBrowseModeButtons;
    14:;
    15: ExportGridStyle(TArcIWStringGrid(Component));
    16: ImportGridStyle(TArcIWStringGrid(Component));
  end;
end;

function TArcIWStringGridComponentEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := 'Edit Columns';
    1: Result := '-';
    2: Result := 'Hide Default Navigation Buttons';
    3: Result := 'Show Default Navigation Buttons';
    4: Result := 'Hide All Navigation Buttons';
    5: Result := 'Show All Navigation Buttons';
    6: Result := '-';
    7: Result := 'Hide Editor Buttons';
    8: Result := 'Show Editor Buttons';
    9: Result := '-';
    10: Result := 'Hide Edit Mode Buttons';
    11: Result := 'Show Edit Mode Buttons';
    12: Result := 'Hide Browse Mode Buttons';
    13: Result := 'Show Browse Mode Buttons';
    14: Result := '-';
    15: Result := 'Export Grid Style...';
    16: Result := 'Import Grid Style...';
  end;
end;

function TArcIWStringGridComponentEditor.GetVerbCount: Integer;
begin
  Result := 17;
end;

{ TINIContentComponentEditor }

procedure TINIContentComponentEditor.ExecuteVerb(Index: Integer);
  procedure DoLoadSections(cmp : TArcIWStringGridINIContent);
  var
    ini : TiniFile;
  begin
    if cmp.Filename = '' then
    begin
      case (MessageDlg('There is no ini filename assigned.  Do you wish clear all sections and values?', mtConfirmation, [mbYes, mbNo], 0)) of
        mrYes: cmp.Sections.Clear;
      end;
    end else
    begin
      if not FileExists(cmp.Filename) then
        raise Exception.Create('Specified INI file does not exist.');
      cmp.Sections.Clear;
      ini := TIniFile.Create(cmp.Filename);
      try
        cmp.Sections.Rebuild(ini);
      finally
        ini.Free;
      end;
    end;
  end;
  procedure DoLoadGridCols(cmp : TArcIWStringGridINIContent);
  begin
    if not Assigned(cmp.Grid) then
      raise Exception.Create('No Grid Assigned.');
    case (MessageDlg('Do you wish clear all columns before adding fields?', mtConfirmation, [mbYes, mbNo], 0)) of
      mrYes: cmp.Grid.Columns.Clear;
    end;
    cmp.Grid.Columns.Add.Caption := 'Name';
    cmp.Grid.Columns.Add.Caption := 'Value';
  end;
begin
  inherited;
  case Index of
    0: ShowCollectionEditor(Designer,Component,TArcIWStringGridINIContent(Component).Sections,'Sections');
    1: DoLoadSections(TArcIWStringGridINIContent(Component));
    2: DoLoadGridCols(TArcIWStringGridINIContent(Component));
  end;
end;

function TINIContentComponentEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := 'Edit Sections';
    1: Result := 'Rebuild Sections/Values';
    2: Result := 'Build Grid Name/Value Columns ';
  end;
end;

function TINIContentComponentEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

{ TTreeContentComponentEditor }

procedure TTreeContentComponentEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0: ShowCollectionEditor(Designer,Component,TArcIWStringGridTreeContent(Component).Items,'Items');
  end;
end;

function TTreeContentComponentEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := 'Edit Tree Items';
  end;
end;

function TTreeContentComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
