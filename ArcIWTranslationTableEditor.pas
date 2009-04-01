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

unit ArcIWTranslationTableEditor;

interface

uses
  Windows, Messages, SysUtils, {$IFNDEF VER130} Variants, {$ENDIF}Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, ImgList,
  ToolWin;

type
  TfrmTranslationTable = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    lbCodes: TListBox;
    Panel3: TPanel;
    Panel4: TPanel;
    tvGroups: TTreeView;
    Panel6: TPanel;
    tbCodes: TToolBar;
    tbbDeleteGroup: TToolButton;
    tbbDelete: TToolButton;
    tbbAdd: TToolButton;
    tbbNewGroup: TToolButton;
    ImageList1: TImageList;
    Panel5: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure lbCodesClick(Sender: TObject);
    procedure tvGroupsChange(Sender: TObject; Node: TTreeNode);
    function SelectedLevel : integer;
    procedure tbbNewGroupClick(Sender: TObject);
    procedure tbbDeleteGroupClick(Sender: TObject);
    procedure tbbAddClick(Sender: TObject);
    procedure tbbDeleteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTranslationTable: TfrmTranslationTable;

implementation

{$R *.dfm}

procedure TfrmTranslationTable.lbCodesClick(Sender: TObject);
begin
  tbbAdd.Enabled := (lbCodes.SelCount > 0) {$IFNDEF VER130}and (tvGroups.SelectionCount > 0){$ENDIF};
end;

function TfrmTranslationTable.SelectedLevel: integer;
var
  i : integer;
begin
  Result := -1;
  {$IFNDEF VER130}
  for i := 0 to tvGroups.SelectionCount-1 do
  begin
    if (Result > -1) then
    begin
      if tvGroups.Selections[i].Level <> Result then
      begin
        Result := -1;
        Exit;
      end;
    end;
    Result := tvGroups.Selections[i].Level;
  end;
  {$ELSE}
  for i := 0 to tvGroups.Items.Count-1 do
  begin
    if (Result > -1) then
    begin
      if tvGroups.Items[i].Selected and (tvGroups.Items[i].Level <> Result) then
      begin
        Result := -1;
        Exit;
      end;
    end;
    if tvGroups.Items[i].Selected then
      Result := tvGroups.Items[i].Level;
  end;
  {$ENDIF}
end;

procedure TfrmTranslationTable.tvGroupsChange(Sender: TObject;
  Node: TTreeNode);
begin
  tbbDelete.Enabled := Assigned(tvGroups.Selected) and (SelectedLevel=1);
  tbbDeleteGroup.Enabled := Assigned(tvGroups.Selected) and (SelectedLevel=0);
  tbbAdd.Enabled := (lbCodes.SelCount > 0) and Assigned(tvGroups.Selected);
end;

procedure TfrmTranslationTable.tbbNewGroupClick(Sender: TObject);
var
  s : string;
begin
  s := '';
  s := InputBox('New Language Group','Enter Language Name',s);
  if s<>'' then
  begin
    tvGroups.Items.Add(nil,s);
  end;
  tvGroups.AlphaSort{$IFNDEF VER130}(True){$ENDIF};
end;

procedure TfrmTranslationTable.tbbDeleteGroupClick(Sender: TObject);
var
  i,j : integer;
begin
  with TList.Create do
  try
    {$IFNDEF VER130}
    for j := 0 to tvGroups.SelectionCount-1 do
    begin
      for i := 0 to tvGroups.Selections[j].Count -1 do
        lbCodes.Items.Add(tvGroups.Selections[j].Item[i].Text);

      Add(tvGroups.Selections[j]);
    end;
    {$ELSE}
    for j := 0 to tvGroups.Items.Count-1 do
    begin
      if tvGroups.Items[j].Selected then
      begin
        for i := 0 to tvGroups.Items[j].Count -1 do
            lbCodes.Items.Add(tvGroups.Items[j].Item[i].Text);

        Add(tvGroups.Items[j]);
      end;
    end;
    {$ENDIF}
    for i := 0 to Count-1 do
      TTreeNode(Items[i]).Delete;
  finally
    free;
  end;
end;

procedure TfrmTranslationTable.tbbAddClick(Sender: TObject);
var
  i : integer;
begin
  if Assigned(tvGroups.Selected) and (lbCodes.SelCount > 0) then
  begin
    i := 0;
    while lbCodes.SelCount > 0 do
    begin
      if lbCodes.Selected[i] then
      begin
        tvGroups.Items.AddChild(tvGroups.Selected,lbCodes.Items[i]);
        lbCodes.Items.Delete(i);
      end else
        inc(i);
    end;
  end;
  tvGroups.AlphaSort{$IFNDEF VER130}(True){$ENDIF};
end;

procedure TfrmTranslationTable.tbbDeleteClick(Sender: TObject);
var
  i : integer;
begin
  with TList.Create do
  try
    {$IFNDEF VER130}
    for i := 0 to tvGroups.SelectionCount-1 do
    begin
      lbCodes.Items.Add(tvGroups.Selections[i].Text);
      Add(tvGroups.Selections[i]);
    end;
    {$ELSE}
    for i := 0 to tvGroups.Items.Count-1 do
    begin
      if tvGroups.Items[i].Selected then
      begin
        lbCodes.Items.Add(tvGroups.Items[i].Text);
        Add(tvGroups.Items[i]);
      end;
    end;
    {$ENDIF}
    for i := 0 to Count-1 do
      TTreeNode(Items[i]).Delete;
  finally
    free;
  end;
end;

procedure TfrmTranslationTable.FormCreate(Sender: TObject);
var
  i : integer;
  sPath : string;
begin
  SetLength(sPath,MAX_PATH);
  GetModuleFileName(HInstance,PCHar(sPath),MAX_PATH);
  SetLength(sPath,StrLen(PChar(sPath)));

  with TStringList.Create do
  try
    LoadFromFile(ExtractFilePath(sPath)+'ArcLanguageCodes.cfg');
    for i := 0 to count-1 do
    begin
      lbCodes.Items.Add(Names[i]+'  '+Values[Names[i]]);
    end;
  finally
    free;
  end;
end;

end.
