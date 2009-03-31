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

unit ArcIWAutoTranslate;

interface

uses
  Windows, Messages, SysUtils, {$IFNDEF VER130}Variants, {$ENDIF} Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls,CheckLst, ExtCtrls;

type
  TfrmAutoTranslate = class(TForm)
    cbBaseLanguage: TComboBox;
    btnTranslate: TButton;
    Label1: TLabel;
    lbTranslateTo: TCheckListBox;
    Label2: TLabel;
    Button1: TButton;
    rgOptions: TRadioGroup;
    procedure cbBaseLanguageChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAutoTranslate: TfrmAutoTranslate;

implementation

{$R *.dfm}

procedure TfrmAutoTranslate.cbBaseLanguageChange(Sender: TObject);
begin
  lbTranslateTo.Items.Clear;
  if cbBaseLanguage.Text='English' then
  begin
    //lbTranslateTo.Items.Add('Chinese');
    lbTranslateTo.Items.Add('French');
    lbTranslateTo.Items.Add('German');
    lbTranslateTo.Items.Add('Italian');
    //lbTranslateTo.Items.Add('Japanese');
    //lbTranslateTo.Items.Add('Korean');
    //lbTranslateTo.Items.Add('Portuguese');
    lbTranslateTo.Items.Add('Spanish');
  end else
    if cbBaseLanguage.Text='Chinese' then
    begin
      lbTranslateTo.Items.Add('English');
    end else
      if cbBaseLanguage.Text='French' then
      begin
        lbTranslateTo.Items.Add('English');
        lbTranslateTo.Items.Add('German');
      end else
        if cbBaseLanguage.Text='German' then
        begin
          lbTranslateTo.Items.Add('English');
          lbTranslateTo.Items.Add('French');
        end else
          if cbBaseLanguage.Text='Italian' then
          begin
            lbTranslateTo.Items.Add('English');
          end else
            if cbBaseLanguage.Text='Japanese' then
            begin
              lbTranslateTo.Items.Add('English');
            end else
              if cbBaseLanguage.Text='Korean' then
              begin
                lbTranslateTo.Items.Add('English');
              end else
                if cbBaseLanguage.Text='Portuguese' then
                begin
                  lbTranslateTo.Items.Add('English');
                end else
                  if cbBaseLanguage.Text='Russian' then
                  begin
                    lbTranslateTo.Items.Add('English');
                  end else
                    if cbBaseLanguage.Text='Spanish' then
                    begin
                      lbTranslateTo.Items.Add('English');
                    end;

end;

procedure TfrmAutoTranslate.FormShow(Sender: TObject);
begin
  MessageDlg( 'The Babelfish web service which this auto translator uses has not '+
              'been running.  We are currently working on a workaround using Open '+
              'Translation Engine.  Keep checking back with each update to see if '+
              'we have been able to fix this issue.', mtError, [mbOK],0);
  cbBaseLanguage.ItemIndex := cbBaseLanguage.Items.IndexOf('English');
  cbBaseLanguageChange(cbBaseLanguage);
end;

end.
