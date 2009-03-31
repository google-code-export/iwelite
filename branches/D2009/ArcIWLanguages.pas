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

unit ArcIWLanguages;

interface

uses
  SysUtils, Classes, IWAppForm, ArcIWLanguageTypes, ArcIWTranslatorBase, IWForm;

type
  TArcIWLanguageString = string;

  TArcIWTranslator = class(TComponent)
  private
    FTranslations: TControlTranslations;
    FLanguages: TTranslationTable;
    FManual: boolean;
    FEvalAlert : boolean;
    FDefaultLanguage: TArcIWLanguageString;
    FCurrentLanguage: TArcIWLanguageString;
    FOldOnRender : TNotifyEvent;
    FLanguageStrings : TStrings;
    procedure SetCurrentLanguage(const Value: TArcIWLanguageString);
    function GetLanguageStrings: TStrings;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure Loaded; override;
    procedure InternalOnRender(Sender : TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property LanguageStrings : TStrings read GetLanguageStrings;
    procedure RefreshLanguage; virtual;
  published
    property Translations : TControlTranslations read FTranslations write FTranslations;
    property Languages : TTranslationTable read FLanguages write FLanguages;
    property DefaultLanguage : TArcIWLanguageString read FDefaultLanguage write FDefaultLanguage;
    property Manual : boolean read FManual write FManual;
    property CurrentLanguage : TArcIWLanguageString read FCurrentLanguage write SetCurrentLanguage;
  end;

const
  ComponentVersion = '1.1.4';

implementation

uses TypInfo;

{ TArcIWTranslator }

constructor TArcIWTranslator.Create(AOwner: TComponent);
var
  i : integer;
begin
  if Assigned(AOwner) and (csDesigning in ComponentState) then
    for i := 0 to AOwner.ComponentCount -1 do
    begin
      if AOwner.Components[i] is TArcIWTranslator then
      begin
        raise Exception.Create('You cannot have multiple TArcIWTranslator components on the same form');
        break;
      end;
    end;
  {if Assigned(AOwner) and (not ((AOwner is TIWAppForm) or (AOwner is TIWPageForm))) then
    raise Exception.Create('This component may only be used on an IW form.');}

  inherited;
  FLanguageStrings := TStringList.Create;

  FEvalAlert := False;
  FDefaultLanguage := 'English';
  FLanguages := TTranslationTable.Create;
  FTranslations := TControlTranslations.Create(TIWAppForm(Owner));
end;

destructor TArcIWTranslator.Destroy;
begin
  FLanguageStrings.Free;
  FLanguages.Free;
  FTranslations.Free;
  inherited;
end;

function TArcIWTranslator.GetLanguageStrings: TStrings;
var
  i : integer;
begin
  Result := FLanguageStrings;
  FLanguageStrings.Clear;
  for i := 0 to Languages.Count -1 do
    FLanguageStrings.Add(Languages.Items[i].Language);
end;

procedure TArcIWTranslator.InternalOnRender(Sender: TObject);
begin
  if Assigned(FOldOnRender) then
    FOldOnRender(TIWAppForm(Owner));
  if not FManual then
    RefreshLanguage;
end;

procedure TArcIWTranslator.Loaded;
begin
  inherited;
  if (not FManual) and (not (csDesigning in ComponentState)) then
  begin
    FOldOnRender := TIWAppForm(Owner).OnRender;
    TIWAppForm(Owner).OnRender := InternalOnRender;
  end;
  if FManual then
    RefreshLanguage;
end;

procedure TArcIWTranslator.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i : integer;
begin
  inherited;
  if Operation = opRemove then
  begin
    for i := 0 to Translations.Count-1 do
    begin
      if Translations[i].Control = AComponent then
        Translations[i].Free;
      break;
    end;
  end;
end;

procedure TArcIWTranslator.RefreshLanguage;
var
  sl : TStringList;
  i : integer;
  sLang : string;
begin
  if not (csDesigning in ComponentState) then
  begin

    if not FManual then
    begin
      if not Assigned(TIWAppForm(Owner).WebApplication.Request) then exit;
      sLang := '';
      sl := TStringList.Create;
      try
        sl.CommaText := TIWAppForm(Owner).WebApplication.Request.GetFieldByName('Accept-Language');
        for i := 0 to sl.Count-1 do
        begin
          sLang := Languages.LanguageLookup(Copy(sl[i],1,2));
          if sLang <> '' then break;
        end;
      finally
        sl.Free;
      end;
    end else
      sLang := FCurrentLanguage;

    if sLang <> '' then
    begin
      for i := 0 to Self.Translations.Count -1 do
        Prop_SetPropertyValue(TIWForm(Self.Owner).FindComponent(Self.Translations[i].ControlName), Self.Translations[i].PropName, Self.Translations[i].Translations.TextByLanguage(sLang));
      TIWForm(Self.Owner).Invalidate;
    end;
  end;
end;

procedure TArcIWTranslator.SetCurrentLanguage(
  const Value: TArcIWLanguageString);
begin
  FCurrentLanguage := Value;
  if not (csLoading in ComponentState) then
    RefreshLanguage;
end;

end.
 
