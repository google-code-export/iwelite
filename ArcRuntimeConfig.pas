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

unit ArcRuntimeConfig;

interface

{$I IntraWebVersion.inc}

uses
  SysUtils, Classes, IniFiles, IWForm,
{$IFNDEF INTRAWEB5} IWColor,{$ENDIF}
{$IFDEF FASTSTRINGS}
  ArcFastStrings,
{$ELSE}
  ArcStrings,
{$ENDIF}
ArcIWControlCommon, ArcIWControlBase;

type
  TArcRuntimeConfig = class(TComponent)
  private
    FTemplateFile: string;
    FOnBeforeProcess: TNotifyEvent;
    FOnAfterProcess: TNotifyEvent;
    FActive: boolean;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active : boolean read FActive write FActive default true;
    property TemplateFile : string read FTemplateFile write FTemplateFile;
    property OnBeforeProcess : TNotifyEvent read FOnBeforeProcess write FOnBeforeProcess;
    property OnAfterProcess : TNotifyEvent read FOnAfterProcess write FOnAfterProcess;
  end;

implementation

uses {$IFNDEF VER130}Variants,{$ENDIF} Graphics, TypInfo;

{ TArcRuntimeConfig }

constructor TArcRuntimeConfig.Create(AOwner: TComponent);
begin
  if not (AOwner is TIWForm) then
    raise EComponentError.Create('TArcRuntimeConfig requires an owner.');
  inherited;
  FActive := True;
end;

destructor TArcRuntimeConfig.Destroy;
begin
  inherited;
end;

procedure TArcRuntimeConfig.Loaded;
var
  i, iProp, iPos : integer;
  i64 : int64;
  pi : PPropInfo;
  slObjs, slProps : TStringList;
  sProp : string;
  obj : TComponent;
  val : variant;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FOnBeforeProcess) then
      FOnBeforeProcess(Self);

    if not FActive then
      exit;
      
    with TMemIniFile.Create(FTemplateFile) do
    try
      {$IFNDEF VER130}
      CaseSensitive := False;
      {$ENDIF}
      slObjs := TStringList.Create;
      slProps := TStringList.Create;
      try
        ReadSections(slObjs);
        for i:=0 to slObjs.Count-1 do
        begin
          slProps.Clear;
          ReadSection(slObjs[i],slProps);
          for iProp := 0 to slProps.Count-1 do
          begin
            obj := Owner.FindComponent(slObjs[i]);
            if obj = nil then
              obj := Owner;
            sProp := slProps[iProp];
            iPos := CharPos('.',sProp);
            while iPos > 0 do
            begin
              obj := TComponent(GetObjectProp(obj,Copy(sProp,1,iPos-1)));
              sProp := Copy(sProp,iPos+1,9999999);
              iPos := CharPos('.',sProp);
            end;

            if TObject(obj) is TStrings then
            begin
              TStrings(obj).CommaText := ReadString(slObjs[i],slProps[iProp],'');
              continue;
            end;

            pi := GetPropInfo(obj,sProp);
            if pi = nil then
              continue;

            val := ReadString(slObjs[i],slProps[iProp],'');

            if pi.PropType^^.Name = 'TColor' then
              val := StringToColor(val);

            {$IFNDEF INTRAWEB5}
            if pi.PropType^^.Name = 'TIWColor' then
              val := StringToIWColor(val);
            {$ENDIF}

            try
              case pi.PropType^^.Kind of
                {$IFNDEF VER130}
                tkInteger, tkChar:
                  SetOrdProp(obj,pi,val);
                tkEnumeration:
                  SetEnumProp(obj,pi,val);
                tkSet:
                  SetSetProp(obj,pi,val);
                {$ELSE}
                tkInteger, tkChar, tkEnumeration, tkSet:
                  SetOrdProp(obj,pi,val);
                {$ENDIF}
                tkFloat:
                  SetFloatProp(obj,pi,val);
                tkLString, tkString:
                  {$IFDEF VER130}ArcIWControlCommon.{$ENDIF}SetStrProp(obj,pi,val);
                tkWChar, tkWString:
                  SetWideStrProp(obj,pi,val);
                {$IFNDEF VER130}
                tkInt64:
                  begin
                    i64 := val;
                    SetInt64Prop(obj,pi,i64);
                  end;
                {$ENDIF}
                tkVariant:
                  SetVariantProp(obj,pi,val);
                tkClass:
                  continue;
                tkRecord, tkMethod:
                  continue;
                else
                  continue;
              end;
            except
              on e: exception do
              begin
                // If this is a "no parent window" exception, then ignore.  It doesn't
                // matter in an intraweb application because technically no window has
                // a parent window.  This is a byproduct of having wincontrols in a non
                // windowed application.
                if FastPosNoCase(e.Message,'parent window',length(e.Message),13,1)=0 then
                begin
                  try
                    TIWForm(Owner).AddToInitProc('alert("Error Setting Property: '+e.message+'. '+obj.Name+':'+obj.ClassName+' to value '+val+'");'#13);
                  except
                    TIWForm(Owner).AddToInitProc('alert("Error Setting Property: '+e.message+'");'#13);
                  end;
                end;
              end;
            end;
          end;
        end;
      finally
        slProps.Free;
        slObjs.Free;
      end;
    finally
      free;
    end;

    if Assigned(FOnAfterProcess) then
      FOnAfterProcess(Self);
      
  end;
  inherited;
end;

end.
