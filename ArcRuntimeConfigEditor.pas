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

unit ArcRuntimeConfigEditor;

interface

{$I IntraWebVersion.inc}

uses SysUtils, {$IFDEF VER130}DsgnIntf,{$ELSE}Variants, DesignIntf,
  DesignEditors, {$ENDIF}Classes, Graphics, IWServerControllerBase,
  IniFiles, IWColor, ColnEdit;

type
  TArcStyleManagerEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): String; override;
  end;

  TArcRuntimeConfigEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): String; override;
  end;

  TArcServerConfigEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): String; override;
  end;

implementation

uses Dialogs, TypInfo, Controls, ArcRuntimeConfig, ArcRuntimeConfigEditorForm,
  ArcServerConfig, ArcWebCommon, ArcStyleManager;

{$IFDEF VER130}
type
  PInteger = ^Integer;

function GetPropList3(TypeInfo: PTypeInfo; out PropList: PPropList): Integer;
begin
  Result := GetTypeData(TypeInfo)^.PropCount;
  if Result > 0 then
  begin
    GetMem(PropList, Result * SizeOf(Pointer));
    GetPropInfos(TypeInfo, PropList);
  end;
end;

procedure SortPropList(PropList: PPropList; PropCount: Integer); assembler;
asm
        { ->    EAX Pointer to prop list        }
        {       EDX Property count              }
        { <-    nothing                         }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     ECX,EAX
        XOR     EAX,EAX
        DEC     EDX
        CALL    @@qsort
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     @@exit

@@qsort:
        PUSH    EAX
        PUSH    EDX
        LEA     EDI,[EAX+EDX]           { pivot := (left + right) div 2 }
        SHR     EDI,1
        MOV     EDI,[ECX+EDI*4]
        ADD     EDI,OFFSET TPropInfo.Name
@@repeat:                               { repeat                        }
@@while1:
        CALL    @@compare               { while a[i] < a[pivot] do inc(i);}
        JAE     @@endWhile1
        INC     EAX
        JMP     @@while1
@@endWhile1:
        XCHG    EAX,EDX
@@while2:
        CALL    @@compare               { while a[j] > a[pivot] do dec(j);}
        JBE     @@endWhile2
        DEC     EAX
        JMP     @@while2
@@endWhile2:
        XCHG    EAX,EDX
        CMP     EAX,EDX                 { if i <= j then begin          }
        JG      @@endRepeat
        MOV     EBX,[ECX+EAX*4]         { x := a[i];                    }
        MOV     ESI,[ECX+EDX*4]         { y := a[j];                    }
        MOV     [ECX+EDX*4],EBX         { a[j] := x;                    }
        MOV     [ECX+EAX*4],ESI         { a[i] := y;                    }
        INC     EAX                     { inc(i);                       }
        DEC     EDX                     { dec(j);                       }
                                        { end;                          }
        CMP     EAX,EDX                 { until i > j;                  }
        JLE     @@repeat

@@endRepeat:
        POP     ESI
        POP     EBX

        CMP     EAX,ESI
        JL      @@rightNonEmpty         { if i >= right then begin      }
        CMP     EDX,EBX
        JG      @@leftNonEmpty1         { if j <= left then exit        }
        RET

@@leftNonEmpty1:
        MOV     EAX,EBX
        JMP     @@qsort                 { qsort(left, j)                }

@@rightNonEmpty:
        CMP     EAX,EBX
        JG      @@leftNonEmpty2
        MOV     EDX,ESI                 { qsort(i, right)               }
        JMP     @@qsort
@@leftNonEmpty2:
        PUSH    EAX
        PUSH    ESI
        MOV     EAX,EBX
        CALL    @@qsort                 { qsort(left, j)                }
        POP     EDX
        POP     EAX
        JMP     @@qsort                 { qsort(i, right)               }

@@compare:
        PUSH    EAX
        PUSH    EDI
        MOV     ESI,[ECX+EAX*4]
        ADD     ESI,OFFSET TPropInfo.Name
        PUSH    ESI
        XOR     EBX,EBX
        MOV     BL,[ESI]
        INC     ESI
        CMP     BL,[EDI]
        JBE     @@firstLenSmaller
        MOV     BL,[EDI]
@@firstLenSmaller:
        INC     EDI
        TEST    BL,BL
        JE      @@endLoop
@@loop:
        MOV     AL,[ESI]
        MOV     AH,[EDI]
        AND     EAX,$DFDF
        CMP     AL,AH
        JNE     @@difference
        INC     ESI
        INC     EDI
        DEC     EBX
        JNZ     @@loop
@@endLoop:
        POP     ESI
        POP     EDI
        MOV     AL,[ESI]
        MOV     AH,[EDI]
        CMP     AL,AH
        POP     EAX
        RET
@@difference:
        POP     ESI
        POP     EDI
        POP     EAX
        RET
@@exit:
end;

function GetPropList2(TypeInfo: PTypeInfo; TypeKinds: TTypeKinds;
  PropList: PPropList; SortList: Boolean=False): Integer;
var
  I, Count: Integer;
  PropInfo: PPropInfo;
  TempList: PPropList;
begin
  Result := 0;
  Count := GetPropList3(TypeInfo, TempList);
  if Count > 0 then
    try
      for I := 0 to Count - 1 do
      begin
        PropInfo := TempList^[I];
        if PropInfo^.PropType^.Kind in TypeKinds then
        begin
          if PropList <> nil then
            PropList^[Result] := PropInfo;
          Inc(Result);
        end;
      end;
      if SortList and (PropList <> nil) and (Result > 1) then
        SortPropList(PropList, Result);
    finally
      FreeMem(TempList);
    end;
end;

function GetPropList(AObject: TObject; out PropList: PPropList): Integer;
var
  tk : TTypeKinds;
begin
  tk := [tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
    tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
    tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray];

  Result := GetPropList2(PTypeInfo(AObject.ClassInfo), tk, PropList);

end;

function GetWideStrProp(Instance: TObject; PropInfo: PPropInfo): WideString;
type
  TWideStringGetProc = function :WideString of object;
  TWideStringIndexedGetProc = function (Index: Integer): WideString of object;
var
  P: PWideString;
  M: TMethod;
  Getter: Longint;
begin
  case PropInfo^.PropType^.Kind of
    tkString,
    tkLString: Result := GetStrProp(Instance, PropInfo);
    tkWString:
      begin
        Getter := Longint(PropInfo^.GetProc);
        if (Getter and $FF000000) = $FF000000 then
        begin  // field - Getter is the field's offset in the instance data
          P := Pointer(Integer(Instance) + (Getter and $00FFFFFF));
          Result := P^;  // auto ref count
        end
        else
        begin
          if (Getter and $FF000000) = $FE000000 then
            // virtual method  - Getter is a signed 2 byte integer VMT offset
            M.Code := Pointer(PInteger(PInteger(Instance)^ + SmallInt(Getter))^)
          else
            // static method - Getter is the actual address
            M.Code := Pointer(Getter);

          M.Data := Instance;
          if PropInfo^.Index = Integer($80000000) then  // no index
            Result := TWideStringGetProc(M)()
          else
            Result := TWideStringIndexedGetProc(M)(PropInfo^.Index);
        end;
      end;
  else
    Result := '';
  end;
end;
{$ENDIF}


{ TArcRuntimeConfigEditor }

procedure TArcRuntimeConfigEditor.ExecuteVerb(Index: Integer);
var
  bExtractAll, bIncludeVisual, bIncludeNonVisual : boolean;

  function DoComponentExtract(cmp : TObject; pName : string; Props : TStringList) : boolean;
  var
    b : boolean;
    i, iCount : integer;
    pl : PPropList;
    val : Variant;
  begin
    Result := False;
    b := False;
    if (pName='') and ((not bIncludeVisual) and (cmp is TControl)) then
      exit;
    if (pName='') and ((not bIncludeNonVisual) and (not (cmp is TControl))) then
      exit;
    if cmp = nil then
      exit;
    if cmp is TStrings then
    begin
      Props.Add(pName+'.Text='+TStrings(cmp).CommaText);
      exit;
    end;
    iCount := GetPropList(cmp,pl);

    for i := 0 to iCount-1 do
    begin
      // Skip problematic properties.
      if (pl[i].Name='ServerOnResize') then
        continue;

      {if not (pl[i].PropType^^.Kind in [tkInteger, tkChar, tkEnumeration,
        tkSet, tkInt64, tkFloat, tkString, tkWChar, tkLString, tkWString]) then
        continue;}
      if System.Copy(pl[i].Name,1,4) = 'Help' then
        continue;
      if pl[i].Name = 'Cursor' then
        continue;
      if pl[i].Name = 'Name' then
        continue;

      case pl[i].PropType^^.Kind of
        tkInteger, tkChar:
          val := GetOrdProp(cmp,pl[i]);
        tkEnumeration:
          val := GetEnumProp(cmp,pl[i]);
        tkFloat:
          val := GetFloatProp(cmp,pl[i]);
        tkSet:
          val := GetSetProp(cmp,pl[i],True);
        tkLString, tkString:
          val := GetStrProp(cmp,pl[i]);
        tkWChar, tkWString:
          val := GetWideStrProp(cmp,pl[i]);
        {$IFNDEF VER130}
        tkInt64:
          val := GetInt64Prop(cmp,pl[i]);
        {$ENDIF}
        tkVariant:
          val := GetVariantProp(cmp,pl[i]);
        tkClass:
          begin
            if pName = '' then
              DoComponentExtract(GetObjectProp(cmp,pl[i]),pl[i].Name,Props)
            else
              DoComponentExtract(GetObjectProp(cmp,pl[i]),pName+'.'+pl[i].Name,Props);
            continue;
          end;
        tkRecord, tkMethod:
          continue;
        else
          continue;
      end;

      if pl[i].PropType^^.Name = 'TColor' then
        val := ColorToString(val);

      {$IFDEF INTRAWEB51}
      if pl[i].PropType^^.Name = 'TIWColor' then
        val := IWColorToString(val);
      {$ENDIF}

      if (not bExtractAll) then
      begin
        case pl[i].PropType^^.Kind of
          tkInteger, tkChar, tkInt64:
            begin
              if pl[i].PropType^^.Name = 'TColor' then
                b := StringToColor(val)<> clNone
              {$IFDEF INTRAWEB51}
              else
                if pl[i].PropType^^.Name = 'TIWColor' then
                  b := StringToIWColor(val)<> clNone
              {$ENDIF}
                else
                  b := val <> pl[i].Default;
            end;
          tkFloat:
            b := val <> 0;
          tkString, tkWChar, tkLString, tkWString:
            b := val <> '';
          tkEnumeration:
            b := val <> 'False';
          tkSet:
            b := val <> '[]';
        end;
      end else
        b := True;

      if b then
        if pl[i].PropType^^.Name = 'TColor' then
        begin
          if pName = '' then
            Props.Add(pl[i].Name+'='+ColorToString(val))
          else
            Props.Add(pName+'.'+pl[i].Name+'='+IWColorToString(val));
        end else
          if pl[i].PropType^^.Name = 'TIWColor' then
          begin
            if pName = '' then
              Props.Add(pl[i].Name+'='+VarToStr(val))
            else
              Props.Add(pName+'.'+pl[i].Name+'='+VarToStr(val));
          end else
          begin
            if pName = '' then
              Props.Add(pl[i].Name+'='+VarToStr(val))
            else
              Props.Add(pName+'.'+pl[i].Name+'='+VarToStr(val));
          end;
    end;
    Result := True;
  end;
var
  sFile : string;
  i : integer;
  slProps : TStringList;
  slObject : TStringList;
begin
  sFile := TArcRuntimeConfig(Component).TemplateFile;
  if sFile = '' then
    sFile := Component.Owner.Name+'.ini';

  with TfrmExtractOptions.Create(nil) do
  try
    edtINI.Text := sFile;
    if ShowModal<>mrOK then
      exit;
    sFile := edtINI.Text;
    bExtractAll := not chkChanged.Checked;
    bIncludeVisual := chkVisual.Checked;
    bIncludeNonVisual := chkNonVisual.Checked;
  finally
    free;
  end;

  if FileExists(sFile) then
    if MessageDlg( 'The file '+sFile+' already exists.  Would you like to overwrite it?',
                   mtConfirmation,[mbYes,mbNo],0)=mrNo then
      exit;

  slProps := TStringList.Create;
  slObject := TStringList.Create;
  try
    DoComponentExtract(Component.Owner,'',slProps);
    slObject.Add('['+Component.Owner.Name+']');
    slProps.Sort;
    slObject.AddStrings(slProps);
    slObject.Add('');
    for i := 0 to Component.Owner.ComponentCount-1 do
    begin
      slProps.Clear;
      if not DoComponentExtract(Component.Owner.Components[i],'',slProps) then
        continue;
      if slProps.Count = 0 then
       continue;
      slObject.Add('['+Component.Owner.Components[i].Name+']');
      slProps.Sort;
      slObject.AddStrings(slProps);
      slObject.Add('');
    end;

    if TArcRuntimeConfig(Component).TemplateFile ='' then
    begin
      TArcRuntimeConfig(Component).TemplateFile := sFile;
      Designer.Modified;
    end;
    slObject.SaveToFile(sFile);
  finally
    slProps.Free;
    slObject.Free;
  end;
end;

function TArcRuntimeConfigEditor.GetVerb(Index: Integer): String;
begin
  Result := 'Create Template File';
end;

function TArcRuntimeConfigEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TArcServerConfigEditor }

{$IFDEF VER130}
function BoolToStr(b, bDummy : boolean) : string;
begin
  if b then
    Result := 'TRUE'
  else
    Result := 'FALSE';
end;
{$ENDIF}

procedure TArcServerConfigEditor.ExecuteVerb(Index: Integer);
var
  sFile : string;
  slOut : TStringList;
begin
  sFile := TArcIWServerConfig(Component).TemplateFile;
  if sFile = '' then
  begin
    sFile := DLLFileName;
    sFile := System.Copy(sFile,1,length(sFile)-length(ExtractFileExt(sFile)))+'.ini';
  end;
  showmessage(sFile);

  if FileExists(sFile) then
    if MessageDlg( 'The file '+sFile+' already exists.  Would you like to overwrite it?',
                   mtConfirmation,[mbYes,mbNo],0)=mrNo then
      exit;

  slOut := TStringList.Create;
  try
    slOut.Values['AppName'] := TIWServerControllerBase(Component.Owner).AppName;
    slOut.Values['BoundIP'] := TIWServerControllerBase(Component.Owner).BoundIP;
    slOut.Values['Description'] := TIWServerControllerBase(Component.Owner).AppName;
    slOut.Values['ExceptionDisplayMode'] := GetEnumProp(Component.Owner,'ExceptionDisplayMode');
    {$IFNDEF INTRAWEB110}
    slOut.Values['ExecCmd'] := TIWServerControllerBase(Component.Owner).ExecCmd;
    {$ENDIF}
    slOut.Values['HistoryEnabled'] := BoolToStr(TIWServerControllerBase(Component.Owner).HistoryEnabled, True);
    slOut.Values['HTMLHeaders'] := TIWServerControllerBase(Component.Owner).HTMLHeaders.CommaText;
    {$IFNDEF INTRAWEB110}
    slOut.Values['InvalidCommandURL'] := TIWServerControllerBase(Component.Owner).InvalidCommandURL;
    {$ENDIF}
    slOut.Values['Port'] := IntToStr(TIWServerControllerBase(Component.Owner).Port);
    {$IFNDEF INTRAWEB110}
    slOut.Values['RestrictIPs'] := BoolToStr(TIWServerControllerBase(Component.Owner).RestrictIPs,True);
    {$ENDIF}

    slOut.Values['SessionTimeout'] := IntToStr(TIWServerControllerBase(Component.Owner).SessionTimeout);
    slOut.Values['SessionTrackingMethod'] := GetEnumProp(Component.Owner,'SessionTrackingMethod');
    {$IFNDEF INTRAWEB110}
    slOut.Values['ShowResyncWarning'] := BoolToStr(TIWServerControllerBase(Component.Owner).ShowResyncWarning,True);
    {$ENDIF}
    {$IFDEF INTRAWEB50}
    slOut.Values['SSLCertificatePassword'] := TIWServerControllerBase(Component.Owner).SSLCertificatePassword;
    slOut.Values['SSLPort'] := IntToStr(TIWServerControllerBase(Component.Owner).SSLPort);
    {$ELSE}
    slOut.Values['SSLCertificatePassword'] := TIWServerControllerBase(Component.Owner).SSLOptions.CertificatePassword;
    slOut.Values['SSLPort'] := IntToStr(TIWServerControllerBase(Component.Owner).SSLOptions.Port);
    {$ENDIF}
    {$IFNDEF INTRAWEB110}
    slOut.Values['StartCmd'] := TIWServerControllerBase(Component.Owner).StartCmd;
    {$ENDIF}
    slOut.Values['SupportedBrowsers'] := GetSetProp(Component.Owner,'SupportedBrowsers');
    slOut.Values['TemplateDir'] := TIWServerControllerBase(Component.Owner).TemplateDir;
    {$IFNDEF INTRAWEB70}
    slOut.Values['TimeoutURL'] := TIWServerControllerBase(Component.Owner).TimeoutURL;
    {$ELSE}
    {$IFNDEF INTRAWEB110}
    slOut.Values['TimeoutURL'] := TIWServerControllerBase(Component.Owner).SessionTimeoutURL.URL;
    {$ENDIF}
    {$ENDIF}
    slOut.SaveToFile(sFile);
  finally
    slOut.Free;
  end;
  TArcIWServerConfig(Component).TemplateFile := ExtractFileName(sFile);
  Designer.Modified;
end;

function TArcServerConfigEditor.GetVerb(Index: Integer): String;
begin
  Result := 'Create Template File';
end;

function TArcServerConfigEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TArcStyleManagerEditor }

procedure TArcStyleManagerEditor.ExecuteVerb(Index: Integer);
var
  cmp : TArcStyleManager;
  procedure LoadStyle;
  var
    od : TOpenDialog;
  begin
    od := TOpenDialog.Create(nil);
    try
      od.FileName := cmp.StyleFile;
      od.Options := [ofExtensionDifferent, ofPathMustExist, ofFileMustExist, ofEnableSizing];
      od.Filter := 'ini Files (*.ini)|*.ini|All Files (*.*)|*.*';
      od.FilterIndex := 0;
      if od.Execute then
      begin
        if cmp.StyleFile = '' then
          cmp.StyleFile := od.FileName;
        cmp.Styles.LoadStyle(cmp.StyleFile);
        Self.Designer.Modified;
      end;
    finally
      od.Free;
    end;
  end;
  procedure SaveStyle;
  var
    od : TSaveDialog;
  begin
    od := TSaveDialog.Create(nil);
    try
      od.FileName := cmp.StyleFile;
      od.Options := [ofOverwritePrompt, ofPathMustExist, ofEnableSizing];
      od.Filter := 'ini Files (*.ini)|*.ini|All Files (*.*)|*.*';
      od.FilterIndex := 0;
      if od.Execute then
      begin
        if cmp.StyleFile = '' then
          cmp.StyleFile := od.FileName;
        cmp.Styles.SaveStyle(cmp.StyleFile);
        Self.Designer.Modified;
      end;
    finally
      od.Free;
    end;
  end;
begin
  cmp := TArcStyleManager(Component);
  case Index of

    0: ShowCollectionEditor(Designer,Component,TArcStyleManager(Component).Styles,'Styles');
    1: LoadStyle;
    2: SaveStyle;
  end;
end;

function TArcStyleManagerEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

function TArcStyleManagerEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := 'Edit Styles';
    1: Result := 'Load Style Template...';
    2: Result := 'Save Style Template...';
  end;
end;

end.
