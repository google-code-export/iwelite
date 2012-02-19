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

unit ArcIWControlCommon;

interface

uses SysUtils, TypInfo, ArcIWControlBase;

function CharPos(c : Char; str : string) : integer;
{$IFDEF VER130}
type
  PInteger      = ^Integer;

function IncMinute(const AValue: TDateTime;
  const ANumberOfMinutes: Int64 = 1): TDateTime;

function FindPropInfo(Instance: TObject; const PropName: string): PPropInfo; overload;
procedure SetStrProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: string);
procedure SetWideStrPropAsLongStr(Instance: TObject; PropInfo: PPropInfo;
  const Value: string);
procedure SetShortStrPropAsLongStr(Instance: TObject; PropInfo: PPropInfo;
  const Value: string);
procedure SetLongStrProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: string); assembler;
procedure AssignLongStr(var Dest: string; const Source: string);
procedure SetShortStrProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: ShortString); assembler;
procedure SetWideStrProp2(Instance: TObject; PropInfo: PPropInfo;
  const Value: WideString);
procedure SetWideStrProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: WideString);

const
  HoursPerDay   = 24;
  MinsPerHour   = 60;
  SecsPerMin    = 60;
  MSecsPerSec   = 1000;
  MinsPerDay    = HoursPerDay * MinsPerHour;
  SecsPerDay    = MinsPerDay * SecsPerMin;
  MSecsPerDay   = SecsPerDay * MSecsPerSec;

{$ENDIF}

implementation

uses
  StrUtils;

{$IFDEF VER130}
procedure SetWideStrProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: WideString);
type
  TWideStringSetProc = procedure (const Value: WideString) of object;
  TWideStringIndexedSetProc = procedure (Index: Integer;
                                        const Value: WideString) of object;
var
  P: PWideString;
  M: TMethod;
  Setter: Longint;
begin
  case PropInfo^.PropType^.Kind of
    tkString,
    tkLString, tkUString: SetStrProp(Instance, PropInfo, Value);
    tkWString:
      begin
        Setter := Longint(PropInfo^.SetProc);
        if (Setter and $FF000000) = $FF000000 then
        begin  // field - Setter is the field's offset in the instance data
          P := Pointer(Integer(Instance) + (Setter and $00FFFFFF));
          P^ := Value;   // auto ref count
        end
        else
        begin
          if (Setter and $FF000000) = $FE000000 then
            // virtual method  - Setter is a signed 2 byte integer VMT offset
            M.Code := Pointer(PInteger(PInteger(Instance)^ + SmallInt(Setter))^)
          else
            // static method - Setter is the actual address
            M.Code := Pointer(Setter);

          M.Data := Instance;
          if PropInfo^.Index = Integer($80000000) then  // no index
            TWideStringSetProc(M)(Value)
          else
            TWideStringIndexedSetProc(M)(PropInfo^.Index, Value);
        end;
      end;
  end;
end;

procedure SetWideStrProp2(Instance: TObject; PropInfo: PPropInfo;
  const Value: WideString);
type
  TWideStringSetProc = procedure (const Value: WideString) of object;
  TWideStringIndexedSetProc = procedure (Index: Integer;
                                        const Value: WideString) of object;
var
  P: PWideString;
  M: TMethod;
  Setter: Longint;
begin
  case PropInfo^.PropType^.Kind of
    tkString,
    tkLString, tkUString: SetStrProp(Instance, PropInfo, Value);
    tkWString:
      begin
        Setter := Longint(PropInfo^.SetProc);
        if (Setter and $FF000000) = $FF000000 then
        begin  // field - Setter is the field's offset in the instance data
          P := Pointer(Integer(Instance) + (Setter and $00FFFFFF));
          P^ := Value;   // auto ref count
        end
        else
        begin
          if (Setter and $FF000000) = $FE000000 then
            // virtual method  - Setter is a signed 2 byte integer VMT offset
            M.Code := Pointer(PInteger(PInteger(Instance)^ + SmallInt(Setter))^)
          else
            // static method - Setter is the actual address
            M.Code := Pointer(Setter);

          M.Data := Instance;
          if PropInfo^.Index = Integer($80000000) then  // no index
            TWideStringSetProc(M)(Value)
          else
            TWideStringIndexedSetProc(M)(PropInfo^.Index, Value);
        end;
      end;
  end;
end;

procedure SetShortStrProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: ShortString); assembler;
asm
        { ->    EAX Pointer to instance         }
        {       EDX Pointer to property info    }
        {       ECX Pointer to string value     }

        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EDX

        MOV     EDX,[ESI].TPropInfo.Index       { pass index in EDX }
        CMP     EDX,$80000000
        JNE     @@hasIndex
        MOV     EDX,ECX                         { pass value in EDX }
@@hasIndex:
        MOV     EDI,[ESI].TPropInfo.SetProc
        CMP     [ESI].TPropInfo.SetProc.Byte[3],$FE
        JA      @@isField
        JB      @@isStaticMethod

        {       SetProc is a virtual method }
        MOVSX   EDI,DI
        ADD     EDI,[EAX]
        CALL    dword ptr [EDI]
        JMP     @@exit

@@isStaticMethod:
        CALL    EDI
        JMP     @@exit

@@isField:
        AND     EDI,$00FFFFFF
        ADD     EDI,EAX
        MOV     EAX,[ESI].TPropInfo.PropType
        MOV     EAX,[EAX]
        MOV     ESI,ECX
        XOR     ECX,ECX
        MOV     CL,[EAX].TTypeInfo.Name.Byte[0]
        MOV     CL,[EAX].TTypeInfo.Name[ECX+1].TTypeData.MaxLength

        LODSB
        CMP     AL,CL
        JB      @@noTruncate
        MOV     AL,CL
@@noTruncate:
        STOSB
        MOV     CL,AL
        REP     MOVSB
@@exit:
        POP     EDI
        POP     ESI
end;

procedure AssignLongStr(var Dest: string; const Source: string);
begin
  Dest := Source;
end;

procedure SetLongStrProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: string); assembler;
asm
        { ->    EAX Pointer to instance         }
        {       EDX Pointer to property info    }
        {       ECX Pointer to string value     }

        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EDX

        MOV     EDX,[ESI].TPropInfo.Index       { pass index in EDX }
        CMP     EDX,$80000000
        JNE     @@hasIndex
        MOV     EDX,ECX                         { pass value in EDX }
@@hasIndex:
        MOV     EDI,[ESI].TPropInfo.SetProc
        CMP     [ESI].TPropInfo.SetProc.Byte[3],$FE
        JA      @@isField
        JB      @@isStaticMethod

@@isVirtualMethod:
        MOVSX   EDI,DI
        ADD     EDI,[EAX]
        CALL    DWORD PTR [EDI]
        JMP     @@exit

@@isStaticMethod:
        CALL    EDI
        JMP     @@exit

@@isField:
        AND  EDI,$00FFFFFF
        ADD  EAX,EDI
        MOV  EDX,ECX
        CALL  AssignLongStr

@@exit:
        POP     EDI
        POP     ESI
end;

procedure SetShortStrPropAsLongStr(Instance: TObject; PropInfo: PPropInfo;
  const Value: string);
var
  Temp: ShortString;
begin
  Temp := Value;
  SetShortStrProp(Instance, PropInfo, Temp);
end;

procedure SetWideStrPropAsLongStr(Instance: TObject; PropInfo: PPropInfo;
  const Value: string);
var
  Temp: WideString;
begin
  Temp := Value;
  SetWideStrProp(Instance, PropInfo, Temp);
end;

procedure SetStrProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: string);
begin    // helper functions minimize temps in general case
  case PropInfo^.PropType^.Kind of
    tkString: SetShortStrPropAsLongStr(Instance, PropInfo, Value);
    tkLString, tkUString: SetLongStrProp(Instance, PropInfo, Value);
    tkWString: SetWideStrPropAsLongStr(Instance, PropInfo, Value);
  end;
end;
function FindPropInfo(Instance: TObject; const PropName: string): PPropInfo; overload;
begin
  Result := GetPropInfo(Instance, PropName);
  if Result = nil then
    raise Exception.Create('Property Not Found: '+PropName);
end;

function IncMinute(const AValue: TDateTime;
  const ANumberOfMinutes: Int64): TDateTime;
begin
  Result := ((AValue * MinsPerDay) + ANumberOfMinutes) / MinsPerDay;
end;

{$ENDIF}

function CharPos(c : Char; str : string) : integer;
var
  i, iLen : integer;
begin
  Result := 0;
  iLen := Length(str);
  for i := 1 to iLen do
  begin
    if str[i]=c then
    begin
      result := i;
      exit;
    end;
  end;
end;

end.
 
