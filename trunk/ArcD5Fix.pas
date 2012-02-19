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

unit ArcD5Fix;

interface

uses Windows, Classes, SysUtils;

type

{$IFDEF CLR}
  TD7StringList = TStringList;
{$ELSE}
{$IFDEF VER150}

  TD7StringList = TStringList;

{$ELSE}

  TStringsDefined = set of (sdDelimiter, sdQuoteChar);
  TD7StringList = class(TStringList)
  private
    FDefined: TStringsDefined;
    FDelimiter: Char;
    FQuoteChar: Char;
    function GetDelimiter : Char;
    procedure SetDelimiter(const value : char);
    function GetDelimitedText : string;
    procedure SetDelimitedText(const value : string);
    function GetQuoteChar : Char;
    procedure SetQuoteChar(const value : char);
    function GetValueByIndex(const idx : integer) : string;
    procedure SetValueByIndex(const idx : integer; const value : string);
  protected
  public
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    property QuoteChar: Char read GetQuoteChar write SetQuoteChar;
    property ValueByIndex[const idx : integer]: string read GetValueByIndex write SetValueByIndex;
  end;

function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean;
function TryEncodeTime(Hour, Min, Sec, MSec: Word; out Time: TDateTime): Boolean;
function TryEncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
  AMilliSecond: Word; out AValue: TDateTime): Boolean;
function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
  AMilliSecond: Word): TDateTime;
function TryStrToDateTime(const S: string; out Value: TDateTime): Boolean;
function DirectoryExists(const Directory : string) : Boolean;
function ForceDirectories(Dir: string) : Boolean;
var
  TrueBoolStrs: array of String;
  FalseBoolStrs: array of String;

const
  DefaultTrueBoolStr = 'True';   // DO NOT LOCALIZE
  DefaultFalseBoolStr = 'False'; // DO NOT LOCALIZE

function StrToBool(const S: string): Boolean;
function StrToBoolDef(const S: string; const Default: Boolean): Boolean;
function TryStrToBool(const S: string; out Value: Boolean): Boolean;

function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
{$ENDIF}
{$ENDIF}

const
  PathDelim = '\';

implementation

{$IFNDEF VER150}
{$IFNDEF CLR}
function ExcludeTrailingPathDelimiter(const S: string): string;
begin
  Result := S;
  if Result[Length(Result)]='\' then
    SetLength(Result, Length(Result)-1);
end;

function ForceDirectories(Dir: string) : Boolean;
begin
  Result := True;
  if Length(Dir) = 0 then
    raise Exception.Create('Cannot create directory');
  Dir := ExcludeTrailingPathDelimiter(Dir);
  if (Length(Dir) < 3) or DirectoryExists(Dir)
    or (ExtractFilePath(Dir) = Dir) then Exit; // avoid 'xyz:\' problem.
  Result := ForceDirectories(ExtractFilePath(Dir)) and CreateDir(Dir);
end;

function DirectoryExists(const Directory : string) : Boolean;
var
  code : Integer;
begin
  {$IFNDEF CLR}
  Code := GetFileAttributes(PChar(Directory));
  {$ELSE}
  Code := GetFileAttributes(Directory);
  {$ENDIF}
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean;
var
  I: Integer;
  DayTable: PDayTable;
begin
  Result := False;
  DayTable := @MonthDays[IsLeapYear(Year)];
  if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and
    (Day >= 1) and (Day <= DayTable^[Month]) then
  begin
    for I := 1 to Month - 1 do Inc(Day, DayTable^[I]);
    I := Year - 1;
    Date := I * 365 + I div 4 - I div 100 + I div 400 + Day - DateDelta;
    Result := True;
  end;
end;

function TryEncodeTime(Hour, Min, Sec, MSec: Word; out Time: TDateTime): Boolean;
begin
  Result := False;
  if (Hour < 24) and (Min < 60) and (Sec < 60) and (MSec < 1000) then
  begin
    Time := (Hour * 3600000 + Min * 60000 + Sec * 1000 + MSec) / MSecsPerDay;
    Result := True;
  end;
end;

function TryEncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
  AMilliSecond: Word; out AValue: TDateTime): Boolean;
var
  LTime: TDateTime;
begin
  Result := TryEncodeDate(AYear, AMonth, ADay, AValue);
  if Result then
  begin
    Result := TryEncodeTime(AHour, AMinute, ASecond, AMilliSecond, LTime);
    if Result then
      AValue := AValue + LTime;
  end;
end;

function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
  AMilliSecond: Word): TDateTime;
begin
  if not TryEncodeDateTime(AYear, AMonth, ADay,
                           AHour, AMinute, ASecond, AMilliSecond, Result) then
    raise Exception.Create('Invalid date time supplied.');
end;

function TryStrToDateTime(const S: string; out Value: TDateTime): Boolean;
begin
  Result := True;
  try
    Value := StrToDateTime(s);
  except
    Result := False;
  end;
end;

{ TD7StringList }

function TD7StringList.GetDelimiter : Char;
begin
  if not (sdDelimiter in FDefined) then
    Delimiter := ',';
  Result := FDelimiter;
end;

procedure TD7StringList.SetDelimiter(const value : char);
begin
  if (FDelimiter <> Value) or not (sdDelimiter in FDefined) then
  begin
    Include(FDefined, sdDelimiter);
    FDelimiter := Value;
  end
end;

function TD7StringList.GetDelimitedText : string;
var
  S: string;
  P: PChar;
  I, Count: Integer;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := QuoteChar + QuoteChar
  else
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PChar(S);
      while not CharInSet(P^,[#0..' ', QuoteChar, Delimiter]) do
        P := CharNext(P);
      if (P^ <> #0) then S := AnsiQuotedStr(S, QuoteChar);
      Result := Result + S + Delimiter;
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

procedure TD7StringList.SetDelimitedText(const value : string);
var
  P, P1: PChar;
  S: string;
begin
  BeginUpdate;
  try
    Clear;
    P := PChar(Value);
    while CharInSet(P^,[#1..' ']) do
      P := CharNext(P);
    while P^ <> #0 do
    begin
      if P^ = QuoteChar then
        S := AnsiExtractQuotedStr(P, QuoteChar)
      else
      begin
        P1 := P;
        while (P^ > ' ') and (P^ <> Delimiter) do
          P := CharNext(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);
      while CharInSet(P^,[#1..' ']) do
        P := CharNext(P);
      if P^ = Delimiter then
      begin
        P1 := P;
        if CharNext(P1)^ = #0 then
          Add('');
        repeat
          P := CharNext(P);
        until not CharInSet(P^,[#1..' ']);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

function TD7StringList.GetQuoteChar : Char;
begin
  if not (sdQuoteChar in FDefined) then
    QuoteChar := '"';
  Result := FQuoteChar;
end;

procedure TD7StringList.SetQuoteChar(const value : char);
begin
  if (FQuoteChar <> Value) or not (sdQuoteChar in FDefined) then
  begin
    Include(FDefined, sdQuoteChar);
    FQuoteChar := Value;
  end;
end;

function TD7StringList.GetValueByIndex(const idx : integer) : string;
begin
  Result := Values[Names[idx]];
end;

procedure TD7StringList.SetValueByIndex(const idx : integer; const value : string);
begin
  Values[Names[idx]] := Value;
end;

procedure VerifyBoolStrArray;
begin
  if Length(TrueBoolStrs) = 0 then
  begin
    SetLength(TrueBoolStrs, 1);
    TrueBoolStrs[0] := DefaultTrueBoolStr;
  end;
  if Length(FalseBoolStrs) = 0 then
  begin
    SetLength(FalseBoolStrs, 1);
    FalseBoolStrs[0] := DefaultFalseBoolStr;
  end;
end;

function StrToBool(const S: string): Boolean;
begin
  if not TryStrToBool(S, Result) then
    raise Exception.create('Invalid Boolean string.');
end;

function StrToBoolDef(const S: string; const Default: Boolean): Boolean;
begin
  if not TryStrToBool(S, Result) then
    Result := Default;
end;

function TryStrToBool(const S: string; out Value: Boolean): Boolean;
  function CompareWith(const aArray: array of string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Low(aArray) to High(aArray) do
      if AnsiSameText(S, aArray[I]) then
      begin
        Result := True;
        Break;
      end;
  end;
begin
  Result := false;
  try
    Value := StrToint(S) <> 0;
    Result := true;
  except
  end;
  if not result then
  begin
    VerifyBoolStrArray;
    Result := CompareWith(TrueBoolStrs);
    if Result then
      Value := True
    else
    begin
      Result := CompareWith(FalseBoolStrs);
      if Result then
        Value := False;
    end;
  end;
end;

function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
const
  cSimpleBoolStrs: array [boolean] of String = ('0', '-1');
begin
  if UseBoolStrs then
  begin
    VerifyBoolStrArray;
    if B then
      Result := TrueBoolStrs[0]
    else
      Result := FalseBoolStrs[0];
  end
  else
    Result := cSimpleBoolStrs[B];
end;
{$ENDIF}
{$ENDIF}
end.
