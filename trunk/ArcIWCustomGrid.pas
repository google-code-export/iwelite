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

unit ArcIWCustomGrid;

// TODO:
//  StringGrid
//  FileGrid
//  DBGrid
//  DBLookupGrid
//  DBPanelGrid


interface

{$I IntraWebVersion.inc}

{$I Eval.inc}

uses
  SysUtils, Classes, Controls, IWControl, IWHTMLTag, IWColor, ArcIWEliteResources,
  {$IFDEF INTRAWEB72}IWVCLBaseControl, IWBaseControl, IWBaseHTMLControl, IWApplication, IWRenderContext, IWTypes,{$ENDIF}
  {$IFDEF VER130}TypInfo,{$ENDIF}ArcIWGridCommon, Graphics, ArcCommon;

type
  TArcIWStringGridContentBase = class(TComponent)
  private
    FActive : boolean;
  protected
    FSelectedDataRow : integer;
    function GetActive: boolean; virtual;
    procedure SetActive(const Value: boolean); virtual;
    procedure SetSelectedDataRow(const Value : integer); virtual;
    procedure UpdateNavButtonState; virtual; abstract;
    function GetIsFirstPage: boolean; virtual; abstract;
    function GetIsFirstRecord: boolean; virtual; abstract;
    function GetIsPriorRecord: boolean; virtual; abstract;
    function GetIsNextRecord: boolean; virtual; abstract;
    function GetIsLastPage: boolean; virtual; abstract;
    function GetIsLastRecord: boolean; virtual; abstract;
  public
    property IsLastPage : boolean read GetIsLastPage;
    property IsFirstRecord : boolean read GetIsFirstRecord;
    property IsLastRecord : boolean read GetIsLastRecord;
    property IsFirstPage : boolean read GetIsFirstPage;
    property IsPriorRecord : boolean read GetIsPriorRecord;
    property IsNextRecord : boolean read GetIsNextRecord;

    property Active : boolean read GetActive write SetActive default true;
    procedure Append; virtual; abstract;
    procedure Edit; virtual; abstract;
    procedure Delete; virtual; abstract;
    procedure Post; virtual; abstract;
    procedure Cancel; virtual; abstract;

    procedure FirstPage; virtual; abstract;
    procedure NextPage; virtual; abstract;
    procedure PriorPage; virtual; abstract;
    procedure LastPage; virtual; abstract;

    procedure First; virtual; abstract;
    procedure Prior; virtual; abstract;
    procedure Next; virtual; abstract;
    procedure Last; virtual; abstract;
    procedure Refresh; virtual; abstract;

    procedure DoAssignGrid(Sender : TObject); virtual; abstract;
    procedure DoResizeGrid(Sender : TObject; Cols, Rows : integer); virtual; abstract;
    procedure DoReloadData(Sender : TObject) ; virtual; abstract;
    procedure DoClickCaption(Sender : TObject; const Col : integer); virtual; abstract;
    procedure DoRenameColumn(Sender : TObject; OldName, NewName : String); virtual; abstract;
    procedure DoRowClick(Sender : TObject; const Row : integer); virtual; abstract;
    procedure DoCellClick(Sender : TObject; const Col, Row : integer; var Data : string); virtual; abstract;
    procedure DoRender(Sender : TObject); virtual; abstract;
    procedure DoSelectCell(Sender : TObject; const x,y : integer; Value : boolean); virtual; abstract;
    procedure DoSelectRow(Sender : TObject; const y : integer; Value : boolean); virtual; abstract;
    procedure DoSelectCol(Sender : TObject; const x : integer; Value : boolean); virtual; abstract;
    procedure DoSubmit(Sender : TObject; const AValue : string); virtual; abstract;
    procedure DoBeforeRenderHTML(Sender : TObject; AContext: TIWBaseHTMLComponentContext); virtual; abstract;
    procedure DoPopulateRow(Sender : TObject; const y : integer; slValues : TStrings); virtual; abstract;
    function DoNeedOnClickEvent(Sender : TObject) : boolean; virtual; abstract;
    function DoNeedStyleOverride(Sender : TObject) : boolean; virtual; abstract;
    procedure DoCellData(Sender : TObject; x, y : integer; var sValue : string); virtual; abstract;
    procedure DoRetrieveObject(Sender : TObject; x,y : integer; var ctrl : TControl); virtual; abstract;
    procedure DoRetrieveCBObject(Sender : TObject; x : integer; var ctrl : TControl); virtual; abstract;
    procedure DoAfterRenderHTML(Sender : TObject); virtual; abstract;
    procedure DoAssignSession(Session : TIWApplication); virtual; abstract;
    function DoNeedScript : string; virtual;
    procedure DoOverrideCellStyle(Sender : TObject; const col : integer; const Row : Integer; Style : TArcGridStyle); virtual; abstract;
    property SelectedDataRow : integer read FSelectedDataRow write SetSelectedDataRow;
    function ProcessCaption(str : string) : string; virtual; abstract;
    constructor Create(AOwner: TComponent); override;
  end;

  TArcIWCustomGrid = class(TIWControl)
  private
  protected
    //FStaticHeader: boolean;
    FShowHeaderRow : boolean;
    FStyleTable: TArcGridStyle;
    FStyleHeader: TArcGridStyle;
    FStyleDetail: TArcGridStyle;
    FStyleDetailAlt: TArcGridStyle;
    FUseAltStyles: boolean;
    FRowCount: Integer;
    FAutoRowHeight: boolean;
    FAutoColumnWidth: boolean;
    procedure SetRowCount(const Value: Integer); virtual;
    procedure SetAutoColumnWidth(const Value : boolean); virtual;
    property AutoColumnWidth : boolean read FAutoColumnWidth write SetAutoColumnWidth;
    property AutoRowHeight : boolean read FAutoRowHeight write FAutoRowHeight;
    property StyleHeader : TArcGridStyle read FStyleHeader write FStyleHeader;
    property StyleDetail : TArcGridStyle read FStyleDetail write FStyleDetail;
    property StyleDetailAlt : TArcGridStyle read FStyleDetailAlt write FStyleDetailAlt;
    property StyleTable : TArcGridStyle read FStyleTable write FStyleTable;
    property UseAltStyles : boolean read FUseAltStyles write FUseAltStyles;
    property RowCount : integer read FRowCount write SetRowCount;
    property ShowHeaderRow : boolean read FShowHeaderRow write FShowHeaderRow;
    //property StaticHeader : boolean read FStaticHeader write FStaticHeader;
    procedure Submit(const AValue: String); override;
  public
    {$IFNDEF INTRAWEB72}
    function RenderHTML : TIWHTMLTag; override;
    {$ELSE}
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFNDEF INTRAWEB72}
    function SupportsSubmit: Boolean; override;
    {$ENDIF}
  published
  end;

{$IFDEF VER130}
function IfThen(AValue: Boolean; const ATrue: Integer; const AFalse: Integer): Integer; overload;
function IfThen(AValue: Boolean; const ATrue: string; AFalse: string = ''): string; overload;
function GetPropList(AObject: TObject; out PropList: PPropList): Integer;
{$ENDIF}


implementation

{$IFNDEF VER130}uses StrUtils, Math, MaskUtils;{$ENDIF}

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
    tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray, tkUString];

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
    tkLString,tkUString: Result := GetStrProp(Instance, PropInfo);
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

function IfThen(AValue: Boolean; const ATrue: Integer; const AFalse: Integer): Integer;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;
function IfThen(AValue: Boolean; const ATrue: string; AFalse: string = ''): string; 
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;
{$ENDIF}


{ TArcIWStringGridContentBase }

constructor TArcIWStringGridContentBase.Create(AOwner: TComponent);
begin
  inherited;
  FActive := True;
end;

function TArcIWStringGridContentBase.DoNeedScript: string;
begin
  Result := '';
end;

function TArcIWStringGridContentBase.GetActive: boolean;
begin
  result := FActive;
end;

procedure TArcIWStringGridContentBase.SetActive(const Value: boolean);
begin
  FActive := Value;
end;

procedure TArcIWStringGridContentBase.SetSelectedDataRow(const Value: integer);
begin
  FSelectedDataRow := Value;
  UpdateNavButtonState;
end;

{ TArcIWCustomGrid }

constructor TArcIWCustomGrid.Create(AOwner: TComponent);
begin
  inherited;
  Width := 300;
  Height := 200;

  {$IFDEF INTRAWEB72}
  SupportsPartial := True;
  {$ENDIF}

  //FStaticHeader := False;
  FShowHeaderRow    := True;
  FStyleTable     := TArcGridStyle.Create(True,True,True,True);
  FStyleHeader    := TArcGridStyle.Create(False,True,False,True);
  FStyleDetail    := TArcGridStyle.Create(False,True,False,True);
  FStyleDetailAlt := TArcGridStyle.Create(False,True,False,True);

  FStyleHeader.TextAlign := taCenter;
  FStyleDetail.TextAlign := taLeftJustify;
  FStyleDetailAlt.TextAlign := taLeftJustify;
  FStyleHeader.TextVertAlign := vaMiddle;
  FStyleDetail.TextVertAlign := vaMiddle;
  FStyleDetailAlt.TextVertAlign := vaMiddle;

  FRowCount := 5;
  FAutoColumnWidth := True;

  FStyleTable.BackgroundColor := clWebWHITE;
  FStyleTable.BorderStyle.Style := brdSolid;
  FStyleTable.BorderStyle.Width := 1;
  FStyleTable.BorderStyle.Color := clWebBlack;

  FStyleHeader.BackgroundColor := $00800000;
  FStyleHeader.BorderStyle.Style := brdSolid;
  FStyleHeader.BorderStyle.Width := 1;
  FStyleHeader.BorderStyle.Color := clWebBlack;
  FStyleHeader.Font.Color := clWebWhite;
  FStyleHeader.Font.Style := [fsBold];
  FStyleHeader.Font.FontName := 'Arial';
  FStyleHeader.Font.Size := 10;
  FStyleHeader.Padding := 2;

  FStyleDetail.BackgroundColor := clWebWHITE;
  FStyleDetail.BorderStyle.Style := brdSolid;
  FStyleDetail.BorderStyle.Width := 1;
  FStyleDetail.BorderStyle.Color := clWebBlack;
  FStyleDetail.Font.Color := clWebBlack;
  FStyleDetail.Font.Style := [];
  FStyleDetail.Font.FontName := 'Arial';
  FStyleDetail.Font.Size := 10;
  FStyleDetail.Padding := 2;

  FStyleDetailAlt.BackgroundColor := $00FFCECE;
  FStyleDetailAlt.BorderStyle.Style := brdSolid;
  FStyleDetailAlt.BorderStyle.Width := 1;
  FStyleDetailAlt.BorderStyle.Color := clWebBlack;
  FStyleDetailAlt.Font.Color := clWebBlack;
  FStyleDetailAlt.Font.Style := [];
  FStyleDetailAlt.Font.FontName := 'Arial';
  FStyleDetailAlt.Font.Size := 10;
  FStyleDetailAlt.Padding := 2;
end;

destructor TArcIWCustomGrid.Destroy;
begin
  FStyleTable.Free;
  FStyleHeader.Free;
  FStyleDetail.Free;
  FStyleDetailAlt.Free;
  inherited;
end;

procedure TArcIWCustomGrid.SetAutoColumnWidth(const Value : boolean); 
begin
  FAutoColumnWidth := Value;
end;

{$IFNDEF INTRAWEB72}
function TArcIWCustomGrid.SupportsSubmit: Boolean;
begin
  Result := True;
end;
{$ENDIF}

{$IFNDEF INTRAWEB72}
function TArcIWCustomGrid.RenderHTML : TIWHTMLTag;
{$ELSE}
function TArcIWCustomGrid.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  raise Exception.Create('Do Not Call Inherited on TArcIWStringGrid');
end;

procedure TArcIWCustomGrid.SetRowCount(const Value: Integer);
begin
  FRowCount := Value;
  if Value < 0 then
    FRowCount := 0;
end;

procedure TArcIWCustomGrid.Submit(const AValue: String);
begin
  inherited;
  // To Be overridden;
end;

end.
