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

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  unit ArcPersistentStream                                                  //
//    Copyright 2002 by Arcana Technologies Incorporated                      //
//    Written By Jason Southwell                                              //
//                                                                            //
//  Description:                                                              //
//    TPersistentStream is an implementation of an advanced stream object.    //
//    It derives from TPersistent instead of TStream which allows it to be    //
//    published and written out by Delphi's streaming system.  It also has    //
//    several additional functions which TStream should have had anyway;      //
//    specifically functions for reading and writing base delphi types.  Also //
//    this stream provides you access to both memory and datastring properties//
//    as well as a new bytes property which allows you access to the stream as//
//    a byte array.                                                           //
//                                                                            //
//    Also, I have built in fast find functionality using Boyer-Moore         //
//    so finding strings or characters within the stream is extremely fast.   //
//    The internal functions that begin with an _ are copyrighted by Peter    //
//    Morris and were taken, with his permission, from his FastStrings        //
//    library.                                                                //
//                                                                            //
//    Note that since this object does not inherit from TStream, you cannot   //
//    use it directly in cases where a TStream is required, such as procedure //
//    parameters.  However, there is a readonly property of the object that   //
//    exposes the internal memory stream.   You can use this property in those//
//    cases where a TStream is needed.                                        //
//                                                                            //
//    TPersistentStream should be compatible with both Delphi and Kylix.      //
//                                                                            //
//  Updates:                                                                  //
//    09/19/2002 - Finished initial release of ArcPersistentStream            //
//                                                                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
unit ArcPersistentStream;

interface

uses SysUtils, Classes;

type
  TBMJumpTable = array[0..255] of Integer;
  TPersistentStream = class(TPersistent)
  private
    FStream : TStream;
    function GetSize: Int64;
    function GetPosition: Int64;
    procedure SetPosition(const Value: Int64);
    procedure SetSize(const Value: Int64);
    function _FastCharPos(const aSource: String; const C: Char; SourceLen,
      StartPos: Integer): Integer;
    function _FastCharPosNoCase(const aSource: String; C: Char; SourceLen,
      StartPos: Integer): Integer;
    function _FastPos(const aSourceString, aFindString : String; const aSourceLen, aFindLen, StartPos : Integer) : Integer;
    function _FastPosNoCase(const aSourceString, aFindString : String; const aSourceLen, aFindLen, StartPos : Integer) : Integer;
    function _FastPosBack(const aSourceString, aFindString : String; const aSourceLen, aFindLen, StartPos : Integer) : Integer;
    function _FastPosBackNoCase(const aSourceString, aFindString : String; const aSourceLen, aFindLen, StartPos : Integer) : Integer;
    procedure _MakeBMTable(Buffer: PChar; BufferLen: Integer; var JumpTable: TBMJumpTable);
    procedure _MakeBMTableNoCase(Buffer: PChar; BufferLen: Integer; var JumpTable: TBMJumpTable);
    function _BMPos(const aSource, aFind: Pointer; const aSourceLen, aFindLen: Integer; var JumpTable: TBMJumpTable): Pointer;
    function _BMPosNoCase(const aSource, aFind: Pointer; const aSourceLen, aFindLen: Integer; var JumpTable: TBMJumpTable): Pointer;
    function GetBytes: PByteArray;
  protected
    function GetDataString: string; virtual;
    function GetMemory: Pointer; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure LoadCompProperty(Reader: TStream); virtual;
    procedure StoreCompProperty(Writer: TStream); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; virtual;
    function Write(const Buffer; Count: Longint): Longint; virtual;
    function Seek(Offset: Longint; Origin: Word): Longint; {$IFNDEF VER130}overload; {$ENDIF}virtual;
    {$IFNDEF VER130}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; virtual;
    {$ENDIF}
    procedure ReadBuffer(var Buffer; Count: Longint); virtual;
    procedure WriteBuffer(const Buffer; Count: Longint); virtual;

    function CopyFrom(Source: TStream; Count: Int64): Int64; virtual;

    function ReadComponent(Instance: TComponent): TComponent; virtual;
    function ReadComponentRes(Instance: TComponent): TComponent; virtual;
    procedure WriteComponent(Instance: TComponent); virtual;
    procedure WriteComponentRes(const ResName: string; Instance: TComponent); virtual;
    procedure WriteDescendent(Instance, Ancestor: TComponent); virtual;
    procedure WriteDescendentRes(const ResName: string; Instance, Ancestor: TComponent); virtual;
    procedure WriteResourceHeader(const ResName: string; out FixupInfo: Integer); virtual;
    procedure FixupResourceHeader(FixupInfo: Integer); virtual;
    procedure ReadResHeader; virtual;

    property Position: Int64 read GetPosition write SetPosition;
    property Size : Int64 read GetSize write SetSize;

    procedure LoadFromStream(Stream: TStream); virtual;
    procedure LoadFromFile(const FileName: string); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SaveToFile(const FileName: string); virtual;

    procedure WriteString(const AString: string; const Len : Integer=-1); virtual;
    procedure WriteInteger(const Value : Integer); overload; virtual;
    procedure WriteInteger(const Value : Byte); overload; virtual;
    procedure WriteInteger(const Value : Word); overload; virtual;
    procedure WriteInteger(const Value : Cardinal); overload; virtual;
    procedure WriteInteger(const Value : Int64); overload; virtual;
    procedure WriteInteger(const Value : ShortInt); overload; virtual;
    procedure WriteInteger(const Value : SmallInt); overload; virtual;
    {$IFNDEF BCB}
    procedure WriteFloat(const Value : Real48); overload; virtual;
    {$ENDIF}
    procedure WriteFloat(const Value : Single); overload; virtual;
    procedure WriteFloat(const Value : Double); overload; virtual;
    procedure WriteFloat(const Value : Extended); overload; virtual;
    procedure WriteFloat(const Value : Comp); overload; virtual;
    procedure WriteFloat(const Value : Currency); overload; virtual;
    procedure WriteBoolean(const Value : Boolean); virtual;

    function ReadString(const len : Integer) : string; virtual;
    function ReadInteger : integer; overload; virtual;
    procedure ReadInteger(var Value : Integer); overload; virtual;
    procedure ReadInteger(var Value : Byte); overload; virtual;
    procedure ReadInteger(var Value : Word); overload; virtual;
    procedure ReadInteger(var Value : Cardinal); overload; virtual;
    procedure ReadInteger(var Value : Int64); overload; virtual;
    procedure ReadInteger(var Value : ShortInt); overload; virtual;
    procedure ReadInteger(var Value : SmallInt); overload; virtual;
    {$IFNDEF BCB}
    procedure ReadFloat(var Value : Real48); overload; virtual;
    {$ENDIF}
    procedure ReadFloat(var Value : Single); overload; virtual;
    procedure ReadFloat(var Value : Double); overload; virtual;
    procedure ReadFloat(var Value : Extended); overload; virtual;
    procedure ReadFloat(var Value : Comp); overload; virtual;
    procedure ReadFloat(var Value : Currency); overload; virtual;
    function ReadBoolean : Boolean; virtual;

    function FindNextMatch(SubStr : string; CaseInsensitive : boolean=False;
      FromStart : boolean=True) : Int64; overload; virtual;
    function FindNextMatch(c : Char; CaseInsensitive : boolean=False;
      FromStart : Boolean=True) : Int64; overload; virtual;
    function FindPriorMatch(SubStr : string; CaseInsensitive : boolean=False;
      FromEnd: boolean=True) : Int64; virtual;

    property Stream : TStream read FStream;
    property DataString : string read GetDataString;
    property Memory     : Pointer read GetMemory;
    property Bytes      : PByteArray read GetBytes;
  end;

implementation

const
  cDeltaSize = 1.5;
var
  GUpcaseTable : array[0..255] of char;
  GUpcaseLUT: Pointer;


{ TPersistentStream }

function TPersistentStream._FastPos(const aSourceString, aFindString : String; const aSourceLen, aFindLen, StartPos : Integer) : Integer;
var
  JumpTable: TBMJumpTable;
begin
  //If this assert failed, it is because you passed 0 for StartPos, lowest value is 1 !!
  Assert(StartPos > 0);

  _MakeBMTable(PChar(aFindString), aFindLen, JumpTable);
  Result := Integer(_BMPos(PChar(aSourceString) + (StartPos - 1), PChar(aFindString),aSourceLen - (StartPos-1), aFindLen, JumpTable));
  if Result > 0 then
    Result := Result - Integer(@aSourceString[1]) +1;
end;

function TPersistentStream._FastPosNoCase(const aSourceString, aFindString : String; const aSourceLen, aFindLen, StartPos : Integer) : Integer;
var
  JumpTable: TBMJumpTable;
begin
  //If this assert failed, it is because you passed 0 for StartPos, lowest value is 1 !!
  Assert(StartPos > 0);

  _MakeBMTableNoCase(PChar(AFindString), aFindLen, JumpTable);
  Result := Integer(_BMPosNoCase(PChar(aSourceString) + (StartPos - 1), PChar(aFindString),aSourceLen - (StartPos-1), aFindLen, JumpTable));
  if Result > 0 then
    Result := Result - Integer(@aSourceString[1]) +1;
end;

function TPersistentStream._FastPosBack(const aSourceString, aFindString : String; const aSourceLen, aFindLen, StartPos : Integer) : Integer;
var
  SourceLen : Integer;
begin
  if aFindLen < 1 then begin
    Result := 0;
    exit;
  end;
  if aFindLen > aSourceLen then begin
    Result := 0;
    exit;
  end;

  if (StartPos = 0) or  (StartPos + aFindLen > aSourceLen) then
    SourceLen := aSourceLen - (aFindLen-1)
  else
    SourceLen := StartPos;

  asm
          push ESI
          push EDI
          push EBX

          mov EDI, aSourceString
          add EDI, SourceLen
          Dec EDI

          mov ESI, aFindString
          mov ECX, SourceLen
          Mov  Al, [ESI]

    @ScaSB:
          cmp  Al, [EDI]
          jne  @NextChar

    @CompareStrings:
          mov  EBX, aFindLen
          dec  EBX
          jz   @FullMatch

    @CompareNext:
          mov  Ah, [ESI+EBX]
          cmp  Ah, [EDI+EBX]
          Jnz  @NextChar

    @Matches:
          Dec  EBX
          Jnz  @CompareNext

    @FullMatch:
          mov  EAX, EDI
          sub  EAX, aSourceString
          inc  EAX
          mov  Result, EAX
          jmp  @TheEnd
    @NextChar:
          dec  EDI
          dec  ECX
          jnz  @ScaSB

          mov  Result,0

    @TheEnd:
          pop  EBX
          pop  EDI
          pop  ESI
  end;
end;


function TPersistentStream._FastPosBackNoCase(const aSourceString, aFindString : String; const aSourceLen, aFindLen, StartPos : Integer) : Integer;
var
  SourceLen : Integer;
begin
  if aFindLen < 1 then begin
    Result := 0;
    exit;
  end;
  if aFindLen > aSourceLen then begin
    Result := 0;
    exit;
  end;

  if (StartPos = 0) or  (StartPos + aFindLen > aSourceLen) then
    SourceLen := aSourceLen - (aFindLen-1)
  else
    SourceLen := StartPos;

  asm
          push ESI
          push EDI
          push EBX

          mov  EDI, aSourceString
          add  EDI, SourceLen
          Dec  EDI

          mov  ESI, aFindString
          mov  ECX, SourceLen

          mov  EDX, GUpcaseLUT
          xor  EBX, EBX

          mov  Bl, [ESI]
          mov  Al, [EDX+EBX]

    @ScaSB:
          mov  Bl, [EDI]
          cmp  Al, [EDX+EBX]
          jne  @NextChar

    @CompareStrings:
          PUSH ECX
          mov  ECX, aFindLen
          dec  ECX
          jz   @FullMatch

    @CompareNext:
          mov  Bl, [ESI+ECX]
          mov  Ah, [EDX+EBX]
          mov  Bl, [EDI+ECX]
          cmp  Ah, [EDX+EBX]
          Jz   @Matches

    //Go back to findind the first char
          POP  ECX
          Jmp  @NextChar

    @Matches:
          Dec  ECX
          Jnz  @CompareNext

    @FullMatch:
          POP  ECX

          mov  EAX, EDI
          sub  EAX, aSourceString
          inc  EAX
          mov  Result, EAX
          jmp  @TheEnd
    @NextChar:
          dec  EDI
          dec  ECX
          jnz  @ScaSB

          mov  Result,0

    @TheEnd:
          pop  EBX
          pop  EDI
          pop  ESI
  end;
end;

function TPersistentStream._FastCharPos(const aSource : String; const C: Char; SourceLen : Integer; StartPos : Integer) : Integer;
var
  L                           : Integer;
begin
  //If this assert failed, it is because you passed 0 for StartPos, lowest value is 1 !!
  Assert(StartPos > 0);

  Result := 0;
  L := SourceLen;
  if L = 0 then exit;
  if StartPos > L then exit;
  Dec(StartPos);
  asm
      PUSH EDI                 //Preserve this register

      mov  EDI, aSource        //Point EDI at aSource
      add  EDI, StartPos
      mov  ECX, L              //Make a note of how many chars to search through
      sub  ECX, StartPos
      mov  AL,  Byte(C)              //and which char we want
    @Loop:
      cmp  Al, [EDI]           //compare it against the SourceString
      jz   @Found
      inc  EDI
      dec  ECX
      jnz  @Loop
      jmp  @NotFound
    @Found:
      sub  EDI, aSource        //EDI has been incremented, so EDI-OrigAdress = Char pos !
      inc  EDI
      mov  Result,   EDI
    @NotFound:

      POP  EDI
  end;
end;

procedure TPersistentStream._MakeBMTable(Buffer: PChar; BufferLen: Integer; var JumpTable: TBMJumpTable);
begin
  if BufferLen = 0 then raise Exception.Create('BufferLen is 0');
  asm
        push    EDI
        push    ESI

        mov     EDI, JumpTable
        mov     EAX, BufferLen
        mov     ECX, $100
        REPNE   STOSD

        mov     ECX, BufferLen
        mov     EDI, JumpTable
        mov     ESI, Buffer
        dec     ECX
        xor     EAX, EAX
@@loop:
        mov     AL, [ESI]
        lea     ESI, ESI + 1
        mov     [EDI + EAX * 4], ECX
        dec     ECX
        jg      @@loop

        pop     ESI
        pop     EDI
  end;
end;

procedure TPersistentStream._MakeBMTableNoCase(Buffer: PChar; BufferLen: Integer; var JumpTable: TBMJumpTable);
begin
  if BufferLen = 0 then raise Exception.Create('BufferLen is 0');
  asm
        push    EDI
        push    ESI

        mov     EDI, JumpTable
        mov     EAX, BufferLen
        mov     ECX, $100
        REPNE   STOSD

        mov     EDX, GUpcaseLUT
        mov     ECX, BufferLen
        mov     EDI, JumpTable
        mov     ESI, Buffer
        dec     ECX
        xor     EAX, EAX
@@loop:
        mov     AL, [ESI]
        lea     ESI, ESI + 1
        mov     AL, [EDX + EAX]
        mov     [EDI + EAX * 4], ECX
        dec     ECX
        jg      @@loop

        pop     ESI
        pop     EDI
  end;
end;

function TPersistentStream._BMPos(const aSource, aFind: Pointer; const aSourceLen, aFindLen: Integer; var JumpTable: TBMJumpTable): Pointer;
var
  LastPos: Pointer;
begin
  LastPos := Pointer(Integer(aSource) + aSourceLen - 1);
  asm
        push    ESI
        push    EDI
        push    EBX

        mov     EAX, aFindLen
        mov     ESI, aSource
        lea     ESI, ESI + EAX - 1
        std
        mov     EBX, JumpTable

@@comparetext:
        cmp     ESI, LastPos
        jg      @@NotFound
        mov     EAX, aFindLen
        mov     EDI, aFind
        mov     ECX, EAX
        push    ESI //Remember where we are
        lea     EDI, EDI + EAX - 1
        xor     EAX, EAX
@@CompareNext:
        mov     al, [ESI]
        cmp     al, [EDI]
        jne     @@LookAhead
        lea     ESI, ESI - 1
        lea     EDI, EDI - 1
        dec     ECX
        jz      @@Found
        jmp     @@CompareNext

@@LookAhead:
        //Look up the char in our Jump Table
        pop     ESI
        mov     al, [ESI]
        mov     EAX, [EBX + EAX * 4]
        lea     ESI, ESI + EAX
        jmp     @@CompareText

@@NotFound:
        mov     Result, 0
        jmp     @@TheEnd
@@Found:
        pop     EDI //We are just popping, we don't need the value
        inc     ESI
        mov     Result, ESI
@@TheEnd:
        cld
        pop     EBX
        pop     EDI
        pop     ESI
  end;
end;

function TPersistentStream._BMPosNoCase(const aSource, aFind: Pointer; const aSourceLen, aFindLen: Integer; var JumpTable: TBMJumpTable): Pointer;
var
  LastPos: Pointer;
begin
  LastPos := Pointer(Integer(aSource) + aSourceLen - 1);
  asm
        push    ESI
        push    EDI
        push    EBX

        mov     EAX, aFindLen
        mov     ESI, aSource
        lea     ESI, ESI + EAX - 1
        std
        mov     EDX, GUpcaseLUT

@@comparetext:
        cmp     ESI, LastPos
        jg      @@NotFound
        mov     EAX, aFindLen
        mov     EDI, aFind
        push    ESI //Remember where we are
        mov     ECX, EAX
        lea     EDI, EDI + EAX - 1
        xor     EAX, EAX
@@CompareNext:
        mov     al, [ESI]
        mov     bl, [EDX + EAX]
        mov     al, [EDI]
        cmp     bl, [EDX + EAX]
        jne     @@LookAhead
        lea     ESI, ESI - 1
        lea     EDI, EDI - 1
        dec     ECX
        jz      @@Found
        jmp     @@CompareNext

@@LookAhead:
        //Look up the char in our Jump Table
        pop     ESI
        mov     EBX, JumpTable
        mov     al, [ESI]
        mov     al, [EDX + EAX]
        mov     EAX, [EBX + EAX * 4]
        lea     ESI, ESI + EAX
        jmp     @@CompareText

@@NotFound:
        mov     Result, 0
        jmp     @@TheEnd
@@Found:
        pop     EDI //We are just popping, we don't need the value
        inc     ESI
        mov     Result, ESI
@@TheEnd:
        cld
        pop     EBX
        pop     EDI
        pop     ESI
  end;
end;

function TPersistentStream._FastCharPosNoCase(const aSource : String; C: Char; SourceLen : Integer; StartPos : Integer) : Integer;
var
  L                           : Integer;
begin
  Result := 0;
  L := SourceLen;
  if L = 0 then exit;
  if StartPos > L then exit;
  Dec(StartPos);
  if StartPos < 0 then StartPos := 0;

  asm
      PUSH EDI                 //Preserve this register
      PUSH EBX
      mov  EDX, GUpcaseLUT

      mov  EDI, aSource        //Point EDI at aSource
      add  EDI, StartPos
      mov  ECX, L              //Make a note of how many chars to search through
      sub  ECX, StartPos

      xor  EBX, EBX
      mov  BL,  Byte(C)
      mov  AL, [EDX+EBX]
    @Loop:
      mov  BL, [EDI]
      inc  EDI
      cmp  Al, [EDX+EBX]
      jz   @Found
      dec  ECX
      jnz  @Loop
      jmp  @NotFound
    @Found:
      sub  EDI, aSource        //EDI has been incremented, so EDI-OrigAdress = Char pos !
      mov  Result,   EDI
    @NotFound:

      POP  EBX
      POP  EDI
  end;
end;

procedure TPersistentStream.LoadCompProperty(Reader: TStream);
begin
  FStream.Size := 0;
  FStream.CopyFrom(Reader,Reader.Size);
  FStream.Position := 0;
end;

procedure TPersistentStream.StoreCompProperty(Writer: TStream);
var
  iPos : Int64;
begin
  iPos := FStream.Position;
  FStream.Position := 0;
  Writer.CopyFrom(FStream,FStream.Size);
  FStream.Position := iPos;
end;

procedure TPersistentStream.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Data', LoadCompProperty, StoreCompProperty, Size>0);
end;

constructor TPersistentStream.Create;
begin
  inherited Create;
  FStream := TMemoryStream.Create;
end;

destructor TPersistentStream.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TPersistentStream.GetSize: Int64;
begin
  Result := FStream.Size;
end;

procedure TPersistentStream.AssignTo(Dest: TPersistent);
begin
  inherited;
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  
end;

function TPersistentStream.CopyFrom(Source: TStream; Count: Int64): Int64;
begin
  Result := FStream.CopyFrom(Source,Count);
end;

procedure TPersistentStream.FixupResourceHeader(FixupInfo: Integer);
begin
  FStream.FixupResourceHeader(FixupInfo);
end;

function TPersistentStream.GetDataString: string;
begin
  SetLength(Result,FStream.Size);
  StrLCopy(PChar(Result),PChar(TMemoryStream(FStream).Memory),FStream.Size);
end;

function TPersistentStream.GetMemory: Pointer;
begin
  Result := TMemoryStream(FStream).Memory;
end;

function TPersistentStream.GetPosition: Int64;
begin
  Result := FStream.Position;
end;

procedure TPersistentStream.LoadFromFile(const FileName: string);
begin
  TMemoryStream(FStream).LoadFromFile(Filename);
end;

procedure TPersistentStream.LoadFromStream(Stream: TStream);
begin
  TMemoryStream(FStream).LoadFromStream(Stream);
end;

function TPersistentStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := FStream.Read(Buffer,Count);
end;

procedure TPersistentStream.ReadBuffer(var Buffer; Count: Integer);
begin
  FStream.ReadBuffer(Buffer,Count);
end;

function TPersistentStream.ReadComponent(Instance: TComponent): TComponent;
begin
  Result := FStream.ReadComponent(Instance);
end;

function TPersistentStream.ReadComponentRes(
  Instance: TComponent): TComponent;
begin
  Result := ReadComponentRes(Instance);
end;

procedure TPersistentStream.ReadResHeader;
begin
  FStream.ReadResHeader;
end;

procedure TPersistentStream.SaveToFile(const FileName: string);
begin
  TMemoryStream(FStream).SaveToFile(Filename);
end;

procedure TPersistentStream.SaveToStream(Stream: TStream);
begin
  TMemoryStream(FStream).SaveToStream(Stream);
end;

function TPersistentStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result := FStream.Seek(Offset,Origin);
end;

{$IFNDEF VER130}
function TPersistentStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  Result := FStream.Seek(Offset,Origin);
end;
{$ENDIF}
procedure TPersistentStream.SetPosition(const Value: Int64);
begin
  FStream.Position := Value;
end;

procedure TPersistentStream.SetSize(const Value: Int64);
begin
  FStream.Size := Value;
end;

function TPersistentStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FStream.Write(Buffer,Count);
end;

procedure TPersistentStream.WriteBuffer(const Buffer; Count: Integer);
begin
  FStream.WriteBuffer(Buffer,Count);
end;

procedure TPersistentStream.WriteComponent(Instance: TComponent);
begin
  FStream.WriteComponent(Instance);
end;

procedure TPersistentStream.WriteComponentRes(const ResName: string;
  Instance: TComponent);
begin
  FStream.WriteComponentRes(ResName,Instance);
end;

procedure TPersistentStream.WriteDescendent(Instance,
  Ancestor: TComponent);
begin
  FStream.WriteDescendent(Instance,Ancestor);
end;

procedure TPersistentStream.WriteDescendentRes(const ResName: string;
  Instance, Ancestor: TComponent);
begin
  FStream.WriteDescendentRes(ResName,Instance,Ancestor);
end;

procedure TPersistentStream.WriteResourceHeader(const ResName: string;
  out FixupInfo: Integer);
begin
  FStream.WriteResourceHeader(ResName,FixupInfo);
end;

procedure TPersistentStream.WriteString(const AString: string; const Len : Integer=-1);
var
  iSize : integer;
begin
  if Len < 0 then
  begin
    iSize := Length(AString);
    WriteInteger(iSize);
  end else
    iSize := Len;
  FStream.Write(PChar(AString)^, iSize);
end;

procedure TPersistentStream.WriteInteger(const Value: Cardinal);
begin
  FStream.Write(Value,SizeOf(Value));
end;

procedure TPersistentStream.WriteInteger(const Value: Integer);
begin
  FStream.Write(Value,SizeOf(Value));
end;

procedure TPersistentStream.WriteInteger(const Value: Int64);
begin
  FStream.Write(Value,SizeOf(Value));
end;

procedure TPersistentStream.WriteInteger(const Value: SmallInt);
begin
  FStream.Write(Value,SizeOf(Value));
end;

procedure TPersistentStream.WriteInteger(const Value: ShortInt);
begin
  FStream.Write(Value,SizeOf(Value));
end;

procedure TPersistentStream.WriteInteger(const Value: Byte);
begin
  FStream.Write(Value,SizeOf(Value));
end;

procedure TPersistentStream.WriteInteger(const Value: Word);
begin
  FStream.Write(Value,SizeOf(Value));
end;

procedure TPersistentStream.WriteFloat(const Value: Double);
begin
  FStream.Write(Value,SizeOf(Value));
end;

procedure TPersistentStream.WriteFloat(const Value: Single);
begin
  FStream.Write(Value,SizeOf(Value));
end;

{$IFNDEF BCB}
procedure TPersistentStream.WriteFloat(const Value: Real48);
begin
  FStream.Write(Value,SizeOf(Value));
end;
{$ENDIF}

procedure TPersistentStream.WriteFloat(const Value: Currency);
begin
  FStream.Write(Value,SizeOf(Value));
end;

procedure TPersistentStream.WriteFloat(const Value: Comp);
begin
  FStream.Write(Value,SizeOf(Value));
end;

procedure TPersistentStream.WriteFloat(const Value: Extended);
begin
  FStream.Write(Value,SizeOf(Value));
end;

procedure TPersistentStream.ReadFloat(var Value: Double);
begin
  FStream.Read(Value,SizeOf(Value));
end;

procedure TPersistentStream.ReadFloat(var Value: Single);
begin
  FStream.Read(Value,SizeOf(Value));
end;

{$IFNDEF BCB}
procedure TPersistentStream.ReadFloat(var Value: Real48);
begin
  FStream.Read(Value,SizeOf(Value));
end;
{$ENDIF}

procedure TPersistentStream.ReadFloat(var Value: Currency);
begin
  FStream.Read(Value,SizeOf(Value));
end;

procedure TPersistentStream.ReadFloat(var Value: Comp);
begin
  FStream.Read(Value,SizeOf(Value));
end;

procedure TPersistentStream.ReadFloat(var Value: Extended);
begin
  FStream.Read(Value,SizeOf(Value));
end;

procedure TPersistentStream.ReadInteger(var Value: Word);
begin
  FStream.Read(Value,SizeOf(Value));
end;

procedure TPersistentStream.ReadInteger(var Value: Byte);
begin
  FStream.Read(Value,SizeOf(Value));
end;

procedure TPersistentStream.ReadInteger(var Value: Integer);
begin
  FStream.Read(Value,SizeOf(Value));
end;

procedure TPersistentStream.ReadInteger(var Value: Cardinal);
begin
  FStream.Read(Value,SizeOf(Value));
end;

procedure TPersistentStream.ReadInteger(var Value: SmallInt);
begin
  FStream.Read(Value,SizeOf(Value));
end;

procedure TPersistentStream.ReadInteger(var Value: ShortInt);
begin
  FStream.Read(Value,SizeOf(Value));
end;

procedure TPersistentStream.ReadInteger(var Value: Int64);
begin
  FStream.Read(Value,SizeOf(Value));
end;

function TPersistentStream.ReadString(const len: Integer): string;
var
  iSize : Integer;
begin
  if len < 0 then
    ReadInteger(iSize)
  else
    iSize := len;
  SetLength(Result,iSize);
  FStream.Read(Result[1],iSize);
end;

function TPersistentStream.ReadInteger: integer;
begin
  ReadInteger(Result);
end;

function TPersistentStream.FindNextMatch(SubStr: string;
  CaseInsensitive, FromStart : boolean) : Int64;
begin
//  Result := 0;
  if FromStart then
    FStream.Position := 0;
  if CaseInsensitive then
    Result := _FastPosNoCase(PChar(TMemoryStream(FStream).Memory),SubStr,FStream.Size,Length(SubStr),FStream.Position+1)
  else
    Result := _FastPos(PChar(TMemoryStream(FStream).Memory),SubStr,FStream.Size,Length(SubStr),FStream.Position+1);
  if Result > 0 then
    FStream.Position := Result;
end;

function TPersistentStream.FindPriorMatch(SubStr: string; CaseInsensitive,
  FromEnd: boolean): Int64;
begin
//  Result := 0;
  if FromEnd then
    FStream.Position := FStream.Size;
  if CaseInsensitive then
    Result := _FastPosBackNoCase(PChar(TMemoryStream(FStream).Memory),SubStr,FStream.Size,Length(SubStr),FStream.Position)
  else
    Result := _FastPosBack(PChar(TMemoryStream(FStream).Memory),SubStr,FStream.Size,Length(SubStr),FStream.Position);
  if Result > 0 then
    FStream.Position := Result-1;
end;

function TPersistentStream.FindNextMatch(c: Char; CaseInsensitive,
  FromStart: Boolean): Int64;
begin
//  Result := 0;
  if FromStart then
    FStream.Position := 0;
  if CaseInsensitive then
    Result := _FastCharPosNoCase(PChar(TMemoryStream(FStream).Memory),c,FStream.Size,FStream.Position+1)
  else
    Result := _FastCharPos(PChar(TMemoryStream(FStream).Memory),c,FStream.Size,FStream.Position+1);
  if Result > 0 then
    FStream.Position := Result;
end;

var
  I: Integer;
function TPersistentStream.GetBytes: PByteArray;
begin
  Result := PByteArray(TMemoryStream(FStream).Memory);
end;

function TPersistentStream.ReadBoolean: Boolean;
begin
  FStream.Read(Result,SizeOf(Boolean));
end;

procedure TPersistentStream.WriteBoolean(const Value: Boolean);
begin
  FStream.Write(Value,SizeOf(Boolean));
end;

initialization
  for I:=0 to 255 do GUpcaseTable[I] := Uppercase(Chr(I))[1];
  GUpcaseLUT := @GUpcaseTable[0];

end.



