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

unit uLoginManager;

interface

uses SysUtils, SyncObjs, Classes;

type
  ELoginManagerException = class(Exception);
  EUserExists = class(ELoginManagerException);
  TSecurityAccess = (saHidden, saDisabled, saReadOnly, saReadWrite);
  TSecurityAccessInt = 1..4;
  TLoginManagerFileEvent = procedure(Sender : TObject; const Filename : string; Stream : TStream; var Skip : boolean) of object;

  TSecurityItem = class(TCollectionItem)
  private
    FObjectName: string;
    FAccess: TSecurityAccess;
    FCategory: string;
    FAccessText: TStringList;
    procedure SetAccessText(const Value: TStringList);
    function GetAccessLevel: TSecurityAccessInt;
    procedure SetAccessLevel(const Value: TSecurityAccessInt);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property AccessLevel : TSecurityAccessInt read GetAccessLevel write SetAccessLevel;
  published
    property Category : string read FCategory write FCategory;
    property ObjectName : string read FObjectName write FObjectName;
    property Access : TSecurityAccess read FAccess write FAccess;
    property AccessText : TStringList read FAccessText write SetAccessText;
  end;

  TSecurityItems = class(TCollection)
  private
    function GetItems(idx: integer): TSecurityItem;
    procedure SetItems(idx: integer; const Value: TSecurityItem);
    function GetFind(Category : string; Name: string): TSecurityItem;
    function GetFindByIndex(Category: string; idx: integer): TSecurityItem;
  public
    property Items[idx : integer] : TSecurityItem read GetItems write SetItems; default;
    property Find[Category : string; Name : string] : TSecurityItem read GetFind;
    property FindByIndex[Category : string; i : integer] : TSecurityItem read GetFindByIndex;
    function Add : TSecurityItem; overload;
    function Add(Category, ObjectName : string; Access : TSecurityAccess) : TSecurityItem; overload;
    function Add(Category, ObjectName : string; AccessLevel : TSecurityAccessInt; Level1Text : string = 'Disabled'; Level2Text : string = 'Hidden'; Level3Text : string = 'Read-Only'; Level4Text : string = 'Read-Write') : TSecurityItem; overload;
    procedure GetCategories(list : TStringList);
    function IndexOf(Category, ObjectName : string) : integer;
  end;

  TLoginItem = class(TCollectionItem)
  private
    FPassword: string;
    FUsername: string;
    FSecurity: TSecurityItems;
    FUserDetails: string;
    function GetPassword: string;
    function GetUsername: string;
    procedure SetPassword(const Value: string);
    procedure SetUsername(const Value: string);
    function GetUserDetails: string;
    procedure SetUserDetails(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    property Username : string read GetUsername write SetUsername;
    property Password : string read GetPassword write SetPassword;
    destructor Destroy; override;
    property UserDetails : string read GetUserDetails write SetUserDetails;
  published
    property _Username : string read FUsername write FUsername;
    property _Password : string read FPassword write FPassword;
    property Security : TSecurityItems read FSecurity write FSecurity;
    property _UserDetails : string read FUserDetails write FUserDetails;
  end;

  TLoginManagerFile = class;

  TLoginItemList = class(TCollection)
  private
    ParentFile : TLoginManagerFile; 
    function GetNames(name: string): TLoginItem;
    procedure SetNames(name: string; const Value: TLoginItem);
    function GetItems(idx: integer): TLoginItem;
    procedure SetItems(idx: integer; const Value: TLoginItem);
  public
    property Items[idx : integer] : TLoginItem read GetItems write SetItems;
    property Names[name : string] : TLoginItem read GetNames write SetNames; default;
    function Add : TLoginItem; overload;
    function Add(Username, Password : string; UserDetails : string='') : TLoginItem; overload;
    function CheckAccess(Username, Category, ObjectName : string) : TSecurityAccess; 
  end;

  TLoginManagerFile = class(TComponent)
  private
    CS : TCriticalSection;
    FFilename : string;
    FLoaded: boolean;
    FSecurityDefs: TSecurityItems;
    FUsers: TLoginItemList;
    FOnBuildSecurityDefs: TNotifyEvent;
    FOnSaveFile: TLoginManagerFileEvent;
    FOnLoadFile: TLoginManagerFileEvent;
    procedure SetUsers(const Value: TLoginItemList);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AdjustSecurityDefs; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property IsLoaded : boolean read FLoaded;
    procedure Reload;
    procedure ReloadSecurityDefs;
    procedure LoadFromFile(filename: string; DefUsername : string=''; DefPassword : string='');
    procedure SaveToFile(filename: string = '');
    procedure FillStringList(list : TStrings);
    procedure FillAuthList(list : TStrings);
    procedure Lock;
    procedure Unlock;
  published
    property Filename : string read FFilename;
    property Users : TLoginItemList read FUsers write SetUsers;
    property SecurityDefs : TSecurityItems read FSecurityDefs write FSecurityDefs;
    property OnBuildSecurityDefs : TNotifyEvent read FOnBuildSecurityDefs write FOnBuildSecurityDefs;
    property OnLoadFile : TLoginManagerFileEvent read FOnLoadFile write FOnLoadFile;
    property OnSaveFile : TLoginManagerFileEvent read FOnSaveFile write FOnSaveFile;
  end;

function Encrypt( S: String; Key: Word=27469): String;
function Decrypt(S: String; Key: Word=27469): String;
function EncryptAsHex(S: String; Key : Word = 8622): string;
function DecryptFromHex(CryptHexStr: String; Key : Word = 8622): string;

implementation

uses Math;
{$R-}

const
  PRE = 9;
  POST = 6;
  C1 = 52840;
  C2 = 22714;

{$IFDEF VER130}
function RandomRange(const AFrom, ATo: Integer): Integer;
begin
  if AFrom > ATo then
    Result := Random(AFrom - ATo) + ATo
  else
    Result := Random(ATo - AFrom) + AFrom;
end;
{$ENDIF}

function RandomStr(len:integer): string;
var
  i: integer;
begin
   Randomize;
  for i :=0  to len-1  do
  begin
    result := result + char(RandomRange(97,122));
  end;
end;

function DecryptFromHex(CryptHexStr: String; Key : Word = 8622): string;
var
  zLen : integer;
begin
    zLen := length(CryptHexStr) div 2;
    setLength(result, zLen);
    HexToBin(@CryptHexStr[1], @result[1], zLen);
    result := Decrypt(result, 8622);
end;

function EncryptAsHex(S: String; Key : Word = 8622): string;
var
  zBuf : string;
  zLen : integer;
begin
    result := Encrypt(S, 8622);
    zLen := length(result);
    setLength(zBuf, zLen*2);
    binToHex(@result[1], @zBuf[1], zLen); // so we can see the string as text
    result := lowerCase(zBuf);
end;

function Encrypt( S: String; Key: Word=27469): String;
var  I: byte;
begin
   s:= RandomStr(PRE)+s+RandomStr(POST);

  setLength(result, Length(S));
  for I := 1 to Length(S) do
  begin
    Result[I] := char(byte(S[I]) xor (Key shr 8));
    Key := (byte(Result[I]) + Key) * C1 + C2;
  end;
end;

function Decrypt(S: String; Key: Word=27469): String;
var  I: byte;
  zLen : integer;
begin
  zLen := Length(S)-(PRE+POST);
  if zLen < 1 then
    exit;
  setLength(result, zLen);
  for I := 1 to PRE do
    Key := (byte(S[I]) + Key) * C1 + C2; //Get starting Key, need to walk S

  for I := 1 to zLen do
  begin
    Result[I] := char(byte(S[I+PRE]) xor (Key shr 8));
    Key := (byte(S[I+PRE]) + Key) * C1 + C2;
  end;
end;

{ TLoginManagerFile }

procedure TLoginManagerFile.AdjustSecurityDefs;
  procedure RemoveDeletedObjects(item : TLoginItem);
    function NotInDefSecurity(si : TSecurityItem) : boolean;
    begin
      result := FSecurityDefs.IndexOf(si.Category,si.ObjectName) < 0;
    end;
  var
    i : integer;
    si : TSecurityItem;
  begin
    i := 0;
    while i < item.Security.Count do
    begin
      si := item.Security[i];
      if NotInDefSecurity(si) then
        si.Free
      else
        inc(i);
    end;
  end;
  procedure AddNewObject(item : TSecurityItem);
    function NotInUserSecurity(User : TLoginItem) : boolean;
    begin
      Result := User.Security.IndexOf(item.Category,item.ObjectName) < 0;
    end;
  var
    iUser : integer;
  begin
    for iUser := 0 to FUsers.Count-1 do
    begin
      if NotInUserSecurity(FUsers.Items[iUser]) then
        FUsers.Items[iUser].Security.Add(item.Category, item.ObjectName, item.Access);
    end;
  end;
var
  iDefs, iUser : integer;
begin
  for iUser := 0 to FUsers.Count-1 do
    RemoveDeletedObjects(FUsers.Items[iUser]);
  for iDefs := 0 to FSecurityDefs.Count-1 do
    AddNewObject(FSecurityDefs[iDefs]);
end;

procedure TLoginManagerFile.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TLoginManagerFile(Dest).FUsers.Assign(FUsers);
  TLoginManagerFile(Dest).SecurityDefs.Assign(FSecurityDefs);
end;

constructor TLoginManagerFile.Create(AOwner: TComponent);
begin
  inherited;
  CS := TCriticalSection.Create;
  FLoaded := False;
  FUsers := TLoginItemList.Create(TLoginItem);
  FSecurityDefs := TSecurityItems.Create(TSecurityItem);
  FUsers.ParentFile := Self; 
end;

destructor TLoginManagerFile.Destroy;
begin
  CS.Free;
  FUsers.Free;
  FSecurityDefs.Free;
  inherited;
end;

procedure TLoginManagerFile.FillAuthList(list: TStrings);
var
  i : integer;
begin
  for i := 0 to FUsers.Count-1 do
    list.AddObject(FUsers.Items[i].Username+'='+FUsers.Items[i].Password,FUsers.Items[i]);
end;

procedure TLoginManagerFile.FillStringList(list : TStrings);
var
  i : integer;
begin
  for i := 0 to FUsers.Count-1 do
    list.AddObject(FUsers.Items[i].Username,FUsers.Items[i]);
end;

procedure TLoginManagerFile.LoadFromFile(filename: string; DefUsername : string=''; DefPassword : string='');
var
  ms : TMemoryStream;
  ss : TStringStream;
  li : TLoginItem;
  i : integer;
  bSkip : boolean;
begin
  try
    FUsers.Clear;

    FFilename := Filename;

    ss := TStringStream.Create('');
    ms := TMemoryStream.Create;
    try
      bSkip := False;

      if not Assigned(FOnLoadFile) then
      begin
        if FileExists(FFilename) then
          ms.LoadFromFile(FFilename)
        else
          bSkip := True;
      end else
        FOnLoadFile(Self, FFilename, ms, bSkip);

      if not bSkip then
      begin
        ms.Position := 0;
        ss.CopyFrom(ms,ms.Size);
        ss.Position := 0;
        ms.Size := 0;
        ObjectTextToBinary(ss,ms);
        ms.Position := 0;
        ms.ReadComponent(Self);
      end;
    finally
      ms.Free;
      ss.Free;
    end;

    ReloadSecurityDefs;
    AdjustSecurityDefs;

    if (FUsers.Count = 0) and (DefUsername<>'') then
    begin
      li := FUsers.Add;
      li.Username := DefUsername;
      li.Password := DefPassword;
      for i := 0 to li.Security.Count-1 do
        li.Security[i].Access := saReadWrite;
    end;
  finally
    FLoaded := True;
  end;
end;

procedure TLoginManagerFile.Lock;
begin
  CS.Enter;
end;

procedure TLoginManagerFile.Reload;
begin
  if (FFilename = '') then
    raise Exception.Create('Cannot reload if file has not previoiusly been loaded');
  LoadFromFile(FFilename);
end;

procedure TLoginManagerFile.ReloadSecurityDefs;
begin
  if Assigned(FOnBuildSecurityDefs) then
  begin
    FSecurityDefs.Clear;
    FOnBuildSecurityDefs(Self);
    if FFilename <> '' then
      SaveToFile;
  end;
end;

procedure TLoginManagerFile.SaveToFile(filename: string);
var
  ms : TMemoryStream;
  ss : TStringStream;
  bSkip : boolean;
begin
  if filename <> '' then
    FFilename := Filename;
  ss := TStringStream.Create('');
  ms := TMemoryStream.Create;
  try
    ms.WriteComponent(Self);
    ms.Position := 0;
    ObjectBinaryToText(ms,ss);
    ms.Size := 0;
    ss.Position := 0;
    ms.CopyFrom(ss,ss.Size);
    ms.Position := 0;

    bSkip := False;
    
    if Assigned(FOnSaveFile) then
      FOnSaveFile(Self,FFilename,ms,bSkip)
    else
      ms.SaveToFile(FFilename);
  finally
    ms.Free;
    ss.Free;
  end;
end;

procedure TLoginManagerFile.SetUsers(const Value: TLoginItemList);
begin
  FUsers.Assign(Value);
end;

procedure TLoginManagerFile.Unlock;
begin
  CS.Leave;
end;

{ TLoginItemList }

function TLoginItemList.Add: TLoginItem;
var
  i : integer;
begin
  Result := TLoginItem(inherited Add);
  for i := 0 to ParentFile.SecurityDefs.Count -1 do
    Result.FSecurity.Add.Assign(ParentFile.SecurityDefs[i]);
end;

function TLoginItemList.Add(Username, Password: string; UserDetails : string=''): TLoginItem;
begin
  if Names[Username] <> nil then
    raise EUserExists.Create('User already exists.');
  Result := Add;
  Result.Username := Username;
  Result.Password := Password;
  Result.UserDetails := UserDetails;
end;

function TLoginItemList.CheckAccess(Username, Category,
  ObjectName: string): TSecurityAccess;
var
  li : TLoginItem;
begin
  li := Names[Username];
  if li <> nil then
    Result := li.Security.Find[Category, ObjectName].Access
  else
    Result := saDisabled;
end;

function TLoginItemList.GetItems(idx: integer): TLoginItem;
begin
  Result := TLoginItem(inherited Items[idx]);
end;

function TLoginItemList.GetNames(name: string): TLoginItem;
var
  i : integer;
  sName : string;
begin
  result := nil;
  if name = '' then exit;
  sName := lowercase(name);
  for i := 0 to Count -1 do
    if lowercase(Items[i].Username) = sName then
    begin
      result := items[i];
      break;
    end;
end;

procedure TLoginItemList.SetItems(idx: integer; const Value: TLoginItem);
begin
  inherited Items[idx] := Value;
end;

procedure TLoginItemList.SetNames(name: string; const Value: TLoginItem);
var
  i : integer;
  sName : string;
  b : boolean;
begin
  b := False;
  sName := lowercase(name);
  for i := 0 to Count -1 do
    if Items[i].Username = sName then
    begin
      items[i] := Value;
      b := True;
      break;
    end;
  if not b then
    Add.Assign(Value);
end;

{ TLoginItem }

procedure TLoginItem.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TLoginItem(Dest)._Username := FUsername;
  TLoginItem(Dest)._Password := FPassword;
  TLoginItem(Dest).Security.Assign(FSecurity);
  TLoginItem(Dest)._UserDetails := _UserDetails;
end;

constructor TLoginItem.Create(Collection: TCollection);
begin
  inherited;
  FSecurity := TSecurityItems.Create(TSecurityItem);
end;

destructor TLoginItem.Destroy;
begin
  FSecurity.Free;
  inherited;
end;

function TLoginItem.GetPassword: string;
begin
  Result := DecryptFromHex(FPassword);
end;

function TLoginItem.GetUserDetails: string;
begin
  Result := DecryptFromHex(FUserDetails);
end;

function TLoginItem.GetUsername: string;
begin
  Result := DecryptFromHex(FUsername);
end;

procedure TLoginItem.SetPassword(const Value: string);
begin
  FPassword := EncryptAsHex(Value);
end;

procedure TLoginItem.SetUserDetails(const Value: string);
begin
  FUserDetails := EncryptAsHex(Value);
end;

procedure TLoginItem.SetUsername(const Value: string);
begin
  if Value = '' then
    raise ELoginManagerException.Create('Username cannot be blank.');
  FUsername := EncryptAsHex(Value);
end;

{ TSecurityItems }

function TSecurityItems.Add: TSecurityItem;
begin
  Result := TSecurityItem(inherited Add);
end;

function TSecurityItems.Add(Category, ObjectName: string;
  Access: TSecurityAccess): TSecurityItem;
begin
  Result := Add;
  Result.Category := Category;
  Result.ObjectName := ObjectName;
  Result.Access := Access;
end;

function TSecurityItems.Add(Category, ObjectName: string;
  AccessLevel: TSecurityAccessInt; Level1Text : string = 'Disabled';
  Level2Text : string = 'Hidden'; Level3Text : string = 'Read-Only';
  Level4Text : string = 'Read-Write'): TSecurityItem;
begin
  Result := Add(Category, ObjectName, TSecurityAccess(AccessLevel));
  Result.AccessText.Add(Level1Text);
  Result.AccessText.Add(Level2Text);
  Result.AccessText.Add(Level3Text);
  Result.AccessText.Add(Level4Text);
end;

procedure TSecurityItems.GetCategories(list: TStringList);
var
  i, idx : integer;
begin
  for i := 0 to Count-1 do
  begin
    idx := list.IndexOf(Items[i].Category);
    if idx < 0 then
      list.AddObject(Items[i].Category,TObject(1))
    else
      list.Objects[idx] := TObject(Integer(list.Objects[idx])+1); 
  end;
end;

function TSecurityItems.GetFind(Category : string; Name: string): TSecurityItem;
var
  i : integer;
  sCat, sName : string;
begin
  Result := nil;
  sName := lowercase(Name);
  sCat := lowercase(Category);

  for i := 0 to Count-1 do
    if (lowercase(items[i].ObjectName) = sName) and
       (lowercase(items[i].Category) = sCat) then
    begin
      result := items[i];
      break;
    end;

  if result = nil then
  begin
    Result := Add;
    Result.ObjectName := Name;
    Result.Category := Category;
    Result.Access := saHidden;
  end;
end;

function TSecurityItems.GetFindByIndex(Category: string;
  idx: integer): TSecurityItem;
var
  i : integer;
  sCat : string;
  ls : TList;
begin
  sCat := lowercase(Category);

  ls := TList.Create;
  try
    for i := 0 to Count-1 do
      if lowercase(Items[i].FCategory) = sCat then
        ls.Add(Items[i]);

    Result := ls[idx];
  finally
    ls.Free;
  end;
end;

function TSecurityItems.GetItems(idx: integer): TSecurityItem;
begin
  Result := TSecurityItem(inherited Items[idx]);
end;

function TSecurityItems.IndexOf(Category, ObjectName: string): integer;
var
  i : integer;
  sCat, sObj : string;
begin
  result := -1;
  sCat := lowercase(Category);
  sObj := lowercase(ObjectName);
  for i := 0 to Count - 1 do
    if (lowercase(items[i].Category) = sCat) and (lowercase(items[i].ObjectName) = sObj) then
    begin
      Result := i;
      break;
    end;
end;

procedure TSecurityItems.SetItems(idx: integer;
  const Value: TSecurityItem);
begin
  inherited Items[idx] := Value;
end;

{ TSecurityItem }

procedure TSecurityItem.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TSecurityItem(Dest).ObjectName := FObjectName;
  TSecurityItem(Dest).Access     := FAccess;
  TSecurityItem(Dest).Category   := FCategory;
  TSecurityItem(Dest).AccessText.Assign(FAccessText);
end;

constructor TSecurityItem.Create(Collection: TCollection);
begin
  inherited;
  FAccessText := TStringList.Create;
end;

destructor TSecurityItem.Destroy;
begin
  FAccessText.Free;
  inherited;
end;

function TSecurityItem.GetAccessLevel: TSecurityAccessInt;
begin
  Result := TSecurityAccessInt(FAccess);
end;

procedure TSecurityItem.SetAccessLevel(const Value: TSecurityAccessInt);
begin
  FAccess := TSecurityAccess(Value);
end;

procedure TSecurityItem.SetAccessText(const Value: TStringList);
begin
  FAccessText.Assign(Value);
end;

end.
