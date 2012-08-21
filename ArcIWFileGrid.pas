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

unit ArcIWFileGrid;

interface

{$I IntraWebVersion.inc}
{$I Eval.inc}

uses
  Windows, SysUtils, Classes, Controls, IWHTMLTag, IWTypes, InURI,
{$IFDEF INTRAWEB72}IWVCLBaseControl, IWBaseControl, IWBaseHTMLControl, IWControl,
  IWRenderContext, IWBaseForm, IWFileReference, {$ENDIF}
{$IFNDEF CLR}
  jpeg,
{$ELSE}
  System.Text, IWNETJpeg,
{$ENDIF}
  ArcIWGridCommon, IWColor, Graphics, ArcIWCustomGrid, IWForm, IWApplication, ArcCommon;

type
{$IFDEF INTRAWEB51}
  TIWWindowOption = (
    woButtons, woStatusBar, woMenuBar, woScrollBars, woResizable,
    woCopyHistory, woFullScreen);
  TIWWindowOptions = set of TIWWindowOption;
{$ENDIF}

  TDownloadFileEvent = procedure(Sender: TObject; Filename: string) of object;
  TFileInterceptEvent = procedure(Sender: TObject; Filename: string; var Continue: boolean) of object;
  TRenameInterceptEvent = procedure(Sender: TObject; NameFrom, NameTo: string; var Continue: boolean) of object;

  TFileGridColumn = (fgcFilename, fgcSize, fgcType, fgcModified);
  TFileGridColumnSort = (soFilename, soFilenameDesc, soSize, soSizeDesc, soType, soTypeDesc, soModified, soModifiedDesc);
  TFileGridColumns = set of TFileGridColumn;
  TFileDownloadStyle = (fdsSendFileAttach, fdsSendFile, fdsLinkNewWindow);

  TArcIWCheckDetailEvent = procedure(Sender: TObject; const Path: string; FileInfo: TSearchRec; var Visible: boolean; var Enabled: boolean) of object;
  TFileGridColumClickEvent = procedure(Sender: TObject; Column: TFileGridColumn) of object;
  TBuildURLEvent = procedure(Sender: TObject; const Filename: string; var AURL, AWindowName: string;
    var ATop, ALeft, AWidth, AHeight: Integer; var AOptions: TIWWindowOptions) of object;

  TColumnCaptions = class(TPersistent)
  private
    FFileType: string;
    FSize: string;
    FModified: string;
    FFilename: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
  published
    property Filename: string read FFilename write FFilename;
    property Size: string read FSize write FSize;
    property FileType: string read FFileType write FFileType;
    property Modified: string read FModified write FModified;
  end;

  TColumnWidths = class(TPersistent)
  private
    FFileType: integer;
    FSize: integer;
    FModified: integer;
    FFilename: integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    function TotalWidths(cols: TFileGridColumns): integer; virtual;
  published
    property Filename: integer read FFilename write FFilename;
    property Size: integer read FSize write FSize;
    property FileType: integer read FFileType write FFileType;
    property Modified: integer read FModified write FModified;
  end;

  TFileInfo = class(TObject)
  public
    Filename: string;
    Size: Integer;
    FileType: string;
    Modified: TDateTime;
    Visible: boolean;
    Enabled: boolean;
    Attributes: Integer;
    Icon: string;
    constructor Create(FilePath, CachePath: string; SR: TSearchRec); virtual;
    function EncFilename: string;
  end;

  TDirectoryRights = class(TPersistent)
  private
    FFileUpload: boolean;
    FFileDelete: boolean;
    FDirectoryDelete: boolean;
    FFileDownload: boolean;
    FFileRename: boolean;
    FDirectoryCreate: boolean;
    FDirectoryRename: boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
  published
    property DirectoryCreate: boolean read FDirectoryCreate write FDirectoryCreate;
    property DirectoryDelete: boolean read FDirectoryDelete write FDirectoryDelete;
    property DirectoryRename: boolean read FDirectoryRename write FDirectoryRename;
    property FileDownload: boolean read FFileDownload write FFileDownload;
    property FileUpload: boolean read FFileUpload write FFileUpload;
    property FileDelete: boolean read FFileDelete write FFileDelete;
    property FileRename: boolean read FFileRename write FFileRename;
  end;

  TArcIWFileGrid = class;

  TArcFileGridIcons = class(TPersistent)
  private
{$IFDEF INTRAWEB72}
    FFileUpload: TIWFileReference;
    FFileDelete: TIWFileReference;
    FDirectoryDelete: TIWFileReference;
    FFileRename: TIWFileReference;
    FDirectoryCreate: TIWFileReference;
    FDirectoryRename: TIWFileReference;
    FParentFolder: TIWFileReference;
    FCaptionFilename: TIWFileReference;
    FCaptionSize: TIWFileReference;
    FCaptionType: TIWFileReference;
    FCaptionModified: TIWFileReference;
{$ELSE}
    FFileUploadURL: string;
    FFileDeleteURL: string;
    FDirectoryDeleteURL: string;
    FFileRenameURL: string;
    FDirectoryCreateURL: string;
    FDirectoryRenameURL: string;
    FParentFolderURL: string;
    FCaptionFilenameURL: string;
    FCaptionSizeURL: string;
    FCaptionTypeURL: string;
    FCaptionModifiedURL: string;
{$ENDIF}
    FOwner: TArcIWFileGrid;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Owner: TArcIWFileGrid); virtual;
    destructor Destroy; override;
    function URLDirectoryCreate: string;
    function URLDirectoryDelete: string;
    function URLDirectoryRename: string;
    function URLFileUpload: string;
    function URLFileDelete: string;
    function URLFileRename: string;
    function URLParentFolder(DefaultURL: string): string;
    function URLCaptionFilename: string;
    function URLCaptionSize: string;
    function URLCaptionType: string;
    function URLCaptionModified: string;
  published
{$IFDEF INTRAWEB72}
    property DirectoryCreate: TIWFileReference read FDirectoryCreate write FDirectoryCreate;
    property DirectoryDelete: TIWFileReference read FDirectoryDelete write FDirectoryDelete;
    property DirectoryRename: TIWFileReference read FDirectoryRename write FDirectoryRename;
    property FileUpload: TIWFileReference read FFileUpload write FFileUpload;
    property FileDelete: TIWFileReference read FFileDelete write FFileDelete;
    property FileRename: TIWFileReference read FFileRename write FFileRename;
    property ParentFolder: TIWFileReference read FParentFolder write FParentFolder;
    property CaptionFilename: TIWFileReference read FCaptionFilename write FCaptionFilename;
    property CaptionSize: TIWFileReference read FCaptionSize write FCaptionSize;
    property CaptionType: TIWFileReference read FCaptionType write FCaptionType;
    property CaptionModified: TIWFileReference read FCaptionModified write FCaptionModified;
{$ELSE}
    property DirectoryCreateURL: string read FDirectoryCreateURL write FDirectoryCreateURL;
    property DirectoryDeleteURL: string read FDirectoryDeleteURL write FDirectoryDeleteURL;
    property DirectoryRenameURL: string read FDirectoryRenameURL write FDirectoryRenameURL;
    property FileUploadURL: string read FFileUploadURL write FFileUploadURL;
    property FileDeleteURL: string read FFileDeleteURL write FFileDeleteURL;
    property FileRenameURL: string read FFileRenameURL write FFileRenameURL;
    property ParentFolderURL: string read FParentFolderURL write FParentFolderURL;
    property CaptionFilenameURL: string read FCaptionFilenameURL write FCaptionFilenameURL;
    property CaptionSizeURL: string read FCaptionSizeURL write FCaptionSizeURL;
    property CaptionTypeURL: string read FCaptionTypeURL write FCaptionTypeURL;
    property CaptionModifiedURL: string read FCaptionModifiedURL write FCaptionModifiedURL;
{$ENDIF}
  end;

  TArcIWFileGrid = class(TArcIWCustomGrid)
  private
    //comment by peter 2005/05/20
    FActive: boolean;
    //
    //comment by peter 2005/05/19
    FScrollToSelectedRow: boolean;
    FSelectedRowValue: string;
    //
    FScrollbars: TArcScrollbarStyle;
    FDownloadStyle: TFileDownloadStyle;
    FOnDownloadFile: TDownloadFileEvent;
    FOnRenameFile: TRenameInterceptEvent;
    FOnDeleteFile: TFileInterceptEvent;
    FOnRenameDirectory: TRenameInterceptEvent;
    FOnDeleteDirectory: TFileInterceptEvent;
    FOnCreateDirectory: TFileInterceptEvent;

    FOnCheckDetail: TArcIWCheckDetailEvent;
    FRights: TDirectoryRights;
    FColumnVisibility: TFileGridColumns;
    FColCount: integer;
    FRootPath: string;
    FPath: string;
    FFileList: TList;
    FCachePath: string;
    FCacheURL: string;
    FColumnCaption: TColumnCaptions;
    FCaptionAlignment: TAlignment;
    FColumnWidths: TColumnWidths;
    FShowIcons: boolean;
    FWebApplication: TIWApplication;
    FShowFullPath: boolean;
    FCaption: string;
    FParentText: string;
    FOnUploadRequest: TNotifyEvent;
    FIcons: TArcFileGridIcons;
    FImageCachePath: string;
    FImageCacheURL: string;
    FColumnSort: TFileGridColumnSort;
    FOnCaptionClick: TNotifyEvent;
    FOnColumnClick: TFileGridColumClickEvent;
    FOnBuildURL: TBuildURLEvent;
    FRoundPrecision: integer;
    //
    FNeedsRefresh:boolean;
    //
    procedure ForceRefresh;
    procedure SetRootPath(const Value: string);
    procedure SetColumnVisibility(const Value: TFileGridColumns);
    procedure SetColumnSort(const Value: TFileGridColumnSort);
    //comment by peter 2005/05/20
    procedure SetActive(const Value: boolean);
    //
  protected
    procedure Loaded; override;
    procedure Submit(const AValue: string); override;
    function _ImageCachePath: string; virtual;
    function _ImageCacheURL: string; virtual;
    function GetCurrentPathShort: string; virtual;
    function GetRelativeURL: string; virtual;
  public
{$IFNDEF INTRAWEB72}
    function RenderHTML: TIWHTMLTag; override;
{$ELSE}
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
{$ENDIF}
    procedure RefreshFiles; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UploadFile; virtual;
    procedure CreateSubDirectory(NewPath: string); virtual;
    procedure RenameDirectory(NameFrom, NameTo: string); virtual;
    procedure RenameFile(NameFrom, NameTo: string); virtual;
    procedure DeleteDirectory(Dirname: string); virtual;
    procedure DeleteFile(Filename: string); virtual;
    procedure SortOnColumn(Column: TFileGridColumn); virtual;
    procedure MoveUpLevel; virtual;
    procedure SortFileType; virtual;
    procedure SortFileSize; virtual;
    procedure SortModified; virtual;
    procedure SortFilename; virtual;
    property CurrentPath: string read FPath;
    property CurrentPathShort: string read GetCurrentPathShort;
    property RelativeURL: string read GetRelativeURL;
  published
    property AutoColumnWidth;
    property AutoRowHeight;
    property StyleHeader;
    property StyleDetail;
    property StyleDetailAlt;
    property StyleTable;
    property UseAltStyles;
    property ShowHeaderRow;
    //property StaticHeader;
    property Scrollbars: TArcScrollbarStyle read FScrollbars write FScrollbars;
    property Caption: string read FCaption write FCaption;
    property ParentText: string read FParentText write FParentText;
    property ColumnVisibility: TFileGridColumns read FColumnVisibility write SetColumnVisibility;
    property OnCheckDetail: TArcIWCheckDetailEvent read FOnCheckDetail write FOnCheckDetail;
    property Rights: TDirectoryRights read FRights write FRights;
    property RootPath: string read FRootPath write SetRootPath;
    property ColumnCaption: TColumnCaptions read FColumnCaption write FColumnCaption;
    property ColumnWidths: TColumnWidths read FColumnWidths write FColumnWidths;
    property CaptionAlignment: TAlignment read FCaptionAlignment write FCaptionAlignment;
    property ShowFullPath: boolean read FShowFullPath write FShowFullPath;
    property ShowIcons: boolean read FShowIcons write FShowIcons;
    property OnUploadRequest: TNotifyEvent read FOnUploadRequest write FOnUploadRequest;
    property Icons: TArcFileGridIcons read FIcons write FIcons;
    property ImageCachePath: string read FImageCachePath write FImageCachePath;
    property ImageCacheURL: string read FImageCacheURL write FImageCacheURL;
    property ColumnSort: TFileGridColumnSort read FColumnSort write SetColumnSort;
    property OnCaptionClick: TNotifyEvent read FOnCaptionClick write FOnCaptionClick;
    property OnColumnClick: TFileGridColumClickEvent read FOnColumnClick write FOnColumnClick;
    property DownloadStyle: TFileDownloadStyle read FDownloadStyle write FDownloadStyle;
    property OnBuildURL: TBuildURLEvent read FOnBuildURL write FOnBuildURL;
    property RoundPrecision: integer read FRoundPrecision write FRoundPrecision default 2;
    property OnDownloadFile: TDownloadFileEvent read FOnDownloadFile write FOnDownloadFile;

    property OnRenameFile: TRenameInterceptEvent read FOnRenameFile write FOnRenameFile;
    property OnDeleteFile: TFileInterceptEvent read FOnDeleteFile write FOnDeleteFile;
    property OnRenameDirectory: TRenameInterceptEvent read FOnRenameDirectory write FOnRenameDirectory;
    property OnDeleteDirectory: TFileInterceptEvent read FOnDeleteDirectory write FOnDeleteDirectory;
    property OnCreateDirectory: TFileInterceptEvent read FOnCreateDirectory write FOnCreateDirectory;
    //comment by peter 2005/05/19
    property ScrollToSelectedRow: boolean read FScrollToSelectedRow write FScrollToSelectedRow default false;
    //
    //comment by peter 2005/05/20
    property Active: boolean read FActive write SetActive default true;
    //
  end;


  PHICON = ^HICON;

implementation

uses {$IFNDEF VER130}StrUtils, Math, {$ELSE}FileCtrl,{$ENDIF}Registry, ShellAPI, IWServer, IWGlobal;

{$IFDEF VER130}
function ReverseString(const AText: string): string;
var
  I: Integer;
  P: PChar;
begin
  SetLength(Result, Length(AText));
  P := PChar(Result);
  for I := Length(AText) downto 1 do
  begin
    P^ := AText[I];
    Inc(P);
  end;
end;
{$ENDIF}



function DelTree(const Path: string; AbortOnFailure: Boolean): Boolean;
  function BuildFileList(const Path: string; const Attr: Integer; const List: TStrings): Boolean;
  var
    SearchRec: TSearchRec;
    R: Integer;
  begin
    Assert(List <> nil);
    R := FindFirst(Path, Attr, SearchRec);
    Result := R = 0;
    try
      if Result then
      begin
        while R = 0 do
        begin
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
            List.Add(SearchRec.Name);
          R := FindNext(SearchRec);
        end;
        Result := R = ERROR_NO_MORE_FILES;
      end;
    finally
      SysUtils.FindClose(SearchRec);
    end;
  end;
  function PathRemoveSeparator(const Path: string): string;
  var
    L: Integer;
  begin
    L := Length(Path);
    if (L <> 0) and (Path[L] = '\') then
      Result := Copy(Path, 1, L - 1)
    else
      Result := Path;
  end;
var
  Files: TStringList;
  LPath: string; // writable copy of Path
  FileName: string;
  I: Integer;
  PartialResult: Boolean;
  Attr: DWORD;
begin
  Result := True;
  Files := TStringList.Create;
  try
    LPath := PathRemoveSeparator(Path);
    BuildFileList(LPath + '\*.*', faAnyFile, Files);
    for I := 0 to Files.Count - 1 do
    begin
      FileName := LPath + '\' + Files[I];
      PartialResult := True;
      // If the current file is itself a directory then recursively delete it
{$IFDEF CLR}
      Attr := GetFileAttributes(FileName);
{$ELSE}
      Attr := GetFileAttributes(PChar(FileName));
{$ENDIF}
      if (Attr <> DWORD(-1)) and ((Attr and FILE_ATTRIBUTE_DIRECTORY) <> 0) then
        PartialResult := DelTree(FileName, AbortOnFailure)
      else
      begin
        if PartialResult then
        begin
          // Set attributes to normal in case it's a readonly file
{$IFDEF CLR}
          PartialResult := SetFileAttributes(FileName, FILE_ATTRIBUTE_NORMAL);
{$ELSE}
          PartialResult := SetFileAttributes(PChar(FileName), FILE_ATTRIBUTE_NORMAL);
{$ENDIF}
          if PartialResult then
            PartialResult := DeleteFile(FileName);
        end;
      end;
      if not PartialResult then
      begin
        Result := False;
        if AbortOnFailure then
          Break;
      end;
    end;
  finally
    FreeAndNil(Files);
  end;
  if Result then
  begin
    // Finally remove the directory itself
{$IFDEF CLR}
    Result := SetFileAttributes(LPath, FILE_ATTRIBUTE_NORMAL);
{$ELSE}
    Result := SetFileAttributes(PChar(LPath), FILE_ATTRIBUTE_NORMAL);
{$ENDIF}
    if Result then
    begin
{$IOCHECKS OFF}
      RmDir(LPath);
{$IFDEF IOCHECKS_ON}
{$IOCHECKS ON}
{$ENDIF IOCHECKS_ON}
      Result := IOResult = 0;
    end;
  end;
end;

{$IFNDEF CLR}

procedure StrResetLength(var S: string);
begin
  SetLength(S, StrLen(PChar(S)));
end;
{$ENDIF}

//comment by peter 2005/05/19

function _SelectedRow: string;
begin
  if GServerController.DebugHTML then
    result := '_SelectedRow'
  else
    result := '_n';
end;

function ExpandEnvironmentVar(var Value: string): Boolean;
var
  R: Integer;
  Expanded: string;
{$IFDEF CLR}
  SB: StringBuilder;
{$ENDIF}
begin
{$IFDEF CLR}
  SB := StringBuilder.Create;
  R := ExpandEnvironmentStrings(Value, SB, 0);
  Result := ExpandEnvironmentStrings(Value, SB, R) <> 0;
  Value := SB.ToString;
{$ELSE}
  SetLength(Expanded, 1);
  R := ExpandEnvironmentStrings(PChar(Value), PChar(Expanded), 0);
  SetLength(Expanded, R);
  Result := ExpandEnvironmentStrings(PChar(Value), PChar(Expanded), R) <> 0;
  if Result then
  begin
    StrResetLength(Expanded);
    Value := Expanded;
  end;
{$ENDIF}
end;

// Gets the icons of a given file

function GetWindowsSystemFolder: string;
var
  Required: Cardinal;
{$IFDEF CLR}
  SB: StringBuilder;
{$ENDIF}
begin
  Result := '';
  Required := GetSystemDirectory(nil, 0);
  if Required <> 0 then
  begin
{$IFDEF CLR}
    SB := StringBuilder.Create;
    GetSystemDirectory(SB, Required);
    Result := SB.ToString;
{$ELSE}
    SetLength(Result, Required);
    GetSystemDirectory(PChar(Result), Required);
    StrResetLength(Result);
{$ENDIF}
  end;
end;

{$IFNDEF CLR}

procedure GetAssociatedIcon(IsDir: boolean; FileName: string; PLargeIcon, PSmallIcon: PHICON; var FileType: string);
  function DropQuotes(Filename: string): string;
  var
    iLen: integer;
  begin
    Result := Filename;
    iLen := length(Result);
    if iLen > 2 then
    begin
      if (Result[1] = '"') and (Result[iLen] = '"') then
        Result := Copy(Result, 2, iLen - 2);
    end;
  end;
var
  IconIndex: word; // Position of the icon in the file
  FileExt: string;
  Reg: TRegistry;
  p: integer;
  p1, p2: pchar;
label noassoc;
begin
  if IsDir then
  begin
    FileType := 'Folder';
    Filename := IncludeTrailingBackslash(GetWindowsSystemFolder) + 'SHELL32.DLL';
    IconIndex := 4; // Folder Closed;
    ExtractIconEx(pchar(FileName), IconIndex, PLargeIcon^, PSmallIcon^, 1);
  end else
  begin
    IconIndex := 0;

    // Get the extension of the file
    FileExt := UpperCase(ExtractFileExt(FileName));

    if ((FileExt <> '.EXE') and (FileExt <> '.ICO')) or not FileExists(FileName) then
    begin
      // If the file is an EXE or ICO and it exists, then
      // we will extract the icon from this file. Otherwise
      // here we will try to find the associated icon in the

      // Windows Registry...

      Reg := nil;
      try
        Reg := TRegistry.Create(KEY_QUERY_VALUE);
        Reg.RootKey := HKEY_CLASSES_ROOT;
        if FileExt = '.EXE' then
          FileExt := '.COM';

        if Reg.OpenKeyReadOnly(FileExt) then
        try
          FileType := Reg.ReadString('');
        finally
          Reg.CloseKey;
        end;

        if (FileType <> '') and Reg.OpenKeyReadOnly(FileType + '\DefaultIcon') then
        begin
          try
            FileName := Reg.ReadString('');
          finally
            Reg.CloseKey;
          end;
          Reg.OpenKeyReadOnly(FileType);
          try
            FileType := Reg.ReadString('');
          finally
            Reg.CloseKey;
          end;
        end;
      finally
        Reg.Free;
      end;

      // If we couldn't find the association, we will
      // try to get the default icons

      if FileName = '' then
        goto noassoc;
{$R-}
      ExpandEnvironmentVar(Filename);
      // Get the filename and icon index from the
      // association (of form '"filaname",index')
      p1 := PChar(FileName);
      p2 := StrRScan(p1, ',');
      if p2 <> nil then
      begin
        p := p2 - p1 + 1; // Position of the comma
        IconIndex := StrToInt(Copy(FileName, p + 1, Length(FileName) - p));
        SetLength(FileName, p - 1);
      end else
        IconIndex := 1;
    end;

    // Drop Quotes from Filename
    Filename := DropQuotes(Filename);

    // Attempt to get the icon
    if ExtractIconEx(pchar(FileName), IconIndex, PLargeIcon^, PSmallIcon^, 1) < 1 then
    begin
      noassoc:

      // The operation failed or the file had no associated
      // icon. Try to get the default icons from SHELL32.DLL
      try // to get the location of SHELL32.DLL
        FileName := IncludeTrailingBackslash(GetWindowsSystemFolder) + 'SHELL32.DLL';
      except
        FileName := 'C:\WINDOWS\SYSTEM\SHELL32.DLL';
      end;

      // Determine the default icon for the file extension
      if (FileExt = '.DOC') then
      begin
        FileType := 'Document';
        IconIndex := 1
      end else if (FileExt = '.EXE') or (FileExt = '.COM') then
      begin
        FileType := 'Application';
        IconIndex := 2
      end else if (FileExt = '.HLP') then
      begin
        FileType := 'Help File';
        IconIndex := 23
      end else if (FileExt = '.INI') or (FileExt = '.INF') then
      begin
        FileType := 'Settings File';
        IconIndex := 0
      end else if (FileExt = '.TXT') then
      begin
        FileType := 'Text File';
        IconIndex := 0
      end else if (FileExt = '.BAT') then
      begin
        FileType := 'Batch File';
        IconIndex := 0
      end else if (FileExt = '.DLL') or (FileExt = '.SYS') or (FileExt = '.VBX') or
        (FileExt = '.OCX') or (FileExt = '.VXD') then
      begin
        FileType := 'System File';
        IconIndex := 0
      end else if (FileExt = '.FON') then
      begin
        FileType := 'Font';
        IconIndex := 0
      end else if (FileExt = '.TTF') then
      begin
        FileType := 'Font';
        IconIndex := 0
      end else if (FileExt = '.FOT') then
      begin
        FileType := 'Font';
        IconIndex := 0
      end else
      begin
        FileType := 'Unknown';
        IconIndex := 0;
      end;

      // Attempt to get the icon.
      ExtractIconEx(pchar(FileName), IconIndex, PLargeIcon^, PSmallIcon^, 1)
    end;
  end;
end;
{$ELSE}

procedure GetAssociatedIcon(IsDir: boolean; FileName: string; LargeIcon, SmallIcon: HICON; var FileType: string);
  function DropQuotes(Filename: string): string;
  var
    iLen: integer;
  begin
    Result := Filename;
    iLen := length(Result);
    if iLen > 2 then
    begin
      if (Result[1] = '"') and (Result[iLen] = '"') then
        Result := Copy(Result, 2, iLen - 2);
    end;
  end;
var
  IconIndex: word; // Position of the icon in the file
  FileExt: string;
  Reg: TRegistry;
  p: integer;
  aryS: array of string;
label noassoc;
begin
  if IsDir then
  begin
    FileType := 'Folder';
    Filename := IncludeTrailingBackslash(GetWindowsSystemFolder) + 'SHELL32.DLL';
    IconIndex := 4; // Folder Closed;
    ExtractIconEx(FileName, IconIndex, LargeIcon, SmallIcon, 1);
  end else
  begin
    IconIndex := 0;

    // Get the extension of the file
    FileExt := UpperCase(ExtractFileExt(FileName));

    if ((FileExt <> '.EXE') and (FileExt <> '.ICO')) or not FileExists(FileName) then
    begin
      // If the file is an EXE or ICO and it exists, then
      // we will extract the icon from this file. Otherwise
      // here we will try to find the associated icon in the

      // Windows Registry...

      Reg := nil;
      try
        Reg := TRegistry.Create(KEY_QUERY_VALUE);
        Reg.RootKey := HKEY_CLASSES_ROOT;
        if FileExt = '.EXE' then
          FileExt := '.COM';

        if Reg.OpenKeyReadOnly(FileExt) then
        try
          FileType := Reg.ReadString('');
        finally
          Reg.CloseKey;
        end;

        if (FileType <> '') and Reg.OpenKeyReadOnly(FileType + '\DefaultIcon') then
        begin
          try
            FileName := Reg.ReadString('');
          finally
            Reg.CloseKey;
          end;
          Reg.OpenKeyReadOnly(FileType);
          try
            FileType := Reg.ReadString('');
          finally
            Reg.CloseKey;
          end;
        end;
      finally
        Reg.Free;
      end;

      // If we couldn't find the association, we will
      // try to get the default icons

      if FileName = '' then
        goto noassoc;

      ExpandEnvironmentVar(Filename);

      // Get the filename and icon index from the
      // association (of form '"filaname",index')
      aryS := FileName.Split([',']);
      Filename := aryS[0];
      if aryS[1] <> '' then
        IconIndex := StrToInt(aryS[1])
      else
        IconIndex := 1;
    end;

    // Drop Quotes from Filename
    Filename := DropQuotes(Filename);

    // Attempt to get the icon
    if ExtractIconEx(FileName, IconIndex, LargeIcon, SmallIcon, 1) < 1 then
    begin
      noassoc:

      // The operation failed or the file had no associated
      // icon. Try to get the default icons from SHELL32.DLL
      try // to get the location of SHELL32.DLL
        FileName := IncludeTrailingBackslash(GetWindowsSystemFolder) + 'SHELL32.DLL';
      except
        FileName := 'C:\WINDOWS\SYSTEM\SHELL32.DLL';
      end;

      // Determine the default icon for the file extension
      if (FileExt = '.DOC') then
      begin
        FileType := 'Document';
        IconIndex := 1
      end else if (FileExt = '.EXE') or (FileExt = '.COM') then
      begin
        FileType := 'Application';
        IconIndex := 2
      end else if (FileExt = '.HLP') then
      begin
        FileType := 'Help File';
        IconIndex := 23
      end else if (FileExt = '.INI') or (FileExt = '.INF') then
      begin
        FileType := 'Settings File';
        IconIndex := 0
      end else if (FileExt = '.TXT') then
      begin
        FileType := 'Text File';
        IconIndex := 0
      end else if (FileExt = '.BAT') then
      begin
        FileType := 'Batch File';
        IconIndex := 0
      end else if (FileExt = '.DLL') or (FileExt = '.SYS') or (FileExt = '.VBX') or
        (FileExt = '.OCX') or (FileExt = '.VXD') then
      begin
        FileType := 'System File';
        IconIndex := 0
      end else if (FileExt = '.FON') then
      begin
        FileType := 'Font';
        IconIndex := 0
      end else if (FileExt = '.TTF') then
      begin
        FileType := 'Font';
        IconIndex := 0
      end else if (FileExt = '.FOT') then
      begin
        FileType := 'Font';
        IconIndex := 0
      end else
      begin
        FileType := 'Unknown';
        IconIndex := 0;
      end;

      // Attempt to get the icon.
      ExtractIconEx(FileName, IconIndex, LargeIcon, SmallIcon, 1)
    end;
  end;
end;
{$ENDIF}

procedure SaveIcon(Icon: HICON; Filename: string);
var
  bmp: TBitmap;
  jpg: TJpegImage;
begin
  bmp := TBitmap.Create;
  jpg := TJpegImage.Create;
  try
    bmp.Width := 16;
    bmp.Height := 16;
    DrawIconEx(bmp.Canvas.Handle, 0, 0, Icon, bmp.Width, bmp.Height, 0, 0, DI_NORMAL);
    jpg.Assign(bmp);
    jpg.SaveToFile(Filename);
  finally
    jpg.Free;
    bmp.Free;
  end;
end;

function FindFileIcon(IsDir: Boolean; FilePath, CachePath, Filename: string; var FileType: string): string;
var
  SmallIcon: HICON;
  LargeIcon: HICON;
begin
{$IFDEF CLR}
  GetAssociatedIcon(IsDir, FilePath + Filename, LargeIcon, SmallIcon, FileType);
{$ELSE}
  GetAssociatedIcon(IsDir, FilePath + Filename, @LargeIcon, @SmallIcon, FileType);
{$ENDIF}
  try
    result := CachePath + FileType + '.jpg';
    if not FileExists(result) then
    begin
      if SmallIcon <> 0 then
      begin
        SaveIcon(SmalLIcon, result);
      end;
    end;
  finally
    if LargeIcon <> 0 then
      DestroyIcon(LargeIcon);
    if SmallIcon <> 0 then
      DestroyIcon(SmallIcon);
  end;
end;

{ TArcIWFileGrid }

constructor TArcIWFileGrid.Create(AOwner: TComponent);
var
  o: TComponent;
begin
{$IFNDEF CLR}
  if (csDesigning in ComponentState) then
  begin
    o := AOwner;
    if (o = nil) then
      raise exception.Create('This component must be used on an IntraWeb Form.');
    repeat
      o := o.Owner;
    until (o is {$IFDEF INTRAWEB72}TIWBaseForm{$ELSE}TIWForm{$ENDIF}) or (o = nil);
    if (o = nil) then
      raise exception.Create('This component must be used on an IntraWeb Form.');
  end;
{$ENDIF}
  inherited Create(AOwner);
  FActive := True;
  FRoundPrecision := 2;
  FCaption := 'Current Folder:';
  FParentText := 'parent folder';
  FShowIcons := True;

  FIcons := TArcFileGridIcons.Create(Self);
  FStyleHeader.BorderStyle.Style := brdNone;
  FStyleDetail.BorderStyle.Style := brdNone;
  FStyleDetailAlt.BorderStyle.Style := brdNone;

  FColumnVisibility := [fgcFilename, fgcSize, fgcType, fgcModified];
  FRights := TDirectoryRights.Create;
  FFileList := TList.Create;
  FColumnCaption := TColumnCaptions.Create;
  FColumnWidths := TCOlumnWidths.Create;

  FRootPath := '.\';
  FNeedsRefresh := True;
end;

destructor TArcIWFileGrid.Destroy;
var
  i: integer;
begin
  for i := 0 to FFileList.Count - 1 do
    TFileInfo(FFileList[i]).Free;
  FFileList.Free;
  FRights.Free;
  FIcons.Free;
  inherited;
end;

procedure TArcIWFileGrid.ForceRefresh;
begin
  FNeedsRefresh := True;
end;

procedure TArcIWFileGrid.UploadFile;
begin
  if Assigned(FOnUploadRequest) then
    FOnUploadRequest(Self);
  RefreshFiles;
end;

procedure TArcIWFileGrid.SortOnColumn(Column: TFileGridColumn);
begin
  case Column of
    fgcFilename: SortFilename;
    fgcSize: SortFileSize;
    fgcType: SortFileType;
    fgcModified: SortModified;
  end;
end;

procedure TArcIWFileGrid.SortFileType;
begin
  case ColumnSort of
    soType: ColumnSort := soTypeDesc;
  else ColumnSort := soType;
  end;
end;

procedure TArcIWFileGrid.SortFileSize;
begin
  case ColumnSort of
    soSize: ColumnSort := soSizeDesc;
  else ColumnSort := soSize;
  end;
end;

procedure TArcIWFileGrid.SortModified;
begin
  case ColumnSort of
    soModified: ColumnSort := soModifiedDesc;
  else ColumnSort := soModified;
  end;
end;

procedure TArcIWFileGrid.SortFilename;
begin
  case ColumnSort of
    soFilename: ColumnSort := soFilenameDesc;
  else ColumnSort := soFilename;
  end;
end;

procedure TArcIWFileGrid.SetColumnSort(const Value: TFileGridColumnSort);
begin
  FColumnSort := Value;
  if not (csLoading in ComponentState) then
    RefreshFiles;
end;

//comment by peter 2005/05/20

procedure TArcIWFileGrid.SetActive(const Value: boolean);
begin
  FActive := Value;
  if (not (csLoading in ComponentState)) and
     (not (csDesigning in ComponentState)) and
     FActive then
    RefreshFiles;
end;


procedure TArcIWFileGrid.CreateSubDirectory(NewPath: string);
begin
  if Copy(NewPath, Length(NewPath), 1) <> '\' then
    NewPath := NewPath + '\';
  while Pos('\..\', NewPath) > 0 do
    NewPath := StringReplace(NewPath, '\..\', '', [rfReplaceAll, rfIgnoreCase]);
  while Pos('\..', NewPath) > 0 do
    NewPath := StringReplace(NewPath, '\..', '', [rfReplaceAll, rfIgnoreCase]);
  while Pos('..\', NewPath) > 0 do
    NewPath := StringReplace(NewPath, '..\', '', [rfReplaceAll, rfIgnoreCase]);
  while Pos('..', NewPath) > 0 do
    NewPath := StringReplace(NewPath, '..', '', [rfReplaceAll, rfIgnoreCase]);
  while Pos('\\', NewPath) > 0 do
    NewPath := StringReplace(NewPath, '\\', '\', [rfReplaceAll, rfIgnoreCase]);
  if DirectoryExists(FPath + NewPath) then
    raise Exception.Create('Directory "' + NewPath + '" already exists.');
  if not ForceDirectories(FPath + NewPath) then
    raise Exception.Create('Directory "' + NewPath + '" could not be created.');
  FPath := FPath + NewPath;
  RefreshFiles;
end;

procedure TArcIWFileGrid.RenameDirectory(NameFrom, NameTo: string);
begin
  if (Pos(':', NameTo) > 0) or (Pos('\', NameTo) > 0) or (Pos('/', NameTo) > 0) then
    raise Exception.Create('You cannot specify paths in your folder name.');
  if (Pos(':', NameFrom) > 0) or (Pos('\', NameFrom) > 0) or (Pos('/', NameFrom) > 0) then
    raise Exception.Create('You cannot specify paths in your folder name.');
  while Pos('..', NameFrom) > 0 do
    NameFrom := StringReplace(NameFrom, '..', '', [rfReplaceAll, rfIgnoreCase]);
  while Pos('..', NameTo) > 0 do
    NameTo := StringReplace(NameTo, '..', '', [rfReplaceAll, rfIgnoreCase]);
  NameFrom := Copy(FPath, 1, length(FPath) - Pos('\', ReverseString(FPath)) + 1) + NameFrom + '\';
  NameTo := Copy(FPath, 1, length(FPath) - Pos('\', ReverseString(FPath)) + 1) + NameTo + '\';
  if not DirectoryExists(NameFrom) then
    raise Exception.Create('Directory "' + NameFrom + '" does not exist.');
  if DirectoryExists(NameTo) then
    raise Exception.Create('Directory "' + NameTo + '" already exists.');
  SysUtils.RenameFile(NameFrom, NameTo);
  RefreshFiles;
end;

procedure TArcIWFileGrid.RenameFile(NameFrom, NameTo: string);
begin
  if (Pos(':', NameTo) > 0) or (Pos('\', NameTo) > 0) or (Pos('/', NameTo) > 0) then
    raise Exception.Create('You cannot specify paths in your file name.');
  if (Pos(':', NameFrom) > 0) or (Pos('\', NameFrom) > 0) or (Pos('/', NameFrom) > 0) then
    raise Exception.Create('You cannot specify paths in your file name.');
  while Pos('..', NameFrom) > 0 do
    NameFrom := StringReplace(NameFrom, '..', '', [rfReplaceAll, rfIgnoreCase]);
  while Pos('..', NameTo) > 0 do
    NameTo := StringReplace(NameTo, '..', '', [rfReplaceAll, rfIgnoreCase]);
  NameFrom := Copy(FPath, 1, length(FPath) - Pos('\', ReverseString(FPath)) + 1) + NameFrom;
  NameTo := Copy(FPath, 1, length(FPath) - Pos('\', ReverseString(FPath)) + 1) + NameTo;
  if not FileExists(NameFrom) then
    raise Exception.Create('File "' + NameFrom + '" does not exist.');
  if FileExists(NameTo) then
    raise Exception.Create('File "' + NameTo + '" already exists.');
  SysUtils.RenameFile(NameFrom, NameTo);
  RefreshFiles;
end;

procedure TArcIWFileGrid.DeleteFile(Filename: string);
begin
  if (Pos(':', Filename) > 0) or (Pos('\', Filename) > 0) or (Pos('/', Filename) > 0) then
    raise Exception.Create('You cannot specify paths in your file name.');
  while Pos('..', Filename) > 0 do
    Filename := StringReplace(Filename, '..', '.', [rfReplaceAll, rfIgnoreCase]);
  Filename := Copy(FPath, 1, length(FPath) - Pos('\', ReverseString(FPath)) + 1) + Filename;
  if not FileExists(Filename) then
    raise Exception.Create('File "' + Filename + '" does not exist.');
  SysUtils.DeleteFile(Filename);
  RefreshFiles;
end;

procedure TArcIWFileGrid.DeleteDirectory(Dirname: string);
begin
  if (Pos(':', Dirname) > 0) or (Pos('\', Dirname) > 0) or (Pos('/', Dirname) > 0) then
    raise Exception.Create('You cannot specify paths in your folder name.');
  while Pos('..', Dirname) > 0 do
    Dirname := StringReplace(Dirname, '..', '.', [rfReplaceAll, rfIgnoreCase]);
  Dirname := Copy(FPath, 1, length(FPath) - Pos('\', ReverseString(FPath)) + 1) + Dirname;
  if not DirectoryExists(Dirname) then
    raise Exception.Create('Folder "' + Dirname + '" does not exist.');
  DelTree(Dirname, False);
  RefreshFiles;

end;

function TArcIWFileGrid._ImageCachePath: string;
var
  o: TComponent;
begin
  if not (csDesigning in ComponentState) then
  begin
    result := FImageCachePath;
    if Result = '' then
    begin
      o := self;
      repeat
        o := o.Owner;
      until o is {$IFDEF INTRAWEB72}TIWBaseForm{$ELSE}TIWForm{$ENDIF};
      Result := {$IFDEF INTRAWEB72}TIWBaseForm{$ELSE}TIWForm{$ENDIF}(o).WebApplication.UserCacheDir;
    end else
    begin
      if Copy(Result, length(result), 1) <> '\' then
        Result := Result + '\';
    end;
  end;
end;

function TArcIWFileGrid._ImageCacheURL: string;
var
  o: TComponent;
begin
  if not (csDesigning in ComponentState) then
  begin
    result := FImageCacheURL;
    if Result = '' then
    begin
      o := self;
      repeat
        o := o.Owner;
      until o is {$IFDEF INTRAWEB72}TIWBaseForm{$ELSE}TIWForm{$ENDIF};
      Result := {$IFDEF INTRAWEB72}TIWBaseForm(o).WebApplication.UserCacheURL{$ELSE}TIWForm(o).WebApplication.AppURLBase + '/Cache/user/' + TIWForm(o).WebApplication.AppID + TIWForm(o).WebApplication.SessionTimeStamp + '/'{$ENDIF};
    end else
    begin
      if Copy(Result, length(result), 1) <> '/' then
        Result := Result + '/';
    end;
  end;
end;

procedure TArcIWFileGrid.Loaded;
begin
  inherited;
  if Active then
  begin
    if FImageCacheURL = '' then
      FCacheURL := _ImageCacheURL
    else
      FCacheURL := FImageCacheURL;
    if FImageCachePath = '' then
      FCachePath := _ImageCachePath
    else
      FCachePath := FImageCachePath;
  end;
end;

{$IFNDEF CLR}

function SortByFilename(Item1, Item2: Pointer): Integer;
var
  s1, s2: string;
begin
  s1 := Copy(TFileInfo(Item1).Filename, 1, 1);
  s2 := Copy(TFileInfo(Item2).Filename, 1, 1);
  if (s1 = '&') and (s2 <> '&') then
    result := -1
  else
    if (s1 <> '&') and (s2 = '&') then
      result := 1
    else
    begin
      if Boolean(TFileInfo(Item1).Attributes and faDirectory) <> Boolean(TFileInfo(Item2).Attributes and faDirectory) then
      begin
        if Boolean(TFileInfo(Item1).Attributes and faDirectory) and (not Boolean(TFileInfo(Item2).Attributes and faDirectory)) then
          Result := -1
        else
          Result := 1;
      end else
        result := CompareStr(Uppercase(TFileInfo(Item1).Filename), Uppercase(TFileInfo(Item2).Filename));
    end;
end;

function SortByFilenameDesc(Item1, Item2: Pointer): Integer;
var
  s1, s2: string;
begin
  s1 := Copy(TFileInfo(Item1).Filename, 1, 1);
  s2 := Copy(TFileInfo(Item2).Filename, 1, 1);
  if (s1 = '&') and (s2 <> '&') then
    result := -1
  else
    if (s1 <> '&') and (s2 = '&') then
      result := 1
    else
    begin
      if Boolean(TFileInfo(Item1).Attributes and faDirectory) <> Boolean(TFileInfo(Item2).Attributes and faDirectory) then
      begin
        if Boolean(TFileInfo(Item1).Attributes and faDirectory) and (not Boolean(TFileInfo(Item2).Attributes and faDirectory)) then
          Result := -1
        else
          Result := 1;
      end else
        result := CompareStr(Uppercase(TFileInfo(Item2).Filename), Uppercase(TFileInfo(Item1).Filename));
    end;
end;

function SortByType(Item1, Item2: Pointer): Integer;
var
  s1, s2: string;
begin
  s1 := Copy(TFileInfo(Item1).Filename, 1, 1);
  s2 := Copy(TFileInfo(Item2).Filename, 1, 1);
  if (s1 = '&') and (s2 <> '&') then
    result := -1
  else
    if (s1 <> '&') and (s2 = '&') then
      result := 1
    else
    begin
      if Boolean(TFileInfo(Item1).Attributes and faDirectory) <> Boolean(TFileInfo(Item2).Attributes and faDirectory) then
      begin
        if Boolean(TFileInfo(Item1).Attributes and faDirectory) and (not Boolean(TFileInfo(Item2).Attributes and faDirectory)) then
          Result := -1
        else
          Result := 1;
      end else
      begin
        result := CompareStr(TFileInfo(Item1).FileType, TFileInfo(Item2).FileType);
        if result = 0 then
          result := CompareStr(TFileInfo(Item1).Filename, TFileInfo(Item2).Filename);
      end;
    end;
end;

function SortByTypeDesc(Item1, Item2: Pointer): Integer;
var
  s1, s2: string;
begin
  s1 := Copy(TFileInfo(Item1).Filename, 1, 1);
  s2 := Copy(TFileInfo(Item2).Filename, 1, 1);
  if (s1 = '&') and (s2 <> '&') then
    result := -1
  else
    if (s1 <> '&') and (s2 = '&') then
      result := 1
    else
    begin
      if Boolean(TFileInfo(Item1).Attributes and faDirectory) <> Boolean(TFileInfo(Item2).Attributes and faDirectory) then
      begin
        if Boolean(TFileInfo(Item1).Attributes and faDirectory) and (not Boolean(TFileInfo(Item2).Attributes and faDirectory)) then
          Result := -1
        else
          Result := 1;
      end else
      begin
        result := CompareStr(TFileInfo(Item2).FileType, TFileInfo(Item1).FileType);
        if result = 0 then
          result := CompareStr(TFileInfo(Item1).Filename, TFileInfo(Item2).Filename);
      end;
    end;
end;

function SortBySize(Item1, Item2: Pointer): Integer;
var
  s1, s2: string;
begin
  s1 := Copy(TFileInfo(Item1).Filename, 1, 1);
  s2 := Copy(TFileInfo(Item2).Filename, 1, 1);
  if (s1 = '&') and (s2 <> '&') then
    result := -1
  else
    if (s1 <> '&') and (s2 = '&') then
      result := 1
    else
    begin
      if Boolean(TFileInfo(Item1).Attributes and faDirectory) <> Boolean(TFileInfo(Item2).Attributes and faDirectory) then
      begin
        if Boolean(TFileInfo(Item1).Attributes and faDirectory) and (not Boolean(TFileInfo(Item2).Attributes and faDirectory)) then
          Result := -1
        else
          Result := 1;
      end else
      begin
        result := TFileInfo(Item1).Size - TFileInfo(Item2).Size;
        if result = 0 then
          result := CompareStr(TFileInfo(Item1).Filename, TFileInfo(Item2).Filename);
      end;
    end;
end;

function SortBySizeDesc(Item1, Item2: Pointer): Integer;
var
  s1, s2: string;
begin
  s1 := Copy(TFileInfo(Item1).Filename, 1, 1);
  s2 := Copy(TFileInfo(Item2).Filename, 1, 1);
  if (s1 = '&') and (s2 <> '&') then
    result := -1
  else
    if (s1 <> '&') and (s2 = '&') then
      result := 1
    else
    begin
      if Boolean(TFileInfo(Item1).Attributes and faDirectory) <> Boolean(TFileInfo(Item2).Attributes and faDirectory) then
      begin
        if Boolean(TFileInfo(Item1).Attributes and faDirectory) and (not Boolean(TFileInfo(Item2).Attributes and faDirectory)) then
          Result := -1
        else
          Result := 1;
      end else
      begin
        result := TFileInfo(Item2).Size - TFileInfo(Item1).Size;
        if result = 0 then
          result := CompareStr(TFileInfo(Item1).Filename, TFileInfo(Item2).Filename);
      end;
    end;
end;

function SortByModified(Item1, Item2: Pointer): Integer;
var
  s1, s2: string;
begin
  s1 := Copy(TFileInfo(Item1).Filename, 1, 1);
  s2 := Copy(TFileInfo(Item2).Filename, 1, 1);
  if (s1 = '&') and (s2 <> '&') then
    result := -1
  else
    if (s1 <> '&') and (s2 = '&') then
      result := 1
    else
    begin
      if Boolean(TFileInfo(Item1).Attributes and faDirectory) <> Boolean(TFileInfo(Item2).Attributes and faDirectory) then
      begin
        if Boolean(TFileInfo(Item1).Attributes and faDirectory) and (not Boolean(TFileInfo(Item2).Attributes and faDirectory)) then
          Result := -1
        else
          Result := 1;
      end else
      begin
        if Trunc(TFileInfo(Item1).Modified) <> Trunc(TFileInfo(Item2).Modified) then
          result := Trunc(TFileInfo(Item1).Modified) - Trunc(TFileInfo(Item2).Modified)
        else
          result := Round(((TFileInfo(Item1).Modified - Trunc(TFileInfo(Item1).Modified)) * 10000000)) - Round(((TFileInfo(Item2).Modified - Trunc(TFileInfo(Item2).Modified)) * 10000000));
        if result = 0 then
          result := CompareStr(TFileInfo(Item1).Filename, TFileInfo(Item2).Filename);
      end;
    end;
end;

function SortByModifiedDesc(Item1, Item2: Pointer): Integer;
var
  s1, s2: string;
begin
  s1 := Copy(TFileInfo(Item1).Filename, 1, 1);
  s2 := Copy(TFileInfo(Item2).Filename, 1, 1);
  if (s1 = '&') and (s2 <> '&') then
    result := -1
  else
    if (s1 <> '&') and (s2 = '&') then
      result := 1
    else
    begin
      if Boolean(TFileInfo(Item1).Attributes and faDirectory) <> Boolean(TFileInfo(Item2).Attributes and faDirectory) then
      begin
        if Boolean(TFileInfo(Item1).Attributes and faDirectory) and (not Boolean(TFileInfo(Item2).Attributes and faDirectory)) then
          Result := -1
        else
          Result := 1;
      end else
      begin
        if Trunc(TFileInfo(Item2).Modified) <> Trunc(TFileInfo(Item1).Modified) then
          result := Trunc(TFileInfo(Item2).Modified) - Trunc(TFileInfo(Item1).Modified)
        else
          result := Round(((TFileInfo(Item2).Modified - Trunc(TFileInfo(Item2).Modified)) * 10000000)) - Round(((TFileInfo(Item1).Modified - Trunc(TFileInfo(Item1).Modified)) * 10000000));
        if result = 0 then
          result := CompareStr(TFileInfo(Item1).Filename, TFileInfo(Item2).Filename);
      end;
    end;
end;
{$ELSE}

function SortByFilename(Item1, Item2: TObject): Integer;
var
  s1, s2: string;
begin
  s1 := Copy(TFileInfo(Item1).Filename, 1, 1);
  s2 := Copy(TFileInfo(Item2).Filename, 1, 1);
  if (s1 = '&') and (s2 <> '&') then
    result := -1
  else
    if (s1 <> '&') and (s2 = '&') then
      result := 1
    else
    begin
      if Boolean(TFileInfo(Item1).Attributes and faDirectory) <> Boolean(TFileInfo(Item2).Attributes and faDirectory) then
      begin
        if Boolean(TFileInfo(Item1).Attributes and faDirectory) and (not Boolean(TFileInfo(Item2).Attributes and faDirectory)) then
          Result := -1
        else
          Result := 1;
      end else
        result := CompareStr(TFileInfo(Item1).Filename, TFileInfo(Item2).Filename);
    end;
end;

function SortByFilenameDesc(Item1, Item2: TObject): Integer;
var
  s1, s2: string;
begin
  s1 := Copy(TFileInfo(Item1).Filename, 1, 1);
  s2 := Copy(TFileInfo(Item2).Filename, 1, 1);
  if (s1 = '&') and (s2 <> '&') then
    result := -1
  else
    if (s1 <> '&') and (s2 = '&') then
      result := 1
    else
    begin
      if Boolean(TFileInfo(Item1).Attributes and faDirectory) <> Boolean(TFileInfo(Item2).Attributes and faDirectory) then
      begin
        if Boolean(TFileInfo(Item1).Attributes and faDirectory) and (not Boolean(TFileInfo(Item2).Attributes and faDirectory)) then
          Result := -1
        else
          Result := 1;
      end else
        result := CompareStr(TFileInfo(Item2).Filename, TFileInfo(Item1).Filename);
    end;
end;

function SortByType(Item1, Item2: TObject): Integer;
var
  s1, s2: string;
begin
  s1 := Copy(TFileInfo(Item1).Filename, 1, 1);
  s2 := Copy(TFileInfo(Item2).Filename, 1, 1);
  if (s1 = '&') and (s2 <> '&') then
    result := -1
  else
    if (s1 <> '&') and (s2 = '&') then
      result := 1
    else
    begin
      if Boolean(TFileInfo(Item1).Attributes and faDirectory) <> Boolean(TFileInfo(Item2).Attributes and faDirectory) then
      begin
        if Boolean(TFileInfo(Item1).Attributes and faDirectory) and (not Boolean(TFileInfo(Item2).Attributes and faDirectory)) then
          Result := -1
        else
          Result := 1;
      end else
      begin
        result := CompareStr(TFileInfo(Item1).FileType, TFileInfo(Item2).FileType);
        if result = 0 then
          result := CompareStr(TFileInfo(Item1).Filename, TFileInfo(Item2).Filename);
      end;
    end;
end;

function SortByTypeDesc(Item1, Item2: TObject): Integer;
var
  s1, s2: string;
begin
  s1 := Copy(TFileInfo(Item1).Filename, 1, 1);
  s2 := Copy(TFileInfo(Item2).Filename, 1, 1);
  if (s1 = '&') and (s2 <> '&') then
    result := -1
  else
    if (s1 <> '&') and (s2 = '&') then
      result := 1
    else
    begin
      if Boolean(TFileInfo(Item1).Attributes and faDirectory) <> Boolean(TFileInfo(Item2).Attributes and faDirectory) then
      begin
        if Boolean(TFileInfo(Item1).Attributes and faDirectory) and (not Boolean(TFileInfo(Item2).Attributes and faDirectory)) then
          Result := -1
        else
          Result := 1;
      end else
      begin
        result := CompareStr(TFileInfo(Item2).FileType, TFileInfo(Item1).FileType);
        if result = 0 then
          result := CompareStr(TFileInfo(Item1).Filename, TFileInfo(Item2).Filename);
      end;
    end;
end;

function SortBySize(Item1, Item2: TObject): Integer;
var
  s1, s2: string;
begin
  s1 := Copy(TFileInfo(Item1).Filename, 1, 1);
  s2 := Copy(TFileInfo(Item2).Filename, 1, 1);
  if (s1 = '&') and (s2 <> '&') then
    result := -1
  else
    if (s1 <> '&') and (s2 = '&') then
      result := 1
    else
    begin
      if Boolean(TFileInfo(Item1).Attributes and faDirectory) <> Boolean(TFileInfo(Item2).Attributes and faDirectory) then
      begin
        if Boolean(TFileInfo(Item1).Attributes and faDirectory) and (not Boolean(TFileInfo(Item2).Attributes and faDirectory)) then
          Result := -1
        else
          Result := 1;
      end else
      begin
        result := TFileInfo(Item1).Size - TFileInfo(Item2).Size;
        if result = 0 then
          result := CompareStr(TFileInfo(Item1).Filename, TFileInfo(Item2).Filename);
      end;
    end;
end;

function SortBySizeDesc(Item1, Item2: TObject): Integer;
var
  s1, s2: string;
begin
  s1 := Copy(TFileInfo(Item1).Filename, 1, 1);
  s2 := Copy(TFileInfo(Item2).Filename, 1, 1);
  if (s1 = '&') and (s2 <> '&') then
    result := -1
  else
    if (s1 <> '&') and (s2 = '&') then
      result := 1
    else
    begin
      if Boolean(TFileInfo(Item1).Attributes and faDirectory) <> Boolean(TFileInfo(Item2).Attributes and faDirectory) then
      begin
        if Boolean(TFileInfo(Item1).Attributes and faDirectory) and (not Boolean(TFileInfo(Item2).Attributes and faDirectory)) then
          Result := -1
        else
          Result := 1;
      end else
      begin
        result := TFileInfo(Item2).Size - TFileInfo(Item1).Size;
        if result = 0 then
          result := CompareStr(TFileInfo(Item1).Filename, TFileInfo(Item2).Filename);
      end;
    end;
end;

function SortByModified(Item1, Item2: TObject): Integer;
var
  s1, s2: string;
begin
  s1 := Copy(TFileInfo(Item1).Filename, 1, 1);
  s2 := Copy(TFileInfo(Item2).Filename, 1, 1);
  if (s1 = '&') and (s2 <> '&') then
    result := -1
  else
    if (s1 <> '&') and (s2 = '&') then
      result := 1
    else
    begin
      if Boolean(TFileInfo(Item1).Attributes and faDirectory) <> Boolean(TFileInfo(Item2).Attributes and faDirectory) then
      begin
        if Boolean(TFileInfo(Item1).Attributes and faDirectory) and (not Boolean(TFileInfo(Item2).Attributes and faDirectory)) then
          Result := -1
        else
          Result := 1;
      end else
      begin
        if Trunc(TFileInfo(Item1).Modified) <> Trunc(TFileInfo(Item2).Modified) then
          result := Trunc(TFileInfo(Item1).Modified) - Trunc(TFileInfo(Item2).Modified)
        else
          result := Round(((TFileInfo(Item1).Modified - Trunc(TFileInfo(Item1).Modified)) * 10000000)) - Round(((TFileInfo(Item2).Modified - Trunc(TFileInfo(Item2).Modified)) * 10000000));
        if result = 0 then
          result := CompareStr(TFileInfo(Item1).Filename, TFileInfo(Item2).Filename);
      end;
    end;
end;

function SortByModifiedDesc(Item1, Item2: TObject): Integer;
var
  s1, s2: string;
begin
  s1 := Copy(TFileInfo(Item1).Filename, 1, 1);
  s2 := Copy(TFileInfo(Item2).Filename, 1, 1);
  if (s1 = '&') and (s2 <> '&') then
    result := -1
  else
    if (s1 <> '&') and (s2 = '&') then
      result := 1
    else
    begin
      if Boolean(TFileInfo(Item1).Attributes and faDirectory) <> Boolean(TFileInfo(Item2).Attributes and faDirectory) then
      begin
        if Boolean(TFileInfo(Item1).Attributes and faDirectory) and (not Boolean(TFileInfo(Item2).Attributes and faDirectory)) then
          Result := -1
        else
          Result := 1;
      end else
      begin
        result := Round((TFileInfo(Item1).Modified - TFileInfo(Item2).Modified) * 1000);
        if result = 0 then
          result := CompareStr(TFileInfo(Item1).Filename, TFileInfo(Item2).Filename);
      end;
    end;
end;
{$ENDIF}

procedure TArcIWFileGrid.RefreshFiles;
  procedure AddItem(SR: TSearchRec);
  var
    fi: TFileInfo;
  begin
    fi := TFileInfo.Create(FPath, FCachePath, SR);
    if Assigned(FOnCheckDetail) then
      FOnCheckDetail(Self, FPath, SR, fi.Visible, fi.Enabled);
    FFileList.Add(fi);
  end;
var
  SR: TSearchRec;
  i: integer;
  sc: TListSortCompare;
begin
  for i := 0 to FFileList.Count - 1 do
    TFileInfo(FFileList[i]).Free;
  FFileList.Clear;

  if FindFirst(FPath + '*.*', faAnyFile, SR) = 0 then
  try
    repeat
      if (SR.Name <> '') and (SR.Name[1] = '.') then
      begin
        if (SR.Name = '..') and (FPath <> FRootPath) then
        begin
          SR.Name := '&lt;' + FParentText + '&gt;';
          AddItem(SR);
        end;
      end else
        AddItem(SR);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;

  sc:= nil;
  case FColumnSort of
    soFilename: sc := SortByFilename;
    soFilenameDesc: sc := SortByFilenameDesc;
    soType: sc := SortByType;
    soTypeDesc: sc := SortByTypeDesc;
    soSize: sc := SortBySize;
    soSizeDesc: sc := SortBySizeDesc;
    soModified: sc := SortByModified;
    soModifiedDesc: sc := SortByModifiedDesc;
  end;
  if Assigned(sc) then
    FFileList.Sort(sc);
  FNeedsRefresh := False;
end;

function TArcIWFileGrid.GetCurrentPathShort: string;
begin
  result := Copy(FPath, length(FRootPath), high(integer));
end;

function TArcIWFileGrid.GetRelativeURL: string;
var
  i, iLen: integer;
begin
  result := Copy(FPath, length(FRootPath), high(integer));
  iLen := length(Result);
  for i := 1 to iLen do
    if Result[i] = '\' then
      Result[i] := '/';
end;

{$IFNDEF INTRAWEB72}

function TArcIWFileGrid.RenderHTML: TIWHTMLTag;
{$ELSE}

function TArcIWFileGrid.RenderHTML(
  AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
  function RenderScrollbarStyle: string;
  begin
{$IFDEF INTRAWEB51}
    case FScrollbars of
      scrNone: result := 'overflow:hidden;';
      scrHorizontal: result := IfThen(WebApplication.Browser in [brIE], 'overflow-y:hidden;overflow-x: auto;', 'overflow:auto;');
      scrVertical: result := IfThen(WebApplication.Browser in [brIE], 'overflow-x:hidden;overflow-y: auto;', 'overflow:auto;');
      scrBoth: result := 'overflow:auto;';
    end;
{$ELSE}
    case FScrollbars of
      scrNone: result := 'overflow:hidden;';
      scrHorizontal: result := IfThen(BrowserIsIE(AContext.Browser), 'overflow-y:hidden;overflow-x: auto;', 'overflow:auto;');
      scrVertical: result := IfThen(BrowserIsIE(AContext.Browser), 'overflow-x:hidden;overflow-y: auto;', 'overflow:auto;');
      scrBoth: result := 'overflow:auto;';
    end;
{$ENDIF}
  end;
  function AlignmentToStr(Align: TAlignment): string;
  begin
    case Align of
      taLeftJustify: result := 'text-align: left;';
      taRightJustify: result := 'text-align: right;';
      taCenter: result := 'text-align: center;';
    end;
  end;
  function URLWindowJS(fi: TFileInfo): string;
  var
    sURL, sName, sOptions: string;
    iTop, iLeft, iWidth, iHeight: integer;
    wo: TIWWindowOptions;
  begin
    Result := '';

    iTop := -1;
    iLeft := -1;
    iWidth := -1;
    iHeight := -1;
    wo := [woButtons, woStatusBar, woMenuBar, woScrollBars, woResizable];

    sURL := '';
    if Lowercase(copy(RootPath,1,length(GServerController.FilesDir))) = lowercase(GServerController.FilesDir) then begin
      sURL := GServerController.FilesUrl;
      if copy(sURL,length(sURL),1)='/' then
        sURL := Copy(sURL,1,length(sURL)-1);
      sURL := sURL + copy(RootPath,length(GServerController.FilesDir)+1,99999)+ RelativeURL + fi.Filename;
    end;

    sName := '_Blank';

    if Assigned(FOnBuildURL) then
      FOnBuildURL(Self, fi.Filename, sURL, sName, iTop, iLeft, iWidth, iHeight, wo);

    sURL := TInURI.PathEncode(sURL);

    sURL := ReplaceText(sURL, '''', '%27');

    sOptions := '';
    if woButtons in wo then
      sOptions := sOptions + ',directories=yes';
    if woStatusBar in wo then
      sOptions := sOptions + ',status=yes';
    if woMenuBar in wo then
      sOptions := sOptions + ',menubar=yes';
    if woScrollBars in wo then
      sOptions := sOptions + ',scrollbars=yes';
    if woResizable in wo then
      sOptions := sOptions + ',resizable=yes';
    if woCopyHistory in wo then
      sOptions := sOptions + ',copyhistory=yes';
    if woFullScreen in wo then
      sOptions := sOptions + ',fullscreen=yes';

    if iLeft >= 0 then
      sOptions := sOptions + ',left=' + IntToStr(iLeft);
    if iTop >= 0 then
      sOptions := sOptions + ',top=' + IntToStr(iTop);
    if iWidth >= 0 then
      sOptions := sOptions + ',width=' + IntToStr(iWidth);
    if iHeight >= 0 then
      sOptions := sOptions + ',height=' + IntToStr(iHeight);

    if sOptions <> '' then
      if sOptions[1] = ',' then
        Delete(sOptions, 1, 1);
    result := 'window.open(''' + sURL + ''',''' + sName + ''',''' + sOptions + ''');return false;';
  end;
  function SizeToString(size: integer): string;
  type
    TBaseSizeType = (bstBytes, bstKB, bstMB, bstGB, bstTB);
  var
    Level: TBaseSizeType;
    fSize: Extended;
    i, iDecs: integer;
  begin
    Level := bstBytes;
    fSize := Size;

    while (fsize >= 1024) or (Level = bstTB) do
    begin
      fsize := fsize / 1024;
      inc(level);
    end;

    iDecs := 1;
    for i := 0 to FRoundPrecision - 1 do
      iDecs := iDecs * 10;

    result := FloatToStr(Round(fSize * iDecs) / iDecs);
    case Level of
      bstBytes: result := result + ' Bytes';
      bstKB: result := result + ' KB';
      bstMB: result := result + ' MB';
      bstGB: result := result + ' GB';
      bstTB: result := result + ' TB';
    end;
  end;
var
  tagRow, tagCol, tagScript, tagStyle, tagTable, tagCaption: TIWHTMLTag;
  y, iColCnt, iTableWidth: integer;
  s, sImg, sStyle: string;
  fi: TFileInfo;
begin
  if FNeedsRefresh then begin
    RefreshFiles;
  end;

  FWebApplication := {$IFDEF INTRAWEB72}AContext.WebApplication{$ELSE}WebApplication{$ENDIF};

  Result := TIWHTMLTag.CreateTag('span');
  Result.AddStringParam('style', 'width:' + IntToStr(Width) + 'px;Height:' + IntToStr(Height) + 'px;');

  if FActive then
  begin
    tagScript := Result.Contents.AddTag('script');
    tagScript.AddStringParam('language', 'JavaScript');

    tagScript.Contents.AddText(#13#10 +
      '  function ' + HTMLName + '_CreateDir() {'#13#10 +
      '    s = "New Folder";'#13#10 +
      '    s = prompt("Enter the New Folder Name",s);'#13#10 +
      '    SubmitClickConfirm("' + HTMLName + '",''C''+s,false,'''');'#13#10 +
      '  }'#13#10 +
      '  function ' + HTMLName + '_FileUpload() {'#13#10 +
      '    SubmitClickConfirm("' + HTMLName + '",''F'',false,'''');'#13#10 +
      '  }'#13#10 +
      '  function ' + HTMLName + '_DoCaptionClick(col) {'#13#10 +
      '    SubmitClickConfirm("' + HTMLName + '","hx"+col,false,"");'#13#10 +
      '  }'#13#10 +
      '  function ' + HTMLName + '_RenameDir(filename) {'#13#10 +
      '    s = filename.replace(":","''");'#13#10 +
      '    s = prompt("Enter the New Folder Name",s);'#13#10 +
      '    if (s!=filename) {' + #13#10 +
      '      SubmitClickConfirm("' + HTMLName + '",''R''+filename+'',''+s,false,'''');'#13#10 +
      '    }'#13#10 +
      '  }'#13#10 +
      '  function ' + HTMLName + '_DeleteDir(filename) {'#13#10 +
      '    filename = filename.replace(":","''");'#13#10 +
      '    if (confirm(''Are you sure you wish to delete the folder "''+filename+''"? This will delete the folder, all files in the folder, and all subfolders underneath.'')) {'#13#10 +
      '      SubmitClickConfirm("' + HTMLName + '",''-''+filename,false,'''');'#13#10 +
      '    }'#13#10 +
      '  }'#13#10 +
      '  function ' + HTMLName + '_RenameFile(filename) {'#13#10 +
      '    s = filename.replace(":","''");'#13#10 +
      '    s = prompt("Enter the New File Name",s);'#13#10 +
      '    if (s!=filename) {' + #13#10 +
      '      SubmitClickConfirm("' + HTMLName + '",''r''+filename+'',''+s,false,'''');'#13#10 +
      '    }'#13#10 +
      '  }'#13#10 +
      '  function ' + HTMLName + '_DeleteFile(filename) {'#13#10 +
      '    filename = filename.replace(":","''");'#13#10 +
      '    if (confirm(''Are you sure you wish to delete the file "''+filename+''"?'')) {'#13#10 +
      '      SubmitClickConfirm("' + HTMLName + '",''_''+filename,false,'''');'#13#10 +
      '    }'#13#10 +
      '  }'#13#10 +
      '  function ' + HTMLName + '_Download(filename) {'#13#10 +
      '    filename = filename.replace(":","''");'#13#10 +
      '    SubmitClickConfirm("' + HTMLName + '","D"+filename,false,'''');'#13#10 +
      '    //return false;'#13#10 +
      '  }'#13#10
      );
  end;

  tagStyle := Result.Contents.AddTag('style');

  iTableWidth := width;

  if BrowserIsIE({$IFDEF INTRAWEB72}AContext.Browser{$ELSE}WebApplication.Browser{$ENDIF}) and (not (FStyleTable.BorderStyle.Style in [brdNone, brdHidden])) then
    iTableWidth := width - (FStyleTable.BorderStyle.Width * 2);

  if not FAutoColumnWidth then
    iTableWidth := FColumnWidths.TotalWidths(FColumnVisibility);

  tagStyle.Contents.AddText(#13#10 +
    '  .' + HTMLName + 'CSS {' + FStyleTable.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF}) + RenderScrollBarStyle + '} ' + #13#10 +
    '  .' + HTMLName + '_tbl {position:absolute; left:0px; top:0px; width:' + IfThen(FAutoColumnWidth, '100%', IntToStr(iTableWidth) + 'px') + ';' + IfThen(FAutoRowHeight, 'height: 100%;') + '}'#13#10 +
    '  .' + HTMLName + '_rowhead { background-color: ' + ColorToRGBString(FStyleHeader.BackgroundColor) + '} ' + #13#10 +
    '  .' + HTMLName + '_colhead {' + FStyleHeader.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF}) + '} ' + #13#10 +
    '  .' + HTMLName + '_row { background-color:' + ColorToRGBString(FStyleDetail.BackgroundColor) + ';} ' + #13#10 +
    '  .' + HTMLName + '_col {' + FStyleDetail.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF}) + '} ' + #13#10 +
    '  .' + HTMLName + '_rowalt { background-color:' + ColorToRGBString(FStyleDetailAlt.BackgroundColor) + ';} ' + #13#10 +
    '  .' + HTMLName + '_colalt {' + FStyleDetailAlt.RenderCSS({$IFDEF INTRAWEB72}AContext{$ELSE}WebApplication.Browser{$ENDIF}) + '} ' + #13#10
    );

  tagTable := Result.Contents.AddTag('table');
  tagTable.AddStringParam('id', HTMLName + '_tbl');
  tagTable.AddStringParam('class', HTMLName + '_tbl');
  tagTable.AddIntegerParam('border', 0);
  tagTable.AddIntegerParam('cellpadding', 0);
  tagTable.AddIntegerParam('cellspacing', 0);
  tagTable.AddStringParam('summary', '');

  {if StaticHeader then
  begin
    tagRow := tagTable.Contents.AddTag('Caption');
    tagRow.addStringParam('style','visibility:hidden;');
  end;}

  if FShowHeaderRow then
  begin
    // Create Header Row
    {if StaticHeader then
    begin
      tagRow := tagTable.Contents.AddTag('thead');
      tagRow := tagRow.Contents.AddTag('tr');
    end else}
    tagRow := tagTable.Contents.AddTag('tr');
    tagRow.AddStringParam('class', HTMLName + '_rowhead');

    // Do Caption Line
    {if StaticHeader then
      tagCaption := tagRow.Contents.AddTag('th')
    else}
    tagCaption := tagRow.Contents.AddTag('td');
    tagCaption.AddStringParam('class', HTMLName + '_colhead');
    sStyle := AlignmentToStr(CaptionAlignment);
    if Assigned(FOnCaptionClick) then
    begin
      tagCaption.AddStringParam('onClick', HTMLName + '_DoCaptionClick(0);');
      sStyle := sStyle + 'cursor: pointer;';
    end;
    tagCaption.AddStringParam('style', sStyle);
    s := '';
    if FRights.FDirectoryCreate then
      s := s + '<img width=16 height=16 style="cursor: pointer;" onClick="' + HTMLName + '_CreateDir();" src="' + FIcons.URLDirectoryCreate + '" alt="Create Folder" title="Create Folder">';
    if FRights.FFileUpload then
      s := s + '<img width=16 height=16 style="cursor: pointer;" onClick="' + HTMLName + '_FileUpload();" src="' + FIcons.URLFileUpload + '" alt="File Upload" title="File Upload">';
    if s <> '' then
      s := s + '&nbsp;';
    if not FShowFullPath then
      s := s + FCaption + ' ' + Copy(FPath, length(FRootPath), high(integer))
    else
      s := s + FCaption + ' ' + FPath;
    tagCaption.Contents.AddText(s);

    iColCnt := 1;

    // Create Header Row
    {if StaticHeader then
    begin
      tagRow := tagRow.Contents.AddTag('tr');
    end else}
    tagRow := tagTable.Contents.AddTag('tr');

    tagRow.AddStringParam('class', HTMLName + '_rowhead');

    // Do Button Column
    if FRights.DirectoryDelete or FRights.DirectoryRename or FRights.FileDelete or FRights.FileRename then
    begin
      inc(iColCnt);
      {if StaticHeader then
        tagCol := tagRow.Contents.AddTag('th')
      else}
      tagCol := tagRow.Contents.AddTag('td');
      tagCol.AddStringParam('class', HTMLName + '_colhead');
      sStyle := AlignmentToStr(CaptionAlignment) + 'width:36px;';
      if Assigned(FOnColumnClick) then
      begin
        tagCol.AddStringParam('onClick', HTMLName + '_DoCaptionClick(1);');
        sStyle := sStyle + 'cursor: pointer;';
      end;
      tagCol.AddStringParam('style', sStyle);
      tagCol.Contents.AddText('&nbsp;');
    end;

    // Do Filename Column
    {if StaticHeader then
      tagCol := tagRow.Contents.AddTag('th')
    else}
    tagCol := tagRow.Contents.AddTag('td');
    tagCol.AddStringParam('class', HTMLName + '_colhead');
    sStyle := AlignmentToStr(CaptionAlignment);
    if not FAutoColumnWidth then
      sStyle := sStyle + 'width: ' + IntToStr(FColumnWidths.Filename) + 'px;';
    if Assigned(FOnColumnClick) then
    begin
      tagCol.AddStringParam('onClick', HTMLName + '_DoCaptionClick(2);');
      sStyle := sStyle + 'cursor: pointer;';
    end;
    tagCol.AddStringParam('style', sStyle);

    if FIcons.URLCaptionFilename <> '' then
      sImg := '<img src="' + FIcons.URLCaptionFIlename + '">'
    else
      sImg := '';
    tagCol.Contents.AddText(FColumnCaption.Filename + sImg);

    // Do Size Column
    if (fgcSize in FColumnVisibility) then
    begin
      inc(iColCnt);
      {if StaticHeader then
        tagCol := tagRow.Contents.AddTag('th')
      else}
      tagCol := tagRow.Contents.AddTag('td');
      tagCol.AddStringParam('class', HTMLName + '_colhead');
      sStyle := AlignmentToStr(CaptionAlignment);
      if not FAutoColumnWidth then
        sStyle := sStyle + 'width: ' + IntToStr(FColumnWidths.Size) + 'px;';
      if Assigned(FOnColumnClick) then
      begin
        tagCol.AddStringParam('onClick', HTMLName + '_DoCaptionClick(3);');
        sStyle := sStyle + 'cursor: pointer;';
      end;
      tagCol.AddStringParam('style', sStyle);

      if FIcons.URLCaptionSize <> '' then
        sImg := '<img src="' + FIcons.URLCaptionSize + '">'
      else
        sImg := '';
      tagCol.Contents.AddText(FColumnCaption.Size + sImg);
    end;

    // Do FileType Column
    if (fgcType in FColumnVisibility) then
    begin
      inc(iColCnt);
      {if StaticHeader then
        tagCol := tagRow.Contents.AddTag('th')
      else}
      tagCol := tagRow.Contents.AddTag('td');
      tagCol.AddStringParam('class', HTMLName + '_colhead');
      sStyle := AlignmentToStr(CaptionAlignment);
      if not FAutoColumnWidth then
        sStyle := sStyle + 'width: ' + IntToStr(FColumnWidths.Size) + 'px;';
      if Assigned(FOnColumnClick) then
      begin
        tagCol.AddStringParam('onClick', HTMLName + '_DoCaptionClick(4);');
        sStyle := sStyle + 'cursor: pointer;';
      end;
      tagCol.AddStringParam('style', sStyle);
      if FIcons.URLCaptionType <> '' then
        sImg := '<img src="' + FIcons.URLCaptionType + '">'
      else
        sImg := '';
      tagCol.Contents.AddText(FColumnCaption.FileType + sImg);
    end;

    // Do Modified Column
    if (fgcModified in FColumnVisibility) then
    begin
      inc(iColCnt);
      {if StaticHeader then
        tagCol := tagRow.Contents.AddTag('th')
      else}
      tagCol := tagRow.Contents.AddTag('td');
      tagCol.AddStringParam('class', HTMLName + '_colhead');
      sStyle := AlignmentToStr(CaptionAlignment);
      if not FAutoColumnWidth then
        sStyle := sStyle + 'width: ' + IntToStr(FColumnWidths.FModified) + 'px;';
      if Assigned(FOnColumnClick) then
      begin
        tagCol.AddStringParam('onClick', HTMLName + '_DoCaptionClick(5);');
        sStyle := sStyle + 'cursor: pointer;';
      end;
      tagCol.AddStringParam('style', sStyle);
      if FIcons.URLCaptionModified <> '' then
        sImg := '<img src="' + FIcons.URLCaptionModified + '">'
      else
        sImg := '';
      tagCol.Contents.AddText(FColumnCaption.FModified + sImg);
    end;

    tagCaption.AddIntegerParam('Colspan', iColCnt);
  end else
  begin
    iColCnt := 1;
    if (fgcSize in FColumnVisibility) then
      inc(iColCnt);
    if (fgcType in FColumnVisibility) then
      inc(iColCnt);
    if (fgcModified in FColumnVisibility) then
      inc(iColCnt);
  end;

  if FActive then
  begin
    // Create Rows
    for y := 0 to FFileList.Count - 1 do
    begin
      fi := TFileInfo(FFileList[y]);
      if fi.Visible then
      begin
        {if StaticHeader then
        begin
          tagRow := tagTable.Contents.AddTag('tbody');
          tagRow := tagRow.Contents.AddTag('tr');
        end else}
        tagRow := tagTable.Contents.AddTag('tr');

        //comment by peter 2005/05/19
        if fi.Filename = FSelectedRowValue then
        begin
          if FScrollToSelectedRow and (BrowserIsIE(AContext.Browser) or BrowserIsNetscape7(AContext.Browser)) then
          begin
            tagRow.AddStringParam('id', HTMLName + _SelectedRow + '_' + IntToStr(y));
            {$IFDEF INTRAWEB110}
            if False then
            {$ELSE}
            if AContext.PageContext.WebApplication.IsPartialUpdate then
            {$ENDIF}
              TIWPageContext40(AContext.PageContext).AddToUpdateInitProc('IWTop().document.getElementById("' + HTMLName + _SelectedRow + '_' + IntToStr(y) + '").scrollIntoView(false)')
            else
              TIWPageContext40(AContext.PageContext).AddToInitProc('IWTop().document.getElementById("' + HTMLName + _SelectedRow + '_' + IntToStr(y) + '").scrollIntoView(false)');
          end;
        end;
        //

        if FUseAltStyles then
          if (y mod 2) <> 0 then
            tagRow.AddStringParam('class', HTMLName + '_rowalt')
          else
            tagRow.AddStringParam('class', HTMLName + '_row')
        else
          tagRow.AddStringParam('class', HTMLName + '_row');

        // Do Button Column
        if FRights.DirectoryDelete or FRights.DirectoryRename or FRights.FileDelete or FRights.FileRename then
        begin
          tagCol := tagRow.Contents.AddTag('td');

          sStyle := AlignmentToStr(taLeftJustify) + 'width:36px;';
          tagCol.AddStringParam('style', sStyle);

          if FUseAltStyles then
            if (y mod 2) <> 0 then
              tagCol.AddStringParam('class', HTMLName + '_colalt')
            else
              tagCol.AddStringParam('class', HTMLName + '_col')
          else
            tagCol.AddStringParam('class', HTMLName + '_col');

          if fi.Enabled then begin
            s := '';
            if FRights.DirectoryDelete and (fi.FileType = 'Folder') and (fi.Filename <> '&lt;' + FParentText + '&gt;') then
              s := s + '<img width=16 height=16 style="cursor: pointer;" onClick="' + HTMLName + '_DeleteDir(''' + fi.EncFilename + ''');" src="' + FIcons.URLDirectoryDelete + '" alt="Delete Folder" title="Delete Folder">';
            if FRights.DirectoryRename and (fi.FileType = 'Folder') and (fi.Filename <> '&lt;' + FParentText + '&gt;') then
              s := s + '<img width=16 height=16 style="cursor: pointer;" onClick="' + HTMLName + '_RenameDir(''' + fi.EncFilename + ''');" src="' + FIcons.URLDirectoryRename + '" alt="Rename Folder" title="Rename Folder">';
            if FRights.FileDelete and (fi.FileType <> 'Folder') then
              s := s + '<img width=16 height=16 style="cursor: pointer;" onClick="' + HTMLName + '_DeleteFile(''' + fi.EncFilename + ''');" src="' + FIcons.URLFileDelete + '" alt="Delete File" title="Delete File">';
            if FRights.FileRename and (fi.FileType <> 'Folder') then
              s := s + '<img width=16 height=16 style="cursor: pointer;" onClick="' + HTMLName + '_RenameFile(''' + fi.EncFilename + ''');" src="' + FIcons.URLFileRename + '" alt="Rename File" title="Rename File">';

            tagCol.Contents.AddText(s);
          end;
        end;

        // Do Filename Column
        if (fgcFileName in FColumnVisibility) then
        begin
          tagCol := tagRow.Contents.AddTag('td');

          sStyle := AlignmentToStr(taLeftJustify);
          tagCol.AddStringParam('style', sStyle);

          if FUseAltStyles then
            if (y mod 2) <> 0 then
              tagCol.AddStringParam('class', HTMLName + '_colalt')
            else
              tagCol.AddStringParam('class', HTMLName + '_col')
          else
            tagCol.AddStringParam('class', HTMLName + '_col');

          if fi.FileType <> 'Folder' then
          begin
            //comment by peter 2005/05/20
            if FRights.FileDownload and fi.Enabled then
            begin
              case FDownloadStyle of
                fdsSendFileAttach: tagCol.Contents.AddText(ifThen(FShowIcons, '<img width=16 height=16 onClick="' + HTMLName + '_Download(''' + fi.EncFilename + ''')" src="' + FCacheURL + fi.FileType + '.jpg">&nbsp;') + '<a href="javascript:' + HTMLName + '_Download(''' + fi.EncFilename + ''')">' + fi.Filename + '</a>');
                fdsSendFile: tagCol.Contents.AddText(ifThen(FShowIcons, '<img width=16 height=16 onClick="' + HTMLName + '_Download('':' + fi.EncFilename + ''')" src="' + FCacheURL + fi.FileType + '.jpg">&nbsp;') + '<a href="javascript:' + HTMLName + '_Download('':' + fi.EncFilename + ''')">' + fi.Filename + '</a>');
                fdsLinkNewWindow: tagCol.Contents.AddText(ifThen(FShowIcons, '<img width=16 height=16 onClick="' + URLWindowJS(fi) + '" src="' + FCacheURL + fi.FileType + '.jpg">&nbsp;') + '<a href="#" onClick="' + URLWindowJS(fi) + '">' + fi.Filename + '</a>');
              end;
            end
            else
              tagCol.Contents.AddText(ifThen(FShowIcons, '<img width=16 height=16 src="' + FCacheURL + fi.FileType + '.jpg">&nbsp;') + fi.Filename);
          end
          else
            //comment by peter 2005/05/20
            //tagCol.Contents.AddText(ifThen(FShowIcons, '<img width=16 height=16 onClick="' + HTMLName + '_Download(''' + fi.EncFilename + ''')" src="' + IfThen(fi.Filename = '&lt;' + FParentText + '&gt;', FIcons.URLParentFolder(FCacheURL + fi.FileType + '.jpg'), FCacheURL + fi.FileType + '.jpg') + '">&nbsp;') + '<a href="javascript:' + HTMLName + '_Download(''' + fi.EncFilename + ''')">' + fi.Filename + '</a>');
            if fi.Enabled then
              tagCol.Contents.AddText(ifThen(FShowIcons, '<img width=16 height=16 onClick="' + HTMLName + '_Download(''' + fi.EncFilename + ''')" src="' + IfThen(fi.Filename = '&lt;' + FParentText + '&gt;', FIcons.URLParentFolder(FCacheURL + fi.FileType + '.jpg'), FCacheURL + fi.FileType + '.jpg') + '">&nbsp;') + '<a href="javascript:' + HTMLName + '_Download(''' + fi.EncFilename + ''')">' + fi.Filename + '</a>')
            else
              tagCol.Contents.AddText(ifThen(FShowIcons, '<img width=16 height=16 src="' + IfThen(fi.Filename = '&lt;' + FParentText + '&gt;', FIcons.URLParentFolder(FCacheURL + fi.FileType + '.jpg'), FCacheURL + fi.FileType + '.jpg') + '">&nbsp;') + fi.Filename);
        end;
        // Do Size Column
        if (fgcSize in FColumnVisibility) then
        begin
          tagCol := tagRow.Contents.AddTag('td');

          sStyle := 'text-align: right;'; //AlignmentToStr(taLeftJustify);
          tagCol.AddStringParam('style', sStyle);

          if FUseAltStyles then
            if (y mod 2) <> 0 then
              tagCol.AddStringParam('class', HTMLName + '_colalt')
            else
              tagCol.AddStringParam('class', HTMLName + '_col')
          else
            tagCol.AddStringParam('class', HTMLName + '_col');

          if fi.FileType <> 'Folder' then
            tagCol.Contents.AddText(SizeToString(fi.Size) + '&nbsp;')
          else
            tagCol.Contents.AddText('&nbsp;')
        end;
        // Do FileType Column
        if (fgcType in FColumnVisibility) then
        begin
          tagCol := tagRow.Contents.AddTag('td');

          sStyle := AlignmentToStr(taLeftJustify);
          tagCol.AddStringParam('style', sStyle);

          if FUseAltStyles then
            if (y mod 2) <> 0 then
              tagCol.AddStringParam('class', HTMLName + '_colalt')
            else
              tagCol.AddStringParam('class', HTMLName + '_col')
          else
            tagCol.AddStringParam('class', HTMLName + '_col');

          if fi.Filename <> '&lt;' + FParentText + '&gt;' then
            tagCol.Contents.AddText(fi.FileType)
          else
            tagCol.Contents.AddText('&nbsp;');
        end;
        // Do Date Column
        if (fgcModified in FColumnVisibility) then
        begin
          tagCol := tagRow.Contents.AddTag('td');

          sStyle := AlignmentToStr(taLeftJustify);
          tagCol.AddStringParam('style', sStyle);

          if FUseAltStyles then
            if (y mod 2) <> 0 then
              tagCol.AddStringParam('class', HTMLName + '_colalt')
            else
              tagCol.AddStringParam('class', HTMLName + '_col')
          else
            tagCol.AddStringParam('class', HTMLName + '_col');

          if fi.Filename <> '&lt;' + FParentText + '&gt;' then
            tagCol.Contents.AddText(DateTimeToStr(fi.Modified))
          else
            tagCol.Contents.AddText('&nbsp;');
        end;
      end;
    end;
  end;
end;

procedure TArcIWFileGrid.SetColumnVisibility(const Value: TFileGridColumns);
begin
  FColumnVisibility := Value;
  FColCount := 1;
  if not (fgcFilename in FColumnVisibility) then
    include(FColumnVisibility, fgcFilename);

  if fgcSize in FColumnVisibility then
    inc(FColCount);
  if fgcType in FColumnVisibility then
    inc(FColCount);
  if fgcModified in FColumnVisibility then
    inc(FColCount);
end;

procedure TArcIWFileGrid.SetRootPath(const Value: string);
begin
  FRootPath := Value;
  if FRootPath = '' then
    FRootPath := '.\';
  if Copy(FRootPath, length(FRootPath), 1) <> '\' then
    FRootPath := FRootPath + '\';
  FPath := FRootPath;
  if (not (csDesigning in ComponentState)) and (not (csLoading in ComponentState)) then
    RefreshFiles;
end;

procedure TArcIWFileGrid.Submit(const AValue: string);
  procedure DoCreateDirectory;
  var
    sFile: string;
    b: boolean;
  begin
    sFile := Copy(AValue, 2, High(Integer)) + '\';
    //comment by peter 2005/05/19
    FSelectedRowValue := sFile;
    //
    if sFile = 'null\' then exit;
    b := True;
    if Assigned(FOnCreateDirectory) then
      FOnCreateDirectory(Self, sFile, b);
    if b then
      CreateSubDirectory(sFile);
  end;
  procedure DoDownload;
  var
    sFile: string;
    bAttach: boolean;
  begin
    sFile := Copy(AValue, 2, High(Integer));
    //comment by peter 2005/05/19
    FSelectedRowValue := sFile;
    //

    bAttach := True;
    if sFile <> '' then
      if sFile[1] = '''' then
      begin
        bAttach := False;
        Delete(sFile, 1, 1);
      end;

    if sFile = '<' + FParentText + '>' then
    begin
      MoveUpLevel;
    end else
    begin
      while Pos('\..\', sFile) > 0 do
        StringReplace(sFile, '\..\', '', [rfReplaceAll, rfIgnoreCase]);
      while Pos('\..', sFile) > 0 do
        StringReplace(sFile, '\..', '', [rfReplaceAll, rfIgnoreCase]);
      while Pos('..\', sFile) > 0 do
        StringReplace(sFile, '..\', '', [rfReplaceAll, rfIgnoreCase]);
      while Pos('..', sFile) > 0 do
        StringReplace(sFile, '..', '', [rfReplaceAll, rfIgnoreCase]);
      while Pos('\\', sFile) > 0 do
        StringReplace(sFile, '\\', '\', [rfReplaceAll, rfIgnoreCase]);
      if DirectoryExists(FPath + sFile) then
      begin
        if Copy(sFile, Length(sFile), 1) <> '\' then
          sFile := sFile + '\';
        FPath := FPath + sFile;
        RefreshFiles;
      end else
      begin
        if FileExists(FPath + sFile) then
        begin
          if Assigned(FOnDownloadFile) then
            FOnDownloadFile(Self, FPath + sFile);
          FWebApplication.SendFile(FPath + sFile, {$IFNDEF INTRAWEB72} '', '', {$ENDIF}bAttach)
        end else
          raise Exception.Create('Error retrieving file "' + FPath + sFile + '"');
      end;
    end;
  end;
  procedure DoRenameDirectory;
  var
    sFrom, sTo: string;
    b: boolean;
  begin
    sFrom := Copy(AValue, 2, High(Integer));
    sTo := Copy(sFrom, Pos(',', sFrom) + 1, High(Integer));
    sFrom := Copy(sFrom, 1, Pos(',', sFrom) - 1);

    if sTo = 'null' then exit;

    //comment by peter 2005/05/19
    FSelectedRowValue := sTo;
    //
    b := True;
    if Assigned(FOnRenameDirectory) then
      FOnRenameDirectory(Self, sFrom, sTo, b);
    if b then
      RenameDirectory(sFrom, sTo);
  end;
  procedure DoRenameFile;
  var
    sFrom, sTo: string;
    b: boolean;
  begin
    sFrom := Copy(AValue, 2, High(Integer));
    sTo := Copy(sFrom, Pos(',', sFrom) + 1, High(Integer));
    sFrom := Copy(sFrom, 1, Pos(',', sFrom) - 1);

    if sTo = 'null' then exit;

    //comment by peter 2005/05/19
    FSelectedRowValue := sTo;
    //

    b := True;
    if Assigned(FOnRenameFile) then
      FOnRenameFile(Self, sFrom, sTo, b);
    if b then
      RenameFile(sFrom, sTo);
  end;
  procedure DoDeleteDirectory;
  var
    sFile: string;
    b: boolean;
  begin
    sFile := Copy(AValue, 2, High(Integer));
    //comment by peter 2005/05/19
    FSelectedRowValue := sFile;
    //

    b := True;
    if Assigned(FOnDeleteDirectory) then
      FOnDeleteDirectory(Self, sFile, b);
    if b then
      DeleteDirectory(sFile);
  end;
  procedure DoDeleteFile;
  var
    sFile: string;
    b: boolean;
  begin
    sFile := Copy(AValue, 2, High(Integer));
    //comment by peter 2005/05/19
    FSelectedRowValue := sFile;
    //
    b := True;
    if Assigned(FOnDeleteFile) then
      FOnDeleteFile(Self, sFile, b);
    if b then
      DeleteFile(sFile);
  end;
  procedure DoColumnClick;
    procedure FireCaptionClickEvent;
    begin
      if Assigned(FOnCaptionClick) then
        FOnCaptionClick(Self);
    end;
    procedure FireColumnClickEvent(Column: TFileGridColumn);
    begin
      if Assigned(FOnColumnClick) then
        FOnColumnClick(Self, Column);
    end;
  var
    s: string;
  begin
    s := Copy(AValue, 3, 1);
    case StrToIntDef(s, -1) of
      0: FireCaptionClickEvent;
      1: ; // blank space above buttons
      2: FireColumnClickEvent(fgcFilename);
      3: FireColumnClickEvent(fgcSize);
      4: FireColumnClickEvent(fgcType);
      5: FireColumnClickEvent(fgcModified);
    end;
  end;
begin
  inherited;
  if AValue <> '' then
  begin
    case AValue[1] of
      'C': DoCreateDirectory;
      'F': UploadFile;
      'R': DoRenameDirectory;
      'r': DoRenameFile;
      '-': DoDeleteDirectory;
      '_': DoDeleteFile;
      'D': DoDownload;
      'h': DoColumnClick;
    end;
  end;
end;

procedure TArcIWFileGrid.MoveUpLevel;
begin
  FPath := Copy(FPath, 1, length(FPath) - 1);
  FPath := Copy(FPath, 1, length(FPath) - Pos('\', ReverseString(FPath)) + 1);
  if Length(FPath) < Length(FRootPath) then
  begin
    FPath := FRootPath;
    raise Exception.Create('Cannot move to this folder.');
  end;
  RefreshFiles;
end;

{ TDirectoryRights }

procedure TDirectoryRights.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TDirectoryRights(Dest).FFileUpload := FFileUpload;
  TDirectoryRights(Dest).FFileDelete := FFileDelete;
  TDirectoryRights(Dest).FDirectoryDelete := FDirectoryDelete;
  TDirectoryRights(Dest).FFileDownload := FFileDownload;
  TDirectoryRights(Dest).FFileRename := FFileRename;
  TDirectoryRights(Dest).FDirectoryCreate := FDirectoryCreate;
  TDirectoryRights(Dest).FDirectoryRename := FDirectoryRename;
end;

constructor TDirectoryRights.Create;
begin
  inherited Create;
  FFileUpload := False;
  FFileDelete := False;
  FDirectoryDelete := False;
  FFileDownload := True;
  FFileRename := False;
  FDirectoryCreate := False;
  FDirectoryRename := False;
end;

{ TFileInfo }

constructor TFileInfo.Create(FilePath, CachePath: string; SR: TSearchRec);
var
  sFileType: string;
begin
  inherited Create;
  Filename := SR.Name;
  Size := SR.Size;


  FindFileIcon(Boolean(SR.Attr and faDirectory), FilePath, CachePath, SR.Name, sFileType);
  FileType := sFileType;
  Icon := sFileType + '.jpg';

  Modified := FileDateToDateTime(SR.Time);
  Attributes := SR.Attr;

  Visible := True;
  Enabled := True;
end;

function TFileInfo.EncFilename: string;
var
  i, iLen: integer;
begin
  Result := Filename;
  iLen := Length(Result);
  for i := 1 to iLen do
    if Result[i] = '''' then
      Result[i] := ':'; // substitute a ' for a : as it is an invalid path char anyway and the ' screws up the js param.  We will reverse in js code.
end;

{ TColumnCaptions }

procedure TColumnCaptions.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TColumnCaptions(Dest).FFilename := FFilename;
  TColumnCaptions(Dest).FFileType := FFileType;
  TColumnCaptions(Dest).FSize := FSize;
  TColumnCaptions(Dest).FModified := FModified;
end;

constructor TColumnCaptions.Create;
begin
  inherited Create;
  FFileType := 'Type';
  FSize := 'Size';
  FModified := 'Modified';
  FFilename := 'Filename';
end;

{ TColumnWidths }

procedure TColumnWidths.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TColumnWidths(Dest).FFilename := FFilename;
  TColumnWidths(Dest).FFileType := FFileType;
  TColumnWidths(Dest).FSize := FSize;
  TColumnWidths(Dest).FModified := FModified;
end;

constructor TColumnWidths.Create;
begin
  inherited Create;
  FFileType := 100;
  FSize := 75;
  FModified := 150;
  FFilename := 200;
end;

function TColumnWidths.TotalWidths(cols: TFileGridColumns): integer;
begin
  Result := FFilename;
  if (fgcType in cols) then
    inc(Result, FFileType);
  if (fgcSize in cols) then
    inc(Result, FSize);
  if (fgcModified in cols) then
    inc(Result, FModified);
end;

{ TArcFileGridIcons }

procedure TArcFileGridIcons.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
{$IFDEF INTRAWEB72}
  TArcFileGridIcons(Dest).FFileUpload := FFileUpload;
  TArcFileGridIcons(Dest).FFileDelete := FFileDelete;
  TArcFileGridIcons(Dest).FDirectoryDelete := FDirectoryDelete;
  TArcFileGridIcons(Dest).FFileRename := FFileRename;
  TArcFileGridIcons(Dest).FDirectoryCreate := FDirectoryCreate;
  TArcFileGridIcons(Dest).FDirectoryRename := FDirectoryRename;
  TArcFileGridIcons(Dest).FParentFolder := FParentFolder;
  TArcFileGridIcons(Dest).FCaptionFilename := FCaptionFilename;
  TArcFileGridIcons(Dest).FCaptionSize := FCaptionSize;
  TArcFileGridIcons(Dest).FCaptionType := FCaptionType;
  TArcFileGridIcons(Dest).FCaptionModified := FCaptionModified;
{$ELSE}
  TArcFileGridIcons(Dest).FFileUploadURL := FFileUploadURL;
  TArcFileGridIcons(Dest).FFileDeleteURL := FFileDeleteURL;
  TArcFileGridIcons(Dest).FDirectoryDeleteURL := FDirectoryDeleteURL;
  TArcFileGridIcons(Dest).FFileRenameURL := FFileRenameURL;
  TArcFileGridIcons(Dest).FDirectoryCreateURL := FDirectoryCreateURL;
  TArcFileGridIcons(Dest).FDirectoryRenameURL := FDirectoryRenameURL;
  TArcFileGridIcons(Dest).FParentFolderURL := FParentFolderURL;
  TArcFileGridIcons(Dest).FCaptionFilenameURL := FCaptionFilenameURL;
  TArcFileGridIcons(Dest).FCaptionSizeURL := FCaptionSizeURL;
  TArcFileGridIcons(Dest).FCaptionTypeURL := FCaptionTypeURL;
  TArcFileGridIcons(Dest).FCaptionModifiedURL := FCaptionModifiedURL;
{$ENDIF}
end;

constructor TArcFileGridIcons.Create(Owner: TArcIWFileGrid);
begin
  inherited Create;
  FOwner := Owner;
{$IFDEF INTRAWEB72}
  FFileUpload := TIWFileReference.Create;
  FFileDelete := TIWFileReference.Create;
  FDirectoryDelete := TIWFileReference.Create;
  FFileRename := TIWFileReference.Create;
  FDirectoryCreate := TIWFileReference.Create;
  FDirectoryRename := TIWFileReference.Create;
  FParentFolder := TIWFileReference.Create;
  FCaptionFilename := TIWFileReference.Create;
  FCaptionSize := TIWFileReference.Create;
  FCaptionType := TIWFileReference.Create;
  FCaptionModified := TIWFileReference.Create;
{$ENDIF}
end;

destructor TArcFileGridIcons.Destroy;
begin
{$IFDEF INTRAWEB72}
  FParentFolder.Free;
  FFileUpload.Free;
  FFileDelete.Free;
  FDirectoryDelete.Free;
  FFileRename.Free;
  FDirectoryCreate.Free;
  FDirectoryRename.Free;
  FCaptionFilename.Free;
  FCaptionSize.Free;
  FCaptionType.Free;
  FCaptionModified.Free;
{$ENDIF}
  inherited;
end;

function TArcFileGridIcons.URLCaptionFilename: string;
begin
{$IFNDEF INTRAWEB72}
  Result := FCaptionFilenameURL;
{$ELSE}
  Result := FCaptionFilename.Location(GServerController.FilesURL);
{$ENDIF}
end;

function TArcFileGridIcons.URLCaptionModified: string;
begin
{$IFNDEF INTRAWEB72}
  Result := FCaptionModifiedURL;
{$ELSE}
  Result := FCaptionModified.Location(GServerController.FilesURL);
{$ENDIF}
end;

function TArcFileGridIcons.URLCaptionSize: string;
begin
{$IFNDEF INTRAWEB72}
  Result := FCaptionSizeURL;
{$ELSE}
  Result := FCaptionSize.Location(GServerController.FilesURL);
{$ENDIF}
end;

function TArcFileGridIcons.URLCaptionType: string;
begin
{$IFNDEF INTRAWEB72}
  Result := FCaptionTypeURL;
{$ELSE}
  Result := FCaptionType.Location(GServerController.FilesURL);
{$ENDIF}
end;

function TArcFileGridIcons.URLDirectoryCreate: string;
begin
{$IFNDEF INTRAWEB72}
  Result := FDirectoryCreateURL;
{$ELSE}
  Result := FDirectoryCreate.Location(GServerController.FilesURL);
{$ENDIF}
  if Result = '' then
    Result := '/gfx/ArcCreateDir.png';
end;

function TArcFileGridIcons.URLDirectoryDelete: string;
begin
{$IFNDEF INTRAWEB72}
  Result := FDirectoryDeleteURL;
{$ELSE}
  Result := FDirectoryDelete.Location(GServerController.FilesURL);
{$ENDIF}
  if Result = '' then
    Result := '/gfx/ArcDelete.png';
end;

function TArcFileGridIcons.URLDirectoryRename: string;
begin
{$IFNDEF INTRAWEB72}
  Result := FDirectoryRenameURL;
{$ELSE}
  Result := FDirectoryRename.Location(GServerController.FilesURL);
{$ENDIF}
  if Result = '' then
    Result := '/gfx/ArcRename.png';
end;

function TArcFileGridIcons.URLFileDelete: string;
begin
{$IFNDEF INTRAWEB72}
  Result := FFileDeleteURL;
{$ELSE}
  Result := FFileDelete.Location(GServerController.FilesURL);
{$ENDIF}
  if Result = '' then
    Result := '/gfx/ArcDelete.png';
end;

function TArcFileGridIcons.URLFileRename: string;
begin
{$IFNDEF INTRAWEB72}
  Result := FFileRenameURL;
{$ELSE}
  Result := FFileRename.Location(GServerController.FilesURL);
{$ENDIF}
  if Result = '' then
    Result := '/gfx/ArcRename.png';
end;

function TArcFileGridIcons.URLFileUpload: string;
begin
{$IFNDEF INTRAWEB72}
  Result := FFileUploadURL;
{$ELSE}
  Result := FFileUpload.Location(GServerController.FilesURL);
{$ENDIF}
  if Result = '' then
    Result := '/gfx/ArcFileUpload.png';
end;

function TArcFileGridIcons.URLParentFolder(DefaultURL: string): string;
begin
{$IFNDEF INTRAWEB72}
  Result := FParentFolderURL;
{$ELSE}
  Result := FParentFolder.Location(GServerController.FilesURL);
{$ENDIF}
  if Result = '' then
    result := DefaultURL;
end;

initialization // Plp: TIWServer.AddInternalFile isnt thread-saft so it must me right here
  TIWServer.AddInternalFile('IW_GFX_ARCFILEUPLOAD', '/gfx/ArcFileUpload.png');
  TIWServer.AddInternalFile('IW_GFX_ARCCREATEDIR', '/gfx/ArcCreateDir.png');
  TIWServer.AddInternalFile('IW_GFX_ARCRENAME', '/gfx/ArcRename.png');
  TIWServer.AddInternalFile('IW_GFX_ARCDELETE', '/gfx/ArcDelete.png');
finalization
end.

