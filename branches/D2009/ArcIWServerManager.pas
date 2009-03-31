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

unit ArcIWServerManager;

{$I IntraWebVersion.inc}

interface

uses
  Windows, SysUtils, Classes, IWServerControllerBase, uStatisticFile, {$IFNDEF CLR}{$IFNDEF BCB}SWService,{$ENDIF}{$ENDIF}
  HTTPApp, IWApplication, IWServer, ActiveX, {$IFDEF INTRAWEB70}IWColor, IWFont, Graphics,{$ENDIF}
  IWBaseForm, SyncObjs, {$IFNDEF VER130} DateUtils, Variants,{$ENDIF} ArcD5Fix,
  uLoginManager, uSMCommon, ArcIWEliteResources;




type
  TArcIWServerManager = class;

  TIWApplicationHack = class(TIWApplication)
  end;

  TSecurityDefsEvent = procedure(Sender : TObject; SecurityDefs : TSecurityItems) of object;

  EServerManCrash = class(Exception)
  public
    constructor Create;
  end;

  TProfilerThread = class(TThread)
  protected
    Owner : TArcIWServerManager;
    procedure Execute; override;
  public
    constructor Create(AOwner : TArcIWServerManager); reintroduce;
    destructor Destroy; override;
  end;

  {$IFDEF INTRAWEB70}
  TLayoutColors = class(TPersistent)
  private
    FFooter: TIWColor;
    FMenus: TIWColor;
    FGraphs: TIWColor;
    FGridHeader: TIWColor;
    FGrids: TIWColor;
    FBody: TIWColor;
    FBorders: TIWColor;
    FHeader: TIWColor;
    FButtons: TIWColor;
    FFields: TIWColor;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Menus      : TIWColor read FMenus      write FMenus      default $00BF532F;
    property Borders    : TIWColor read FBorders    write FBorders    default clWebBlack;
    property Header     : TIWColor read FHeader     write FHeader     default $00BF532F;
    property Footer     : TIWColor read FFooter     write FFooter     default $00BF532F;
    property Grids      : TIWColor read FGrids      write FGrids      default clWebDIMGRAY;
    property Graphs     : TIWColor read FGraphs     write FGraphs     default $00EBB99D;
    property Body       : TIWColor read FBody       write FBody       default clWebWhite;
    property GridHeader : TIWColor read FGridHeader write FGridHeader default $00BF532F;
    property Fields     : TIWColor read FFields     write FFields     default clWebWhite;
    property Buttons    : TIWColor read FButtons    write FButtons    default clWebLIGHTGRAY;
  end;

  TLayoutFonts = class(TPersistent)
  private
    FGridText: TIWFont;
    FPageTitle: TIWFont;
    FMenuCaption: TIWFont;
    FMenuItem: TIWFont;
    FFieldCaption: TIWFont;
    FButtonText: TIWFont;
    FCopyright: TIWFont;
    FGridHeader: TIWFont;
    FFieldText: TIWFont;
    FVersion: TIWFont;
    FSectionTitle: TIWFont;
    FGraphText: TIWFont;
    procedure SetCopyright(const Value: TIWFont);
    procedure SetFieldCaption(const Value: TIWFont);
    procedure SetMenuCaption(const Value: TIWFont);
    procedure SetMenuItem(const Value: TIWFont);
    procedure SetPageTitle(const Value: TIWFont);
    procedure SetSectionTitle(const Value: TIWFont);
    procedure SetVersion(const Value: TIWFont);
    procedure SetButtonText(const Value: TIWFont);
    procedure SetFieldText(const Value: TIWFont);
    procedure SetGraphText(const Value: TIWFont);
    procedure SetGridHeader(const Value: TIWFont);
    procedure SetGridText(const Value: TIWFont);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor destroy; override;
  published
    property MenuItem     : TIWFont read FMenuItem     write SetMenuItem     ;
    property MenuCaption  : TIWFont read FMenuCaption  write SetMenuCaption  ;
    property Version      : TIWFont read FVersion      write SetVersion      ;
    property Copyright    : TIWFont read FCopyright    write SetCopyright    ;
    property PageTitle    : TIWFont read FPageTitle    write SetPageTitle    ;
    property SectionTitle : TIWFont read FSectionTitle write SetSectionTitle ;
    property FieldCaption : TIWFont read FFieldCaption write SetFieldCaption ;
    property FieldText    : TIWFont read FFieldText    write SetFieldText    ;
    property ButtonText   : TIWFont read FButtonText   write SetButtonText   ;
    property GridHeader   : TIWFont read FGridHeader   write SetGridHeader   ;
    property GridText     : TIWFont read FGridText     write SetGridText     ;
    property GraphText    : TIWFont read FGraphText    write SetGraphText    ;
  end;

  TLayoutProperties = class(TPersistent)
  private
    FFonts: TLayoutFonts;
    FColors: TLayoutColors;
    FEnableOverride: boolean;
    procedure SetFonts(const Value: TLayoutFonts);
    procedure SetColors(const Value: TLayoutColors);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property EnableOverride : boolean read FEnableOverride write FEnableOverride;
    property Colors : TLayoutColors read FColors write SetColors;
    property Fonts : TLayoutFonts read FFonts write SetFonts;
  end;
  {$ENDIF}

  TdllInitialize = procedure(const IP : PChar; const Port : integer); stdcall;
  TdllProcedure  = procedure; stdcall;
  TdllChangeVar  = procedure(const VarName: PChar; const Value: Variant); stdcall;
  TdllLog        = procedure(const Msg: PChar); stdcall;
  TdllGetCmd     = function(var Command, Value: Variant) : Boolean; stdcall;
  TdllGetPage    = procedure(var Page : Variant); stdcall;

  TAdminAppDLL = class(TObject)
  private
    dll : THandle;
    FIP : string;
    FPort : integer;
    FInitialize : TdllInitialize;
    FStart : TdllProcedure;
    FStop : TdllProcedure;
    FChangeSettingsVariable : TdllChangeVar;
    FChangeApplicationVariable : TdllChangeVar;
    FShutdown : TdllProcedure;
    FLogMessage : TdllLog;
    FGetCommand : TdllGetCmd;
    FRssXMLPage : TdllGetPage;
    FAlertAppStarted : TdllProcedure;
    FAlertAppTerminated : TdllProcedure;
    FAlertAppNotResponding : TdllProcedure;
    FAdminLoc : string;
    FDLLPath : string;
    FServerMan : TArcIWServerManager;
    procedure LoadEXE;
    procedure ReloadDLL;
    procedure UnloadDLL;
  public
    constructor Create(ServerMan : TArcIWServerManager; const IP : string; const Port : integer; dllfilepath : string; AdminLoc : string); virtual;
    destructor Destroy; override;

    procedure Initialize(const IP : string; const Port : integer);
    procedure Start;
    procedure Stop;
    procedure ChangeSettingsVariable(const VarName: string; const Value: Variant);
    procedure ChangeApplicationVariable(const VarName: string; const Value: Variant);
    procedure Shutdown;
    procedure LogMessage(const Msg: string);
    function  GetCommand(var Command, Value: Variant) : Boolean;
    function RssXmlPage : WideString;
    procedure AlertAppStarted;
    procedure AlertAppTerminated;
    procedure AlertAppNotResponding;
  end;

  TCustomEventType = (etDBLookup, etDBPost, etAction, etButtonClick, etSelection);
  TArcIWServerManager = class(TComponent)
  private
    FAdminLoc : string;
    FAdminApp : TAdminAppDLL;
    FActive: boolean;
    FListening: boolean;
    FPort: integer;
    FRegKey: string;
    FIniFile: TFilename;
    FPagesPerSecond: integer;
    FHitsPerSecond: integer;
    FLoginLocalhost: boolean;
    //FInternalISAPI: TFilename;
    FDefAdminPass: string;
    FDefAdminUser: string;
    FNewSessionsDenied : boolean;
    FNewSessionsDeniedMsg : string;
    FDropSessionsMsg : string;
    FLastError : string;
    FLogLicenseInfo: boolean;
    FUserAdministration: boolean;
    FHideDisabledItems: boolean;
    FMemCacheHitLog: integer;
    FMemCacheProLog: integer;
    FRegistrationOverride: TRegType;
    FLogoutRedirectURL: string;
    FHideIWUserAdmin: boolean;
    FOnSetSecurityDefs: TSecurityDefsEvent;
    FOnSaveAuthFile: TLoginManagerFileEvent;
    FOnLoadAuthFile: TLoginManagerFileEvent;
    FHideCopyright: boolean;
    FAdminAppEXE: TFilename;
    FEventPollInterval: integer;
    FOverrideAppName: string;
    FOverrideCopyright: string;
    FEnableProfiling: boolean;
    {$IFDEF INTRAWEB70}
    FAdminLayout: TLayoutProperties;
    {$ENDIF}
    function GetMemoryUsage: integer;
    function GetMemoryUsageMax: integer;
    function GetSessionCount: integer;
    function GetSessionCountMax: integer;
    function GetThreadCount: integer;
    function GetThreadCountMax: integer;
    procedure SetActive(const Value: boolean);
    procedure SetListening(const Value: boolean);
    function AcquireHitLogRecord(Request: TWebRequest): THitLogRecord;
    function FindSessionForRequest(Request: TWebRequest): TIWApplication;
    function AcquireSession(Request: TWebRequest): TIWApplication;
    function GetUsers: TLoginItemList;
    procedure _SetSecurityDefs(Sender : TObject);
    procedure SetOnLoadAuthFile(const Value: TLoginManagerFileEvent);
    procedure SetOnSaveAuthFile(const Value: TLoginManagerFileEvent);
    //procedure SetInternalISAPI(const Value: TFilename);
    procedure SetAdminAppEXE(const Value: TFilename);
    procedure SetDefAdminPass(const Value: string);
    procedure SetDefAdminUser(const Value: string);
    procedure SetHideCopyright(const Value: boolean);
    procedure SetHideDisabledItems(const Value: boolean);
    procedure SetHideIWUserAdmin(const Value: boolean);
    procedure SetIniFile(const Value: TFilename);
    procedure SetLoginLocalhost(const Value: boolean);
    procedure SetLogoutRedirectURL(const Value: string);
    procedure SetRegistrationCode(const Value: string);
    procedure SetRegistrationEmail(const Value: string);
    procedure SetRegistrationName(const Value: string);
    procedure SetRegistrationOverride(const Value: TRegistrationType);
    procedure SetRegKey(const Value: string);
    procedure SetUserAdministration(const Value: boolean);
    procedure ChangeApplicationVariable(Varname: string; Value: Variant);
    procedure ChangeSettingsVariable(Varname: string; Value: Variant);
    procedure SetEnableProfiling(const Value: boolean);
    procedure SetOverrideAppName(const Value: string);
    procedure SetOverrideCopyright(const Value: string);
    {$IFDEF INTRAWEB70}
    procedure SetAdminLayout(const Value: TLayoutProperties);
    {$ENDIF}
    //procedure SetInternalISAPI(const Value: TFilename);
  protected
    SessionUsers : TStringList;

    FRegistrationName  : string;
    FRegistrationEmail : string;
    FRegistrationCode  : string;

    FUsersLoaded       : boolean;
    FSecurity          : TLoginManagerFile;

    AppFileDate : string;
    AppVersion : string;
    HitLogCS : TCriticalSection;
    ProLogCS : TCriticalSection;
    MsgLogCS : TCriticalSection;
    SessionUsersCS : TCriticalSection;

    AppHost : string;
    LastProLog : string;
    LastHitLog : string;
    HitLogFilenameRoot : string;
    ProLogFilenameRoot : string;
    LogMessageFilenameRoot : string;
    //httpServer : TISAPIServer;
    ProfilerRecords : TThreadList;
    ProfilerRecordsCount : integer;
    HitLogRecords : TThreadList;
    HitLogRecordsCount : integer;
    ProfilerThread : TProfilerThread;
    ServerController : TIWServerControllerBase;
    FMessageLog : TD7StringList;
    FCustomEvents : TStringList;
    CustomEventsCS : TCriticalSection;
    FDropImmediately : boolean;

    SessionList : TThreadList;
    __OnCreateSession : TNotifyEvent;
    __OnDestroySession : TNotifyEvent;
    __OnBeforeDispatch : TOnDispatch;
    __OnAfterDispatch : TOnDispatch;
    __OnNewSession : TOnNewSessionEvent;
    __OnCloseSession : TOnCloseSessionEvent;
    procedure InternalOnCreateSession(Sender : TObject);
    procedure InternalOnNewSession(ASession: TIWApplication; var VMainForm: TIWBaseForm);
    procedure InternalOnCloseSession(ASession: TIWApplication);
    procedure InternalOnDestroySession(Sender : TObject);
    procedure InternalBeforeDispatch(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure InternalAfterDispatch(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure _LoadAuthFile(Sender : TObject; const Filename : string; Stream : TStream; var Skip : boolean);
    procedure _SaveAuthFile(Sender : TObject; const Filename : string; Stream : TStream; var Skip : boolean);
    procedure UpdateAdminAppVars; virtual;
    procedure UpdateSettingsVars; virtual;
    procedure UpdateLayoutVars; virtual;

    //procedure BeforeISAPI(AThread : TIdPeerThread; RequestInfo : TIdHTTPRequestInfo;
    //  ResponseInfo : TIdHTTPResponseInfo); virtual;
    //procedure AfterISAPI(AThread : TIdPeerThread; RequestInfo : TIdHTTPRequestInfo;
    //  ResponseInfo : TIdHTTPResponseInfo); virtual;

    function CookieName : string; virtual;
    procedure Loaded; override;
    procedure StartListening; virtual;
    procedure StopListening; virtual;
    procedure StartProfiling; virtual;
    procedure StopProfiling; virtual;
    function RestartIW : boolean; virtual;
    procedure OnCommand(Command: WideString; Value: Variant);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LogMessage(Text : string); virtual;
    procedure FlushHitLog; virtual;
    procedure FlushProLog; virtual;

    property ThreadCount       : integer      read GetThreadCount;
    property ThreadCountMax    : integer      read GetThreadCountMax;
    property SessionCount      : integer      read GetSessionCount;
    property SessionCountMax   : integer      read GetSessionCountMax;
    property MemoryUsage       : integer      read GetMemoryUsage;
    property MemoryUsageMax    : integer      read GetMemoryUsageMax;
    property PagesPerSecond    : integer      read FPagesPerSecond;
    property HitsPerSecond     : integer      read FHitsPerSecond;
    property DenyNewSessions   : boolean      read FNewSessionsDenied;
    property LastError         : string       read FLastError;
    property RegistrationName  : string       read FRegistrationName   write SetRegistrationName;
    property RegistrationEmail : string       read FRegistrationEmail  write SetRegistrationEmail;
    property RegistrationCode  : string       read FRegistrationCode   write SetRegistrationCode;
    property AdminAppEXE   : TFilename        read FAdminAppEXE           write SetAdminAppEXE;
    property Users         : TLoginItemList   read GetUsers;
    property EventPollInterval : integer read FEventPollInterval write FEventPollInterval;

    procedure CustomEvent(EventType, EventName : string); overload;
    procedure CustomEvent(EventType : TCustomEventType; EventName : string); overload;
    procedure ReloadUsers;
    procedure SaveUsers;
    //property InternalISAPI : TFilename        read FInternalISAPI         write SetInternalISAPI;
    procedure LoginUser(AppID, UserName : string);
    procedure LogoutUser(AppID : string);
    function RssXMLPage : string;
  published
    property Active          : boolean           read FActive             write SetActive;
    property EnableProfiling : boolean           read FEnableProfiling    write SetEnableProfiling default true;
    {$IFDEF INTRAWEB70}
    property AdminLayout     : TLayoutProperties read FAdminLayout        write SetAdminLayout;
    {$ENDIF}
    property Listening     : boolean          read FListening             write SetListening;
    property Port          : integer          read FPort                  write FPort;
    property DefAdminUser  : string           read FDefAdminUser          write SetDefAdminUser;
    property DefAdminPass  : string           read FDefAdminPass          write SetDefAdminPass;
    property LoginLocalhost: boolean          read FLoginLocalhost        write SetLoginLocalhost;
    property IniFile       : TFilename        read FIniFile               write SetIniFile;
    property RegKey        : string           read FRegKey                write SetRegKey;
    property DenyNewSessionsMsg : string      read FNewSessionsDeniedMsg  write FNewSessionsDeniedMsg;
    property DropSessionsMsg : string         read FDropSessionsMsg       write FDropSessionsMsg;
    property LogLicenseInfo : boolean         read FLogLicenseInfo        write FLogLicenseInfo;
    property UserAdministration : boolean     read FUserAdministration    write SetUserAdministration;
    property HideIWUserAdmin : boolean        read FHideIWUserAdmin       write SetHideIWUserAdmin;
    property HideDisabledItems : boolean      read FHideDisabledItems     write SetHideDisabledItems;
    property MemCacheHitLog : integer         read FMemCacheHitLog        write FMemCacheHitLog;
    property MemCacheProLog : integer         read FMemCacheProLog        write FMemCacheProLog;
    property RegistrationOverride : TRegistrationType read FRegistrationOverride  write SetRegistrationOverride;
    property LogoutRedirectURL : string       read FLogoutRedirectURL     write SetLogoutRedirectURL;
    property HideCopyright : boolean          read FHideCopyright         write SetHideCopyright;
    property OnSetSecurityDefs : TSecurityDefsEvent read FOnSetSecurityDefs write FOnSetSecurityDefs;
    property OnLoadAuthFile : TLoginManagerFileEvent read FOnLoadAuthFile write SetOnLoadAuthFile;
    property OnSaveAuthFile : TLoginManagerFileEvent read FOnSaveAuthFile write SetOnSaveAuthFile;
    property OverrideAppName : string read FOverrideAppName write SetOverrideAppName;
    property OverrideCopyright : string read FOverrideCopyright write SetOverrideCopyright;
  end;

  TAdminAppEventCheckThread = class(TThread)
  private
    FPath : string;
    FPort : integer;
    FInterval : integer;
    FAdminApp : TAdminAppDLL;
    FServerMan : TArcIWServerManager;
  protected
    procedure Execute; override;
  public
    constructor Create(const Path : string; const ServerMan : TArcIWServerManager; const Port, Interval : integer); reintroduce; virtual;
    destructor Destroy; override;
  end;


procedure GetThreadCount(var aTotalThreads, aThreadsForApplication : LongWord  );
procedure GetApplicationMemory(var Current, Max : LongWord);
function GetSysMemoryLoad: Byte;
function GetCPUMHz : Double;
function GetCPUUsage : integer;

implementation

uses
{$IFDEF FASTSTRINGS}
  ArcFastStrings,
{$ELSE}
  ArcStrings,
{$ENDIF}
  IWResourceStrings, IWKlooch, ArcWebCommon,
  TlHelp32, psAPI, Math, IWFileReference, SWStrings {$IFNDEF VER130}, StrUtils{$ENDIF};

var
  AppFileVersionMajor : Cardinal;
  AppFileVersionMinor : Cardinal;
  AppFileVersionRelease : Cardinal;
  AppFileVersionBuild : Cardinal;

  _ThreadCount: integer;
  _ThreadCountMax: integer;
  _SessionCount: integer;
  _SessionCountPrev: integer;
  _SessionCountMax: integer;
  _MemoryUsage: integer;
  _MemoryUsageMax: integer;
  _ContentBytes : integer;
  _ContentBytesPrev : integer;
  _HitCountPrev : integer;
  _HitCount : integer;
  _HitCountMax : Integer;
  _NewVisitor : Integer;
  _ReturnVisitor : Integer;
  _StartedRunning : TDateTime;

  ServerManagerComponentCount : integer;
  DefaultAppFile : string;
  DefaultRestartEXE : string;

  LastTickCount     : cardinal = 0;
  LastProcessorTime : int64    = 0;

  EventChecker : TAdminAppEventCheckThread;

  SOAPErrorCnt : integer;

{$IFDEF VER130}
function IfThen(b : boolean; ATrue : string; AFalse : string='') : string;
begin
  if b then
    Result := ATrue
  else
    Result := AFalse;
end;
{$ENDIF}

function GetProcessorTime : int64;
type
 TPerfDataBlock = packed record
   signature              : array [0..3] of wchar;
   littleEndian           : cardinal;
   version                : cardinal;
   revision               : cardinal;
   totalByteLength        : cardinal;
   headerLength           : cardinal;
   numObjectTypes         : integer;
   defaultObject          : cardinal;
   systemTime             : TSystemTime;
   perfTime               : comp;
   perfFreq               : comp;
   perfTime100nSec        : comp;
   systemNameLength       : cardinal;
   systemnameOffset       : cardinal;
 end;
 TPerfObjectType = packed record
   totalByteLength        : cardinal;
   definitionLength       : cardinal;
   headerLength           : cardinal;
   objectNameTitleIndex   : cardinal;
   objectNameTitle        : PWideChar;
   objectHelpTitleIndex   : cardinal;
   objectHelpTitle        : PWideChar;
   detailLevel            : cardinal;
   numCounters            : integer;
   defaultCounter         : integer;
   numInstances           : integer;
   codePage               : cardinal;
   perfTime               : comp;
   perfFreq               : comp;
 end;
 TPerfCounterDefinition = packed record
   byteLength             : cardinal;
   counterNameTitleIndex  : cardinal;
   counterNameTitle       : PWideChar;
   counterHelpTitleIndex  : cardinal;
   counterHelpTitle       : PWideChar;
   defaultScale           : integer;
   defaultLevel           : cardinal;
   counterType            : cardinal;
   counterSize            : cardinal;
   counterOffset          : cardinal;
 end;
 TPerfInstanceDefinition = packed record
   byteLength             : cardinal;
   parentObjectTitleIndex : cardinal;
   parentObjectInstance   : cardinal;
   uniqueID               : integer;
   nameOffset             : cardinal;
   nameLength             : cardinal;
 end;
var  c1, c2, c3      : cardinal;
    i1, i2          : integer;
    perfDataBlock   : ^TPerfDataBlock;
    perfObjectType  : ^TPerfObjectType;
    perfCounterDef  : ^TPerfCounterDefinition;
    perfInstanceDef : ^TPerfInstanceDefinition;
begin
 result := 0;
 perfDataBlock := nil;
 try
   c1 := $10000;
   while true do begin
     ReallocMem(perfDataBlock, c1);
     c2 := c1;
     case RegQueryValueEx(HKEY_PERFORMANCE_DATA, '238', nil, @c3, pointer(perfDataBlock), @c2) of
       ERROR_MORE_DATA : c1 := c1 * 2;
       ERROR_SUCCESS   : break;
       else              exit;
     end;
   end;
   perfObjectType := pointer(cardinal(perfDataBlock) + perfDataBlock^.headerLength);
   for i1 := 0 to perfDataBlock^.numObjectTypes - 1 do begin
     if perfObjectType^.objectNameTitleIndex = 238 then begin   // 238 -> "Processor"
       perfCounterDef := pointer(cardinal(perfObjectType) + perfObjectType^.headerLength);
       for i2 := 0 to perfObjectType^.numCounters - 1 do begin
         if perfCounterDef^.counterNameTitleIndex = 6 then begin    // 6 -> "% Processor Time"
           perfInstanceDef := pointer(cardinal(perfObjectType) + perfObjectType^.definitionLength);
           result := PInt64(cardinal(perfInstanceDef) + perfInstanceDef^.byteLength + perfCounterDef^.counterOffset)^;
           break;
         end;
         inc(perfCounterDef);
       end;
       break;
     end;
     perfObjectType := pointer(cardinal(perfObjectType) + perfObjectType^.totalByteLength);
   end;
 finally FreeMem(perfDataBlock) end;
end;

function GetCPUCount : integer;
var
  si : _System_Info;
begin
  GetSystemInfo(si);
  Result := si.dwNumberOfProcessors;
end;

procedure GetMemoryInfo(var TotalPhysical, FreePhysical, TotalVirtual,
  FreeVirtual, TotalPage, FreePage, MemoryFree : Int64; var MemoryLoad : byte);
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  TotalPhysical := MemoryStatus.dwTotalPhys;
  FreePhysical  := MemoryStatus.dwAvailPhys;
  TotalVirtual  := MemoryStatus.dwTotalVirtual;
  FreeVirtual   := MemoryStatus.dwAvailVirtual;
  TotalPage     := MemoryStatus.dwTotalPageFile;
  FreePage      := MemoryStatus.dwAvailPageFile;
  MemoryLoad    := MemoryStatus.dwMemoryLoad;
  MemoryFree    := MemoryStatus.dwAvailPhys + MemoryStatus.dwAvailPageFile + MemoryStatus.dwAvailVirtual;
end;

function GetCPUUsage : integer;
var tickCount     : cardinal;
   processorTime : int64;
begin
 result := 0;
 tickCount     := GetTickCount;
 processorTime := GetProcessorTime;
 if (LastTickCount <> 0) and (tickCount <> LastTickCount) then
   result := 100 - Round(((processorTime - LastProcessorTime) div 100) / (tickCount - LastTickCount));
 LastTickCount     := tickCount;
 LastProcessorTime := processorTime;
end;

function GetCPUMHz : Double;
const
  DelayTime = 500; // measure time in ms
var
  TimerHi, TimerLo : DWORD;
  PriorityClass, Priority : Integer;
begin
  PriorityClass := GetPriorityClass(GetCurrentProcess);
  Priority := GetThreadPriority(GetCurrentThread);

  SetPriorityClass(GetCurrentProcess,
                     REALTIME_PRIORITY_CLASS);
  SetThreadPriority(GetCurrentThread,
                     THREAD_PRIORITY_TIME_CRITICAL);

  Sleep(10);
  asm
    dw 310Fh // rdtsc
    mov TimerLo, eax
    mov TimerHi, edx
  end;
  Sleep(DelayTime);
  asm
    dw 310Fh // rdtsc
    sub eax, TimerLo
    sbb edx, TimerHi
    mov TimerLo, eax
    mov TimerHi, edx
  end;

  SetThreadPriority(GetCurrentThread, Priority);
  SetPriorityClass(GetCurrentProcess, PriorityClass);

  Result := TimerLo / (1000.0 * DelayTime);
end;

function GetAppVersion(Filename : string) : string;
var
  Size, FixInfoLen: DWORD;
  Handle: THandle;
  Buffer: string;
  FixInfoBuf: PVSFixedFileInfo;
begin
  Result := '(no version)';
  Size := GetFileVersionInfoSize(PChar(FileName), Handle);
  if Size > 0 then
  begin
    SetLength(Buffer, Size);
    if GetFileVersionInfo(PChar(FileName), Handle, Size, Pointer(Buffer)) and
      VerQueryValue(Pointer(Buffer), '\', Pointer(FixInfoBuf), FixInfoLen) and
      (FixInfoLen = SizeOf(TVSFixedFileInfo)) then
    begin
      Result := IntToStr(hiWord(FixInfoBuf^.dwFileVersionMS))+'.'+IntToStr(loWord(FixInfoBuf^.dwFileVersionMS))+'.'+IntToStr(hiWord(FixInfoBuf^.dwFileVersionLS))+'.'+IntToStr(loWord(FixInfoBuf^.dwFileVersionLS));
    end;
  end;
end;

function FileDateTime(const FileName: string): TDateTime;
var
  iFileAge: Longint;
begin
  iFileAge := FileAge(FileName);
  if iFileAge = -1 then
    Result := 0
  else
    Result := FileDateToDateTime(iFileAge);
end;

procedure GetThreadCount(var aTotalThreads, aThreadsForApplication : LongWord  );
var
  c : Cardinal;
  te : THREADENTRY32;
  iTotal, iAppTotal : LongWord;
  CurrentProcess : DWord;
  // ThreadID : DWord;
begin
  iTotal := 0;
  iAppTotal := 0;
  try
    // ThreadID := GetCurrentThreadId;

    c := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
    try

      CurrentProcess := GetCurrentProcessID;
      te.dwSize := sizeof(THREADENTRY32);

      if Thread32First(c,te) then
      repeat
        if te.th32OwnerProcessID=CurrentProcess then
          inc(iAppTotal);
        inc(iTotal);
      until not Thread32Next(c,te);


    finally
      CloseHandle(c);
    end;
  finally
    aTotalThreads := iTotal;
    aThreadsForApplication := iAppTotal; // +1;  // maybe +1 to account for main application thread???
  end;
end;

procedure GetApplicationMemory(var Current, Max : LongWord);
var
  CurrentProcess : Cardinal;
  pmc : PROCESS_MEMORY_COUNTERS;
begin
  CurrentProcess := GetCurrentProcess;
  pmc.cb := sizeOf(pmc);
  if not GetProcessMemoryInfo(CurrentProcess,@pmc,sizeof(pmc)) then
    RaiseLastWin32Error;
  Current := pmc.WorkingSetSize;
  Max := pmc.PeakWorkingSetSize;
end;

function GetSysMemoryLoad: Byte;
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwMemoryLoad;
end;

procedure ExtractResourceFile(resource, fname : string);
var
  rs : TResourceStream;
  fs : TFileStream;
  s : string;
begin
  rs := TResourceStream.Create(HInstance, resource, RT_RCDATA);
  try
    try
      fs := TFileStream.Create(fname,fmCreate or fmShareExclusive);
    except
      s :=
        'The server manager executable is locked and cannot be overwritten.  '+
        'Please verify that IWServerManager.exe is not currently running and then '+
        'restart your intraweb application.';
      LogEvent(lnApplication, lmtError, s);
      raise Exception.Create(s);
    end;
    try
      rs.Position := 0;
      fs.CopyFrom(rs,rs.Size);
    finally
      fs.Free;
    end;
  finally
    rs.Free;
  end;
end;

function LookupSession(AppID : string) : TIWApplication;
{$IFDEF INTRAWEB51}
var
  i : integer;
  l : TList;
begin
  Result := nil;
  l := GSessions.LockList;
  try
    for i := 0 to l.Count-1 do
    begin
      if TIWAPplication(l[i]).AppID = AppID then
      begin
        Result := TIWApplication(l[i]);
        break;
      end;
    end;
  finally
    GSessions.UnlockList;
  end;
{$ELSE}
begin
  {$IFDEF INTRAWEB80}
    Result := GSessions.LookupAndLock(AppID);
  {$ELSE}
    Result := GSessions.LookupSession(AppID);
  {$ENDIF}
{$ENDIF}
end;

procedure RaiseSOAPError(Call, Msg : string; AvoidRaise : boolean = False);
var
  s : string;
begin
  s := '"'+Call+'" call to AdminApp failed with exception "'+Msg+'"';
  LogEvent(lnApplication, lmtError, s);
  inc(SOAPErrorCnt);
  if (SOAPErrorCnt > 4) and (not AvoidRaise) then
    raise Exception.Create(s);
end;

{ TAdminAppEventCheckThread }

constructor TAdminAppEventCheckThread.Create(const Path : string; const ServerMan : TArcIWServerManager; const Port, Interval : integer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FServerMan := ServerMan;
  FInterval := Interval;
  FPort := Port;
  FPath := Path;
  Resume;
end;

destructor TAdminAppEventCheckThread.Destroy;
begin
  inherited;
end;

procedure TAdminAppEventCheckThread.Execute;
  procedure Poll;
  var
    vCommand : Variant;
    vValue : Variant;
    b : boolean;
  begin
    repeat
      try
        b := FAdminApp.GetCommand(vCommand, vValue);
      except
        on e: exception do
          try
            RaiseSOAPError('GetCommand("'+vCommand+'","'+vValue+'")', e.Message);
          except
            RaiseSOAPError('GetCommand("'+vCommand+'",[ERROR])', e.Message);
          end;
      end;
      if VarToStr(vCommand) <> '' then
        FServerMan.OnCommand(vCommand, vValue);
    until not b;
  end;
var
  i, iInterval : integer;
begin
  try
    FAdminApp := TAdminAppDLL.Create(FServerMan, '127.0.0.1',FPort, FPath+'SMSoapClient.dll',FServerMan.FAdminLoc);
    iInterval := FInterval div 100;
    try
      repeat
        Poll;
        for i := 0 to 100 do
          if not Terminated then
            Sleep(iInterval);
      until Terminated;
    finally
      //FAdminApp := nil;
    end;
  finally
    FAdminApp.Free;
    //CoUninitialize;
  end;
end;

{ TArcIWServerManager }

procedure TArcIWServerManager.ChangeApplicationVariable(Varname : string; Value : Variant);
var
  ws : Widestring;
begin
  try
    if VarIsNull(Value) or VarIsEmpty(Value) then
      LogEvent(lnApplication, lmtWarning, 'Variable '+Varname+' about to be set as null.');
    if VarType(Value) = varString then
    begin
      ws := value;
      FAdminApp.ChangeApplicationVariable(VarName, ws);
    end else
      FAdminApp.ChangeApplicationVariable(VarName, Value);
  except
    on e: exception do
      try
        RaiseSOAPError('ChangeApplicationVaraible("'+Varname+'", "'+Value+'") as type $'+IntToHex(VarType(Value),4), e.Message);
      except
        RaiseSOAPError('ChangeApplicationVaraible("'+Varname+'", [ERROR])', e.Message);
      end;
  end;
end;

procedure TArcIWServerManager.ChangeSettingsVariable(Varname : string; Value : Variant);
var
  ws : Widestring;
begin
  try
    if VarIsNull(Value) or VarIsEmpty(Value) then
      LogEvent(lnApplication, lmtWarning, 'Variable '+Varname+' about to be set as null.');
    case VarType(Value) of
      varString:
        begin
          ws := value;
          FAdminApp.ChangeSettingsVariable(VarName, ws);
        end;
      varBoolean:
        begin
          if Value then
            FAdminApp.ChangeSettingsVariable(VarName, 'true')
          else
            FAdminApp.ChangeSettingsVariable(VarName, 'false');
        end;
      else
        FAdminApp.ChangeSettingsVariable(VarName, Value);
    end;
  except
    on e: exception do
      try
        RaiseSOAPError('ChangeSettingsVariable("'+Varname+'", "'+Value+'") as type $'+IntToHex(VarType(Value),4), e.Message);
      except
        RaiseSOAPError('ChangeSettingsVariable("'+Varname+'", [ERROR])', e.Message);
      end;
  end;
end;

function TArcIWServerManager.CookieName: string;
begin
  Result := 'Arcana_ArcIWServerStatistics_'+Name+'_'+ExtractFilename(DLLFilename)
end;

constructor TArcIWServerManager.Create(AOwner: TComponent);
begin
  if not (AOwner is TIWServerControllerBase) then
    raise Exception.Create('This component must be used on a TIWServerController.');
  if ServerManagerComponentCount > 0 then
    raise EComponentError.Create('Only one TArcIWServerManager component is allowed per application.');

  inherited;
  FEventPollInterval := 1000;
  FEnableProfiling := True;
  {$IFDEF INTRAWEB70}
  FAdminLayout := TLayoutProperties.Create;
  {$ENDIF}
  
  FMessageLog := TD7StringList.Create;
  FMessageLog.Delimiter := '~';
  FMessageLog.QuoteChar := '`';

  FSecurity := TLoginManagerFile.Create(nil);
  FSecurity.OnBuildSecurityDefs := _SetSecurityDefs;

  FMemCacheHitLog := 1000;
  FMemCacheProLog := 1000;

  FCustomEvents := TStringList.Create;
  CustomEventsCS := TCriticalSection.Create;

  SessionUsersCS := TCriticalSection.Create;
  SessionUsers := TStringList.Create;

  FRegistrationOverride := rtEvaluation;

  AppVersion := GetAppVersion(DLLFileName);

  AppFileDate := DateTimeToStr(FileDateTime(DLLFilename));

  HitLogCS := TCriticalSection.Create;
  ProLogCS := TCriticalSection.Create;
  MsgLogCS := TCriticalSection.Create;

  inc(ServerManagerComponentCount);
  ServerController := TIWServerControllerBase(AOwner);

  HitLogFilenameRoot := DLLFileName+'.Admin\Logs'+PathDelim;
  ProLogFilenameRoot := DLLFileName+'.Admin\Logs'+PathDelim;
  LogMessageFilenameRoot := DLLFileName+'.Admin\Logs'+PathDelim;

  SessionList := TThreadList.Create;
  ProfilerRecords  := TThreadList.Create;
  HitLogRecords := TThreadList.Create;
  FActive          := True;
  FListening       := True;
  FPort            := 8889;
  FDefAdminUser    := 'administrator';
  FDefAdminPass    := 'password';
  FRegKey          := '';
  FIniFile         := '';
  _ThreadCountMax  := 0;
  _SessionCount    := 0;
  _SessionCountMax := 0;
  _MemoryUsage     := 0;
  _MemoryUsageMax  := 0;
  _ThreadCount     := 0;
  FPagesPerSecond  := 0;
  FHitsPerSecond   := 0;
  //httpServer       := TISAPIServer.Create(Self);
  FNewSessionsDenied := False;
  FNewSessionsDeniedMsg := 'This application is currently unavailable.  Please try again later.';
  FDropSessionsMsg := 'Your session has been dropped for administrative purposes.';

  ForceDirectories(ExtractFilePath(DLLFilePath+DefaultAppFile));

  if not (csDesigning in ComponentState) then
  begin
    if FileExists(DLLFilePath+DefaultAppFile) then
    begin
      try
        RenameFile(DLLFilePath+DefaultAppFile, DLLFilePath+'ToDelete.tmp');
        ExtractResourceFile('AppEXE',DLLFilePath+DefaultAppFile);
        DeleteFile(DLLFilePath+'ToDelete.tmp');
      except
        on e: exception do
          LogEvent(lnApplication, lmtError, 'Error extracting Server Manager Application: '+e.Message);
      end;
    end else
      ExtractResourceFile('AppEXE',DLLFilePath+DefaultAppFile);

    if FileExists(DLLFilePath+ExtractFilePath(DefaultAppFile)+'SMSoapClient.dll') then
    begin
      try
        RenameFile(DLLFilePath+ExtractFilePath(DefaultAppFile)+'SMSoapClient.dll', DLLFilePath+'ToDelete2.tmp');
        ExtractResourceFile('SoapDll',DLLFilePath+ExtractFilePath(DefaultAppFile)+'SMSoapClient.dll');
        DeleteFile(DLLFilePath+'ToDelete2.tmp');
      except
        on e: exception do
          LogEvent(lnApplication, lmtError, 'Error extracting Soap Client Library: '+e.Message);
      end;
    end else
      ExtractResourceFile('SoapDll',DLLFilePath+ExtractFilePath(DefaultAppFile)+'SMSoapClient.dll');
    BreakupFileVersion(DLLFilePath+DefaultAppFile, AppFileVersionMajor, AppFileVersionMinor, AppFileVersionRelease, AppFileVersionBuild);
  end;

  FUserAdministration := True;
end;

destructor TArcIWServerManager.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    Active := False;
    FlushHitLog;
    FlushProLog;
    Listening := False;
  end;
  HitLogCS.Free;
  ProLogCS.Free;
  MsgLogCS.Free;
  SessionUsersCS.Free;
  FMessageLog.Free;
  FCustomEvents.Free;
  CustomEventsCS.Free;
  SessionUsers.Free;
  FSecurity.Free;

  {$IFDEF INTRAWEB70}
  FAdminLayout.Free;
  {$ENDIF}
  ProfilerRecords.Free;
  HitLogRecords.Free;
  SessionList.Free;
  dec(ServerManagerComponentCount);

  inherited;
end;

procedure TArcIWServerManager.FlushProLog;
var
  sLogFilename : string;
  fs : TFileStream;
  i : integer;
  FileHeader : TFileHeader;
begin
  if not FEnableProfiling then
    exit;
  ProLogCS.Enter;
  try
    DateTimeToString(sLogFilename,'_yyyymmdd_hhnnss',Now);
    sLogFilename := ProLogFilenameRoot+sLogFilename+'.prolog';
    LastProLog := sLogFilename;

    with ProfilerRecords.LockList do
    try
      if not FileExists(sLogFilename) then
        ForceDirectories(ExtractFilePath(sLogFilename));

      fs := TFileStream.Create(sLogFilename,fmOpenRead or fmCreate or fmShareExclusive);
      try
        fs.Position := fs.Size;
        if fs.Position = 0 then
        begin
          FileHeader.FileCode := ProLogFileCode;
          FileHeader.DataVersion := 1;
          FileHeader.TimeStamp := Now;
          fs.Write(FileHeader,SizeOf(FileHeader));
        end;
        for i := 0 to Count-1 do
        begin
          fs.Write(PProfilerRecord(Items[i])^,SizeOf(TProfilerRecord));
          Dispose(PProfilerRecord(Items[i]));
        end;
        Clear;
        ProfilerRecordsCount := 0;
      finally
        fs.Free;
      end;
    finally
      ProfilerRecords.UnlockList;
    end;
  finally
    ProLogCS.Leave;
  end;
end;

function TArcIWServerManager.GetMemoryUsage: integer;
begin
  Result := _MemoryUsage;
end;

function TArcIWServerManager.GetMemoryUsageMax: integer;
begin
  Result := _MemoryUsageMax;
end;

function TArcIWServerManager.GetSessionCount: integer;
begin
  Result := _SessionCount;
end;

function TArcIWServerManager.GetSessionCountMax: integer;
begin
  Result := _SessionCountMax;
end;

function TArcIWServerManager.GetThreadCount: integer;
begin
  Result := _ThreadCount;
end;

function TArcIWServerManager.GetThreadCountMax: integer;
begin
  Result := _ThreadCountMax;
end;

procedure TArcIWServerManager.InternalOnCreateSession(Sender: TObject);
begin
  inc(_SessionCount);
  _SessionCountMax := Max(_SessionCount,_SessionCountMax);
  if Assigned(__OnCreateSession) then
    __OnCreateSession(Sender);  
end;

procedure TArcIWServerManager.InternalOnDestroySession(Sender: TObject);
begin
  dec(_SessionCount);
  if Assigned(__OnDestroySession) then
    __OnDestroySession(Sender);
end;

procedure TArcIWServerManager.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    if FListening then
      StartListening;

    if FActive then
    begin
      ReloadUsers;
      StartProfiling;
    end;
  end;
end;

procedure TArcIWServerManager.LogMessage(Text: string);
begin
  MsgLogCS.Enter;
  try
      FMessageLog.Add(DateTimeToStr(Now)+#9+Text);
  finally
    MsgLogCS.Leave;
  end;
end;

procedure TArcIWServerManager.InternalBeforeDispatch(
  Sender: TObject; Request: TWebRequest; Response: TWebResponse;
  var Handled: Boolean);
var
  ne : TNotifyEvent;
  hr : THitLogRecord;
  {$IFNDEF VER150}
  s : string;
  {$ENDIF}
  sURL : string;
  SessionApp : TIWApplication;
  i : integer;
  sTmp : string;
begin
   inc(_HitCount);
  _HitCountMax := Max(_HitCount,_HitCountMax);

  inc(_ContentBytes,Request.ContentLength);

  hr := THitLogRecord.Create(nil);

  SessionApp := FindSessionForRequest(Request);
  try
    {$IFNDEF VER150}
    s := Request.ContentFields.Text;
    {$ENDIF}

    i := Request.QueryFields.Count;
    sTmp := Request.QueryFields.Text;
    LogEvent(lnApplication, lmtInformation, 'Before Add: '+IntToStr(i)+': '+sTmp);

    if SessionApp <> nil then
      Request.QueryFields.Add('~~~~ss~~~~='+IntToStr(Integer(SessionApp)));
  finally
    {$IFDEF INTRAWEB80}
    if SessionApp <> nil then
      SessionApp.Unlock;
    {$ENDIF}
  end;
  Request.QueryFields.Add('~~~~hr~~~~='+IntToStr(Integer(hr)));
  {$IFNDEF VER150}
  Request.ContentFields.Text := s;
  {$ENDIF}

  i := Request.QueryFields.Count;
  sTmp := Request.QueryFields.Text;
  LogEvent(lnApplication, lmtInformation, 'After Add: '+sTmp+': '+IntToStr(i));

  hr.Assigned := True;

  hr.ScreenMetrix := Request.ContentFields.Values['SMEnvironment'];

  if Request.CookieFields.Values[CookieName] = '' then
  begin
    inc(_NewVisitor);
    hr.NewVisitor := True;
  end else
  begin
    inc(_ReturnVisitor);
    hr.NewVisitor := False;
  end;
  with Response.Cookies.Add do
  begin
    Name := CookieName;
    Value := DateTimeToStr(Now);
    Expires := EncodeDate(2999,12,31); // StrToDateTime('12/31/2999'); was causing error in non us locations
  end;

  AppHost := Request.Host;

  hr.Started := Now;
  hr.MethodType := Request.MethodType;
  hr.Protocol := Request.ProtocolVersion;
  hr.PathInfo := Request.PathInfo;
  if Request.PathInfo <> '/' then
  begin
    hr.FileRequest := FastPosNoCase(Request.PathInfo,'/'+ServerController.ExecCmd+'/',length(Request.PathInfo),length(ServerController.ExecCmd)+2,1)=0;
    if hr.FileRequest then
    begin
      if FastPosNoCase(Request.PathInfo,'/js/',length(Request.PathInfo),4,1)>0 then
        hr.RequestType := rtIntScript
      else
      if FastPosNoCase(Request.PathInfo,'/cache/',length(Request.PathInfo),4,1)>0 then
        hr.RequestType := rtCache
      else
      if FastPosNoCase(Request.PathInfo,'/gfx/',length(Request.PathInfo),4,1)>0 then
        hr.RequestType := rtIntGraphic
      else
        hr.RequestType := rtFile;
    end else
      hr.RequestType := rtForm;
  end else
  begin
    hr.FileRequest := False;
    hr.RequestType := rtRoot;
  end;
  hr.IP := Request.RemoteAddr;
  hr.HostName := Request.RemoteHost;
  hr.Referrer := Request.Referer;
  hr.UserAgent := Request.UserAgent;
  hr.ScriptName := Request.ScriptName;
  hr.ContentEncoding := Request.ContentEncoding;
  hr.ContentType := Request.ContentType;
  hr.ContentLength := Request.ContentLength;
  hr.ContentVersion := Request.ContentVersion;

  HitLogRecords.Add(hr);
  inc(HitLogRecordsCount);

  ne := InternalOnCreateSession;
  if @GSessions.OnCreateSession <> @ne then
  begin
    __OnCreateSession := GSessions.OnCreateSession;
    GSessions.OnCreateSession := InternalOnCreateSession;
  end;
  ne := InternalOnDestroySession;
  if @GSessions.OnDestroySession <> @ne then
  begin
    __OnDestroySession := GSessions.OnDestroySession;
    GSessions.OnDestroySession := InternalOnDestroySession;
  end;

  if Assigned(__OnBeforeDispatch) then
    __OnBeforeDispatch(Sender, Request, Response, Handled);
end;

procedure TArcIWServerManager.StartListening;
var
  sURL : string;
begin
  if GLicense.{$IFDEF INTRAWEB70}LicenseVal{$ELSE}License{$ENDIF} = ltEval then
    raise Exception.Create('Due to limitations in the IntraWeb Evaluation, ServerManager will not work when IntraWeb is in Evaluation mode.');

  LogEvent(lnApplication, lmtInformation, 'Attempting to Start Server Manager Admin...');
  try
    FAdminLoc := DLLFilePath+DefaultAppFile+' '+IntToStr(FPort);
    if AdminAppEXE = '' then
    begin
      FAdminApp := TAdminAppDLL.Create(Self, '127.0.0.1',FPort, DLLFilePath+ExtractFilePath(DefaultAppFile)+'SMSoapClient.dll', FAdminLoc);
      FAdminApp.LoadEXE;
    end else
      FAdminApp := TAdminAppDLL.Create(Self, '127.0.0.1',FPort, DLLFilePath+ExtractFilePath(FAdminAppEXE)+'SMSoapClient.dll',FAdminLoc);

    if FAdminAppExe = '' then
      EventChecker := TAdminAppEventCheckThread.Create(
                      DLLFilePath+ExtractFilePath(DefaultAppFile), Self, FPort,
                      FEventPollInterval)
    else
      EventChecker := TAdminAppEventCheckThread.Create(
                      DLLFilePath+ExtractFilePath(FAdminAppEXE), Self, FPort,
                      FEventPollInterval);

    try
      FAdminApp.AlertAppStarted;
    except
      on e: exception do
        RaiseSOAPError('AlertAppStarted()', e.Message);

    end;
  except
    on e: exception do
    begin
      LogEvent(lnApplication, lmtError, 'Error Starting Server Manager Admin: '+e.Message);
      raise;
    end;
  end;
end;

procedure TArcIWServerManager.StartProfiling;
begin
  if GLicense.{$IFDEF INTRAWEB70}LicenseVal{$ELSE}License{$ENDIF} = ltEval then
    raise Exception.Create('Due to limitations in the IntraWeb Evaluation, ServerManager will not work when IntraWeb is in Evaluation mode.');

  if FEnableProfiling then
    ProfilerThread := TProfilerThread.Create(Self);
  __OnCreateSession := GSessions.OnCreateSession;
  GSessions.OnCreateSession := InternalOnCreateSession;
  __OnDestroySession := GSessions.OnDestroySession;
  GSessions.OnDestroySession := InternalOnDestroySession;
  __OnBeforeDispatch := ServerController.OnBeforeDispatch;
  ServerController.OnBeforeDispatch := InternalBeforeDispatch;
  __OnAfterDispatch := ServerController.OnAfterDispatch;
  ServerController.OnAfterDispatch := InternalAfterDispatch;
  __OnNewSession := ServerController.OnNewSession;
  ServerController.OnNewSession := InternalOnNewSession;
  __OnCloseSession := ServerController.OnCloseSession;
  ServerController.OnCloseSession := InternalOnCloseSession;

  if FLogLicenseInfo then
  begin
    if GLicense.{$IFDEF INTRAWEB70}LicenseVal{$ELSE}License{$ENDIF} = ltEval then
    begin
      LogMessage('Evaluation Mode');
      if GLicense.{$IFDEF INTRAWEB70}IsOldKeyVal{$ELSE}IsOldKey{$ENDIF} then begin
        LogMessage('WARNING: The installed key is NOT a valid 6.0 key');
        LogMessage('Please make sure the key starts with +007');
      end;
    end else
    begin
      if GLicense.{$IFDEF INTRAWEB70}RegisteredNameVal{$ELSE}RegisteredName{$ENDIF} <> '' then
        LogMessage('Registered to: ' + GLicense.{$IFDEF INTRAWEB70}RegisteredNameVal{$ELSE}RegisteredName{$ENDIF});
      if GLicense.{$IFDEF INTRAWEB70}CompanyVal{$ELSE}Company{$ENDIF} <> '' then
        LogMessage('Company: ' + GLicense.{$IFDEF INTRAWEB70}CompanyVal{$ELSE}Company{$ENDIF});
      if GLicense.{$IFDEF INTRAWEB70}ExpireDateVal{$ELSE}ExpireDate{$ENDIF} > 0 then
        LogMessage('Expiration Date: ' + DateToStr(GLicense.{$IFDEF INTRAWEB70}ExpireDateVal{$ELSE}ExpireDate{$ENDIF}))
      else
        LogMessage('Expiration Date: Never');

      case GLicense.{$IFDEF INTRAWEB70}LicenseVal{$ELSE}License{$ENDIF} of
        ltEnterprise: LogMessage('Enterprise Edition');
        ltDeveloper: LogMessage('Developer Edition');
        ltPersonal: LogMessage('Personal Edition');
        {$IFNDEF INTRAWEB90}
        ltPackagedPage: LogMessage('Packaged Page');
        ltPackagedEnterprise: LogMessage('Packaged Enterprise');
        {$ENDIF}
      end;
      if GLicense.{$IFDEF INTRAWEB70}SerialNoVal{$ELSE}SerialNo{$ENDIF} <> 0 then
        LogMessage(Format(RSLicenseNumber, [IntToStr(GLicense.{$IFDEF INTRAWEB70}SerialNoVal{$ELSE}SerialNo{$ENDIF})]));
    end;
    LogMessage('');
    LogMessage(Format(RSIWVersion, [ServerController.Version]));
    LogMessage('IntraWeb Build Date: ' + DateToStr(GLicense.{$IFDEF INTRAWEB70}BuildDateVal{$ELSE}BuildDate{$ENDIF}));
    if ServerController.Port = 0 then
      LogMessage(RSBindError)
    else
      LogMessage(Format(RSHTTPPort, [IntToStr(ServerController.Port)]));
    if ServerController.SSLOptions.Port > 0 then
      LogMessage(Format(RSHTTPSPort, [IntToStr(ServerController.SSLOptions.Port)]));
    if GLicense.{$IFDEF INTRAWEB70}ThirdPartyTextVal{$ELSE}ThirdPartyText{$ENDIF} <> '' then
    begin
      LogMessage('');
      LogMessage(GLicense.{$IFDEF INTRAWEB70}ThirdPartyTextVal{$ELSE}ThirdPartyText{$ENDIF});
    end;
    LogMessage('');
    LogMessage('---------------------');
    LogMessage('');
  end;
end;

procedure TArcIWServerManager.StopListening;
begin
  LogEvent(lnApplication, lmtInformation, 'Attempting to Stop Server Manager Admin...');
  try
    FreeAndNil(EventChecker);
    //FAdminApp.Stop;
    try
      FAdminApp.Shutdown;
    except
      on e: exception do
        RaiseSOAPError('AlertAppStarted()', e.Message, True);
    end;
    FAdminApp.Free;
 except
    on e: exception do
      LogEvent(lnApplication, lmtError, 'Error Stopping Server Manager Admin: '+e.Message);
  end;
end;

procedure TArcIWServerManager.StopProfiling;
begin
  ProfilerThread.Free;
  if GSessions <> nil then
  begin
    GSessions.OnCreateSession := __OnCreateSession;
    GSessions.OnDestroySession := __OnDestroySession;
  end;
  ServerController.OnBeforeDispatch := __OnBeforeDispatch;
  ServerController.OnAfterDispatch := __OnAfterDispatch;
  __OnNewSession := ServerController.OnNewSession;
  ServerController.OnNewSession := __OnNewSession;
  ServerController.OnCloseSession := __OnCloseSession;
end;

procedure TArcIWServerManager.SetActive(const Value: boolean);
begin
  if (FActive <> Value) and
     (not (csDesigning in ComponentState)) and
     (not (csLoading in ComponentState)) then
  begin
    if Value then
    begin
      ReloadUsers;
      StartProfiling;
    end else
      StopProfiling;
  end;
  FActive := Value;
end;

procedure TArcIWServerManager.SetListening(const Value: boolean);
begin
  if (FListening <> Value) and
     (not (csDesigning in ComponentState)) and
     (not (csLoading in ComponentState)) then
  begin
    if Value then
      StartListening
    else
      StopListening;
  end;
  FListening := Value;
end;

procedure TArcIWServerManager.FlushHitLog;
var
  sLogFilename : string;
  fs : TFileStream;
  i : integer;
  lst : TList;
  FileHeader : TFileHeader;
begin
  HitLogCS.Enter;
  try
    DateTimeToString(sLogFilename,'_yyyymmdd_hhnnss',Now);
    sLogFilename := HitLogFilenameRoot+sLogFilename+'.hitlog';

    LastHitLog := sLogFilename;

    lst := TList.Create;
    try
      with HitLogRecords.LockList do
      try
        if not FileExists(sLogFilename) then
          ForceDirectories(ExtractFilePath(sLogFilename));

        fs := TFileStream.Create(sLogFilename,fmOpenRead or fmCreate or fmShareExclusive);
        try
          fs.Position := fs.Size;
          if fs.Position = 0 then
          begin
            FileHeader.FileCode := HitLogFileCode;
            FileHeader.DataVersion := 0;
            FileHeader.TimeStamp := Now;
            fs.Write(FileHeader,SizeOf(FileHeader));
          end;
          for i := 0 to Count-1 do
          begin
            if not THitLogRecord(Items[i]).Assigned then
            begin
              fs.WriteComponent(THitLogRecord(Items[i]));
              THitLogRecord(Items[i]).Free;
            end else
              lst.Add(Items[i]);
          end;
          Clear;
          {$IFNDEF VER130}
          Assign(lst);
          {$ELSE}
          for i := 0 to Count-1 do
            Add(lst[i]);
          {$ENDIF}
          HitLogRecordsCount := lst.Count;
        finally
          fs.Free;
        end;
      finally
        HitLogRecords.UnlockList;
      end;
    finally
      lst.Free;
    end;
  finally
    HitLogCS.Leave;
  end;
  CustomEventsCS.Enter;
  try
    if FCustomEvents.Count > 0 then
    begin
      DateTimeToString(sLogFilename,'_yyyymmdd_hhnnss',Now);
      FCustomEvents.SaveToFile(HitLogFilenameRoot+sLogFilename+'.custlog');
      FCustomEvents.Clear;
    end;
  finally
    CustomEventsCS.Leave;
  end;
end;

function TArcIWServerManager.AcquireHitLogRecord(Request : TWebRequest) : THitLogRecord;
var
  i : integer;
  sTmp : string;
begin
  i := Request.QueryFields.Count;
  sTmp := Request.QueryFields.Text;
  LogEvent(lnApplication, lmtInformation, 'Before Acquire Hit: '+sTmp+': '+IntToStr(i));

  i := StrToIntDef(Request.QueryFields.Values['~~~~hr~~~~'],0);

  if i = 0 then
    raise Exception.Create('Error in Request After Dispatch.');

  Result := THitLogRecord(Pointer(i));

  i := Request.QueryFields.Count;
  sTmp := Request.QueryFields.Text;
  LogEvent(lnApplication, lmtInformation, 'After Acquire Hit: '+sTmp+': '+IntToStr(i));
end;

function TArcIWServerManager.AcquireSession(Request : TWebRequest) : TIWApplication;
var
  i : integer;
  sTmp : string;
begin
  Result := nil;
  i := Request.QueryFields.Count;
  sTmp := Request.QueryFields.Text;
  LogEvent(lnApplication, lmtInformation, 'Before Acquire Session: '+sTmp+': '+IntToStr(i));

  i := StrToIntDef(Request.QueryFields.Values['~~~~ss~~~~'],0);

  if i > 0 then
  begin
    Result := TIWApplication(Pointer(i));
    {$IFDEF INTRAWEB80}
    if Result <> nil then begin
      if GSessions.IsValidSession(Result) then begin
        Result.Lock;
      end else begin
        Result := nil;
      end;
    end;
    {$ENDIF}
  end;
  i := Request.QueryFields.Count;
  sTmp := Request.QueryFields.Text;
  LogEvent(lnApplication, lmtInformation, 'After Acquire Session: '+sTmp+': '+IntToStr(i));
end;

function TArcIWServerManager.FindSessionForRequest(Request : TWebRequest) : TIWApplication;
var
  sAppID, sURL : string;
  iLen : integer;
  bIsValid : boolean;
begin
  Result := nil;

  sURL := Request.PathInfo;
  if Copy(sURL, 1, 4) = '/../' then
    Delete(sURL, 1, 4)
  else
    Delete(sURL, 1, 1);

  /// Calling Fetch alters sURL.
  if Fetch(sURL, '/') ='32' then
    Fetch(sURL, '/');
  Fetch(sURL, '/');

  case ServerController.SessionTrackingMethod of
    tmCookie: sAppID := Request.CookieFields.Values['IW_SessionID_'];
    tmURL:    sAppID := Fetch(sURL, '/');
    tmHidden:
      begin
        if (Request.Method = 'GET') then
          sAppID := Fetch(sURL, '/')
        else
          sAppID := Request.ContentFields.Values['IW_SessionID_'];
      end;
  end;
  {$IFNDEF INTRAWEB71}
  iLen := 24;
  {$ELSE}
  {$IFNDEF INTRAWEB72}
  iLen := 26;
  {$ELSE}
  iLen := 28;
  {$ENDIF}
  {$ENDIF}
  sAppID := Copy(sAppID,1,iLen);
  bIsValid := (sAppID <> '') and
              (length(sAppID) = iLen) and
              (FastCharPos(sAppID,'.',1) = 0) and
              (FastCharPos(sAppID,'/',1) = 0);
  if bIsValid then
    Result := LookupSession(sAppID);
end;

procedure TArcIWServerManager.InternalAfterDispatch(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  function InsertTrackerJS(str : string) : string;
  var
    sJS : string;
    idx : integer;
  begin
    sJS := '<script language="javascript">'+
           'document.write(''<input type="hidden" name="SMEnvironment" value="''+'+
           'window.screen.colorDepth+'',''+'+
           'window.screen.width+'',''+'+
           'window.screen.height+'',''+'+
           'document.body.clientWidth+'',''+'+
           'document.body.clientHeight+''">'')'+
           ';</script><input ';
    idx := FastPosNoCase(str,'<input ',length(str),7,1);
    if idx > 0 then
      result := Copy(str,1,idx-1)+sJS+Copy(str,idx+7,high(Integer))
    else
      result := str;
  end;
var
  hr : THitLogRecord;
  Session : TIWApplication;
begin
  inc(_ContentBytes,Response.ContentLength);
  try
    try
      hr := AcquireHitLogRecord(Request);
    except
      Abort;
    end;
    Session := AcquireSession(Request);
    try
      if (Session <> nil) then
      begin
        if not Session.Terminated then
          if Session.ActiveForm = nil then
            hr.FormName := '(unknown)'
          else
            hr.FormName := Session.ActiveForm.Name
        else
          hr.FormName := '(terminated)';

        hr.UserName := Session.AuthUser;
        hr.SessionID := Session.AppID;
        hr.ClientType := Session.ClientType;
        hr.FormAction := Session.FormAction;
        {$IFDEF INTRAWEB51}
          // TODO: Must figure out how to determine that a session is secure in IW 5.1
        {$ELSE}
        hr.Secure     := Session.SecureMode;
        {$ENDIF}
        hr.Terminated := Session.Terminated;
      end;
    finally
      {$IFDEF INTRAWEB80}
        if Session <> nil then
          Session.Unlock;
      {$ENDIF}
    end;
    hr.Finished := Now;
    hr.Assigned := False;

    if HitLogRecordsCount > FMemCacheHitLog then
      FlushHitLog;

    Response.Content := InsertTrackerJS(Response.Content);
  finally
    if Assigned(__OnAfterDispatch) then
      __OnAfterDispatch(Sender, Request, Response, Handled);
  end;
end;

procedure TArcIWServerManager.InternalOnCloseSession(
  ASession: TIWApplication);
var
  lst : TList;
  i : integer;
begin
  lst := SessionList.LockList;
  try
    i := lst.IndexOf(ASession);
    if i >= 0 then
      lst.Delete(i);
  finally
    SessionList.UnlockList;
  end;
  if Assigned(__OnCloseSession) then
    __OnCloseSession(ASession);
end;

procedure TArcIWServerManager.InternalOnNewSession(
  ASession: TIWApplication; var VMainForm: TIWBaseForm);
var
  hr : THitLogRecord;
  i : integer;
  sTmp : string;
begin
  try
    i := ASession.Request.QueryFields.Count;
    sTmp := ASession.Request.QueryFields.Text;
    LogEvent(lnApplication, lmtInformation, 'Before test CloseSession: '+sTmp+': '+IntToStr(i));

    if ASession.Request.QueryFields.Values['~~~closesession~~~'] = '~t~' then
    begin
      ASession.Terminate('Test');
      exit;
    end;

    i := ASession.Request.QueryFields.Count;
    sTmp := ASession.Request.QueryFields.Text;
    LogEvent(lnApplication, lmtInformation, 'After test CloseSession: '+sTmp+': '+IntToStr(i));

    if FNewSessionsDenied then
    begin

      ASession.Terminate(FNewSessionsDeniedMsg);
      exit;
    end;

    hr := AcquireHitLogRecord(ASession.Request);
    hr.NewSession := True;

    SessionList.Add(ASession);
  finally
    if Assigned(__OnNewSession) then
      __OnNewSession(ASession,VMainForm);
  end;
end;

function TArcIWServerManager.RestartIW : boolean;
  procedure RestartISAPI;
  begin
    CopyFile(PChar(DLLFileName),PChar(DLLFileName+'.update'),False);
  end;
  procedure RestartApp;
    function IsService : string;
    begin
      {$IFNDEF BCB}
      Result := IfThen(SWServiceModule = nil, 'N','Y');
      {$ELSE}
      Result := 'N';
      {$ENDIF}
    end;
  begin
    ExtractResourceFile('RestartEXE',DLLFilePath+DefaultRestartEXE);
    WinExec(PAnsiChar('"'+DLLFilePath+DefaultRestartEXE+'" "'+DLLFileName+'" "'+ServerController.AppName+'" "'+IsService+'"'), SW_HIDE);
  end;
  procedure RestartDSO;
  begin
    FLastError := 'DSOs cannot be restarted in this version of ServerManager';
  end;
begin
  FLastError := '';
  try
    if pos('.dll',DLLFilename)>0 then
      RestartISAPI
    else
      if pos('.exe',DLLFilename)>0 then
        RestartApp
      else
        if pos('.dso',DLLFilename)>0 then
          RestartDSO
        else
          FLastError := 'Could Not Restart IntraWeb.  Unknown Application Type';
  except
    on e: Exception do
      FLastError := e.Message;
  end;
  Result := FLastError = '';
end;

procedure TArcIWServerManager.CustomEvent(EventType, EventName: string);
begin
  CustomEventsCS.Enter;
  try
    FCustomEvents.Add('`'+DateToStr(Now)+'`~`'+IntToStr(GetTickCount)+'`~`'+EventType+'`~`'+EventName+'`');
  finally
    CustomEventsCS.Leave;
  end;
end;

procedure TArcIWServerManager.CustomEvent(EventType: TCustomEventType;
  EventName: string);
begin
  case EventType of
    etDBLookup:     CustomEvent('DB Lookup',     EventName);
    etDBPost:       CustomEvent('DB Post',       EventName);
    etAction:       CustomEvent('Action',        EventName);
    etButtonClick:  CustomEvent('Button Click',  EventName);
    etSelection:    CustomEvent('Selection',     EventName);
  end;
end;

procedure TArcIWServerManager.ReloadUsers;
begin
  FUsersLoaded := True;
  FSecurity.LoadFromFile(ExtractFilePath(DLLFilePath+FAdminAppEXE)+'IWUsers.sec');
  ServerController.AuthList.Clear;
  FSecurity.FillAuthList(ServerController.AuthList);
end;

function TArcIWServerManager.GetUsers: TLoginItemList;
begin
  Result := FSecurity.Users;
end;

procedure TArcIWServerManager._SetSecurityDefs(Sender: TObject);
begin
  if Assigned(FOnSetSecurityDefs) then
    FOnSetSecurityDefs(Self,FSecurity.SecurityDefs);
end;

procedure TArcIWServerManager.SaveUsers;
begin
  FSecurity.SaveToFile;
end;

procedure TArcIWServerManager._LoadAuthFile(Sender: TObject;
  const Filename: string; Stream: TStream; var Skip: boolean);
begin
  if Assigned(FOnLoadAuthFile) then
    FOnLoadAuthFile(Self, Filename, Stream, Skip);
end;

procedure TArcIWServerManager._SaveAuthFile(Sender: TObject;
  const Filename: string; Stream: TStream; var Skip: boolean);
begin
  if Assigned(FOnSaveAuthFile) then
    FOnSaveAuthFile(Self, Filename, Stream, Skip);
end;

procedure TArcIWServerManager.SetOnLoadAuthFile(
  const Value: TLoginManagerFileEvent);
begin
  FOnLoadAuthFile := Value;
  if Assigned(Value) then
    FSecurity.OnLoadFile := _LoadAuthFile
  else
    FSecurity.OnLoadFile := nil;
end;

procedure TArcIWServerManager.SetOnSaveAuthFile(
  const Value: TLoginManagerFileEvent);
begin
  FOnSaveAuthFile := Value;
  if Assigned(Value) then
    FSecurity.OnSaveFile := _SaveAuthFile
  else
    FSecurity.OnSaveFile := nil;
end;

{procedure TArcIWServerManager.SetInternalISAPI(const Value: TFilename);
begin
  FInternalISAPI := Value;
end;
}
procedure TArcIWServerManager.LoginUser(AppID, UserName: string);
begin
  SessionUsersCS.Enter;
  try
    SessionUsers.Values[AppID] := UserName;     
  finally
    SessionUsersCS.Leave;
  end;
end;

procedure TArcIWServerManager.LogoutUser(AppID : string);
var
  idx : integer;
begin
  SessionUsersCS.Enter;
  try
    idx := SessionUsers.IndexOf(AppID);
    if idx >= 0 then
      SessionUsers.Delete(idx);
  finally
    SessionUsersCS.Leave;
  end;
end;

procedure TArcIWServerManager.UpdateAdminAppVars;
var
  lst : TList;
  i : integer;
  s, sTimeStamp : string;
begin
(*  {$IFDEF INTRAWEB72}
  ChangeApplicationVariable('IWClass', '7.2');
  {$ELSE} {$IFDEF INTRAWEB71}
  ChangeApplicationVariable('IWClass', '7.1');
  {$ELSE} {$IFDEF INTRAWEB70}
  ChangeApplicationVariable('IWClass', '7.0');
  {$ELSE} {$IFDEF INTRAWEB60}
  ChangeApplicationVariable('IWClass', '6.0');
  {$ELSE} {$IFDEF INTRAWEB51}
  ChangeApplicationVariable('IWClass', '5.1');
  {$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
*)
  ProLogCS.Enter;
  try
    if LastProLog = '' then
      LastProLog := ProLogFilenameRoot+'none.ProLog';
    ChangeApplicationVariable('LastProLog', LastProLog);
  finally
    ProLogCS.Leave;
  end;
  HitLogCS.Enter;
  try
    if LastHitLog = '' then
      LastHitLog := HitLogFilenameRoot+'none.HitLog';
    ChangeApplicationVariable('LastHitLog', LastHitLog);
  finally
    HitLogCS.Leave;
  end;
  ChangeApplicationVariable('SessionCountCurrent', _SessionCount);
  ChangeApplicationVariable('SessionCountMax', _SessionCountMax);
  ChangeApplicationVariable('ThreadCountCurrent', _ThreadCount);
  ChangeApplicationVariable('ThreadCountMax', _ThreadCountMax);
  ChangeApplicationVariable('MemoryUsageCurrent', _MemoryUsage);
  ChangeApplicationVariable('MemoryUsageMax', _MemoryUsageMax);
  ChangeApplicationVariable('BytesTransmitted', _ContentBytes);
  ChangeApplicationVariable('StartedTime', _StartedRunning);
  ChangeApplicationVariable('CurrentTime', Now);
  ChangeApplicationVariable('Port', ServerController.Port); // do before appname.
  ChangeApplicationVariable('ApplicationName', ServerController.AppName);
  ChangeApplicationVariable('CacheURL', ServerController.CacheURL);
  ChangeApplicationVariable('FilesURL', ServerController.FilesURL);
  {$IFDEF INTRAWEB51}
  ChangeApplicationVariable('FilesNCURL', ServerController.FilesURL);
  {$ELSE}
  ChangeApplicationVariable('FilesNCURL', ServerController.FilesNCURL);
  {$ENDIF}
  ChangeApplicationVariable('FilesPath', ServerController.FilesDir);
  ChangeApplicationVariable('BoundTo', ServerController.BoundIP);
  ChangeApplicationVariable('CachePath', ServerController.CacheDir);
  ChangeApplicationVariable('Charset', ServerController.CharSet);
  ChangeApplicationVariable('AppDescription', ServerController.Description);
  ChangeApplicationVariable('ExecCmd', ServerController.ExecCmd);
  ChangeApplicationVariable('InternalFilesDir', ServerController.InternalFilesDir);
  ChangeApplicationVariable('InternalFilesURL', ServerController.InternalFilesURL);
  ChangeApplicationVariable('InvalidCommandURL', ServerController.InvalidCommandURL);
  ChangeApplicationVariable('NoJavascriptFilename', ServerController.NoJavaScriptSupport.Filename);
  ChangeApplicationVariable('NoJavascriptURL', ServerController.NoJavaScriptSupport.URL);
  ChangeApplicationVariable('NoCookieFilename', ServerController.NoCookieSupport.URL);
  ChangeApplicationVariable('NoCookieURL', ServerController.NoCookieSupport.URL);
  ChangeApplicationVariable('StartCmd', ServerController.StartCmd);
  ChangeApplicationVariable('TemplateDir', ServerController.TemplateDir);

  {$IFNDEF INTRAWEB70}
  ChangeApplicationVariable('TimeoutURL', ServerController.TimeoutURL);
  {$ELSE}
  ChangeApplicationVariable('TimeoutURL', ServerController.SessionTimeoutURL.URL);
  {$ENDIF}

  ChangeApplicationVariable('UnknownBrowserFilename', ServerController.UnknownBrowser.Filename);
  ChangeApplicationVariable('UnknownBrowserURL', ServerController.UnknownBrowser.URL);
  ChangeApplicationVariable('URLBase', ServerController.URLBase);
  ChangeApplicationVariable('IWVersion', ServerController.Version);
  ChangeApplicationVariable('AppPath', ServerController.AppPath);
  ChangeApplicationVariable('AppFilename', DLLFileName);
  ChangeApplicationVariable('AppExt', ExtractFileExt(DLLFileName));
  ChangeApplicationVariable('AppHost', AppHost);
  ChangeApplicationVariable('LastError', FLastError);
  ChangeApplicationVariable('AppFileDate', AppFileDate);
  ChangeApplicationVariable('AppVersion', AppVersion);
  ChangeApplicationVariable('SecurityFilePath', ExtractFilePath(DLLFilePath+FAdminAppEXE));

  MsgLogCS.Enter;
  try
    while FMessageLog.Count > 0 do
    begin
      FAdminApp.LogMessage(FMessageLog[0]);
      FMessageLog.Delete(0);
    end;
  finally
    MsgLogCS.Leave;
  end;

  SessionUsersCS.Enter;
  try
    ChangeApplicationVariable('SessionUsers', SessionUsers.CommaText);
  finally
    SessionUsersCS.Leave;
  end;

  s := '';
  lst := SessionList.LockList;
  try
    for i := 0 to lst.Count-1 do
    begin
      {$IFDEF INTRAWEB72}
      DateTimeToString(sTimeStamp,'yyyy-mm-dd hh:nn:ss.zzz',TIWApplication(lst[i]).SessionTimeStamp);
      {$ELSE}
      sTimeStamp := TIWApplication(lst[i]).SessionTimeStamp;
      {$ENDIF}
      s := s+'`"'+
           TIWApplication(lst[i]).AppID+'";"'+
           TIWApplication(lst[i]).AuthUser+'";"'+
           TIWApplication(lst[i]).ActiveForm.ClassName+'";"'+
           TIWApplication(lst[i]).ReferringURL+'";"'+
           TIWApplication(lst[i]).IP+'";"'+
           sTimeStamp+'"`~';
    end;
  finally
    SessionList.UnlockList;
  end;
  ChangeApplicationVariable('Sessions',s);
end;

procedure TArcIWServerManager.OnCommand(Command: WideString; Value: Variant);
  procedure DropAllSessions;
  var
    i : integer;
    lst, lsToDelete : TList;
  begin
    lsToDelete := TList.Create;
    try
      lst := GSessions.LockList;
      try
        if FDropImmediately then
        begin
          for i := 0 to lst.Count-1 do
            lsToDelete.Add(TIWApplication(lst[i]));
        end else
        begin
          for i := 0 to lst.Count-1 do
            TIWApplication(lst[i]).Terminate(FDropSessionsMsg);
        end;
      finally
        GSessions.UnlockList;
      end;
      for i := 0 to lsToDelete.Count -1 do
      begin
        GSessions.Remove(lsToDelete[i]);
        TIWApplication(lsToDelete[i]).Terminate('');
        TIWApplicationHack(lsToDelete[i]).FSessionTimeStamp := 0;
        TIWApplication(lsToDelete[i]).Free;
      end;
    finally
      lsToDelete.Free;
    end;
  end;
  procedure DropSingleSession;
  var
    sl : TD7StringList;
    app : TIWApplication;
  begin
    if FastCharPos(Value,'~',1)>0 then
    begin
      sl := TD7StringList.Create;
      try
        sl.Delimiter := '~';
        sl.QuoteChar := '`';
        sl.DelimitedText := Value;
        if sl[0] = 'Drop' then
        begin
          if sl.Count <> 3 then
            raise exception.Create('Error in request from Admin App');
          app := LookupSession(sl[1]);
          try
            if app <> nil then
            begin
              if FDropImmediately then
              begin
                GSessions.Remove(app);
                app.Terminate('');
                TIWApplicationHack(app).FSessionTimeStamp := 0;
                app.Free;
              end else
                app.Terminate(sl[2]);
            end;
          finally
            {$IFDEF INTRAWEB80}
            app.Unlock;
            {$ENDIF}
          end;
        end else
          if sl[0] = 'Notify' then
          begin
            if sl.Count <> 3 then
              raise exception.Create('Error in request from Admin App');
            app := LookupSession(sl[1]);
            try
              if app <> nil then
              begin
                app.ShowMessage(sl[2]);
              end;
            finally
              {$IFDEF INTRAWEB80}
              app.Unlock;
              {$ENDIF}
            end;
          end;
      finally
        sl.Free;
      end;
    end;
  end;
  function EscapeString(Text : string) : string;
  begin
    Result := FastReplace(Text,#13,' ',False);
    Result := FastReplace(Result,#10,' ',False);
    Result := FastReplace(Result,'\','\\',False);
    Result := FastReplace(Result,'"','\"',False);
    Result := FastReplace(Result,'''','\''',False);
  end;
begin
  FLastError := '';
  if Command = 'UpdateVars' then
    UpdateAdminAppVars
  else if Command = 'DenySessionsMsg' then
  begin
    if (Copy(Value,1,1)='|') and FileExists(Copy(Value,2,High(Integer))) then
    begin
      with TStringList.Create do
      try
        LoadFromFile(Copy(Value,2,High(Integer)));
        FNewSessionsDeniedMsg := EscapeString(Text);
      finally
        free;
      end;
    end else
      FNewSessionsDeniedMsg := Value
  end else if Command = 'DropSessionsMsg' then
    FDropSessionsMsg := Value
  else if Command = 'ReloadAppUsers' then
    ReloadUsers
  else if Command = 'RestartIW' then
    RestartIW
  else if Command = 'DropImmediately' then
    FDropImmediately := Value
  else if Command = 'SessionAction' then
    begin
      if Value = 'DropAll' then
        DropAllSessions
      else
        DropSingleSession
    end
  else if Command = 'DenySessions' then
    FNewSessionsDenied := Value;

//  ChangeApplicationVariable('LastError',FLastError);
end;

procedure TArcIWServerManager.SetAdminAppEXE(const Value: TFilename);
begin
  FAdminAppEXE := Value;
end;

procedure TArcIWServerManager.SetDefAdminPass(const Value: string);
begin
  FDefAdminPass := Value;
  if FAdminApp <> nil then
    ChangeSettingsVariable('DefAdminPass', FDefAdminPass);
end;

procedure TArcIWServerManager.SetDefAdminUser(const Value: string);
begin
  FDefAdminUser := Value;
  if FAdminApp <> nil then
    ChangeSettingsVariable('DefAdminUser', FDefAdminUser);
end;

procedure TArcIWServerManager.SetHideCopyright(const Value: boolean);
begin
  FHideCopyright := Value;
  if FAdminApp <> nil then
    ChangeSettingsVariable('HideCopyright', Integer(FHideCopyright));
end;

procedure TArcIWServerManager.SetHideDisabledItems(const Value: boolean);
begin
  FHideDisabledItems := Value;
  if FAdminApp <> nil then
    ChangeSettingsVariable('HideDisabledItems', Integer(FHideDisabledItems));
end;

procedure TArcIWServerManager.SetHideIWUserAdmin(const Value: boolean);
begin
  FHideIWUserAdmin := Value;
  if FAdminApp <> nil then
    ChangeSettingsVariable('HideIWUserAdmin', Integer(FHideIWUserAdmin));
end;

procedure TArcIWServerManager.SetIniFile(const Value: TFilename);
begin
  FIniFile := Value;
  if FAdminApp <> nil then
    ChangeSettingsVariable('IniFile', FIniFile);
end;

procedure TArcIWServerManager.SetLoginLocalhost(const Value: boolean);
begin
  FLoginLocalhost := Value;
  if FAdminApp <> nil then
    ChangeSettingsVariable('LoginLocalhost', Integer(FLoginLocalhost));
end;

procedure TArcIWServerManager.SetLogoutRedirectURL(const Value: string);
begin
  FLogoutRedirectURL := Value;
  if FAdminApp <> nil then
    ChangeSettingsVariable('LogoutRedirectURL', FLogoutRedirectURL);
end;

procedure TArcIWServerManager.SetRegistrationCode(const Value: string);
begin
  FRegistrationCode := Value;
  if FAdminApp <> nil then
    ChangeSettingsVariable('RegistrationCode', FRegistrationCode);
end;

procedure TArcIWServerManager.SetRegistrationEmail(const Value: string);
begin
  FRegistrationEmail := Value;
  if FAdminApp <> nil then
    ChangeSettingsVariable('RegistrationEmail', FRegistrationEmail);
end;

procedure TArcIWServerManager.SetRegistrationName(const Value: string);
begin
  FRegistrationName := Value;
  if FAdminApp <> nil then
    ChangeSettingsVariable('RegistrationName', FRegistrationName);
end;

procedure TArcIWServerManager.SetRegistrationOverride(
  const Value: TRegistrationType);
begin
  FRegistrationOverride := Value;
  if FAdminApp <> nil then
    ChangeSettingsVariable('RegistrationOverride', Integer(FRegistrationOverride));
end;

procedure TArcIWServerManager.SetRegKey(const Value: string);
begin
  FRegKey := Value;
  if FAdminApp <> nil then
    ChangeSettingsVariable('RegKey', FRegKey);
end;

procedure TArcIWServerManager.SetUserAdministration(const Value: boolean);
begin
  FUserAdministration := Value;
  if FAdminApp <> nil then
    ChangeSettingsVariable('UserAdministration', Integer(FUserAdministration));
end;

procedure TArcIWServerManager.UpdateSettingsVars;
begin
  // Force variable update to the Admin App.
  RegistrationName := FRegistrationName;
  RegistrationEmail := FRegistrationEmail;
  RegistrationCode := FRegistrationCode;
  DefAdminUser := FDefAdminUser;
  DefAdminPass := FDefAdminPass;
  LoginLocalhost := FLoginLocalhost;
  IniFile := FIniFile;
  RegKey := FRegKey;
  UserAdministration := FUserAdministration;
  HideIWUserAdmin := FHideIWUserAdmin;
  HideDisabledItems := FHideDisabledItems;
  RegistrationOverride := FRegistrationOverride;
  LogoutRedirectURL := FLogoutRedirectURL;
  HideCopyright := FHideCopyright;

  ChangeSettingsVariable('Port', FPort);
  ChangeSettingsVariable('DenyNewSessionsMsg', DenyNewSessionsMsg);
  ChangeSettingsVariable('DropSessionsMsg', DropSessionsMsg);
  ChangeSettingsVariable('LogLicenseInfo', Integer(FLogLicenseInfo));
  ChangeSettingsVariable('MemCacheHitLog', FMemCacheHitLog);
  ChangeSettingsVariable('MemCacheProLog', FMemCacheProLog);
  ChangeSettingsVariable('IWProLogPath', ProLogFilenameRoot);
  ChangeSettingsVariable('IWHitLogPath', HitLogFilenameRoot);
  ChangeSettingsVariable('IWMsgLogPath', LogMessageFilenameRoot);

end;

function TArcIWServerManager.RssXMLPage: string;
begin
  Result := FAdminApp.RssXmlPage;
end;

{$IFDEF INTRAWEB70}
procedure TArcIWServerManager.SetAdminLayout(
  const Value: TLayoutProperties);
begin
  FAdminLayout.Assign(Value);
end;
{$ENDIF}

procedure TArcIWServerManager.UpdateLayoutVars;
begin
  {$IFDEF INTRAWEB70}
  if FAdminLayout.EnableOverride then
  begin
    ChangeSettingsVariable('HasLayout',True);
    ChangeSettingsVariable('ColorMenus',FAdminLayout.Colors.Menus);
    ChangeSettingsVariable('ColorBorders',FAdminLayout.Colors.Borders);
    ChangeSettingsVariable('ColorHeader',FAdminLayout.Colors.Header);
    ChangeSettingsVariable('ColorFooter',FAdminLayout.Colors.Footer);
    ChangeSettingsVariable('ColorGrids',FAdminLayout.Colors.Grids);
    ChangeSettingsVariable('ColorGraphs',FAdminLayout.Colors.Graphs);
    ChangeSettingsVariable('ColorBody',FAdminLayout.Colors.Body);
    ChangeSettingsVariable('ColorGridHeader',FAdminLayout.Colors.GridHeader);
    ChangeSettingsVariable('ColorFields',FAdminLayout.Colors.Fields);
    ChangeSettingsVariable('ColorButtons',FAdminLayout.Colors.Buttons);

    ChangeSettingsVariable('FontMenuItemSize',FAdminLayout.Fonts.MenuItem.Size);
    ChangeSettingsVariable('FontMenuCaptionSize',FAdminLayout.Fonts.MenuCaption.Size);
    ChangeSettingsVariable('FontVersionSize',FAdminLayout.Fonts.Version.Size);
    ChangeSettingsVariable('FontCopyrightSize',FAdminLayout.Fonts.Copyright.Size);
    ChangeSettingsVariable('FontPageTitleSize',FAdminLayout.Fonts.PageTitle.Size);
    ChangeSettingsVariable('FontSectionTitleSize',FAdminLayout.Fonts.SectionTitle.Size);
    ChangeSettingsVariable('FontFieldCaptionSize',FAdminLayout.Fonts.FieldCaption.Size);
    ChangeSettingsVariable('FontFieldTextSize',FAdminLayout.Fonts.FieldText.Size);
    ChangeSettingsVariable('FontButtonTextSize',FAdminLayout.Fonts.ButtonText.Size);
    ChangeSettingsVariable('FontGridHeaderSize',FAdminLayout.Fonts.GridHeader.Size);
    ChangeSettingsVariable('FontGridTextSize',FAdminLayout.Fonts.GridText.Size);
    ChangeSettingsVariable('FontGraphTextSize',FAdminLayout.Fonts.GraphText.Size);

    ChangeSettingsVariable('FontMenuItemFontFamily',FAdminLayout.Fonts.MenuItem.FontFamily);
    ChangeSettingsVariable('FontMenuCaptionFontFamily',FAdminLayout.Fonts.MenuCaption.FontFamily);
    ChangeSettingsVariable('FontVersionFontFamily',FAdminLayout.Fonts.Version.FontFamily);
    ChangeSettingsVariable('FontCopyrightFontFamily',FAdminLayout.Fonts.Copyright.FontFamily);
    ChangeSettingsVariable('FontPageTitleFontFamily',FAdminLayout.Fonts.PageTitle.FontFamily);
    ChangeSettingsVariable('FontSectionTitleFontFamily',FAdminLayout.Fonts.SectionTitle.FontFamily);
    ChangeSettingsVariable('FontFieldCaptionFontFamily',FAdminLayout.Fonts.FieldCaption.FontFamily);
    ChangeSettingsVariable('FontFieldTextFontFamily',FAdminLayout.Fonts.FieldText.FontFamily);
    ChangeSettingsVariable('FontButtonTextFontFamily',FAdminLayout.Fonts.ButtonText.FontFamily);
    ChangeSettingsVariable('FontGridHeaderFontFamily',FAdminLayout.Fonts.GridHeader.FontFamily);
    ChangeSettingsVariable('FontGridTextFontFamily',FAdminLayout.Fonts.GridText.FontFamily);
    ChangeSettingsVariable('FontGraphTextFontFamily',FAdminLayout.Fonts.GraphText.FontFamily);

    ChangeSettingsVariable('FontMenuItemFontName',FAdminLayout.Fonts.MenuItem.FontName);
    ChangeSettingsVariable('FontMenuCaptionFontName',FAdminLayout.Fonts.MenuCaption.FontName);
    ChangeSettingsVariable('FontVersionFontName',FAdminLayout.Fonts.Version.FontName);
    ChangeSettingsVariable('FontCopyrightFontName',FAdminLayout.Fonts.Copyright.FontName);
    ChangeSettingsVariable('FontPageTitleFontName',FAdminLayout.Fonts.PageTitle.FontName);
    ChangeSettingsVariable('FontSectionTitleFontName',FAdminLayout.Fonts.SectionTitle.FontName);
    ChangeSettingsVariable('FontFieldCaptionFontName',FAdminLayout.Fonts.FieldCaption.FontName);
    ChangeSettingsVariable('FontFieldTextFontName',FAdminLayout.Fonts.FieldText.FontName);
    ChangeSettingsVariable('FontButtonTextFontName',FAdminLayout.Fonts.ButtonText.FontName);
    ChangeSettingsVariable('FontGridHeaderFontName',FAdminLayout.Fonts.GridHeader.FontName);
    ChangeSettingsVariable('FontGridTextFontName',FAdminLayout.Fonts.GridText.FontName);
    ChangeSettingsVariable('FontGraphTextFontName',FAdminLayout.Fonts.GraphText.FontName);

    ChangeSettingsVariable('FontMenuItemColor',FAdminLayout.Fonts.MenuItem.Color);
    ChangeSettingsVariable('FontMenuCaptionColor',FAdminLayout.Fonts.MenuCaption.Color);
    ChangeSettingsVariable('FontVersionColor',FAdminLayout.Fonts.Version.Color);
    ChangeSettingsVariable('FontCopyrightColor',FAdminLayout.Fonts.Copyright.Color);
    ChangeSettingsVariable('FontPageTitleColor',FAdminLayout.Fonts.PageTitle.Color);
    ChangeSettingsVariable('FontSectionTitleColor',FAdminLayout.Fonts.SectionTitle.Color);
    ChangeSettingsVariable('FontFieldCaptionColor',FAdminLayout.Fonts.FieldCaption.Color);
    ChangeSettingsVariable('FontFieldTextColor',FAdminLayout.Fonts.FieldText.Color);
    ChangeSettingsVariable('FontButtonTextColor',FAdminLayout.Fonts.ButtonText.Color);
    ChangeSettingsVariable('FontGridHeaderColor',FAdminLayout.Fonts.GridHeader.Color);
    ChangeSettingsVariable('FontGridTextColor',FAdminLayout.Fonts.GridText.Color);
    ChangeSettingsVariable('FontGraphTextColor',FAdminLayout.Fonts.GraphText.Color);

    //ChangeSettingsVariable('FontMenuItemCSSStyle',FAdminLayout.Fonts.MenuItem.CSSStyle);
    //ChangeSettingsVariable('FontMenuCaptionCSSStyle',FAdminLayout.Fonts.MenuCaption.CSSStyle);
    //ChangeSettingsVariable('FontVersionCSSStyle',FAdminLayout.Fonts.Version.CSSStyle);
    //ChangeSettingsVariable('FontCopyrightCSSStyle',FAdminLayout.Fonts.Copyright.CSSStyle);
    //ChangeSettingsVariable('FontPageTitleCSSStyle',FAdminLayout.Fonts.PageTitle.CSSStyle);
    //ChangeSettingsVariable('FontSectionTitleCSSStyle',FAdminLayout.Fonts.SectionTitle.CSSStyle);
    //ChangeSettingsVariable('FontFieldCaptionCSSStyle',FAdminLayout.Fonts.FieldCaption.CSSStyle);
    //ChangeSettingsVariable('FontFieldTextCSSStyle',FAdminLayout.Fonts.FieldText.CSSStyle);
    //ChangeSettingsVariable('FontButtonTextCSSStyle',FAdminLayout.Fonts.ButtonText.CSSStyle);
    //ChangeSettingsVariable('FontGridHeaderCSSStyle',FAdminLayout.Fonts.GridHeader.CSSStyle);
    //ChangeSettingsVariable('FontGridTextCSSStyle',FAdminLayout.Fonts.GridText.CSSStyle);
    //ChangeSettingsVariable('FontGraphTextCSSStyle',FAdminLayout.Fonts.GraphText.CSSStyle);

    ChangeSettingsVariable('FontMenuItemBold',(fsBold in FAdminLayout.Fonts.MenuItem.Style));
    ChangeSettingsVariable('FontMenuCaptionBold',(fsBold in FAdminLayout.Fonts.MenuCaption.Style));
    ChangeSettingsVariable('FontVersionBold',(fsBold in FAdminLayout.Fonts.Version.Style));
    ChangeSettingsVariable('FontCopyrightBold',(fsBold in FAdminLayout.Fonts.Copyright.Style));
    ChangeSettingsVariable('FontPageTitleBold',(fsBold in FAdminLayout.Fonts.PageTitle.Style));
    ChangeSettingsVariable('FontSectionTitleBold',(fsBold in FAdminLayout.Fonts.SectionTitle.Style));
    ChangeSettingsVariable('FontFieldCaptionBold',(fsBold in FAdminLayout.Fonts.FieldCaption.Style));
    ChangeSettingsVariable('FontFieldTextBold',(fsBold in FAdminLayout.Fonts.FieldText.Style));
    ChangeSettingsVariable('FontButtonTextBold',(fsBold in FAdminLayout.Fonts.ButtonText.Style));
    ChangeSettingsVariable('FontGridHeaderBold',(fsBold in FAdminLayout.Fonts.GridHeader.Style));
    ChangeSettingsVariable('FontGridTextBold',(fsBold in FAdminLayout.Fonts.GridText.Style));
    ChangeSettingsVariable('FontGraphTextBold',(fsBold in FAdminLayout.Fonts.GraphText.Style));

    ChangeSettingsVariable('FontMenuItemItalic',(fsItalic in FAdminLayout.Fonts.MenuItem.Style));
    ChangeSettingsVariable('FontMenuCaptionItalic',(fsItalic in FAdminLayout.Fonts.MenuCaption.Style));
    ChangeSettingsVariable('FontVersionItalic',(fsItalic in FAdminLayout.Fonts.Version.Style));
    ChangeSettingsVariable('FontCopyrightItalic',(fsItalic in FAdminLayout.Fonts.Copyright.Style));
    ChangeSettingsVariable('FontPageTitleItalic',(fsItalic in FAdminLayout.Fonts.PageTitle.Style));
    ChangeSettingsVariable('FontSectionTitleItalic',(fsItalic in FAdminLayout.Fonts.SectionTitle.Style));
    ChangeSettingsVariable('FontFieldCaptionItalic',(fsItalic in FAdminLayout.Fonts.FieldCaption.Style));
    ChangeSettingsVariable('FontFieldTextItalic',(fsItalic in FAdminLayout.Fonts.FieldText.Style));
    ChangeSettingsVariable('FontButtonTextItalic',(fsItalic in FAdminLayout.Fonts.ButtonText.Style));
    ChangeSettingsVariable('FontGridHeaderItalic',(fsItalic in FAdminLayout.Fonts.GridHeader.Style));
    ChangeSettingsVariable('FontGridTextItalic',(fsItalic in FAdminLayout.Fonts.GridText.Style));
    ChangeSettingsVariable('FontGraphTextItalic',(fsItalic in FAdminLayout.Fonts.GraphText.Style));

    ChangeSettingsVariable('FontMenuItemUnderline',(fsUnderline in FAdminLayout.Fonts.MenuItem.Style));
    ChangeSettingsVariable('FontMenuCaptionUnderline',(fsUnderline in FAdminLayout.Fonts.MenuCaption.Style));
    ChangeSettingsVariable('FontVersionUnderline',(fsUnderline in FAdminLayout.Fonts.Version.Style));
    ChangeSettingsVariable('FontCopyrightUnderline',(fsUnderline in FAdminLayout.Fonts.Copyright.Style));
    ChangeSettingsVariable('FontPageTitleUnderline',(fsUnderline in FAdminLayout.Fonts.PageTitle.Style));
    ChangeSettingsVariable('FontSectionTitleUnderline',(fsUnderline in FAdminLayout.Fonts.SectionTitle.Style));
    ChangeSettingsVariable('FontFieldCaptionUnderline',(fsUnderline in FAdminLayout.Fonts.FieldCaption.Style));
    ChangeSettingsVariable('FontFieldTextUnderline',(fsUnderline in FAdminLayout.Fonts.FieldText.Style));
    ChangeSettingsVariable('FontButtonTextUnderline',(fsUnderline in FAdminLayout.Fonts.ButtonText.Style));
    ChangeSettingsVariable('FontGridHeaderUnderline',(fsUnderline in FAdminLayout.Fonts.GridHeader.Style));
    ChangeSettingsVariable('FontGridTextUnderline',(fsUnderline in FAdminLayout.Fonts.GridText.Style));
    ChangeSettingsVariable('FontGraphTextUnderline',(fsUnderline in FAdminLayout.Fonts.GraphText.Style));

    ChangeSettingsVariable('FontMenuItemStrikeout',(fsStrikeout in FAdminLayout.Fonts.MenuItem.Style));
    ChangeSettingsVariable('FontMenuCaptionStrikeout',(fsStrikeout in FAdminLayout.Fonts.MenuCaption.Style));
    ChangeSettingsVariable('FontVersionStrikeout',(fsStrikeout in FAdminLayout.Fonts.Version.Style));
    ChangeSettingsVariable('FontCopyrightStrikeout',(fsStrikeout in FAdminLayout.Fonts.Copyright.Style));
    ChangeSettingsVariable('FontPageTitleStrikeout',(fsStrikeout in FAdminLayout.Fonts.PageTitle.Style));
    ChangeSettingsVariable('FontSectionTitleStrikeout',(fsStrikeout in FAdminLayout.Fonts.SectionTitle.Style));
    ChangeSettingsVariable('FontFieldCaptionStrikeout',(fsStrikeout in FAdminLayout.Fonts.FieldCaption.Style));
    ChangeSettingsVariable('FontFieldTextStrikeout',(fsStrikeout in FAdminLayout.Fonts.FieldText.Style));
    ChangeSettingsVariable('FontButtonTextStrikeout',(fsStrikeout in FAdminLayout.Fonts.ButtonText.Style));
    ChangeSettingsVariable('FontGridHeaderStrikeout',(fsStrikeout in FAdminLayout.Fonts.GridHeader.Style));
    ChangeSettingsVariable('FontGridTextStrikeout',(fsStrikeout in FAdminLayout.Fonts.GridText.Style));
    ChangeSettingsVariable('FontGraphTextStrikeout',(fsStrikeout in FAdminLayout.Fonts.GraphText.Style));
  end else
    ChangeSettingsVariable('HasLayout',False);
  {$ENDIF}
end;

procedure TArcIWServerManager.SetOverrideAppName(const Value: string);
begin
  FOverrideAppName := Value;
  if FAdminApp <> nil then
    ChangeSettingsVariable('OverrideAppName', Value);
end;

procedure TArcIWServerManager.SetOverrideCopyright(const Value: string);
begin
  FOverrideCopyright := Value;
  if FAdminApp <> nil then
    ChangeSettingsVariable('OverrideCopyright', Value);
end;

procedure TArcIWServerManager.SetEnableProfiling(const Value: boolean);
begin
  FEnableProfiling := Value;
end;

{ TProfilerThread }

constructor TProfilerThread.Create(AOwner : TArcIWServerManager);
begin
  inherited Create(True);
  Owner := AOwner;
  Resume;
end;

destructor TProfilerThread.Destroy;
begin

  inherited;
end;

procedure TProfilerThread.Execute;
var
  iMem, iMemMax : LongWord;
  pr : PProfilerRecord;
  dummy, cThreads : Cardinal;
  //iTimerInterval : integer;
const
  TimerInterval = 1000;
begin
  try
    while not Terminated do
    begin
      New(pr);
      try
        pr.TimeStamp := Now;
        pr.TickCount := GetTickCount;
        GetApplicationMemory(iMem, iMemMax);
        pr.MemoryUsage := iMem;
        _MemoryUsage := iMem;
        _MemoryUsageMax := iMemMax;
        pr.OverallCPUUsage := GetCPUUsage;
        
        GetThreadCount(dummy,cThreads);
        pr.ThreadCount    := cThreads;
        _ThreadCount      := cThreads;
        _ThreadCountMax   := Max(cThreads,_ThreadCountMax);
        pr.SessionCount   := _SessionCount;
        pr.SessionCountPS := _SessionCount-_SessionCountPrev;
        _SessionCountPrev := _SessionCount;
        pr.HitCounter     := _HitCount;
        pr.HitCounterPS   := _HitCount-_HitCountPrev;
        _HitCountPrev     := _HitCount;
        pr.ContentBytes   := _ContentBytes;
        pr.ContentBytesPS := _ContentBytes-_ContentBytesPrev;
        _ContentBytesPrev := _ContentBytes;
        pr.VisitorsNew    := _NewVisitor;
        pr.VisitorsReturn := _ReturnVisitor;
        GetMemoryInfo(pr.MemTotalPhysical, pr.MemFreePhysical, pr.MemTotalVirtual, pr.MemFreeVirtual,
          pr.MemTotalPage, pr.MemFreePage, pr.MemFree, pr.MemLoad);
        pr.CPUCount       := GetCPUCount;
      except
        Dispose(pr);
        Sleep(TimerInterval);
        continue;
      end;
      Owner.ProfilerRecords.Add(pr);
      inc(Owner.ProfilerRecordsCount);
      if Owner.ProfilerRecordsCount > Owner.FMemCacheProLog then
      begin
        Owner.FlushProLog;
      end;
      Sleep(TimerInterval);
    end;
  except
    // Should log these exceptions!!!
  end;
end;

{ TAdminAppDLL }

procedure TAdminAppDLL.AlertAppNotResponding;
begin
  try
    if @FAlertAppNotResponding <> nil then
      FAlertAppNotResponding;
  except
    on e: exception do
    begin
      if FastPos(e.Message,'connection with the server',length(e.Message),26,1)>0 then
      begin
        LoadEXE;
        raise EServerManCrash.Create;
      end;
    end else
      raise;
  end;

end;

procedure TAdminAppDLL.AlertAppStarted;
begin
  try
    if @FAlertAppStarted <> nil then
      FAlertAppStarted;
  except
    on e: exception do
    begin
      if FastPos(e.Message,'connection with the server',length(e.Message),26,1)>0 then
      begin
        LoadEXE;
        raise EServerManCrash.Create;
      end;
    end else
      raise;
  end;
end;

procedure TAdminAppDLL.AlertAppTerminated;
begin
  try
    if @FAlertAppTerminated <> nil then
      FAlertAppTerminated;
  except
    on e: exception do
    begin
      if FastPos(e.Message,'connection with the server',length(e.Message),26,1)>0 then
      begin
        LoadEXE;
        raise EServerManCrash.Create;
      end;
    end else
      raise;
  end;
end;

procedure TAdminAppDLL.ChangeApplicationVariable(const VarName: string;
  const Value: Variant);
begin
  try
    if @FChangeApplicationVariable <> nil then
      FChangeApplicationVariable(PChar(VarName), Value);
  except
    on e: exception do
    begin
      if FastPos(e.Message,'connection with the server',length(e.Message),26,1)>0 then
      begin
        LoadEXE;
        raise EServerManCrash.Create;
      end;
    end else
      raise;
  end;
end;

procedure TAdminAppDLL.ChangeSettingsVariable(const VarName: string;
  const Value: Variant);
begin
  try
    if @FChangeSettingsVariable <> nil then
      FChangeSettingsVariable(PChar(VarName), Value);
  except
    on e: exception do
    begin
      if FastPos(e.Message,'connection with the server',length(e.Message),26,1)>0 then
      begin
        LoadEXE;
        raise EServerManCrash.Create;
      end;
    end else
      raise;
  end;
end;

constructor TAdminAppDLL.Create(ServerMan : TArcIWServerManager; const IP : string; const Port : integer; dllfilepath: string; AdminLoc : string);
begin
  CoInitialize(nil);
  FServerMan := ServerMan;
  FIP := IP;
  FPort := Port;
  FAdminLoc := AdminLoc;
  FDLLPath := dllFilePath;
  ReloadDLL;
end;

destructor TAdminAppDLL.Destroy;
begin
  UnloadDLL;
  inherited;
end;

function TAdminAppDLL.GetCommand(var Command, Value: Variant): Boolean;
begin
  try
    if @FGetCommand <> nil then
      result := FGetCommand(Command, Value);
  except
    on e: exception do
    begin
      if FastPos(e.Message,'connection with the server',length(e.Message),26,1)>0 then
      begin
        LoadEXE;
        raise EServerManCrash.Create;
      end;
    end else
      raise;
  end;
end;

procedure TAdminAppDLL.Initialize(const IP: string; const Port: integer);
begin
  try
    FIP := IP;
    FPort := Port;
    if @FInitialize <> nil then
      FInitialize(PChar(IP), Port);
  except
    on e: exception do
    begin
      if FastPos(e.Message,'connection with the server',length(e.Message),26,1)>0 then
      begin
        LoadEXE;
        raise EServerManCrash.Create;
      end;
    end else
      raise;
  end;
end;

procedure TAdminAppDLL.LoadEXE;
begin
  WinExec(PAnsiChar(FAdminLoc),SW_NORMAL);
  Initialize(FIP,FPort);
  FServerMan.UpdateSettingsVars;
  FServerMan.UpdateLayoutVars;
  FServerMan.UpdateAdminAppVars;  
end;

procedure TAdminAppDLL.LogMessage(const Msg: string);
begin
  if @FLogMessage <> nil then
    FLogMessage(PChar(Msg));
end;

procedure TAdminAppDLL.ReloadDLL;
begin
  dll := LoadLibrary(PChar(FDLLPath));
  if dll = 0 then
    raise exception.Create('Cannot load SOAP client library.');
  @FInitialize                := GetProcAddress(dll,'Initialize');
  @FStart                     := GetProcAddress(dll,'Start');
  @FStop                      := GetProcAddress(dll,'Stop');
  @FChangeSettingsVariable    := GetProcAddress(dll,'ChangeSettingsVariable');
  @FChangeApplicationVariable := GetProcAddress(dll,'ChangeApplicationVariable');
  @FShutdown                  := GetProcAddress(dll,'Shutdown');
  @FLogMessage                := GetProcAddress(dll,'LogMessage');
  @FGetCommand                := GetProcAddress(dll,'GetCommand');
  @FAlertAppStarted           := GetProcAddress(dll,'AlertAppStarted');
  @FAlertAppTerminated        := GetProcAddress(dll,'AlertAppTerminated');
  @FAlertAppNotResponding     := GetProcAddress(dll,'AlertAppNotResponding');
  @FRssXmlPage                := GetProcAddress(dll,'RssXmlPage');
end;

function TAdminAppDLL.RssXmlPage: WideString;
var
  v : variant;
begin
  v := '';
  if @FRssXmlPage <> nil then
    FRssXMLPage(v);
  Result := v;
end;

procedure TAdminAppDLL.Shutdown;
begin
  if @FShutdown <> nil then
    FShutdown;
end;

procedure TAdminAppDLL.Start;
begin
  if @FStart <> nil then
    FStart;
end;

procedure TAdminAppDLL.Stop;
begin
  if @FStop <> nil then
    FStop;
end;

procedure TAdminAppDLL.UnloadDLL;
begin
  FreeLibrary(dll);
  @FInitialize                := nil;
  @FStart                     := nil;
  @FStop                      := nil;
  @FChangeSettingsVariable    := nil;
  @FChangeApplicationVariable := nil;
  @FShutdown                  := nil;
  @FLogMessage                := nil;
  @FGetCommand                := nil;
  @FAlertAppStarted           := nil;
  @FAlertAppTerminated        := nil;
  @FAlertAppNotResponding     := nil;
  @FRssXmlPage                := nil;
end;

{$IFDEF INTRAWEB70}

{ TLayoutFonts }

procedure TLayoutFonts.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TLayoutFonts(Dest).FGridText.Assign(FGridText);
  TLayoutFonts(Dest).FPageTitle.Assign(FPageTitle);
  TLayoutFonts(Dest).FMenuCaption.Assign(FMenuCaption);
  TLayoutFonts(Dest).FMenuItem.Assign(FMenuItem);
  TLayoutFonts(Dest).FFieldCaption.Assign(FFieldCaption);
  TLayoutFonts(Dest).FButtonText.Assign(FButtonText);
  TLayoutFonts(Dest).FCopyright.Assign(FCopyright);
  TLayoutFonts(Dest).FGridHeader.Assign(FGridHeader);
  TLayoutFonts(Dest).FFieldText.Assign(FFieldText);
  TLayoutFonts(Dest).FVersion.Assign(FVersion);
  TLayoutFonts(Dest).FSectionTitle.Assign(FSectionTitle);
  TLayoutFonts(Dest).FGraphText.Assign(FGraphText);
end;

constructor TLayoutFonts.Create;
begin
  inherited Create;
  FGridText := TIWFont.create;
  FPageTitle := TIWFont.create;
  FMenuCaption := TIWFont.create;
  FMenuItem := TIWFont.create;
  FFieldCaption := TIWFont.create;
  FButtonText := TIWFont.create;
  FCopyright := TIWFont.create;
  FGridHeader := TIWFont.create;
  FFieldText := TIWFont.create;
  FVersion := TIWFont.create;
  FSectionTitle := TIWFont.create;
  FGraphText := TIWFont.create;

  FGridText.FontFamily := 'Arial, Sans-Serif, Verdana';
  FGridText.FontName := 'Arial';
  FGridText.Size := 8;
  FGridText.Color := clWebBlack;
  FGridText.Style := [];
  FPageTitle.FontFamily := 'Arial, Sans-Serif, Verdana';
  FPageTitle.FontName := 'Arial';
  FPageTitle.Size := 12;
  FPageTitle.Color := clWebWhite;
  FPageTitle.Style := [fsBold];
  FMenuCaption.FontFamily := 'Arial, Sans-Serif, Verdana';
  FMenuCaption.FontName := 'Arial';
  FMenuCaption.Size := 10;
  FMenuCaption.Color := clWebWhite;
  FMenuCaption.Style := [fsBold];
  FMenuItem.FontFamily := 'Arial, Sans-Serif, Verdana';
  FMenuItem.FontName := 'Arial';
  FMenuItem.Size := 8;
  FMenuItem.Color := clWebBlack;
  FMenuItem.Style := [fsBold];
  FFieldCaption.FontFamily := 'Arial, Sans-Serif, Verdana';
  FFieldCaption.FontName := 'Arial';
  FFieldCaption.Size := 8;
  FFieldCaption.Color := clWebBlack;
  FFieldCaption.Style := [];
  FButtonText.FontFamily := 'Arial, Sans-Serif, Verdana';
  FButtonText.FontName := 'Arial';
  FButtonText.Size := 8;
  FButtonText.Color := clWebBlack;
  FButtonText.Style := [];
  FCopyright.FontFamily := 'Arial, Sans-Serif, Verdana';
  FCopyright.FontName := 'Arial';
  FCopyright.Size := 8;
  FCopyright.Color := clWebWhite;
  FCopyright.Style := [fsBold];
  FGridHeader.FontFamily := 'Arial, Sans-Serif, Verdana';
  FGridHeader.FontName := 'Arial';
  FGridHeader.Size := 8;
  FGridHeader.Color := clWebBlack;
  FGridHeader.Style := [];
  FFieldText.FontFamily := 'Arial, Sans-Serif, Verdana';
  FFieldText.FontName := 'Arial';
  FFieldText.Size := 8;
  FFieldText.Color := clWebBlack;
  FFieldText.Style := [];
  FVersion.FontFamily := 'Arial, Sans-Serif, Verdana';
  FVersion.FontName := 'Arial';
  FVersion.Size := 8;
  FVersion.Color := clWebWhite;
  FVersion.Style := [fsBold];
  FSectionTitle.FontFamily := 'Arial, Sans-Serif, Verdana';
  FSectionTitle.FontName := 'Arial';
  FSectionTitle.Size := 10;
  FSectionTitle.Color := clWebBlack;
  FSectionTitle.Style := [fsBold];
  FGraphText.FontFamily := 'Arial, Sans-Serif, Verdana';
  FGraphText.FontName := 'Arial';
  FGraphText.Size := 8;
  FGraphText.Color := clWebBlack;
  FGraphText.Style := [fsBold];
end;

destructor TLayoutFonts.destroy;
begin
  FGridText.Free;
  FPageTitle.Free;
  FMenuCaption.Free;
  FMenuItem.Free;
  FFieldCaption.Free;
  FButtonText.Free;
  FCopyright.Free;
  FGridHeader.Free;
  FFieldText.Free;
  FVersion.Free;
  FSectionTitle.Free;
  FGraphText.Free;
  inherited;
end;

procedure TLayoutFonts.SetButtonText(const Value: TIWFont);
begin
  FButtonText.Assign(Value);
end;

procedure TLayoutFonts.SetCopyright(const Value: TIWFont);
begin
  FCopyright.Assign(Value);
end;

procedure TLayoutFonts.SetFieldCaption(const Value: TIWFont);
begin
  FFieldCaption.Assign(Value);
end;

procedure TLayoutFonts.SetFieldText(const Value: TIWFont);
begin
  FFieldText.Assign(Value);
end;

procedure TLayoutFonts.SetGraphText(const Value: TIWFont);
begin
  FGraphText.Assign(Value);
end;

procedure TLayoutFonts.SetGridHeader(const Value: TIWFont);
begin
  FGridHeader.Assign(Value);
end;

procedure TLayoutFonts.SetGridText(const Value: TIWFont);
begin
  FGridText.Assign(Value);
end;

procedure TLayoutFonts.SetMenuCaption(const Value: TIWFont);
begin
  FMenuCaption.Assign(Value);
end;

procedure TLayoutFonts.SetMenuItem(const Value: TIWFont);
begin
  FMenuItem.Assign(Value);
end;

procedure TLayoutFonts.SetPageTitle(const Value: TIWFont);
begin
  FPageTitle.Assign(Value);
end;

procedure TLayoutFonts.SetSectionTitle(const Value: TIWFont);
begin
  FSectionTitle.Assign(Value);
end;

procedure TLayoutFonts.SetVersion(const Value: TIWFont);
begin
  FVersion.Assign(Value);
end;

{ TLayoutProperties }

procedure TLayoutProperties.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TLayoutProperties(Dest).Colors.Assign(Colors);
  TLayoutProperties(Dest).Fonts.Assign(Fonts);
end;

constructor TLayoutProperties.Create;
begin
  FColors := TLayoutColors.Create;
  FFonts := TLayoutFonts.Create;
end;

destructor TLayoutProperties.Destroy;
begin
  FColors.Free;
  FFonts.Free;
  inherited;
end;

procedure TLayoutProperties.SetColors(const Value: TLayoutColors);
begin
  FColors.Assign(Value);
end;

procedure TLayoutProperties.SetFonts(const Value: TLayoutFonts);
begin
  FFonts.Assign(Value);
end;

{ TLayoutColors }

procedure TLayoutColors.AssignTo(Dest: TPersistent);
begin
  if not (Dest is Self.ClassType) then
    raise Exception.Create('You cannot assign a '+Dest.Classname+' to a '+Self.Classname+'.');
  TLayoutColors(Dest).FFooter     := FFooter;
  TLayoutColors(Dest).FMenus      := FMenus;
  TLayoutColors(Dest).FGraphs     := FGraphs;
  TLayoutColors(Dest).FGridHeader := FGridHeader;
  TLayoutColors(Dest).FGrids      := FGrids;
  TLayoutColors(Dest).FBody       := FBody;
  TLayoutColors(Dest).FBorders    := FBorders;
  TLayoutColors(Dest).FHeader     := FHeader;
  TLayoutColors(Dest).FButtons    := FButtons;
  TLayoutColors(Dest).FFields     := FFields;
end;

constructor TLayoutColors.Create;
begin
  inherited Create;
  FFooter     := $00BF532F;
  FMenus      := $00BF532F;
  FGraphs     := $00EBB99D;
  FGridHeader := $00BF532F;
  FGrids      := clWebDIMGRAY;
  FBody       := clWebWhite;
  FBorders    := clWebBlack;
  FHeader     := $00BF532F;
  FButtons    := clWebLIGHTGRAY;
  FFields     := clWebWhite;
end;

destructor TLayoutColors.Destroy;
begin

  inherited;
end;

{$ENDIF}

{ EServerManCrash }

constructor EServerManCrash.Create;
begin
  inherited Create('Server Manager was unavailable and has been restarted.'); 
end;

initialization
  SOAPErrorCnt := 0;
  ServerManagerComponentCount := 0;
  _StartedRunning := Now;

  DefaultAppFile := ExtractFilename(DLLFileName)+'.Admin\IWServerManager.exe';
  DefaultRestartEXE := ExtractFilename(DLLFileName)+'.Admin\Restart.exe';
  SetLogApplication('Server Manager');
end.
