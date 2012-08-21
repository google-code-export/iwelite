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

unit ArcIWTemplateProcessor7;

interface

{$I IntraWebVersion.inc}

{$IFNDEF INTRAWEB72}
  ERROR: This unit is designed to work only with IW 7.2 and above.
{$ENDIF}

uses
  Classes,
  Windows, IWContainerLayout, IWRenderContext, IWTypes, IWUtils,
  IWBaseInterfaces, IWBaseRenderContext, IWBaseHTMLInterfaces, IWMarkupLanguageTag,
  IWHTML40Interfaces, IWTemplateProcessorHTML, ArcIWControlBase,
  IWHTMLTag, Graphics, IWControl, IWScriptEvents, ArcIWControlCommon,
  IWRegion, Controls, Forms, IWBaseControl, IWColor,
  {$IFNDEF INTRAWEB110}
  IWAppForm32, IWCompLabel32, IWTemplateProcessorHTML32, IWControl32, IWCompEdit32,
  IWCompButton32, IWHTMLControls32, IWCompListBox32, IWExtCtrls32, IWGrids32,
  IWCompMemo32, IWCompText32, IWCompRectangle32, IWCompCheckBox32, IWDBStdCtrls32,
  IWDBExtCtrls32, IWLayoutMgrHTML32, IWProducer32, IWPageForm32, IWCompDynamicChart,
  IWCompDynamicChartLegend, IWClientSideDataset, IWClientSideDatasetBase, IWProducer,
  IWModuleController, IWClientSideDatasetDBLink, IWPageForm, IWDynGrid, IWCSStdCtrls,
  {$ENDIF}
  IWCompProgressBar, IWCompActiveX, IWCompCalendar, IWImageList, IWBaseForm,
  IWCompCheckbox, IWCompEdit, IWCompLabel, IWCompListbox, IWCompMemo, IWCompText,
  IWGlobal, IWCompButton, IWCompRectangle, IWCompFlash, IWKlooch,
  IWDBExtCtrls, IWDBStdCtrls, IWDBGrids, IWServerControllerBase,
  IWContainer, IWHTMLControls, IWAppForm, IWFileReference,
  IWForm, IWCompMenu, IWStandAloneServer, IWLayoutMgrHTML, ArcPersistentStream
  {$IFDEF INTRAWEB120}
  , IWRenderStream, IWCompExtCtrls, IWCompGrids, IWCompTreeview, IWCompFile, IW.Http.Request
  {$ELSE}
  , IWStreams, IWExtCtrls, IWGrids, IWTreeview {$ENDIF}
  , ArcCommon;

type
  TTagType = (ttNone, ttHead, ttBody, ttEndBody, ttTitle, ttIW, ttIW2, ttSSI, ttScript);

  TEchoVars = (evUnknown, evDocumentName, evDocumentURI, evQueryStringUnescaped,
               evDateLocal, evDateGMT, evLastModified, evServerSoftware, evServerName,
               evGatewayInterface, evServerProtocol, evServerPort, evRequestMethod,
               evPathInfo, evPathTranslated, evScriptName, evQueryString, evRemoteHost,
               evRemoteAddr, evAuthType, evRemoteUser, evRemoteIdent, evContentType,
               evContentLength, evHTTPAccept, evHTTPUserAgent);

  TIWCustomControlHack = class(TIWCustomControl);
  TIWFormHack = class(TIWForm);
const
  SSIEchoVars = '                        ' +
                'DOCUMENT_NAME           ' +
                'DOCUMENT_URI            ' +
                'QUERY_STRING_UNESCAPED  ' +
                'DATE_LOCAL              ' +
                'DATE_GMT                ' +
                'LAST_MODIFIED           ' +
                'SERVER_SOFTWARE         ' +
                'SERVER_NAME             ' +
                'GATEWAY_INTERFACE       ' +
                'SERVER_PROTOCOL         ' +
                'SERVER_PORT             ' +
                'REQUEST_METHOD          ' +
                'PATH_INFO               ' +
                'PATH_TRANSLATED         ' +
                'SCRIPT_NAME             ' +
                'QUERY_STRING            ' +
                'REMOTE_HOST             ' +
                'REMOTE_ADDR             ' +
                'AUTH_TYPE               ' +
                'REMOTE_USER             ' +
                'REMOTE_IDENT            ' +
                'CONTENT_TYPE            ' +
                'CONTENT_LENGTH          ' +
                'HTTP_ACCEPT             ' +
                'HTTP_USER_AGENT         ';
  SSIEchoVarsLen = length(SSIEchoVars);

type
  TSSIOptions = class(TPersistent)
  private
    FAllowInclude: boolean;
    FAllowExec: boolean;
    FEnabled: boolean;
  published
    property AllowInclude : boolean read FAllowInclude write FAllowInclude;
    property AllowExec : boolean read FAllowExec write FAllowExec;
    property Enabled : boolean read FEnabled write FEnabled;
  end;

  TArcIWNewControlEvent = procedure(Sender : TObject; Component : TComponent; PageContext : TIWBasePageContext) of object;
  TArcIWBeforeRenderTagEvent = procedure(Sender : TObject; TagName : string; Params : TStringList; var DoRender : boolean) of object;
  TArcIWAfterRenderTagEvent = procedure(Sender : TObject; TagName : string; Params : TStringList) of object;
  TArcIWOnFullUnknownTagEvent = procedure(const AName: string; Params: TStrings; var VValue: string) of object;
  TArcIWTemplateProcessor = class(TIWTemplateProcessorHTML)
  private
    FOnNewControl: TArcIWNewControlEvent;
  protected
    FUseTwoPassRender: boolean;
    FCacheTemplate: boolean;
    FSSIErrorMsg : string;
    FSSITimeFormat : string;
    FSSISizeFormat : string;
    FSSIOptions: TSSIOptions;
    FOnAfterRenderTag: TArcIWAfterRenderTagEvent;
    FOnBeforeRenderTag: TArcIWBeforeRenderTagEvent;
    AlreadyProcessedTemplate : boolean;  
    //
    FUseMasterTemplate: boolean;
    FMasterTemplate: string;
    FCacheLimit: Integer;
    FCacheUpdated: TDateTime;
    FOnFullUnknownTag: TArcIWOnFullUnknownTagEvent;
    FCachedTemplate: TMemoryStream;
    function GetMasterTemplate: string;
    function DoFullUnknownTag(const AName: string; Params: TStrings): string; virtual;
    //
    procedure SetSSIOptions(const Value: TSSIOptions);
    function ProcessSSI(AContainerContext: TIWContainerContext;
      APageContext: TIWBasePageContext; TokenStr : string; Params : TStringList): string; virtual;
    function ProcessIW(AContainerContext: TIWContainerContext;
      APageContext: TIWBasePageContext; aCtrlName : string; Params : TStringList): string; virtual;
    function DoBeforeRenderTag(TagName : string; Params : TStringList) : boolean; virtual;
    procedure DoAfterRenderTag(TagName : string; Params : TStringList); virtual;
    procedure _Process(aTarget, aSource : TStream; aContainerContext : TIWContainerContext; aPageContext : TIWBaseHTMLPageContext; Pass : integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ProcessControl(AContainerContext: TIWContainerContext;
      APageContext: TIWBaseHTMLPageContext; AControl: IIWBaseHTMLComponent); override;
    procedure Process(AStream : TIWRenderStream;
      AContainerContext: TIWContainerContext; APageContext: TIWBasePageContext); override;
  published
    property SSIOptions : TSSIOptions read FSSIOptions write SetSSIOptions;
    property OnBeforeRenderTag : TArcIWBeforeRenderTagEvent read FOnBeforeRenderTag write FOnBeforeRenderTag;
    property OnAfterRenderTag : TArcIWAfterRenderTagEvent read FOnAfterRenderTag write FOnAfterRenderTag;
    property OnNewControl : TArcIWNewControlEvent read FOnNewControl write FOnNewControl;
    property CacheTemplate : boolean read FCacheTemplate write FCacheTemplate;
    property UseTwoPassRender : boolean read FUseTwoPassRender write FUseTwoPassRender;
    //
    property MasterTemplate: string read FMasterTemplate write FMasterTemplate;
    property UseMasterTemplate: boolean read FUseMasterTemplate write FUseMasterTemplate;
    property OnFullUnknownTag: TArcIWOnFullUnknownTagEvent read FOnFullUnknownTag write FOnFullUnknownTag;
    property CacheLimit: Integer read FCacheLimit write FCacheLimit;
  end;

implementation

Uses
  SysUtils,
  {$IFDEF VCL6PRABOVE}
  Variants,
  {$ENDIF}
  InCoderMIME, IWApplication, HTTPApp, IWTemplateProcessing,
  {$IFDEF INTRAWEB110} IWStrings, IWSystem, {$ELSE} SWStrings, SWSystem, {$ENDIF}
  {$IFNDEF VER130}DateUtils,{$ENDIF} IWBaseContainerLayout, TypInfo,
  InCoder, StrUtils, uSMCommon;

function MimeDecodeString(str : string) : string;
begin
  Result := TIdDecoderMIME.DecodeString(str);
end;

function DecodeUsername(AuthStr : string) : string;
var
  DecodedString : string;
begin
  AuthStr := Copy(AuthStr, 7, Length(AuthStr));
  DecodedString := MimeDecodeString(AuthStr);
  Result := Copy(DecodedString,1,Pos(':', DecodedString) -1);
end;

function NowGMT : TDateTime;
var
  tz : TTimeZoneInformation;
begin
  case GetTimeZoneInformation(tz) of
    TIME_ZONE_ID_STANDARD: Result := IncMinute(Now, tz.Bias);
    TIME_ZONE_ID_DAYLIGHT: Result := IncMinute(Now, tz.DaylightBias);
    else                   Result := Now;
  end;
end;

procedure _ExtractHeaderFields(Separators, WhiteSpace: TSysCharSet; Content: PChar;
  Strings: TStrings; Decode: Boolean; StripQuotes: Boolean = False);
var
  Head, Tail: PChar;
  EOS, InQuote, LeadQuote: Boolean;
  QuoteChar: Char;
  ExtractedField: string;
  WhiteSpaceWithCRLF: TSysCharSet;
  SeparatorsWithCRLF: TSysCharSet;

  function DoStripQuotes(const S: string): string;
  var
    I: Integer;
    InStripQuote: Boolean;
    StripQuoteChar: Char;
  begin
    Result := S;
    InStripQuote := False;
    StripQuoteChar := #0;
    if StripQuotes then
      for I := Length(Result) downto 1 do
        if CharInSet(Result[I],['''', '"']) then
          if InStripQuote and (StripQuoteChar = Result[I]) then
          begin
          Delete(Result, I, 1);
            InStripQuote := False;
          end
          else if not InStripQuote then
          begin
            StripQuoteChar := Result[I];
            InStripQuote := True;
            Delete(Result, I, 1);
          end
  end;

begin
  if (Content = nil) or (Content^ = #0) then Exit;
  WhiteSpaceWithCRLF := WhiteSpace + [#13, #10];
  SeparatorsWithCRLF := Separators + [#0, #13, #10, '"'];
  Tail := Content;
  QuoteChar := #0;
  repeat
    while CharInSet(Tail^,WhiteSpaceWithCRLF) do Inc(Tail);
    Head := Tail;
    InQuote := False;
    LeadQuote := False;
    while True do
    begin
     while (InQuote and not CharInSet(Tail^,[#0, '"'])) or
        not CharInSet(Tail^,SeparatorsWithCRLF) do Inc(Tail);
      if Tail^ = '"' then
      begin
        if (QuoteChar <> #0) and (QuoteChar = Tail^) then
          QuoteChar := #0
        else
        begin
          LeadQuote := Head = Tail;
          QuoteChar := Tail^;
          if LeadQuote then Inc(Head);
        end;
        InQuote := QuoteChar <> #0;
        if InQuote then
          Inc(Tail)
        else Break;
      end else Break;
    end;
    if not LeadQuote and (Tail^ <> #0) and (Tail^ = '"') then
      Inc(Tail);
    EOS := Tail^ = #0;
    if Head^ <> #0 then
    begin
      SetString(ExtractedField, Head, Tail-Head);
      if Decode then
        // JS: W1058 Implicit string cast with potential data loss from 'string' to 'AnsiString'
        Strings.Add(HTTPDecode(DoStripQuotes(ExtractedField)))
      else Strings.Add(DoStripQuotes(ExtractedField));
    end;
    Inc(Tail);
  until EOS;
end;

{ TArcIWTemplateProcessor }

constructor TArcIWTemplateProcessor.Create(AOwner: TComponent);
begin
  if (not (AOwner is TIWForm)) and (not (AOwner is TFrame)) then
    raise Exception.Create('TArcIWTemplateProcessor requires an IntraWeb Form as an owner');

  inherited;

  FSSIOptions := TSSIOptions.Create;
  FSSIOptions.AllowInclude := True;
  FSSIOptions.AllowExec := False;
  FSSIOptions.Enabled := True;
  FCacheTemplate := True;

  if AOwner is TIWForm then
  begin
    if (csDesigning in ComponentState) then
    begin
      if (TIWForm(AOwner).LayoutMgr = nil) then
        TIWForm(AOwner).LayoutMgr := Self;
    end;
  end;
  AlreadyProcessedTemplate := False;
  FUseMasterTemplate := True;
  FCacheLimit := 0;
  FCachedTemplate := TMemoryStream.Create
end;

destructor TArcIWTemplateProcessor.Destroy;
begin
  FSSIOptions.Free;
  FreeAndNil(FCachedTemplate);
  inherited;
end;

function TArcIWTemplateProcessor.DoFullUnknownTag(
  const AName: string; Params: TStrings): string;
begin
  Result := '';
  if Assigned(FOnFullUnknownTag) then
    OnFullUnknownTag(AName, Params, Result)
  else if Assigned(OnUnknownTag) then    //so old apps aren't broken
    OnUnknownTag(AName, Result);
end;

function TArcIWTemplateProcessor.GetMasterTemplate: string;
begin
  result := '';
  if HTML40FormInterface(Container.InterfaceInstance)<>nil then begin
     if not FUseMasterTemplate then
        exit;
     result := FMasterTemplate;
     if (result <> '') then begin
       result := GServerController.TemplateDir + result;
       if not FileExists(result) then
          result := '';
     end;

     if result = '' then begin
        result := GGetWebApplicationThreadVar.ActiveMasterTemplate;
        if result <> '' then begin
           result := GServerController.TemplateDir + result;
        if not FileExists(result) then
           result := '';
        end;
     end;
  end;
end;

procedure TArcIWTemplateProcessor._Process(aTarget, aSource: TStream; aContainerContext: TIWContainerContext; aPageContext: TIWBaseHTMLPageContext; Pass : integer);
var
  bHeadWritten : boolean;
  function HandleIWTag(var Tag : string; TagLen : integer) : Integer;
  var
    slParams : TStringList;
    s, sCtrlName : string;
    i, iColonPos, iSpacePos : integer;
  begin
    slParams := TStringList.Create;
    try
      iSpacePos := PosEx(' ',Tag,3);
      if iSpacePos > 0 then
      begin
        sCtrlName := Copy(Tag,3,iSpacePos-3);
        iColonPos := Pos(':',Copy(Tag,iSpacePos,5));
        if iColonPos > 0 then
        begin
          s := TrimLeft(Copy(Tag,iColonPos+iSpacePos,high(Integer)));
          i := Pos(' ',s);
          if i > 0 then
            s := Copy(s,1,i-1);

          sCtrlName := sCtrlName+':'+s;
          inc(iSpacePos, iColonPos);
        end;
        _ExtractHeaderFields( [' '], [' '], PChar(Copy(Tag,iSpacePos,TagLen-iSpacePos-1)),slParams, False, True);
      end else
        sCtrlName := Copy(Tag,3,TagLen-4);
      if DoBeforeRenderTag(sCtrlName, slParams) then
      begin
        Tag := ProcessIW(AContainerContext,APageContext, sCtrlName, slParams);
      end;
      DoAfterRenderTag(sCtrlName, slParams);
    finally
      slParams.Free;
    end;
    result := length(Tag);
  end;

  function HandleSSITag(var Tag : string; TagLen : integer) : Integer;
  var
    slParams : TStringList;
    sCtrlName : string;
    iSpacePos : integer;
  begin
    slParams := TStringList.Create;
    try
      iSpacePos := PosEx(' ',Tag,3);
      sCtrlName := Copy(Tag,6,iSpacePos-6);
      _ExtractHeaderFields( [' '], [' '], PChar(Copy(Tag,iSpacePos,TagLen-iSpacePos-1)),slParams, False, True);
      if DoBeforeRenderTag(sCtrlName, slParams) then
      begin
        Tag := ProcessSSI(AContainerContext,APageContext, sCtrlName, slParams);
      end;
      DoAfterRenderTag(sCtrlName, slParams);
    finally
      slParams.Free;
    end;
    result := length(Tag);
  end;

  function HandleScriptTag(var Tag : string; TagLen : integer) : Integer;
  begin
    result := TagLen;
  end;



  procedure PostRender(var ACode:string; var ALen: integer);

      function Contains(AString, AContains:string):boolean;
      begin
        Result := FastPosNoCase(AString, AContains,1)>0;
      end;
      
  var
    rs : TIWRenderStream;
  begin
    rs := TIWRenderStream.Create;
    try
      if Contains(ACode,'{/STYLE\}') then begin
        if TIWPageContext40(APageContext).StyleTag.Contents.Count > 0 then
          TIWPageContext40(APageContext).StyleTag.Render(rs);
        ACode := StringReplace(ACode,'{/STYLE\}',{$IFDEF INTRAWEB122} rs.AsString {$ELSE} rs.Extract {$ENDIF},[rfReplaceAll]);
      end;
      if Contains(ACode,'{/SCRIPT\}') then begin
        ACode := StringReplace(ACode,'{/SCRIPT\}',ScriptSection(TIWPageContext40(APageContext)),[rfReplaceAll]);
      end;
      if Contains(ACode,'{/FORM\}') then begin
        APageContext.FormTag.Render(rs);
        ACode := StringReplace(ACode,'{/FORM\}',{$IFDEF INTRAWEB122} rs.AsString {$ELSE} rs.Extract {$ENDIF},[rfReplaceAll]);
      end;
      ALen := Length(ACode);
    finally
      rs.Free;
    end;
  end;

  function HandleHead(var Tag : string; TagLen : integer) : Integer;
    function AdditionalHeaders : string;
    begin
      Result := '<meta http-equiv="cache-control" content="no-cache">'+EOL+
                '<meta http-equiv="pragma" content="no-cache">';
    end;
  var
    sTmp : string;
  begin
    sTmp := EOL + AdditionalHeaders + EOL + '{/SCRIPT\}' + HeadContent + EOL + '{/STYLE\}' + EOL;
    Tag := Copy(Tag,1,6)+sTmp+Copy(Tag,7,High(Integer));
    result := length(sTmp)+6; // sTmp instead of Tag: Because we want to process any headers the template had included.
    bHeadWritten := True;
  end;

  function HandleBody(var Tag : string; TagLen : integer) : Integer;
    function DropQuotes(const str : string) : string;
    var
      iLen : integer;
    begin
      Result := str;
      iLen := Length(str);
      if (iLen > 2) then
      begin
        if (Result[1] = '"') and (Result[iLen] = '"') then
          Result := Copy(Result,2,iLen-2);
      end;
    end;
  var
    slParams : TStringList;
    i : integer;
    sAttributes : string;
    sVal, sAttrName : string;
    rs : TIWRenderStream;
    tagTmp : TIWHTMLTag;
  begin
    slParams := TStringList.Create;
    try
      sAttributes := Copy(Tag,6,High(Integer));
      sAttributes := Copy(sAttributes,1,Pos('>',sAttributes)-1);
      _ExtractHeaderFields([' '],[' '],PChar(sAttributes),slParams,False,False);
      for i := 0 to slParams.Count-1 do
      begin
        sAttrName := Uppercase(slParams.Names[i]);
        sVal := DropQuotes({$IFNDEF VER150}slParams.Values[slParams.Names[i]]{$ELSE}slParams.ValueFromIndex[i]{$ENDIF});
        if sAttrName = 'BGCOLOR' then
          APageContext.BodyTag.AddStringParam('BGCOLOR', sVal)
        else if sAttrName = 'ONLOAD' then
          TIWPageContext40(APageContext).AddToInitProc(sVal + ';')
        else if sAttrName = 'ONBLUR' then
          APageContext.BodyTag.Params.Values[sAttrName] := APageContext.BodyTag.Params.Values[sAttrName] + sVal + ';'
        else
          APageContext.BodyTag.Params.Values[sAttrName] := sVal;
      end;

      APageContext.BodyTag.ClosingTag := cbFalse;

      rs := TIWRenderStream.Create;
      try
        APageContext.BodyTag.Render(rs);
        APageContext.FormTag.ClosingTag := cbTrue;
        {$IFDEF INTRAWEB122}
        rs.WriteText('{/FORM\}');
        {$ELSE}
        rs.WriteString('{/FORM\}');
        {$ENDIF}
        if MasterFormTag then
        begin
          tagTmp := TIWHTMLTag.CreateHTMLTag('form',cbFalse);
          try
            tagTmp.AddStringParam('onSubmit','return FormDefaultSubmit();');
            tagTmp.AddStringParam('style','display:inline;');
            tagTmp.Render(rs);
          finally
            tagTmp.Free;
          end;
        end;
        if not bHeadWritten then
        begin
          Tag := '<HEAD></HEAD>';
          HandleHead(Tag, 13);
          Tag := Tag+{$IFDEF INTRAWEB122} rs.AsString {$ELSE} rs.Extract {$ENDIF};
        end else
          Tag := {$IFDEF INTRAWEB122} rs.AsString {$ELSE} rs.Extract {$ENDIF};
      finally
        rs.Free;
      end;
    finally
      slParams.Free;
    end;
    result := length(Tag);
  end;
  function HandleEndBody(var Tag : string; TagLen : integer) : Integer;
  begin
    if MasterFormTag then
      Tag := '</form>'+Tag;
    result := length(Tag);
  end;
  function HandleTitle(var Tag : string; TagLen : integer) : Integer;
  begin
    if APageContext.Title <> '' then
      Tag := '<TITLE>' + APageContext.Title + '</TITLE>' + EOL;
    result := length(Tag);
  end;
  function ProcessTag(var str : string; var Len : integer; Pos : Integer; TagType : TTagType) : Integer;
  var
    iCutPos, iTagLen, iEndTagLen : integer;
    sTag : string;
    sEndTag : string;
  begin
    iTagLen := 0;
    iEndTagLen := 0;
    case TagType of
      ttHead:
        begin
          iTagLen := 5;
          sEndTag := '</HEAD>';
          iEndTagLen := 7;
        end;
      ttBody:
        begin
          iTagLen := 5;
          sEndTag := '>';
          iEndTagLen := 1;
        end;
      ttEndBody:
        begin
          iTagLen := 6;
          sEndTag := '>';
          iEndTagLen := 1;
        end;
      ttTitle:
        begin
          iTagLen := 6;
          sEndTag := '</TITLE>';
          iEndTagLen := 8;
        end;
      ttIW :
        begin
          iTagLen := 2;
          sEndTag := '%}';
          iEndTagLen := 2;
        end;
      ttIW2 :
        begin
          iTagLen := 2;
          sEndTag := '>';
          iEndTagLen := 1;
        end;
      ttSSI:
        begin
          iTagLen := 5;
          sEndTag := '-->';
          iEndTagLen := 3;
        end;
      ttScript:
        begin
          iTagLen := 2;
          sEndTag := '</SCRIPT>';
          iEndTagLen := 9;
        end;
    end;

    iCutPos := FastPosNoCase(str,sEndTag,Pos+iTagLen);

    if iCutPos = 0 then
    begin
      Result := 1;
      exit;
    end;

    sTag := Copy(str,Pos,iCutPos-Pos+iEndTagLen);
    iTagLen := iCutPos-Pos+iEndTagLen;
    Result := 0;
    case TagType of
      ttHead:   result := HandleHead(sTag, iTagLen);
      ttBody:   result := HandleBody(sTag, iTagLen);
      ttEndBody:result := HandleEndBody(sTag, iTagLen);
      ttTitle:  result := HandleTitle(sTag, iTagLen);
      ttIW :    result := HandleIWTag(sTag, iTagLen);
      ttIW2:    result := HandleIWTag(sTag, iTagLen);
      ttSSI:    result := HandleSSITag(sTag, iTagLen);
      ttScript: result := HandleScriptTag(sTag, iTagLen);
    end;
    str := Copy(str,1,Pos-1)+sTag+Copy(str,iCutPos+iEndTagLen,High(Integer));
    Len := Length(str);//Len+Result+iTagLen-iEndTagLen+1; should figure it out via calcuation.  Will be much faster.
  end;
var
  sTmp, sTest : string;
  i, iLen, iIncLen : integer;
  tt : TTagType;
begin
  if (not UseTwoPassRender) and (Pass = 2) then
    exit;
  if (Pass = 2) and Assigned(aPageContext.WebApplication.ActiveForm) then
  begin
    TIWFormHack(aPageContext.WebApplication.ActiveForm).ClearRealTabOrders;
    TIWFormHack(aPageContext.WebApplication.ActiveForm).FillRealTabOrders;
  end;

  bHeadWritten := False;

  iLen := ASource.Size;
  SetLength(sTmp,iLen);
  ASource.Position := 0;
  ASource.ReadBuffer(sTmp[1],iLen);
  i := 1;
  while i < iLen do
  begin
    iIncLen := 1;
    sTest := Uppercase(Copy(PChar(@sTmp[i]),1,7));
    tt := ttNone;
    if (TagType = ttIntraWeb) and (Copy(sTest,1,2) = '{%') then
      tt := ttIW
    else if (TagType = ttBorland) and (Copy(sTest,1,2) = '<#') then
      tt := ttIW2
    else if (SSIOptions.Enabled) and (Copy(sTest,1,5) = '<!--#') then
      tt := ttSSI
    else if Copy(sTest,1,5) = '<HEAD' then
      tt := ttHead
    else if Copy(sTest,1,5) = '<BODY' then
      tt := ttBody
    else if Copy(sTest,1,6) = '</BODY' then
      tt := ttEndBody
    else if sTest = '<SCRIPT' then
      tt := ttScript
    else if Copy(sTest,1,6) = '<TITLE' then
      tt := ttTitle;

    if tt <> ttNone then
      iIncLen := ProcessTag(sTmp,iLen, i, tt);

    //Assert(iLen = length(sTmp),'iLen is out of synch: Pos:'+IntToStr(i));

    inc(i, iIncLen);
  end;
  if (not UseTwoPassRender) or (Pass = 2) then begin
    PostRender(sTmp,iLen);
    ATarget.WriteBuffer(sTmp[1],iLen);
  end;
end;

procedure TArcIWTemplateProcessor.Process(AStream: TIWRenderStream;
  AContainerContext: TIWContainerContext; APageContext: TIWBasePageContext);
var
  fsTemplate : TStream;
  LMasterSrc: TStream;
  masterFileName: TFileName;
begin
  if FCacheTemplate and (FCachedTemplate.Size > 0) and
     ((FCacheLimit = 0) or
      (MinutesBetween(Now, FCacheUpdated) < FCacheLimit))
  then
  begin
    fsTemplate := FCachedTemplate;
    if Assigned(FOnBeforeProcess) then
      OnBeforeProcess(fsTemplate);

    _Process(AStream{$IFDEF INTRAWEB122}.Stream{$ENDIF}, fsTemplate, AContainerContext, TIWBaseHTMLPageContext(APageContext),1);
    _Process(AStream{$IFDEF INTRAWEB122}.Stream{$ENDIF}, fsTemplate, AContainerContext, TIWBaseHTMLPageContext(APageContext),2);

  end else
  begin
    AlreadyProcessedTemplate := false;
    masterFileName := GetMasterTemplate; //needs a $body tag
                                         //page templates need to be std html <html>...</html>

    if FileExists(TemplatePathname) then
    begin
      fsTemplate := TMemoryStream.Create;
      TMemoryStream(fsTemplate).LoadFromFile(TemplatePathname);
      try
        if Assigned(FOnBeforeProcess) then begin
          OnBeforeProcess(fsTemplate);
        end;

        if masterFileName <> '' then
        begin
          LMasterSrc := TMemoryStream.Create;
          TMemoryStream(LMasterSrc).LoadFromFile(MasterFileName);
          try
            MergeTemplates(fsTemplate, LMasterSrc);
          finally
            FreeAndNil(LMasterSrc);
          end;
        end;

        _Process(AStream{$IFDEF INTRAWEB122}.Stream{$ENDIF}, fsTemplate, AContainerContext, TIWBaseHTMLPageContext(APageContext),1);
        _Process(AStream{$IFDEF INTRAWEB122}.Stream{$ENDIF}, fsTemplate, AContainerContext, TIWBaseHTMLPageContext(APageContext),2);
      finally
        FCachedTemplate.Clear;
        if FCacheTemplate then
        begin
          fsTemplate.Seek(0, soFromBeginning);
          FCachedTemplate.CopyFrom(fsTemplate, fsTemplate.Size);
          AlreadyProcessedTemplate := true;
        end;
        FreeAndNil(fsTemplate);
      end;
    end else
    begin
      fsTemplate := nil;
      if Assigned(FOnBeforeProcess) then
        OnBeforeProcess(fsTemplate);

      if Assigned(fsTemplate) then
      begin
        try
          _Process(AStream{$IFDEF INTRAWEB122}.Stream{$ENDIF}, fsTemplate, AContainerContext, TIWBaseHTMLPageContext(APageCOntext),1);
          _Process(AStream{$IFDEF INTRAWEB122}.Stream{$ENDIF}, fsTemplate, AContainerContext, TIWBaseHTMLPageContext(APageCOntext),2);
        finally
        	FCachedTemplate.Clear;
        	if FCacheTemplate then begin
          	fsTemplate.Seek(0, soFromBeginning);
          	FCachedTemplate.CopyFrom(fsTemplate, fsTemplate.Size);
            AlreadyProcessedTemplate := true;
        	end;
        end;
      end;
    end;
  end;

  if Assigned(FOnAfterProcess) then
    OnAfterProcess(fsTemplate);
end;

procedure TArcIWTemplateProcessor.DoAfterRenderTag(TagName: string;
  Params: TStringList);
begin
  if Assigned(FOnAfterRenderTag) then
    FOnAfterRenderTag(Self, TagName, Params);
end;

function TArcIWTemplateProcessor.DoBeforeRenderTag(TagName: string;
  Params: TStringList): boolean;
begin
  Result := True;
  if Assigned(FOnBeforeRenderTag) then
    FOnBeforeRenderTag(Self, TagName, Params, Result);
end;

procedure TArcIWTemplateProcessor.ProcessControl(AContainerContext: TIWContainerContext;
  APageContext: TIWBaseHTMLPageContext; AControl: IIWBaseHTMLComponent);
Var
  tagHTML: TIWMarkupLanguageTag;
  ctrli : IIWBaseHTMLControl;
begin
  with AControl do begin
    tagHTML :=
        AContainerContext.MarkupLanguageTag[AControl.HTMLName];
    if Assigned(tagHTML) then
    begin
      if AContainerContext.Components[AControl.ComponentName] is TIWCustomControl then
        tagHTML.AddStringParam('CLASS', TIWCustomControl(AContainerContext.Components[AControl.ComponentName]).RenderCSSClass(nil));
      // relative positioning is not used from IW controls
      // AContainerContext.ComponentContext[AControl.HTMLName]
      if AControl.QueryInterface(IIWBaseHTMLControl,ctrli) = 0 then
        tagHTML.AddStringParam('STYLE', RenderStyle(ctrli) + tagHTML.Params.Values['STYLE']);
    end;
  end;
  inherited ProcessControl(AContainerContext, APageContext, AControl);
end;

function TArcIWTemplateProcessor.ProcessIW(
  AContainerContext: TIWContainerContext; APageContext: TIWBasePageContext;
  aCtrlName: string; Params: TStringList): string;
  procedure SetProperty(Control : TComponent; Param : string);
    function FindMethod(Control : TObject; name : string) : TMethod;
    var
      iPos : integer;
      obj : TComponent;
    begin
      Result.Code := nil;
      Result.Data := nil;

      iPos := CharPos('.',Name);
      if iPos > 0 then
      begin
        repeat
          obj := Owner.FindComponent(Copy(Name,1,iPos-1));
          Name := Copy(Name,iPos+1,9999999);
          iPos := CharPos('.',Name);
        until iPos = 0;
      end else
        obj := Owner;
      if Assigned(obj) then
      begin
        Result.Data := obj; //Pointer(Control);
        Result.Code := obj.MethodAddress(Name);
      end;
    end;

    function FindObject(name:string):TObject;
    var
      iPos : integer;
      obj : TObject;
      currpart:string;
    begin
      Result := nil;
      obj := Owner;
      while (Name<>'') and (obj<>nil) do begin
        iPos := CharPos('.',Name);
        if iPos=0 then begin
          currpart := Name;
          Name := '';
        end else begin
          currpart := Copy(Name,1,iPos-1);
          Name := Copy(Name,iPos+1,9999999);
        end;
        

        if lowercase(currpart)='usersession' then begin
          obj := GGetWebApplicationThreadVar.Data;
        end else if lowercase(currpart)='webapplication' then begin
          obj := GGetWebApplicationThreadVar;
        end else if lowercase(currpart)='servercontroller' then begin
          obj := GServerController;
        end else if IsPublishedProp(obj,currpart) then begin
          obj := GetObjectProp(obj,currpart);
        end else if obj is TComponent then begin
          obj := TComponent(obj).FindComponent(currpart);
        end;
      end;
      Result := obj;
    end;

  var
    o, obj : TObject;
    iPos, iPos2 : integer;
    i64 : Int64;
    sProp : string;
    sObj, sIdx : string;
    pi : PPropInfo;
    Val : variant;
  begin
    iPos := CharPos('=',Param);
    sProp := Copy(Param,1,iPos-1);
    Val := Copy(Param,iPos+1,9999999999999);

    obj := Control;

    iPos := CharPos('.',sProp);

    if iPos = 0 then
    begin
      iPos2 := Pos('[',sProp);
      if iPos2 > 0 then
      begin
        sObj := Copy(sProp,1,iPos2-1);
        sIdx := Copy(sProp,iPos2+1,High(Integer));
        iPos2 := Pos(']',sIdx);
        sIdx := Copy(sIdx,1,iPos2-1);

        obj := TComponent(GetObjectProp(obj,sObj));

        if obj is TIWScriptEvents then
        begin
          TIWScriptEvents(obj).Values[sIdx] := Val;
          exit;
        end else
          obj := TCollection(obj).Items[StrToInt(sIdx)];
      end;
    end else
    begin
      while iPos > 0 do
      begin
        iPos2 := Pos('[',sProp);
        if iPos2 <= 0 then
        begin
          obj := TComponent(GetObjectProp(obj,Copy(sProp,1,iPos-1)));
          sProp := Copy(sProp,iPos+1,9999999);
          iPos := CharPos('.',sProp);
        end else
        begin
          sObj := Copy(sProp,1,iPos2-1);
          sIdx := Copy(sProp,iPos2+1,High(Integer));
          iPos2 := Pos(']',sIdx);
          sIdx := Copy(sIdx,1,iPos2-1);

          obj := TComponent(GetObjectProp(obj,sObj));

          if obj is TIWScriptEvents then
          begin
            TIWScriptEvents(obj).Values[sIdx] := Val;
            exit;
          end else
            obj := TCollection(obj).Items[StrToInt(sIdx)];

          sProp := Copy(sProp,iPos+1,9999999);
          iPos := CharPos('.',sProp);
        end;
      end;
    end;

    if TObject(obj) is TStrings then
    begin
      TStrings(obj).CommaText := Val;
      exit;
    end;

    pi := GetPropInfo(obj,sProp);
    if pi = nil then
      exit;

    if pi.PropType^^.Name = 'TColor' then
      val := StringToColor(val);

    if pi.PropType^^.Name = 'TIWColor' then
      val := StringToIWColor(val);

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
      tkLString, tkString, tkUString:
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
        begin
          if GetObjectProp(obj,pi) is TStrings then
          begin
            TStrings(GetObjectProp(obj,pi)).Text := Val;
            exit;
          end;
          o := FindObject(val);
          if o <> nil then
            SetObjectProp(obj, pi, FindObject(val));
        end;
      tkRecord:
        exit;
      tkMethod:
        SetMethodProp(Obj,pi,FindMethod(Obj, val));
      else
        exit;
    end;
  end;
var
  ctrl : TComponent;
  sScripts : string;

  IWCtrl : IIWBaseHTMLComponent;
  myTag : TIWMarkupLanguageTag;

  cxt : TIWComponent40Context;
  i, iPos : integer;
  sType : string;
  cls : TPersistentClass;
  bNewComponent : boolean;
  bAddToContext : boolean;
  rs : TIWRenderStream;
  ShouldFreeMyTag: boolean;
begin
  Result := '';
  bNewComponent := False;
  bAddToContext := False;
  ShouldFreeMyTag := False;

  if Length(aCtrlName) > 0 then
  begin
    ctrl := AContainerContext.Components[aCtrlName];
    if ctrl = nil then
    begin
      iPos := CharPos(':',aCtrlName);
      if iPos > 0 then
      begin
        sType := Copy(aCtrlName,iPos+1,9999999);
        aCtrlName := Copy(aCtrlName,1,iPos-1);

        ctrl := Owner.FindComponent(aCtrlName);
        if not Assigned(ctrl) then
        begin
          cls := GetClass(sType);
          if cls = nil then
            raise Exception.Create('Class '+sType+' not found and cannot be created');
          ctrl := TComponentClass(cls).Create(Owner);
          ctrl.Name := aCtrlName;
          Self.Container.IWAddComponent(ctrl);

          if Ctrl is TControl then
            TControl(Ctrl).Parent := TWInControl(Self.Owner);


          if HTML40ControlInterface(Ctrl).SupportsInput then // more like a hack, but it seems to work
            TIWPageContext40(APageContext).FormTag.Contents.AddText('<input type="HIDDEN" name="'+Uppercase(aCtrlName)+'">');

          bNewComponent := True;
          bAddToContext := True;
        end else
          if AContainerContext.Components[aCtrlName] = nil then
            bAddToContext := True;
      end;
    end;


    if Assigned(ctrl) then
    begin
      cxt := TIWComponent40Context.Create(ctrl, AContainerContext, APageContext);
      try
        IWCtrl := BaseHTMLComponentInterface(AContainerContext.Components[aCtrlName]);
        if (Params.Count >0) and (not AlreadyProcessedTemplate) then
        begin
          for i := 0 to Params.Count -1 do
            SetProperty(ctrl, Params[i]);

          if Assigned(FOnNewControl) then
            FOnNewControl(Self,ctrl, APageContext);

          if bAddToContext then
            AContainerContext.AddComponent(cxt);

          if bNewComponent then
          begin
            ctrl.GetInterface(IIWBaseHTMLComponent, IWCtrl);
            myTag := IWCtrl.RenderMarkupLanguageTag(cxt);
            ShouldFreeMyTag := true;
            if (AContainerContext.Components[aCtrlName] is TIWCustomControl) then
            begin
              TIWCustomControlHack(AContainerContext.Components[aCtrlName]).RenderScripts(cxt); // maybe need to change!
              TIWCustomControlHack(AContainerContext.Components[aCtrlName]).ScriptEvents.Clear;
              myTag.AddStringParam('CLASS', TIWCustomControlHack(AContainerContext.Components[Uppercase(aCtrlName)]).RenderCSSClass(cxt));

              myTag.AddStringParam('STYLE', TIWCustomControlHack(AContainerContext.Components[Uppercase(aCtrlName)]).RenderStyle(cxt)+ myTag.Params.Values['STYLE']);
              //myTag.AddStringParam('style', HTML40ComponentInterface(ctrl).RenderStyle(TIWBaseHTMLComponentContext(AContainerContext.ComponentContext[aCtrlName])));
            end;
            myTag.AddStringParam('ID', IWCtrl.HTMLName);
          end else
          begin
            //myTag := AContainerContext.ComponentContext[aCtrlName].MarkupLanguageTag;
            // Had to change the above line to the below 2 lines in order to fix the problem where the
            // changed object properties were only being rendered on the second render.
            ctrl.GetInterface(IIWBaseHTMLComponent, IWCtrl);
            myTag := BaseComponentInterface(ctrl).RenderMarkupLanguageTag(AContainerContext.ComponentContext[aCtrlName]);
            TIWCustomControlHack(AContainerContext.Components[aCtrlName]).RenderScripts(cxt);
            ShouldFreeMyTag := true;
            myTag.AddStringParam('STYLE',HTML40ComponentInterface(ctrl).RenderStyle(TIWBaseHTMLComponentContext(AContainerContext.ComponentContext[aCtrlName])) + myTag.Params.Values['STYLE']);
            myTag.AddStringParam('CLASS',IWCtrl.HTMLName+'CSS');
            myTag.AddStringParam('ID', IWCtrl.HTMLName);

            sScripts := '';
          end;
        end else
          myTag := AContainerContext.ComponentContext[aCtrlName].MarkupLanguageTag;
        if Assigned(myTag) then
        begin
          rs := TIWRenderStream.Create;
          try
            IWCtrl.MakeHTMLTag(TIWHTMLTag(myTag),rs);
            Result := sScripts+{$IFDEF INTRAWEB122}rs.AsString{$ELSE}rs.Extract{$ENDIF};
          finally
            rs.Free;
          end;
        end;
      finally
        if ShouldFreeMyTag and Assigned(myTag) then
          FreeAndNil(myTag);
        
        APageContext.AppendContext(cxt);
        if not bNewComponent then
          cxt.Free;
      end;
    end else
    begin
      Result := DoFullUnknownTag(aCtrlName, Params);
    end;
  end;
end;

function TArcIWTemplateProcessor.ProcessSSI(
  AContainerContext: TIWContainerContext; APageContext: TIWBasePageContext;
  TokenStr: string; Params : TStringList): string;
var
  ss : TStringStream;
  ms : TPersistentStream;
  s, sTag : string;
  b : boolean;
begin
  Result := '';
  if FSSIOptions.Enabled then
  begin
      sTag := TokenStr;

      if CompareText(sTag, 'config')=0 then
      begin
        FSSIErrorMsg := Params.values['errmsg'];
        FSSITimeFormat := Params.Values['timefmt'];
        FSSISizeFormat := Params.Values['sizefmt'];
      end else
        if CompareText(sTag, 'include')=0 then
        begin
          s := Params.values['file'];
          if s <> '' then
          begin
            if Pos('..',s)>0 then
            begin
              Result := 'ERROR in SSI Include Path';
              exit;
            end;

            s := ExtractFilePath(gsAppPath)+s;

            ss := TStringStream.Create('');
            ms := TPersistentStream.Create;
            try
              ms.LoadFromFile(s);
              ms.Position := 0;

              if TagType=ttIntraweb then
                b := ms.FindNextMatch('{%')>0
              else
                b := ms.FindNextMatch('<#')>0;
              ms.Position := 0;
              if b then
              begin
                {$IFDEF INTRAWEB70}
                _Process(ss, ms.Stream, AContainerContext, TIWBaseHTMLPageContext(APageContext),1);
                _Process(ss, ms.Stream, AContainerContext, TIWBaseHTMLPageContext(APageContext),2);
                {$ELSE}
                _Process(ss, ms.Stream, AContainerContext, APageContext);
                {$ENDIF}
              end else
                ss.CopyFrom(ms.Stream, ms.Size);
              ss.Position := 0;
              Result := ss.DataString;
            finally
              ms.Free;
              ss.Free;
            end;
          end else
          begin
            s := Params.Values['virtual'];
            if s <> '' then
            begin
              if Pos('..',s)>0 then
              begin
                Result := 'ERROR in SSI Include Path';
                exit;
              end;
              Result := 'ERROR SSI Virtual Paths not yet allowed';
            end;
          end;
        end else
          if CompareText(sTag, 'echo')=0 then
          begin
            s := Params.Values['var'];
            case TEchoVars(Pos(s,SSIEchoVars) div 24) of
              evUnknown:;
              evDocumentName:          Result := TIWForm(Owner).Name;
              evDocumentURI:           Result := TIWForm(Owner).WebApplication.Request.URL;
              evQueryStringUnescaped:  Result := TIWForm(Owner).WebApplication.Request.Query;
              evDateLocal:             Result := DateTimeToStr(Now);
              evDateGMT:               Result := DateTimeToStr(NowGMT);
              evLastModified:          Result := DateTimeToStr(Now);
              evServerSoftware:        Result := 'IntraWeb '+GServerController.Version;
              evServerName:            Result := TIWForm(Owner).WebApplication.Request.Host;
              evGatewayInterface:      Result := 'CGI/unknown';
              {$IFDEF INTRAWEB120}
              evServerProtocol:        Result := ''; // TIWForm(Owner).WebApplication.Request.ProtocolVersion;
              {$ELSE}
              evServerProtocol:        Result := TIWForm(Owner).WebApplication.Request.ProtocolVersion;
              {$ENDIF}
              evServerPort:            Result := IntToStr(GServerController.Port);
              {$IFDEF INTRAWEB120}
              evRequestMethod:
              begin
                Result:= GetEnumName(TypeInfo(THttpMethod),Ord(TIWForm(Owner).WebApplication.Request.HttpMethod));
                System.Delete(Result,1,2); // hm
              end;
              {$ELSE}
              evRequestMethod:         Result := TIWForm(Owner).WebApplication.Request.Method;
              {$ENDIF}
              evPathInfo:              Result := TIWForm(Owner).WebApplication.Request.PathInfo;
              {$IFDEF INTRAWEB120}
              evPathTranslated:        Result := TIWForm(Owner).WebApplication.Request.PathInfo; {*}
              {$ELSE}
              evPathTranslated:        Result := TIWForm(Owner).WebApplication.Request.PathTranslated;
              {$ENDIF}
              evScriptName:            Result := TIWForm(Owner).WebApplication.Request.ScriptName;
              evQueryString:           Result := TIWForm(Owner).WebApplication.Request.Query;
              {$IFDEF INTRAWEB120}
              evRemoteHost:            Result := TIWForm(Owner).WebApplication.Request.Host;
              {$ELSE}
              evRemoteHost:            Result := TIWForm(Owner).WebApplication.Request.RemoteHost;
              {$ENDIF}
              evRemoteAddr:            Result := TIWForm(Owner).WebApplication.Request.RemoteAddr;
              evAuthType:              Result := TIWForm(Owner).WebApplication.Request.Authorization;
              evRemoteUser:            Result := DecodeUsername(TIWForm(Owner).WebApplication.Request.Authorization);
              evRemoteIdent:           Result := DecodeUsername(TIWForm(Owner).WebApplication.Request.Authorization);
              {$IFDEF INTRAWEB120}     {TODO -oPlp -cConversion : Check This}
              evContentType:           Result := ''; // TIWForm(Owner).WebApplication.Request.ContentType;
              evContentLength:         Result := ''; // IntToStr(length(TIWForm(Owner).WebApplication.Request.Content));
              evHTTPAccept:            Result := ''; // TIWForm(Owner).WebApplication.Request.Accept;
              {$ELSE}
              evContentType:           Result := TIWForm(Owner).WebApplication.Request.ContentType;
              evContentLength:         Result := IntToStr(length(TIWForm(Owner).WebApplication.Request.Content));
              evHTTPAccept:            Result := TIWForm(Owner).WebApplication.Request.Accept;
              {$ENDIF}
              evHTTPUserAgent:         Result := TIWForm(Owner).WebApplication.Request.UserAgent;
            end;
          end else
            if CompareText(sTag, 'fsize')=0 then
            begin
              s := Params.values['file'];
              if s <> '' then
              begin
                if Pos('..',s)>0 then
                begin
                  Result := 'ERROR in SSI Include Path';
                  exit;
                end;
                if s[1] <> PathDelim then
                  s := PathDelim+s;
                s := ExtractFilePath(gsAppPath)+s;
                with TStringList.Create do
                try
                  LoadFromFile(s);
                  Result := IntToStr(Length(Text));
                finally
                  free;
                end;
              end else
              begin
                s := Params.Values['virtual'];
                if s <> '' then
                begin
                  if Pos('..',s)>0 then
                  begin
                    Result := 'ERROR in SSI Include Path';
                    exit;
                  end;
                  Result := 'ERROR SSI Virtual Paths not yet allowed';
                end;
              end;
            end else
              if CompareText(sTag, 'flastmod')=0 then
              begin
                Result := DateTimeToStr(Now);
              end else
                if CompareText(sTag, 'exec')=0 then
                begin
                  Result := 'ERROR CGI support is not yet ready.';
                end;
  end;
end;

procedure TArcIWTemplateProcessor.SetSSIOptions(const Value: TSSIOptions);
begin
  FSSIOptions.Assign(Value);
end;

initialization
  RegisterClasses([ TIWApplet,
      TIWButton, TIWCheckBox, TIWComboBox, TIWEdit, TIWFile, TIWFlash,
      TIWHRule, TIWImage, TIWImageFile, TIWList, TIWLabel, TIWListBox, TIWLink,
      TIWMemo, TIWMenu, TIWRadioGroup, TIWRectangle, TIWRegion, TIWText,
      TIWTimer, TIWGrid, TIWTreeView, TIWURL, TIWDBCheckBox, TIWDBComboBox,
      TIWDBEdit, TIWDBGrid, TIWDBImage, TIWDBLabel, TIWDBListBox,
      TIWDBLookupListBox, TIWDBLookupComboBox, TIWDBFile, TIWDBMemo,
      TIWDBNavigator, TIWDBText,
      {$IFNDEF INTRAWEB110}
      TIWCSLabel, TIWCSNavigator, TIWDynamicChart, TIWDynamicChartLegend, TIWDynGrid,
      TIWPageProducer, TIWModuleController, TIWClientSideDataset, TIWClientSideDatasetDBLink,
      TIWTemplateProcessorHTML32, TIWPageProducer32, TIWLAyoutMgrHTML32,
      TIWImageList, TIWLabel32, TIWEdit32, TIWButton32, TIWList32, TIWHRule32,
      TIWListBox32, TIWComboBox32, TIWRadioGroup32, TIWImage32, TIWImageFile32,
      TIWURL32, TIWGrid32, TIWMemo32, TIWText32, TIWRectangle32, TIWCheckBox32,
      TIWHyperLink32, TIWDBEdit32, TIWDBCheckBox32, TIWDBComboBox32, TIWDBLabel32,
      TIWDBListBox32, TIWDBLookupComboBox32, TIWDBLookupListBox32, TIWDBMemo32,
      TIWDBText32, TIWDBImage32, TIWDBRadioGroup32,
      {$ENDIF}
      TIWTemplateProcessorHTML, TIWStandAloneServer, TIWLayoutMgrHTML,
      TIWProgressBar, TIWURLWindow, TIWActiveX, TIWCalendar, TIWDBRadioGroup

   ]);


end.
