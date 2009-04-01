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
//  unit ArcIWFilledBox                                                       //
//    Copyright 2002 by Arcana Technologies Incorporated                      //
//    Written By Jason Southwell                                              //
//    Based on Javascript written by Jim Silver @ jimsilver47@yahoo.com       //
//                                                                            //
//  Description:                                                              //
//    This component provides IntraWeb a filled box, simular to the           //
//    TIWRectangle, but with a border, fill and line styles and no bug in     //
//    minimum width/height.  It also renders the same on IE, NS6 and Opera.   //
//                                                                            //
//    It works by dynamically creating jpg images of the box at runtime in    //
//    the application's files directory.  These images are prefaced with a    //
//    tripple ~.  They are deleted from the directory when the application    //
//    is closed.  If you are debugging and reset the application, or if you   //
//    have an abnormal termination of the application, you may have to        //
//    manually delete ~~~*.jpg from your files subdirectory.  You only need   //
//    to do this though if you changed the look of the component at all.      //
//                                                                            //
//    This component is compatible with both IntraWeb versions 4 and 5,       //
//    though no testing has been performed on IW4.  If you have IW4,          //
//    please let me know if there are any problems.                           //
//
//    To compile for the particular version you have installed, change        //
//    the compiler directive comment in IWVersion.inc.                        //
//                                                                            //
//    Information on IntraWeb can be found at www.atozedsoftware.com          //
//    Arcana Technologies Incorporated has no affilation with IntraWeb        //
//    or Atozed Software with the exception of being a satisfied customer.    //
//                                                                            //
//  Updates:                                                                  //
//    07/18/2002 - Released to TArcIWFilledBox to Open Source.                //
//    07/18/2002 - Added ForceDirectories when jpg is created to allow for    //
//                 the possibility that you haven't yet created a files       //
//                 subdirectory.  Also added events OnClick and OnMouseDown.  //
//                 In doing so, I also added Opera support for the events.    //
//    07/18/2002 - Removed unnecessary <a> tag wrapper caused by default IW   //
//                 call.  Now doing this by hand.                             //
//    07/27/2002 - Fixed OnMouseDown in IE and Opera                          //
//    08/08/2002 - Added hidden Friend property.  The purpose of this is to   //
//                 make pages load faster at runtime when you have multiple   //
//                 TArcIWFilledBoxes on the form with the same attributes.    //
//                 Previously, a seperate image was created for each instance //
//                 which meant that a seperate image must be sent to the      //
//                 browser for each instance.  Now, the component will        //
//                 automatically figure out if there are any other components //
//                 on the parent form with the same attributes and use that   //
//                 image file instead of creating it's own.  If you use this  //
//                 component for seperator lines or block background images   //
//                 this could save you several kb per page.                   //
//                 Also added a JPGQuality property.  This property will      //
//                 set the outputted jpg source image quality.  It defaults   //
//                 to 5.  This is an extremely low image quality, but for the //
//                 purposes of most boxes, this will suffice.  If you need    //
//                 you can pump it up at the risk of bigger file sizes.       //
//    05/12/2003 - Removed support for IW4, Added support for IW6             //
//    10/02/2003 - Added support for IW7                                      //
//                                                                            //
//  License:                                                                  //
//    This code is covered by the Mozilla Public License 1.1 (MPL 1.1)        //
//    Full text of this license can be found at                               //
//    http://www.opensource.org/licenses/mozilla1.1.html                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit ArcIWFilledBox;

interface

{$I IntrawebVersion.inc}

uses
  Windows, Messages, Graphics, SysUtils, Classes, {$IFNDEF CLR}JPeg, {$ENDIF}IWControl, IWTypes,
  {$IFDEF VER130} FileCtrl, {$ENDIF} ArcIWOperaFix, SWSystem,
  {$IFDEF IWVERCLASS6} IWRenderContext, IWBaseControlInterface, IWScriptEvents, {$ENDIF}
  {$IFDEF INTRAWEB70} IWRenderContext, {$ENDIF} {$IFDEF INTRAWEB72} IWStreams, {$ENDIF}
  IWHTMLTag;

type
  TArcIWFilledBoxOnMouseDown = procedure(ASender: TObject; const AX: Integer; const AY: Integer) of object;
  TArcIWFilledBox = class(TIWControl)
  private
    FFillColor: TColor;
    FBorderColor: TColor;
    FFillStyle: TBrushStyle;
    FBorderStyle: TPenStyle;
    FBorderWidth: integer;
    FOnMouseDown: TArcIWFilledBoxOnMouseDown;
    FFriend: TArcIWFilledBox;
    {$IFNDEF CLR}
    FJPGQuality: TJPEGQualityRange;
    {$ENDIF}
    procedure SetFillColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderStyle(const Value: TPenStyle);
    procedure SetFillStyle(const Value: TBrushStyle);
    procedure SetBorderWidth(const Value: integer);
    function GetOnClick: TNotifyEvent;
    procedure SetOnClick(const Value: TNotifyEvent);
    procedure SetOnMouseDown(const Value: TArcIWFilledBoxOnMouseDown);
  protected
    procedure Loaded; override;
    procedure Submit(const AValue: string); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure FindNewFriend;
    procedure DefineProperties(Filer: TFiler); override;
    procedure LoadCompProperty(Reader: TReader);
    procedure StoreCompProperty(Writer: TWriter);
    procedure Resize; override;
    procedure IWPaint; override;
  public
    {$IFDEF IWVERCLASS5}
    function RenderHTML: TIWHTMLTag; override;
    {$ELSE}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ELSE}
    function RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$ENDIF}
    constructor Create(AOwner: TComponent); override;
    property Friend : TArcIWFilledBox read FFriend write FFriend;
  published
    property FillColor : TColor read FFillColor write SetFillColor default clNavy;
    property BorderColor : TColor read FBorderColor write SetBorderColor default clBlack;
    property FillStyle : TBrushStyle read FFillStyle write SetFillStyle default bsSolid;
    property BorderStyle : TPenStyle read FBorderStyle write SetBorderStyle default psSolid;
    property BorderWidth : integer read FBorderWidth write SetBorderWidth default 1;
    {$IFNDEF CLR}
    property JPGQuality : TJPEGQualityRange read FJPGQuality write FJPGQuality default 95;
    {$ENDIF}

    property DoSubmitValidation;
    property ScriptEvents;
    property OnClick: TNotifyEvent read GetOnClick write SetOnClick;
    property OnMouseDown: TArcIWFilledBoxOnMouseDown read FOnMouseDown write SetOnMouseDown;
  end;

implementation

uses SWStrings, IWAppForm;

var
  FilePath : string;

procedure DeleteTemporaryFiles;
var
  SR : TSearchRec;
begin
  if FindFirst(FilePath+'~~~*.jpg',faAnyFile,SR)=0 then
  try
    repeat
      DeleteFile(FilePath+SR.Name);
    until FindNext(SR)<>0;
  finally
    FindClose(SR);
  end;
end;

{ TArcIWFilledBox }

constructor TArcIWFilledBox.Create(AOwner: TComponent);
begin
  inherited;
  FFillColor   := clNavy;
  FFillStyle   := bsSolid;

  FBorderColor := clBlack;
  FBorderStyle := psSolid;
  FBorderWidth := 1;

  FNeedsFormTag := True;

  {$IFDEF IWVERCLASS5}
    FSupportsSubmit := True;
    FSupportedScriptEvents := 'OnAbort,OnError,OnKeyDown,OnKeyPress,OnKeyUp,OnLoad';
  {$ENDIF}

  {$IFNDEF CLR}
  FJPGQuality := 95;
  {$ENDIF}
end;

procedure TArcIWFilledBox.DefineProperties(Filer: TFiler);
begin
  inherited; { allow base classes to define properties }
  Filer.DefineProperty('Friend', LoadCompProperty, StoreCompProperty, True);
end;

procedure TArcIWFilledBox.FindNewFriend;
var
  i : integer;
begin
  FFriend := nil;
  // Loop through components to remove this friend from any other components.
  for i := 0 to TIWAppForm(Owner).ComponentCount-1 do
  begin
    if TArcIWFilledBox(TIWAppForm(Owner).Components[i]).Friend = Self then
    begin
      TArcIWFilledBox(TIWAppForm(Owner).Components[i]).Friend := nil;
    end;
  end;

  // Loop through components to find a suitable friend.
  for i := 0 to TIWAppForm(Owner).ComponentCount-1 do
  begin
    if (TIWAppForm(Owner).Components[i] is TArcIWFilledBox) and
       (TIWAppForm(Owner).Components[i] <> Self) and
       (TArcIWFilledBox(TIWAppForm(Owner).Components[i]).BorderColor = FBorderColor) and
       (TArcIWFilledBox(TIWAppForm(Owner).Components[i]).BorderStyle = FBorderStyle) and
       (TArcIWFilledBox(TIWAppForm(Owner).Components[i]).FillColor = FFillColor) and
       (TArcIWFilledBox(TIWAppForm(Owner).Components[i]).FillStyle = FFillStyle) and
       (TArcIWFilledBox(TIWAppForm(Owner).Components[i]).BorderWidth = FBorderWidth) and
       (TArcIWFilledBox(TIWAppForm(Owner).Components[i]).Width = Width) and
       (TArcIWFilledBox(TIWAppForm(Owner).Components[i]).Height = Height) then
    begin
      FFriend := TArcIWFilledBox(TIWAppForm(Owner).Components[i]).Friend;
      if not Assigned(FFriend) then
        FFriend := TArcIWFilledBox(TIWAppForm(Owner).Components[i]);

      Break;
    end;
  end;
end;

function TArcIWFilledBox.GetOnClick: TNotifyEvent;
begin
  Result := (inherited OnClick);
end;

procedure TArcIWFilledBox.LoadCompProperty(Reader: TReader);
var
  s : string;
  i : integer;
begin
  s := Reader.ReadString;
  FFriend := nil;
  if s <> '' then
  begin
    for i := 0 to TIWAppForm(Owner).ComponentCount-1 do
      if CompareText(TIWAppForm(Owner).Components[i].Name,s)=0 then
      begin
        FFriend := TArcIWFilledBox(TIWAppForm(Owner).Components[i]);
        break;
      end;
  end;
end;

{$IFNDEF CLR}
procedure TArcIWFilledBox.Loaded;
var
  bmp : TBitmap;
  jpg : TJPEGImage;
  sFilename : string;
begin
  inherited;
  if not (csDesigning in ComponentState) and (not Assigned(FFriend)) then
  begin
    ForceDirectories(FilePath);
    sFilename := FilePath+'~~~'+TComponent(Parent).Name+'_'+Name+'.jpg';
    if not FileExists(sFilename) then
    begin
      bmp := TBitmap.Create;
      try
        bmp.Width := width;
        bmp.Height := height;
        bmp.Canvas.Pen.Color := FBorderColor;
        bmp.Canvas.Pen.Style := FBorderStyle;
        bmp.Canvas.Pen.Width := FBorderWidth;
        bmp.Canvas.Brush.Color := FFillColor;
        bmp.Canvas.Brush.Style := FFillStyle;
        bmp.Canvas.Rectangle(0,0,Width,Height);
        jpg := TJPEGImage.Create;
        try
          jpg.Assign(bmp);
          jpg.CompressionQuality := FJPGQuality;
          jpg.SaveToFile(sFilename);
        finally
          jpg.Free;
        end;
      finally
        bmp.free;
      end;
    end;
  end;
end;
{$ELSE}
procedure TArcIWFilledBox.Loaded;
var
  bmp : TBitmap;
  jpg : TGraphic;
  sFilename : string;
begin
  inherited;
  if not (csDesigning in ComponentState) and (not Assigned(FFriend)) then
  begin
    ForceDirectories(FilePath);
    sFilename := FilePath+'~~~'+TComponent(Parent).Name+'_'+Name+'.jpg';
    if not FileExists(sFilename) then
    begin
      bmp := TBitmap.Create;
      try
        bmp.Width := width;
        bmp.Height := height;
        bmp.Canvas.Pen.Color := FBorderColor;
        bmp.Canvas.Pen.Style := FBorderStyle;
        bmp.Canvas.Pen.Width := FBorderWidth;
        bmp.Canvas.Brush.Color := FFillColor;
        bmp.Canvas.Brush.Style := FFillStyle;
        bmp.Canvas.Rectangle(0,0,Width,Height);
        jpg := TGraphic.Create;
        try
          jpg.Assign(bmp);
          jpg.SaveToFile(sFilename);
        finally
          jpg.Free;
        end;
      finally
        bmp.free;
      end;
    end;
  end;
end;
{$ENDIF}

procedure TArcIWFilledBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent=FFriend) then
  begin
    FFriend := nil;
    FindNewFriend;
  end;
end;

procedure TArcIWFilledBox.IWPaint;
begin
  Canvas.Lock;
  try
    Canvas.Brush.Color := clWhite;
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Color := clWhite;
    Canvas.Pen.Style := psSolid;
    Canvas.Rectangle(0,0,Width,Height);

    Canvas.Pen.Color := FBorderColor;
    Canvas.Pen.Style := FBorderStyle;
    Canvas.Pen.Width := FBorderWidth;
    Canvas.Brush.Color := FFillColor;
    Canvas.Brush.Style := FFillStyle;
    Canvas.Rectangle(0,0,Width,Height);
  finally
    Canvas.Unlock;
  end;
end;

{$IFDEF IWVERCLASS5}
function TArcIWFilledBox.RenderHTML: TIWHTMLTag;
var
  LTag: TIWHTMLTag;
begin
  {$IFNDEF IWVERSION51}
  // WorkAround to fix Opera Submits
  if WebApplication.Browser = brOpera then
    FixForOpera(Self);
  {$ENDIF}

  // Tag Output
  Result := TIWHTMLTag.CreateTag('IMG');
  Result.AddStringParam('width', IntToStr(Width)+'px');
  Result.AddStringParam('height', IntToStr(Height)+'px');
  Result.AddIntegerParam('border', 0);
  {$IFNDEF IWVERSION51}
  if not Assigned(FFriend) then
    Result.AddStringParam('src',WebApplication.URLBase+'/files/~~~'+TComponent(Parent).Name+'_'+Name+'.jpg')
  else
    Result.AddStringParam('src',WebApplication.URLBase+'/files/~~~'+TComponent(FFriend.Parent).Name+'_'+FFriend.Name+'.jpg');
  {$ELSE}
  if not Assigned(FFriend) then
    Result.AddStringParam('src',WebApplication.AppURLBase+'/files/~~~'+TComponent(Parent).Name+'_'+Name+'.jpg')
  else
    Result.AddStringParam('src',WebApplication.AppURLBase+'/files/~~~'+TComponent(FFriend.Parent).Name+'_'+FFriend.Name+'.jpg');
  {$ENDIF}
  if Assigned(FOnMouseDown) then begin
    case WebApplication.Browser of
      brIE:
        begin
          Result.AddStringParam( 'onClick', 'return SubmitClickConfirm('''+HTMLName+''','+
                                 'event.offsetX + '','' + event.offsetY, '+
                                 iif(DoSubmitValidation, 'true', 'false')+','''+
                                 Confirmation+''');');
        end;
      brNetscape6:
        begin
          LTag := TIWHTMLTag.CreateTag('A');
          try
            LTag.AddStringParam('HREF', '#');
            LTag.AddStringParam('OnMouseOver', 'return ImageSetEvent(this,''' + HTMLName + ''''
              + ',' + iif(DoSubmitValidation, 'true', 'false')
              + ',' + '''' + Confirmation + ''''
              + ');"');
            LTag.Contents.AddText(Result.Render);
          except
            FreeAndNil(LTag);
            raise;
          end;
          FreeAndNil(Result);
          Result := LTag;
        end;
      brOpera:
        begin
          Result.AddStringParam( 'onClick', 'return SubmitClickConfirm('''+HTMLName+''','+
                                 '(event.clientX-SubmitForm.'+HTMLName+'.style.pixelLeft)+ '','' + '+
                                 '(event.clientY-SubmitForm.'+HTMLName+'.style.pixelTop), '+
                                 iif(DoSubmitValidation, 'true', 'false')+','''+
                                 Confirmation+''');');
        end;
    end;
  end;

  // OnMouseDown and OnClick are mutually exclusive, no need to check
  if Assigned(OnClick) then
  begin
    Result.AddStringParam('onClick','return SubmitClickConfirm('''+HTMLName+''','''', true, '''')');
  end;
end;
{$ELSE}
{$IFDEF INTRAWEB70}
function TArcIWFilledBox.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ELSE}
function TArcIWFilledBox.RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
var
  LTag: TIWHTMLTag;
  LStream: TIWRenderStream;
begin
  // Tag Output
  Result := TIWHTMLTag.CreateTag('IMG');
  Result.AddStringParam('width', IntToStr(Width)+'px');
  Result.AddStringParam('height', IntToStr(Height)+'px');
  Result.AddIntegerParam('border', 0);
  if not Assigned(FFriend) then
    Result.AddStringParam('src',AContext.WebApplication.AppURLBase+'/files/~~~'+TComponent(Parent).Name+'_'+Name+'.jpg')
  else
    Result.AddStringParam('src',AContext.WebApplication.AppURLBase+'/files/~~~'+TComponent(FFriend.Parent).Name+'_'+FFriend.Name+'.jpg');

  if Assigned(FOnMouseDown) then begin
    case AContext.Browser of
      brIE:
        begin
          Result.AddStringParam( 'onClick', 'return SubmitClickConfirm('''+HTMLName+''','+
                                 'event.offsetX + '','' + event.offsetY, '+
                                 iif(DoSubmitValidation, 'true', 'false')+','''+
                                 Confirmation+''');');
        end;
      brNetscape6:
        begin
          LTag := TIWHTMLTag.CreateTag('A');
          try
            LTag.AddStringParam('HREF', '#');
            LTag.AddStringParam('OnMouseOver', 'return ImageSetEvent(this,''' + HTMLName + ''''
              + ',' + iif(DoSubmitValidation, 'true', 'false')
              + ',' + '''' + Confirmation + ''''
              + ');"');
            {$IFDEF INTRAWEB72}
              LStream := TIWRenderStream.Create;
              try
                Result.Render(LStream);
                LTag.Contents.AddText(LStream.Extract);
              finally
                LSTream.Free;
              end;
            {$ELSE}
              LTag.Contents.AddText(Result.Render);
            {$ENDIF}
          except
            FreeAndNil(LTag);
            raise;
          end;
          FreeAndNil(Result);
          Result := LTag;
        end;
      brOpera:
        begin
          Result.AddStringParam( 'onClick', 'return SubmitClickConfirm('''+HTMLName+''','+
                                 '(event.clientX-SubmitForm.'+HTMLName+'.style.pixelLeft)+ '','' + '+
                                 '(event.clientY-SubmitForm.'+HTMLName+'.style.pixelTop), '+
                                 iif(DoSubmitValidation, 'true', 'false')+','''+
                                 Confirmation+''');');
        end;
    end;
  end;

  // OnMouseDown and OnClick are mutually exclusive, no need to check
  if Assigned(OnClick) then
  begin
    Result.AddStringParam('onClick','return SubmitClickConfirm('''+HTMLName+''','''', true, '''')');
  end;
end;
{$ENDIF}

procedure TArcIWFilledBox.Resize;
begin
  inherited;
  if (csDesigning in ComponentState) then
  begin
    FindNewFriend;
  end;
end;

procedure TArcIWFilledBox.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  if (csDesigning in ComponentState) then
  begin
    Invalidate;
    FindNewFriend;
  end;
end;

procedure TArcIWFilledBox.SetBorderStyle(const Value: TPenStyle);
begin
  FBorderStyle := Value;
  if FBorderStyle <> psSolid then
    FBorderWidth := 1;
  if (csDesigning in ComponentState) then
  begin
    Invalidate;
    FindNewFriend;
  end;
end;

procedure TArcIWFilledBox.SetBorderWidth(const Value: integer);
begin
  FBorderWidth := Value;
  if FBorderWidth < 1 then FBorderWidth := 1;
  if FBorderWidth > 1 then FBorderStyle := psSolid;
  if (csDesigning in ComponentState) then
  begin
    Invalidate;
    FindNewFriend;
  end;
end;

procedure TArcIWFilledBox.SetFillColor(const Value: TColor);
begin
  FFillColor := Value;
  if (csDesigning in ComponentState) then
  begin
    Invalidate;
    FindNewFriend;
  end;
end;

procedure TArcIWFilledBox.SetFillStyle(const Value: TBrushStyle);
begin
  FFillStyle := Value;
  if (csDesigning in ComponentState) then
    Invalidate;
end;

procedure TArcIWFilledBox.SetOnClick(const Value: TNotifyEvent);
begin
  FOnMouseDown := nil;
  inherited OnClick := Value;
end;

procedure TArcIWFilledBox.SetOnMouseDown(const Value: TArcIWFilledBoxOnMouseDown);
begin
  inherited OnClick := nil;
  FOnMouseDown := Value;
end;

procedure TArcIWFilledBox.StoreCompProperty(Writer: TWriter);
var
  s : string;
begin
  if Assigned(FFriend) then
    s := FFriend.Name
  else
    s := '';
  Writer.WriteString(s);
end;

procedure TArcIWFilledBox.Submit(const AValue: string);
var
  s: string;
  LX: Integer;
  LY: Integer;
begin
  if Assigned(FOnMouseDown) then
  begin
    s := AValue;
    LX := StrToInt(Fetch(s, ','));
    LY := StrToInt(s);
    OnMouseDown(Self, LX, LY);
  end else begin
    DoClick;
  end;
end;

initialization
  FilePath := gsAppPath+'Files\';
finalization
  DeleteTemporaryFiles;

end.
