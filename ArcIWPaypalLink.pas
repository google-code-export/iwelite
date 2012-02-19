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
//  unit ArcIWPaypalLink                                                      //
//    Copyright 2002 by Arcana Technologies Incorporated                      //
//    Written By Jason Southwell                                              //
//                                                                            //
//  Description:                                                              //
//    This component provides an IntraWeb interface to the PayPal ecommerce   //
//    system.  It includes the following components:                          //
//                                                                            //
//      - TArcIWPayPalDonation:                                               //
//        Use this component to accept donations from your users.             //
//      - TArcIWPayPalItemPurchase:                                           //
//        Use this component to allow your users to purchase a single item    //
//        from your site.  Don't use this if you want the users to select     //
//        multiple items before submitting payment information.               //
//      - TArcIWPayPalSubscription:                                           //
//        Use this component to accept subscription payments.  You can setup  //
//        two trial periods and one regular subscription period.              //
//      - TArcIWPayPalSubscriptionCancel:                                     //
//        Use this component to provide your users a way to cancel their      //
//        subscriptions.                                                      //
//      - TArcIWPayPalAddToCart:                                              //
//        Use this component to provide your users a way to select multiple   //
//        items before submitting payment.                                    //
//      - TArcIWPayPalViewCart:                                               //
//        Use this component to provide your users a way to view the cart     //
//        without adding any items.                                           //
//                                                                            //
//    This component is currently only compatible with IntraWeb version 5.    //
//                                                                            //
//    Information on IntraWeb can be found at www.atozedsoftware.com          //
//    Arcana Technologies Incorporated has no affilation with IntraWeb        //
//    or Atozed Software with the exception of being a satisfied customer.    //
//                                                                            //
//  Updates:                                                                  //
//    08/02/2002 - Released ArcIWPaypalLink to Open Source                    //
//    05/12/2003 - Removed support for IW4, Added support for IW6             //
//    10/02/2003 - Added support for IW7                                      //
//                                                                            //
//  License:                                                                  //
//    This code is covered by the Mozilla Public License 1.1 (MPL 1.1)        //
//    Full text of this license can be found at                               //
//    http://www.opensource.org/licenses/mozilla1.1.html                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit ArcIWPaypalLink;

interface

{$I IntrawebVersion.inc}

uses
  Windows, Messages, SysUtils, Classes, Controls, IWControl,
  ArcIWOperaFix,
  {$IFDEF IWVERCLASS6} IWRenderContext, IWBaseControlInterface, IWScriptEvents, {$ENDIF}
  {$IFDEF INTRAWEB70} IWRenderContext, {$ENDIF}
  IWHTMLTag, ArcCommon;

type
  TArcIWDefImageDonation = (dfdRounded, dfdCCDButton, dfdPaypal, dfdStandard);
const
  ArcIWDefImageDonationURL : array[0..3] of string = (
                     'https://www.paypal.com/images/x-click-but21.gif',
                     'https://www.paypal.com/images/x-click-butcc-donate.gif',
                     'https://www.paypal.com/images/x-click-but04.gif',
                     'https://www.paypal.com/images/x-click-but11.gif');
  ArcIWDefImageDonationX : array[0..3] of Integer = (110, 73, 62, 60);
  ArcIWDefImageDonationY : array[0..3] of Integer = (23,  44, 31, 23);

type
  TArcIWDefImageSubscription = (dfsCCDButton1, dfsCCDButton2, dfsPaypal1,
                                dfsPaypal2, dfsRounded, dfsStandard);
const
  ArcIWDefImageSubscriptionURL : array[0..5] of string = (
                     'https://www.paypal.com/images/x-click-butcc-subscribe.gif',
                     'https://www.paypal.com/images/x-click-butcc.gif',
                     'https://www.paypal.com/images/x-click-but20.gif',
                     'https://www.paypal.com/images/x-click-but06.gif',
                     'https://www.paypal.com/images/x-click-but24.gif',
                     'https://www.paypal.com/images/x-click-but12.gif');
  ArcIWDefImageSubscriptionX : array[0..5] of Integer = (73, 73, 62, 62, 76, 84);
  ArcIWDefImageSubscriptionY : array[0..5] of Integer = (44, 44, 31, 31, 23, 23);

type
  TArcIWDefImageSubscriptionCancel = (dfscRounded1, dfscRounded2, dfscStandard);
const
  ArcIWDefImageSubscriptionCancelURL : array[0..2] of string = (
                     'https://www.paypal.com/images/cancel_subscribe_gen.gif',
                     'https://www.paypal.com/images/cancel_subscribe_gen_2.gif',
                     'https://www.paypal.com/images/cancel_subscribe_gen_3.gif');
  ArcIWDefImageSubscriptionCancelX : array[0..2] of Integer = (139, 90, 136);
  ArcIWDefImageSubscriptionCancelY : array[0..2] of Integer = (21,  21, 23);

type
  TArcIWDefImageItemPurchase = (dfiRounded, dfiCCDButton, dfiPaypal1, dfiPaypal2,
                                dfiPaypal3, dfiStandard, dfiPaypalBig1, dfiPaypalBig2);
const
  ArcIWDefImageItemPurchaseURL : array[0..7] of string = (
                     'https://www.paypal.com/images/x-click-but23.gif',
                     'https://www.paypal.com/images/x-click-butcc.gif',
                     'https://www.paypal.com/images/x-click-but02.gif',
                     'https://www.paypal.com/images/x-click-but03.gif',
                     'https://www.paypal.com/images/x-click-but01.gif',
                     'https://www.paypal.com/images/x-click-but9.gif',
                     'https://www.paypal.com/images/x-click-but5.gif',
                     'https://www.paypal.com/images/x-click-but6.gif');
  ArcIWDefImageItemPurchaseX : array[0..7] of Integer = (68, 73, 62, 62, 62, 72, 150, 150);
  ArcIWDefImageItemPurchaseY : array[0..7] of Integer = (23, 44, 31, 31, 31, 23,  52,  52);

type
  TArcIWDefImageAddToCart = (dfacRounded, dfacAddImage, dfacPaypalBig,
                             dfacPaypal, dfacStandard);
const
  ArcIWDefImageAddToCartURL : array[0..4] of string = (
                     'https://www.paypal.com/images/x-click-but22.gif',
                     'https://www.paypal.com/images/sc-but-03.gif',
                     'https://www.paypal.com/images/sc-but-02.gif',
                     'https://www.paypal.com/images/sc-but-01.gif',
                     'https://www.paypal.com/images/x-click-but10.gif');
  ArcIWDefImageAddToCartX : array[0..4] of Integer = (87, 106, 150, 70, 90);
  ArcIWDefImageAddToCartY : array[0..4] of Integer = (23,  24, 52, 35, 23);

type
  TArcIWDefImageViewCart = (dfvcViewImage, dfvcRounded, dfvcStandard1, dfvcStandard2);
const
  ArcIWDefImageViewCartURL : array[0..3] of string = (
                     'https://www.paypal.com/images/view_cart.gif',
                     'https://www.paypal.com/images/view_cart_02.gif',
                     'https://www.paypal.com/images/view_cart_03.gif',
                     'https://www.paypal.com/images/view_cart_04.gif');
  ArcIWDefImageViewCartX : array[0..3] of Integer = (130, 74, 78, 118);
  ArcIWDefImageViewCartY : array[0..3] of Integer = ( 32, 21, 23,  24);

type
  {$IFDEF IWVERCLASS5}
  TArcIWPayPalBase = class(TIWControl)
  {$ELSE}
  TArcIWPayPalBase = class(TIWCustomControl)
  {$ENDIF}
  private
  protected
    FUseBorder: Boolean;
    FAutoSize: Boolean;
    FPaypalAccount: string;
    FSuccessURL: string;
    FLogoURL: string;
    FAltText: string;
    FImageURL: string;
    FCancelURL: string;
    function GetImageProps : string; virtual;
    function FixText(txt : string) : string; virtual;
    procedure SetAutoSize(const Value: Boolean); reintroduce; virtual;
    procedure ResizeSelf; virtual;
  public
    constructor Create(AOwner : TComponent); override;
    property URLSuccess : string read FSuccessURL write FSuccessURL;
    property URLCancel : string read FCancelURL write FCancelURL;
  published
    property URLImage : string read FImageURL write FImageURL;
    property PaypalAccount : string read FPaypalAccount write FPaypalAccount;
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property AltText: string read FAltText write FAltText;
    property UseBorder: Boolean read FUseBorder write FUseBorder;
    property URLLogo : string read FLogoURL write FLogoURL;
  end;

  TArcIWPayPalDonation = class(TArcIWPayPalBase)
  private
  protected
    FSpecialInstructions: string;
    FDonationAmount: Double;
    FDefaultImage: TArcIWDefImageDonation;
    FDonationText: string;
    FDonationID: string;
    procedure SetDefaultImage(const Value: TArcIWDefImageDonation); virtual;
    procedure ResizeSelf; override;
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
  published
    property DonationID : string read FDonationID write FDonationID;
    property DonationText : string read FDonationText write FDonationText;
    property DonationAmount : Double read FDonationAmount write FDonationAmount;
    property DefaultImage : TArcIWDefImageDonation read FDefaultImage write SetDefaultImage;
    property SpecialInstructions : string read FSpecialInstructions write FSpecialInstructions;
    property URLSuccess;
    property URLCancel;
  end;

  TPayPalFrequency = (pfDays, pfWeeks, pfMonths, pfYears);
  TPayPalPeriod = class(TPersistent)
  private
    FAmount: Double;
    FCycleLength: integer;
    FCycleFreq: TPayPalFrequency;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Amount : Double read FAmount write FAmount;
    property CycleLength : integer read FCycleLength write FCycleLength;
    property CycleFreq : TPayPalFrequency read FCycleFreq write FCycleFreq;
  end;

  TArcIWPayPalSubscriptionCancel = class(TArcIWPayPalBase)
  private
    FDefaultImage: TArcIWDefImageSubscriptionCancel;
    procedure SetDefaultImage(
      const Value: TArcIWDefImageSubscriptionCancel);
  protected
    procedure ResizeSelf; override;
    {$IFDEF IWVERCLASS5}
    function RenderHTML: TIWHTMLTag; override;
    {$ELSE}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ELSE}
    function RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$ENDIF}
  published
    property DefaultImage : TArcIWDefImageSubscriptionCancel read FDefaultImage write SetDefaultImage;
  end;

  TArcIWPayPalSubscription = class(TArcIWPayPalBase)
  private
    FPeriodTrial1: TPayPalPeriod;
    FPeriodTrial2: TPayPalPeriod;
    FPeriodRegular: TPayPalPeriod;
    FOption1: string;
    FOption2: string;
    procedure SetPeriodRegular(const Value: TPayPalPeriod);
    procedure SetPeriodTrial1(const Value: TPayPalPeriod);
    procedure SetPeriodTrial2(const Value: TPayPalPeriod);
  protected
    FSubscriptionText: string;
    FPasswordManagement: boolean;
    FDefaultImage: TArcIWDefImageSubscription;
    FSubscriptionID: string;
    FReattemptOnFailure: boolean;
    FRecurring: boolean;
    FStopRecurring: integer;
    procedure SetDefaultImage(const Value: TArcIWDefImageSubscription); virtual;
    procedure ResizeSelf; override;
  public
    function GetPeriodLetter( per : TPayPalFrequency) : char;
    constructor Create(AOwner : TComponent); override;
    {$IFDEF IWVERCLASS5}
    function RenderHTML: TIWHTMLTag; override;
    {$ELSE}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ELSE}
    function RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$ENDIF}
    destructor Destroy; override;
  published
    property SubscriptionID : string read FSubscriptionID write FSubscriptionID;
    property SubscriptionText : string read FSubscriptionText write FSubscriptionText;
    property PeriodTrial1 : TPayPalPeriod read FPeriodTrial1 write SetPeriodTrial1;
    property PeriodTrial2: TPayPalPeriod read FPeriodTrial2 write SetPeriodTrial2;
    property PeriodRegular: TPayPalPeriod read FPeriodRegular write SetPeriodRegular;
    property Recurring : boolean read FRecurring write FRecurring;
    property StopRecurring : integer read FStopRecurring write FStopRecurring;
    property ReattemptOnFailure : boolean read FReattemptOnFailure write FReattemptOnFailure;
    property DefaultImage : TArcIWDefImageSubscription read FDefaultImage write SetDefaultImage;
    property PasswordManagement : boolean read FPasswordManagement write FPasswordManagement;
    property Option1 : string read FOption1 write FOption1;
    property Option2 : string read FOption2 write FOption2;
    property URLSuccess;
    property URLCancel;
  end;

  TArcIWPayPalAddToCart = class(TArcIWPayPalBase)
  private
    FSpecialInstructions: string;
    FOption1: string;
    FOption2: string;
  protected
    FDefaultImage: TArcIWDefImageAddToCart;
    FItemText: string;
    FItemID: string;
    FItemAmount: Double;
    procedure SetDefaultImage(const Value: TArcIWDefImageAddToCart); virtual;
    procedure ResizeSelf; override;
  public
    constructor Create(AOwner : TComponent); override;
    {$IFDEF IWVERCLASS5}
    function RenderHTML: TIWHTMLTag; override;
    {$ELSE}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ELSE}
    function RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$ENDIF}
    destructor Destroy; override;
  published
    property ItemID : string read FItemID write FItemID;
    property ItemText : string read FItemText write FItemText;
    property ItemAmount : Double read FItemAmount write FItemAmount;
    property DefaultImage : TArcIWDefImageAddToCart read FDefaultImage write SetDefaultImage;
    property Option1 : string read FOption1 write FOption1;
    property Option2 : string read FOption2 write FOption2;
    property SpecialInstructions : string read FSpecialInstructions write FSpecialInstructions;
    property URLSuccess;
    property URLCancel;
  end;

  TArcIWPayPalViewCart = class(TArcIWPayPalBase)
  private
    FDefaultImage: TArcIWDefImageViewCart;
    procedure SetDefaultImage(const Value: TArcIWDefImageViewCart);
  protected
    procedure ResizeSelf; override;
  public
    constructor Create(AOwner : TComponent); override;
    {$IFDEF IWVERCLASS5}
    function RenderHTML: TIWHTMLTag; override;
    {$ELSE}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ELSE}
    function RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$ENDIF}
  published
    property DefaultImage : TArcIWDefImageViewCart read FDefaultImage write SetDefaultImage;
  end;

  TArcIWPayPalItemPurchase = class(TArcIWPayPalBase)
  private
    FAskForQuantity: boolean;
    FSpecialInstructions: string;
    FOption1: string;
    FOption2: string;
  protected
    FItemText: string;
    FItemID: string;
    FDefaultImage: TArcIWDefImageItemPurchase;
    FItemAmount: Double;
    procedure SetDefaultImage(const Value: TArcIWDefImageItemPurchase); virtual;
    procedure ResizeSelf; override;
  public
    constructor Create(AOwner : TComponent); override;
    {$IFDEF IWVERCLASS5}
    function RenderHTML: TIWHTMLTag; override;
    {$ELSE}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ELSE}
    function RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$ENDIF}
    destructor Destroy; override;
  published
    property ItemID : string read FItemID write FItemID;
    property ItemText : string read FItemText write FItemText;
    property ItemAmount : Double read FItemAmount write FItemAmount;
    property DefaultImage : TArcIWDefImageItemPurchase read FDefaultImage write SetDefaultImage;
    property AskForQuantity : boolean read FAskForQuantity write FAskForQuantity;
    property Option1 : string read FOption1 write FOption1;
    property Option2 : string read FOption2 write FOption2;
    property SpecialInstructions : string read FSpecialInstructions write FSpecialInstructions;
    property URLSuccess;
    property URLCancel;
  end;

implementation

{ TArcIWPayPalBase }

constructor TArcIWPayPalBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSize := True;
  FNeedsFormTag := False;
  {$IFDEF IWVERCLASS5}
  FSupportsInput := False;
  FSupportsSubmit := False;
  {$ENDIF}
end;

function TArcIWPayPalBase.FixText(txt: string): string;
var
  i : integer;
const
  InvalidChars = [' ',',','"','@',':'];
begin
  for i := 1 to length(txt) do
  begin
    if CharInSet(txt[i],InvalidChars) then
      Result := Result+'%'+IntTohex(Ord(txt[i]),2)
    else
      Result := Result+txt[i];
  end;
end;

function TArcIWPayPalBase.GetImageProps: string;
begin
  Result := ' NAME="'+HTMLName+'" BORDER='+IntToStr(Integer(FUseBorder))+' ALT="'+FAltText+'"';
end;

procedure TArcIWPayPalBase.ResizeSelf;
begin

end;

procedure TArcIWPayPalBase.SetAutoSize(const Value: Boolean);
begin
  FAutoSize := Value;
  if (csDesigning in ComponentState) and FAutoSize then
    ResizeSelf;
end;

{ TArcIWPayPalDonation }

procedure TArcIWPayPalDonation.SetDefaultImage(
  const Value: TArcIWDefImageDonation);
begin
  FDefaultImage := Value;
  if (csDesigning in ComponentState) then
    ResizeSelf;
end;

procedure TArcIWPayPalDonation.ResizeSelf;
begin
  inherited;
  if FAutoSize and (FImageURL = '') then
  begin
    Width := ArcIWDefImageDonationX[Integer(FDefaultImage)];
    Height := ArcIWDefImageDonationY[Integer(FDefaultImage)];
  end;
end;

{$IFDEF IWVERCLASS5}
function TArcIWPayPalDonation.RenderHTML: TIWHTMLTag;
{$ELSE}
{$IFDEF INTRAWEB70}
function TArcIWPayPalDonation.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ELSE}
function TArcIWPayPalDonation.RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$ENDIF}
begin
  result := TIWHTMLTag.CreateTag('form');
  result.AddStringParam('ID',HTMLName+'_Form');
  result.AddStringParam('CLASS',HTMLName+'_FormCSS');
  result.AddStringParam('action','https://www.paypal.com/cgi-bin/webscr');
  result.AddStringParam('method','post');
  result.AddStringParam('target','paypal');
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','cmd');
    AddStringParam('value','_xclick');
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','business');
    AddStringParam('value',FPaypalAccount);
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','item_name');
    AddStringParam('value',FDonationText);
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','item_number');
    AddStringParam('value',FDonationID);
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','amount');
    AddStringParam('value',FloatToStr(FDonationAmount));
  end;
  if FLogoURL<>'' then
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','image_url');
      AddStringParam('value',FLogoURL);
    end;
  if FSuccessURL<>'' then
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','return');
      AddStringParam('value',FSuccessURL);
    end;
  if FCancelURL<>'' then
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','cancel_return');
      AddStringParam('value',FCancelURL);
    end;
  if FSpecialInstructions<>'' then
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','cn');
      AddStringParam('value',FSpecialInstructions);
    end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','image');
    AddStringParam('name','submit');
    AddIntegerParam('border',Integer(FUseBorder));
    AddStringParam('alt',FAltText);
    AddStringParam('ID',HTMLName);
    AddStringParam('CLASS',HTMLName+'CSS');
    if not FAutoSize then
    begin
      AddIntegerParam('width',Width);
      AddIntegerParam('height',Height);
    end;
    if FImageURL <> '' then
      AddStringParam('src',FImageURL)
    else
      AddStringParam('src',ArcIWDefImageDonationURL[Integer(FDefaultImage)]);
  end;
  {

<form action="https://www.paypal.com/cgi-bin/webscr" method="post">
<input type="hidden" name="cmd" value="_xclick">
<input type="hidden" name="business" value="jsouthwell@arcanatech.com">
<input type="hidden" name="item_name" value="DonationName">
<input type="hidden" name="item_number" value="DonationID">
<input type="hidden" name="amount" value="$100.00">
<input type="hidden" name="image_url" value="https://MyLogoURL">
<input type="hidden" name="return" value="http://MySuccessURL">
<input type="hidden" name="cancel_return" value="http://MyCancelURL">
<input type="hidden" name="cn" value="MySpecialInstructions">
<input type="image" src="https://www.paypal.com/images/x-click-but21.gif" border="0" name="submit" alt="Make payments with PayPal - it's fast, free and secure!">
</form>


    https://www.paypal.com/xclick/business=jsouthwell%40arcanatech.com
    &item_name=DonationName
    &item_number=DonationID
    &amount=%24100.00
    ?_url=https%3A//MyLogoURL
    &return=http%3A//MySuccessURL
    &cancel_return=http%3A//MyCancelURL
    &cn=MySpecialInstructions
  }
end;

{ TArcIWPayPalSubscription }

constructor TArcIWPayPalSubscription.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPeriodTrial1:= TPayPalPeriod.Create;
  FPeriodTrial2 := TPayPalPeriod.Create;
  FPeriodRegular := TPayPalPeriod.Create;
end;

destructor TArcIWPayPalSubscription.Destroy;
begin
  FPeriodTrial1.Free;
  FPeriodTrial2.Free;
  FPeriodRegular.Free;
  inherited;
end;

function TArcIWPayPalSubscription.GetPeriodLetter(
  per: TPayPalFrequency): char;
begin
  Result := 'X';
  case per of
    pfDays:   Result := 'D';
    pfWeeks:  Result := 'W';
    pfMonths: Result := 'M';
    pfYears:  Result := 'Y';
  end;
end;

{$IFDEF IWVERCLASS5}
function TArcIWPayPalSubscription.RenderHTML: TIWHTMLTag;
{$ELSE}
{$IFDEF INTRAWEB70}
function TArcIWPayPalSubscription.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ELSE}
function TArcIWPayPalSubscription.RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$ENDIF}
var
  sURL : string;
  i : integer;
begin
  result := TIWHTMLTag.CreateTag('form');
  result.AddStringParam('ID',HTMLName+'_Form');
  result.AddStringParam('CLASS',HTMLName+'_FormCSS');
  result.AddStringParam('action','https://www.paypal.com/cgi-bin/webscr');
  result.AddStringParam('method','post');
  result.AddStringParam('target','paypal');
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','cmd');
    AddStringParam('value','_xclick-subscriptions');
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','business');
    AddStringParam('value',FPaypalAccount);
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','item_name');
    AddStringParam('value',FSubscriptionText);
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','item_number');
    AddStringParam('value',FSubscriptionID);
  end;
  if FLogoURL <> '' then
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','image_url');
      AddStringParam('value',FLogoURL);
    end;
  if FSuccessURL<> '' then
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','return');
      AddStringParam('value',FSuccessURL);
    end;
  if FCancelURL<> '' then
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','cancel_return');
      AddStringParam('value',FCancelURL);
    end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','no_note');
    AddStringParam('value','1');
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','image');
    AddStringParam('name','submit');
    AddIntegerParam('border',Integer(FUseBorder));
    AddStringParam('alt',FAltText);
    AddStringParam('ID',HTMLName);
    AddStringParam('CLASS',HTMLName+'CSS');
    if not FAutoSize then
    begin
      AddIntegerParam('width',Width);
      AddIntegerParam('height',Height);
    end;
    if FImageURL <> '' then
      AddStringParam('src',FImageURL)
    else
      AddStringParam('src',ArcIWDefImageDonationURL[Integer(FDefaultImage)]);
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','image');
    AddStringParam('name','submit');
    AddIntegerParam('border',Integer(FUseBorder));
    AddStringParam('alt',FAltText);
    AddStringParam('ID',HTMLName);
    AddStringParam('CLASS',HTMLName+'CSS');
    if not FAutoSize then
    begin
      AddIntegerParam('width',Width);
      AddIntegerParam('height',Height);
    end;
    if FImageURL <> '' then
      AddStringParam('src',FImageURL)
    else
      AddStringParam('src',ArcIWDefImageSubscriptionURL[Integer(FDefaultImage)]);
  end;
  if FPeriodTrial1.FAmount > 0 then
  begin
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','a1');
      AddStringParam('value',FloatToStr(FPeriodTrial1.FAmount));
    end;
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','p1');
      AddStringParam('value',IntToStr(FPeriodTrial1.FCycleLength));
    end;
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','t1');
      AddStringParam('value',GetPeriodLetter(FPeriodTrial1.FCycleFreq));
    end;
  end;
  if FPeriodTrial2.FAmount > 0 then
  begin
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','a2');
      AddStringParam('value',FloatToStr(FPeriodTrial2.FAmount));
    end;
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','p2');
      AddStringParam('value',IntToStr(FPeriodTrial2.FCycleLength));
    end;
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','t2');
      AddStringParam('value',GetPeriodLetter(FPeriodTrial2.FCycleFreq));
    end;
  end;
  if FPeriodRegular.FAmount > 0 then
  begin
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','a3');
      AddStringParam('value',FloatToStr(FPeriodRegular.FAmount));
    end;
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','p3');
      AddStringParam('value',IntToStr(FPeriodRegular.FCycleLength));
    end;
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','t3');
      AddStringParam('value',GetPeriodLetter(FPeriodRegular.FCycleFreq));
    end;
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','src');
    if FRecurring then
      AddStringParam('value','1')
    else
      AddStringParam('value','0');
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','sra');
    if FReattemptOnFailure then
      AddStringParam('value','1')
    else
      AddStringParam('value','0');
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','srt');
    AddIntegerParam('value',FStopRecurring);
  end;
end;

procedure TArcIWPayPalSubscription.ResizeSelf;
begin
  inherited;
  if FAutoSize and (FImageURL = '') then
  begin
    Width := ArcIWDefImageDonationX[Integer(FDefaultImage)];
    Height := ArcIWDefImageDonationY[Integer(FDefaultImage)];
  end;
end;

procedure TArcIWPayPalSubscription.SetDefaultImage(
  const Value: TArcIWDefImageSubscription);
begin
  FDefaultImage := Value;
  if (csDesigning in ComponentState) then
    ResizeSelf;
end;

procedure TArcIWPayPalSubscription.SetPeriodRegular(
  const Value: TPayPalPeriod);
begin
  FPeriodRegular.Assign(Value);
end;

procedure TArcIWPayPalSubscription.SetPeriodTrial1(
  const Value: TPayPalPeriod);
begin
  FPeriodTrial1.Assign(Value);
end;

procedure TArcIWPayPalSubscription.SetPeriodTrial2(
  const Value: TPayPalPeriod);
begin
  FPeriodTrial2.Assign(Value);
end;

{ TArcIWPayPalItemPurchase }

procedure TArcIWPayPalItemPurchase.SetDefaultImage(
  const Value: TArcIWDefImageItemPurchase);
begin
  FDefaultImage := Value;
  if (csDesigning in ComponentState) then
    ResizeSelf;
end;

procedure TArcIWPayPalItemPurchase.ResizeSelf;
begin
  if FAutoSize and (FImageURL = '') then
  begin
    Width := ArcIWDefImageItemPurchaseX[Integer(FDefaultImage)];
    Height := ArcIWDefImageItemPurchaseY[Integer(FDefaultImage)];
  end;
end;

{$IFDEF IWVERCLASS5}
function TArcIWPayPalItemPurchase.RenderHTML: TIWHTMLTag;
{$ELSE}
{$IFDEF INTRAWEB70}
function TArcIWPayPalItemPurchase.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ELSE}
function TArcIWPayPalItemPurchase.RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$ENDIF}
var
  i : integer;
begin
  result := TIWHTMLTag.CreateTag('form');
  result.AddStringParam('ID',HTMLName+'_Form');
  result.AddStringParam('CLASS',HTMLName+'_FormCSS');
  result.AddStringParam('action','https://www.paypal.com/cgi-bin/webscr');
  result.AddStringParam('method','post');
  result.AddStringParam('target','paypal');
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','cmd');
    AddStringParam('value','_xclick');
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','business');
    AddStringParam('value',FPaypalAccount);
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','undefined_quantity');
    AddIntegerParam('value',Integer(FAskForQuantity));
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','item_name');
    AddStringParam('value',FItemText);
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','item_number');
    AddStringParam('value',FItemID);
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','amount');
    AddStringParam('value',FloatToStr(FItemAmount));
  end;
  if FLogoURL<>'' then
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','image_url');
      AddStringParam('value',FLogoURL);
    end;
  if FSuccessURL<>'' then
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','return');
      AddStringParam('value',FSuccessURL);
    end;
  if FCancelURL<>'' then
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','cancel_return');
      AddStringParam('value',FCancelURL);
    end;
  if FSpecialInstructions<>'' then
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','cn');
      AddStringParam('value',FSpecialInstructions);
    end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','image');
    AddStringParam('name','submit');
    AddIntegerParam('border',Integer(FUseBorder));
    AddStringParam('alt',FAltText);
    AddStringParam('ID',HTMLName);
    AddStringParam('CLASS',HTMLName+'CSS');
    if not FAutoSize then
    begin
      AddIntegerParam('width',Width);
      AddIntegerParam('height',Height);
    end;
    if FImageURL <> '' then
      AddStringParam('src',FImageURL)
    else
      AddStringParam('src',ArcIWDefImageItemPurchaseURL[Integer(FDefaultImage)]);
  end;
  {
<form action="https://www.paypal.com/cgi-bin/webscr" method="post">
<input type="hidden" name="cmd" value="_xclick">
<input type="hidden" name="business" value="jsouthwell@arcanatech.com">
<input type="hidden" name="undefined_quantity" value="1">
<input type="hidden" name="item_name" value="MySingleItemName">
<input type="hidden" name="item_number" value="MySingleItemID">
<input type="hidden" name="amount" value="$100.00">
<input type="hidden" name="image_url" value="https://MyLogoURL">
<input type="hidden" name="return" value="http://MySuccessURL">
<input type="hidden" name="cancel_return" value="http://MyCancelURL">
<input type="hidden" name="cn" value="MySpecialInstructionsTitle">
<table><tr><td><input type="hidden" name="on0" value="MyDropDownMenu">MyDropDownMenu</td><td><select name="os0"><option value="FirstChoice">FirstChoice<option value="SecondChoice">SecondChoice<option value="ThirdChoice">ThirdChoice</select>
</td></tr><tr><td><input type="hidden" name="on1" value="MyTextOption">MyTextOption</td><td><input type="text" name="os1" maxlength="200"></td></tr></table><input type="image" src="https://www.paypal.com/images/x-click-but23.gif" border="0" name="submit" alt="Make payments with PayPal - it's fast, free and secure!">
</form>
}
end;

constructor TArcIWPayPalItemPurchase.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TArcIWPayPalItemPurchase.Destroy;
begin
  inherited;
end;

{ TArcIWPayPalAddToCart }

constructor TArcIWPayPalAddToCart.Create(AOwner: TComponent);
begin
  inherited;
end;

{$IFDEF IWVERCLASS5}
function TArcIWPayPalAddToCart.RenderHTML: TIWHTMLTag;
{$ELSE}
{$IFDEF INTRAWEB70}
function TArcIWPayPalAddToCart.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ELSE}
function TArcIWPayPalAddToCart.RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$ENDIF}
var
  i : integer;
begin
{
<form target="paypal" action="https://www.paypal.com/cgi-bin/webscr" method="post">
<input type="hidden" name="cmd" value="_cart">
<input type="hidden" name="business" value="jsouthwell@arcanatech.com">
<input type="hidden" name="item_name" value="MyCartItemName">
<input type="hidden" name="item_number" value="MyCartItemID">
<input type="hidden" name="amount" value="$100.00">
<input type="hidden" name="image_url" value="https://MyLogoURL">
<input type="hidden" name="return" value="http://MySuccessURL">
<input type="hidden" name="cancel_return" value="http://MyCancelURL">
<input type="hidden" name="cn" value="MySpecialInstructions">
<table><tr><td><input type="hidden" name="on0" value="MyDropdownMenu">MyDropdownMenu</td><td><select name="os0"><option value="MenuItem1">MenuItem1<option value="MenuItem2">MenuItem2</select>
</td></tr><tr><td><input type="hidden" name="on1" value="MyTExtOption">MyTExtOption</td><td><input type="text" name="os1" maxlength="200"></td></tr></table><input type="image" src="https://www.paypal.com/images/x-click-but22.gif" border="0" name="submit" alt="Make payments with PayPal - it's fast, free and secure!">
<input type="hidden" name="add" value="1">
</form>
}
  result := TIWHTMLTag.CreateTag('form');
  result.AddStringParam('ID',HTMLName+'_Form');
  result.AddStringParam('CLASS',HTMLName+'_FormCSS');
  result.AddStringParam('action','https://www.paypal.com/cgi-bin/webscr');
  result.AddStringParam('method','post');
  result.AddStringParam('target','paypal');
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','cmd');
    AddStringParam('value','_cart');
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','business');
    AddStringParam('value',FPaypalAccount);
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','item_name');
    AddStringParam('value',FItemText);
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','item_number');
    AddStringParam('value',FItemID);
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','amount');
    AddStringParam('value',FloatToStr(FItemAmount));
  end;
  if FLogoURL<>'' then
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','image_url');
      AddStringParam('value',FLogoURL);
    end;
  if FSuccessURL<>'' then
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','return');
      AddStringParam('value',FSuccessURL);
    end;
  if FCancelURL<>'' then
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','cancel_return');
      AddStringParam('value',FCancelURL);
    end;
  if FSpecialInstructions<>'' then
    with result.Contents.AddTag('input') do
    begin
      AddStringParam('type','hidden');
      AddStringParam('name','cn');
      AddStringParam('value',FSpecialInstructions);
    end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','image');
    AddStringParam('name','submit');
    AddIntegerParam('border',Integer(FUseBorder));
    AddStringParam('alt',FAltText);
    AddStringParam('ID',HTMLName);
    AddStringParam('CLASS',HTMLName+'CSS');
    if not FAutoSize then
    begin
      AddIntegerParam('width',Width);
      AddIntegerParam('height',Height);
    end;
    if FImageURL <> '' then
      AddStringParam('src',FImageURL)
    else
      AddStringParam('src',ArcIWDefImageAddToCartURL[Integer(FDefaultImage)]);
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','add');
    AddStringParam('value','1');
  end;
end;

procedure TArcIWPayPalAddToCart.ResizeSelf;
begin
  inherited;
  if FAutoSize and (FImageURL = '') then
  begin
    Width := ArcIWDefImageAddToCartX[Integer(FDefaultImage)];
    Height := ArcIWDefImageAddToCartY[Integer(FDefaultImage)];
  end;
end;

procedure TArcIWPayPalAddToCart.SetDefaultImage(
  const Value: TArcIWDefImageAddToCart);
begin
  FDefaultImage := Value;
  if (csDesigning in ComponentState) then
    ResizeSelf;
end;

{ TArcIWPayPalViewCart }

{<form name="_xclick" target="paypal" action="https://www.paypal.com/cgi-bin/webscr" method="post">
<input type="hidden" name="cmd" value="_cart">
<input type="hidden" name="business" value="jsouthwell@arcanatech.com">
<input type="image" src="https://www.paypal.com/images/view_cart.gif" border="0" name="submit" alt="Make payments with PayPal - it's fast, free and secure!">
<input type="hidden" name="display" value="1">
</form>

https://www.paypal.com/images/view_cart.gif
https://www.paypal.com/images/view_cart_02.gif
https://www.paypal.com/images/view_cart_03.gif
https://www.paypal.com/images/view_cart_04.gif
}
constructor TArcIWPayPalViewCart.Create(AOwner: TComponent);
begin
  inherited;
end;

{$IFDEF IWVERCLASS5}
function TArcIWPayPalViewCart.RenderHTML: TIWHTMLTag;
{$ELSE}
{$IFDEF INTRAWEB70}
function TArcIWPayPalViewCart.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ELSE}
function TArcIWPayPalViewCart.RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$ENDIF}
begin
{
<form name="_xclick" target="paypal" action="https://www.paypal.com/cgi-bin/webscr" method="post">
<input type="hidden" name="cmd" value="_cart">
<input type="hidden" name="business" value="jsouthwell@arcanatech.com">
<input type="image" src="view_cav.gif" border="0" name="submit" alt="Make payments with PayPal - it's fast, free and secure!">
<input type="hidden" name="display" value="1">
</form>
}

  result := TIWHTMLTag.CreateTag('form');
  result.AddStringParam('ID',HTMLName+'_Form');
  result.AddStringParam('CLASS',HTMLName+'_FormCSS');
  result.AddStringParam('name','_xclick');
  result.AddStringParam('action','https://www.paypal.com/cgi-bin/webscr');
  result.AddStringParam('method','post');
  result.AddStringParam('target','paypal');
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','cmd');
    AddStringParam('value','_cart');
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','business');
    AddStringParam('value',FPaypalAccount);
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','hidden');
    AddStringParam('name','display');
    AddStringParam('value','1');
  end;
  with result.Contents.AddTag('input') do
  begin
    AddStringParam('type','image');
    AddStringParam('name','submit');
    AddIntegerParam('border',Integer(FUseBorder));
    AddStringParam('alt',FAltText);
    AddStringParam('ID',HTMLName);
    AddStringParam('CLASS',HTMLName+'CSS');
    if not FAutoSize then
    begin
      AddIntegerParam('width',Width);
      AddIntegerParam('height',Height);
    end;
    if FImageURL <> '' then
      AddStringParam('src',FImageURL)
    else
      AddStringParam('src',ArcIWDefImageViewCartURL[Integer(FDefaultImage)]);
  end;
end;

procedure TArcIWPayPalViewCart.ResizeSelf;
begin
  if FAutoSize and (FImageURL = '') then
  begin
    Width := ArcIWDefImageViewCartX[Integer(FDefaultImage)];
    Height := ArcIWDefImageViewCartY[Integer(FDefaultImage)];
  end;
end;

destructor TArcIWPayPalAddToCart.Destroy;
begin
  inherited;
end;

{ TPayPalPeriod }

procedure TPayPalPeriod.AssignTo(Dest: TPersistent);
begin
  if not (Dest is TPayPalPeriod) then
    raise Exception.Create('You cannot assign this to a TPayPalPeriod');
  TPayPalPeriod(Dest).FAmount := FAmount;
  TPayPalPeriod(Dest).FCycleLength := FCycleLength;
  TPayPalPeriod(Dest).FCycleFreq := FCycleFreq;
end;

{ TArcIWPayPalSubscriptionCancel }

{$IFDEF VERCLASS5}
function TArcIWPayPalSubscriptionCancel.RenderHTML: TIWHTMLTag;
{$ELSE}
{$IFDEF INTRAWEB70}
function TArcIWPayPalSubscriptionCancel.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ELSE}
function TArcIWPayPalSubscriptionCancel.RenderHTML(AContext: TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$ENDIF}
begin
{
  <A HREF="https://www.paypal.com/cgi-bin/webscr?cmd=_subscr-find&alias=jsouthwell%40arcanatech.com">
  <IMG SRC="cancel_v.gif" BORDER="0">
  </A>
}

  Result := TIWHTMLTag.CreateTag('A');
  result.AddStringParam('ID',HTMLName+'_A');
  result.AddStringParam('CLASS',HTMLName+'_ACSS');
  Result.AddStringParam('href',
    'https://www.paypal.com/cgi-bin/webscr?cmd=_subscr-find&alias='+FixText(FPaypalAccount));
  result.AddStringParam('target','paypal');
  with Result.Contents.AddTag('img') do
  begin
    AddIntegerParam('border',Integer(FUseBorder));
    AddStringParam('ID',HTMLName);
    AddStringParam('CLASS',HTMLName+'CSS');
    if FImageURL <> '' then
      AddStringParam('src',FImageURL)
    else
      AddStringParam('src',
        ArcIWDefImageSubscriptionCancelURL[Integer(FDefaultImage)]);
  end;
end;

procedure TArcIWPayPalSubscriptionCancel.ResizeSelf;
begin
  inherited;
  if FAutoSize and (FImageURL = '') then
  begin
    Width := ArcIWDefImageSubscriptionCancelX[Integer(FDefaultImage)];
    Height := ArcIWDefImageSubscriptionCancelY[Integer(FDefaultImage)];
  end;
end;

procedure TArcIWPayPalViewCart.SetDefaultImage(
  const Value: TArcIWDefImageViewCart);
begin
  FDefaultImage := Value;
  if (csDesigning in ComponentState) then
    ResizeSelf;
end;

procedure TArcIWPayPalSubscriptionCancel.SetDefaultImage(
  const Value: TArcIWDefImageSubscriptionCancel);
begin
  FDefaultImage := Value;
  if (csDesigning in ComponentState) then
    ResizeSelf;
end;

end.
