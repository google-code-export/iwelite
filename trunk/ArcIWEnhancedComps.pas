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

unit ArcIWEnhancedComps;

interface

{$I IntrawebVersion.inc}

uses
  SysUtils, Classes, IWCompEdit, IWColor, IWFont, ArcIWEnhancedInterface,
  IWHTMLTag, IWBaseControl, IWControl, IWCompCalendar, IWCompActiveX,
  IWCompButton, IWCompCheckBox, IWCompFlash, IWCompLabel,
  IWCompListbox, IWCompMemo, IWCompMPEG, IWCOmpProgressBar, IWCompQuicktime,
  IWCompText, IWHTMLControls{$IFNDEF INTRAWEB51}, IWRenderContext{$ENDIF}
  {$IFDEF INTRAWEB71}, IWCompTimeEdit, IWCompOrderedListbox{$ENDIF}, ArcCommon
  {$IFDEF INTRAWEB120}, IWCompExtCtrls {$ELSE}, IWExtCtrls {$ENDIF};

type
  TArcIWEnhEdit = class(TIWEdit)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhCalendar = class(TIWCalendar)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhActiveX = class(TIWActiveX)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhButton = class(TIWButton)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhCheckBox = class(TIWCheckBox)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhFlash = class(TIWFlash)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhLabel = class(TIWLabel)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhListbox = class(TIWListbox)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhComboBox = class(TIWComboBox)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhMemo = class(TIWMemo)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhMPEG = class(TIWMPEG)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhProgressBar = class(TIWProgressBar)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhQuicktime = class(TIWQuicktime)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhText = class(TIWText)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhRadioGroup = class(TIWRadioGroup)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhImage = class(TIWImage)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhImageFile = class(TIWImageFile)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhHRule = class(TIWHRule)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhList = class(TIWList)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhLink = class(TIWLink)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhURL = class(TIWURL)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhApplet = class(TIWApplet)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  TArcIWEnhURLWindow = class(TIWURLWindow)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    {$IFNDEF INTRAWEB51}
    procedure IWPaint; override;
    {$ELSE}
    procedure Paint; override;
    {$ENDIF}
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF INTRAWEB51}
    function RenderHTML: TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB60}
    function RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag; override;
    {$ENDIF}
    {$IFDEF INTRAWEB70}
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
    {$ENDIF}
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;

  {$IFDEF INTRAWEB71}
  TArcIWEnhTimeEdit = class(TIWTimeEdit)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    procedure IWPaint; override;
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;
  {$ENDIF}

  {$IFDEF INTRAWEB71}
  TArcIWEnhOrderedListBox = class(TIWOrderedListBox)
  private
    FEnhancer : TArcIWEnhancer;
    procedure SetEnhancer(const Value: TArcIWEnhancer);
  protected
    procedure IWPaint; override;
    procedure Resize; override;
    procedure Loaded; override;
    function get_ShouldRenderTabOrder: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag; override;
  published
    property Enhancer : TArcIWEnhancer read FEnhancer write SetEnhancer;
  end;
  {$ENDIF}

implementation

{ TArcIWEnhEdit }

constructor TArcIWEnhEdit.Create(AOwner: TComponent);
begin
  inherited;
  Width := 209;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhEdit.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhEdit.get_ShouldRenderTabOrder : boolean;
begin
  result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhEdit.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhEdit.IWPaint;
{$ELSE}
procedure TArcIWEnhEdit.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhEdit.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhEdit.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhEdit.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhEdit.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhEdit.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhCalendar }

constructor TArcIWEnhCalendar.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhCalendar.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhCalendar.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhCalendar.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhCalendar.IWPaint;
{$ELSE}
procedure TArcIWEnhCalendar.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhCalendar.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhCalendar.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhCalendar.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhCalendar.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhCalendar.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhActiveX }

constructor TArcIWEnhActiveX.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhActiveX.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhActiveX.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhActiveX.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhActiveX.IWPaint;
{$ELSE}
procedure TArcIWEnhActiveX.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhActiveX.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhActiveX.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhActiveX.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhActiveX.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhActiveX.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhButton }

constructor TArcIWEnhButton.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhButton.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhButton.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhButton.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhButton.IWPaint;
{$ELSE}
procedure TArcIWEnhButton.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhButton.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhButton.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhButton.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhButton.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhButton.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhCheckBox }

constructor TArcIWEnhCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhCheckBox.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhCheckBox.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhCheckBox.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhCheckBox.IWPaint;
{$ELSE}
procedure TArcIWEnhCheckBox.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhCheckBox.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhCheckBox.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhCheckBox.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhCheckBox.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhCheckBox.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhFlash }

constructor TArcIWEnhFlash.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhFlash.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhFlash.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhFlash.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhFlash.IWPaint;
{$ELSE}
procedure TArcIWEnhFlash.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhFlash.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhFlash.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhFlash.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhFlash.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhFlash.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhLabel }

constructor TArcIWEnhLabel.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhLabel.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhLabel.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhLabel.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhLabel.IWPaint;
{$ELSE}
procedure TArcIWEnhLabel.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhLabel.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhLabel.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhLabel.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhLabel.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhLabel.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhListbox }

constructor TArcIWEnhListBox.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhListBox.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhListBox.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhListBox.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhListBox.IWPaint;
{$ELSE}
procedure TArcIWEnhListBox.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhListBox.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhListBox.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhListBox.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhListBox.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhListBox.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhComboBox }

constructor TArcIWEnhComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhComboBox.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhComboBox.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhComboBox.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhComboBox.IWPaint;
{$ELSE}
procedure TArcIWEnhComboBox.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhComboBox.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhComboBox.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhComboBox.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhComboBox.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhComboBox.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhMemo }

constructor TArcIWEnhMemo.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhMemo.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhMemo.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhMemo.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhMemo.IWPaint;
{$ELSE}
procedure TArcIWEnhMemo.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhMemo.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhMemo.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhMemo.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhMemo.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhMemo.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhMPEG }

constructor TArcIWEnhMPEG.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhMPEG.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhMPEG.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhMPEG.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhMPEG.IWPaint;
{$ELSE}
procedure TArcIWEnhMPEG.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhMPEG.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhMPEG.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhMPEG.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhMPEG.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhMPEG.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhProgressBar }

constructor TArcIWEnhProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhProgressBar.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhProgressBar.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhProgressBar.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhProgressBar.IWPaint;
{$ELSE}
procedure TArcIWEnhProgressBar.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhProgressBar.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhProgressBar.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhProgressBar.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhProgressBar.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhProgressBar.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhQuicktime }

constructor TArcIWEnhQuicktime.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhQuicktime.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhQuicktime.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhQuicktime.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhQuicktime.IWPaint;
{$ELSE}
procedure TArcIWEnhQuicktime.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhQuicktime.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhQuicktime.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhQuicktime.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhQuicktime.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhQuicktime.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhText }

constructor TArcIWEnhText.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhText.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhText.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhText.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhText.IWPaint;
{$ELSE}
procedure TArcIWEnhText.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhText.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhText.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhText.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhText.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhText.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhRadioGroup }

constructor TArcIWEnhRadioGroup.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhRadioGroup.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhRadioGroup.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhRadioGroup.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhRadioGroup.IWPaint;
{$ELSE}
procedure TArcIWEnhRadioGroup.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhRadioGroup.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhRadioGroup.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhRadioGroup.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhRadioGroup.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhRadioGroup.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhImage }

constructor TArcIWEnhImage.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhImage.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhImage.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhImage.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhImage.IWPaint;
{$ELSE}
procedure TArcIWEnhImage.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhImage.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhImage.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhImage.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhImage.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhImage.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhImageFile }

constructor TArcIWEnhImageFile.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhImageFile.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhImageFile.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhImageFile.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhImageFile.IWPaint;
{$ELSE}
procedure TArcIWEnhImageFile.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhImageFile.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhImageFile.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhImageFile.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhImageFile.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhImageFile.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhHRule }

constructor TArcIWEnhHRule.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhHRule.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhHRule.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhHRule.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhHRule.IWPaint;
{$ELSE}
procedure TArcIWEnhHRule.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhHRule.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhHRule.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhHRule.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhHRule.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhHRule.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhList }

constructor TArcIWEnhList.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhList.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhList.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhList.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhList.IWPaint;
{$ELSE}
procedure TArcIWEnhList.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhList.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhList.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhList.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhList.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhList.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhLink }

constructor TArcIWEnhLink.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhLink.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhLink.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhLink.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhLink.IWPaint;
{$ELSE}
procedure TArcIWEnhLink.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhLink.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhLink.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhLink.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhLink.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhLink.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhURL }

constructor TArcIWEnhURL.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhURL.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhURL.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhURL.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhURL.IWPaint;
{$ELSE}
procedure TArcIWEnhURL.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhURL.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhURL.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhURL.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhURL.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhURL.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhApplet }

constructor TArcIWEnhApplet.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhApplet.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhApplet.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhApplet.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhApplet.IWPaint;
{$ELSE}
procedure TArcIWEnhApplet.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhApplet.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhApplet.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhApplet.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhApplet.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhApplet.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhURLWindow }

constructor TArcIWEnhURLWindow.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhURLWindow.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhURLWindow.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhURLWindow.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhURLWindow.IWPaint;
{$ELSE}
procedure TArcIWEnhURLWindow.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhURLWindow.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhURLWindow.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhURLWindow.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhURLWindow.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhURLWindow.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhTimeEdit }
{$IFDEF INTRAWEB71}
constructor TArcIWEnhTimeEdit.Create(AOwner: TComponent);
begin
  inherited;
  Width := 209;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhTimeEdit.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhTimeEdit.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhTimeEdit.IWPaint;
begin
  FEnhancer.Paint;
end;

procedure TArcIWEnhTimeEdit.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

function TArcIWEnhTimeEdit.RenderHTML(
  AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
begin
  Result := FEnhancer.Render(AContext, inherited RenderHTML(AContext));
end;

procedure TArcIWEnhTimeEdit.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhTimeEdit.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhOrderedListBox }

constructor TArcIWEnhOrderedListBox.Create(AOwner: TComponent);
begin
  inherited;
  Width := 209;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhOrderedListBox.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhOrderedListBox.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhOrderedListBox.IWPaint;
begin
  FEnhancer.Paint;
end;

procedure TArcIWEnhOrderedListBox.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

function TArcIWEnhOrderedListBox.RenderHTML(
  AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
begin
  Result := FEnhancer.Render(AContext, inherited RenderHTML(AContext));
end;

procedure TArcIWEnhOrderedListBox.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhOrderedListBox.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;
{$ENDIF}

end.
