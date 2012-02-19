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

unit ArcIWEnhancedDBComps;

interface

{$I Intrawebversion.inc}

uses
  SysUtils, Classes, IWColor, IWFont, ArcIWEnhancedInterface,
  IWHTMLTag, IWBaseControl, IWControl, IWDBStdCtrls, IWDBExtCtrls, IWDBGrids
  {$IFNDEF INTRAWEB51}, IWRenderContext{$ENDIF}, ArcCommon;

type
  TArcIWEnhDBImage = class(TIWDBImage)
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

  TArcIWEnhDBRadioGroup = class(TIWDBRadioGroup)
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

  TArcIWEnhDBCheckBox = class(TIWDBCheckBox)
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

  TArcIWEnhDBComboBox = class(TIWDBComboBox)
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

  TArcIWEnhDBEdit = class(TIWDBEdit)
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

  TArcIWEnhDBFile = class(TIWDBFile)
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

  TArcIWEnhDBLabel = class(TIWDBLabel)
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

  TArcIWEnhDBListbox = class(TIWDBListbox)
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

  TArcIWEnhDBLookupComboBox = class(TIWDBLookupComboBox)
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

  TArcIWEnhDBLookupListBox = class(TIWDBLookupListBox)
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

  TArcIWEnhDBMemo = class(TIWDBMemo)
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

  TArcIWEnhDBNavigator = class(TIWDBNavigator)
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

  TArcIWEnhDBText = class(TIWDBText)
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

implementation

{ TArcIWEnhDBImage }

constructor TArcIWEnhDBImage.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhDBImage.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhDBImage.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhDBImage.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhDBImage.IWPaint;
{$ELSE}
procedure TArcIWEnhDBImage.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhDBImage.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhDBImage.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhDBImage.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhDBImage.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhDBImage.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhDBRadioGroup }

constructor TArcIWEnhDBRadioGroup.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhDBRadioGroup.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhDBRadioGroup.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhDBRadioGroup.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhDBRadioGroup.IWPaint;
{$ELSE}
procedure TArcIWEnhDBRadioGroup.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhDBRadioGroup.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhDBRadioGroup.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhDBRadioGroup.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhDBRadioGroup.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhDBRadioGroup.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhDBCheckBox }

constructor TArcIWEnhDBCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhDBCheckBox.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhDBCheckBox.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhDBCheckBox.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhDBCheckBox.IWPaint;
{$ELSE}
procedure TArcIWEnhDBCheckBox.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhDBCheckBox.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhDBCheckBox.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhDBCheckBox.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhDBCheckBox.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhDBCheckBox.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhDBComboBox }

constructor TArcIWEnhDBComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhDBComboBox.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhDBComboBox.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhDBComboBox.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhDBComboBox.IWPaint;
{$ELSE}
procedure TArcIWEnhDBComboBox.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhDBComboBox.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhDBComboBox.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhDBComboBox.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhDBComboBox.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhDBComboBox.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhDBEdit }

constructor TArcIWEnhDBEdit.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhDBEdit.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhDBEdit.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhDBEdit.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhDBEdit.IWPaint;
{$ELSE}
procedure TArcIWEnhDBEdit.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhDBEdit.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhDBEdit.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhDBEdit.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhDBEdit.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhDBEdit.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhDBFile }

constructor TArcIWEnhDBFile.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhDBFile.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhDBFile.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhDBFile.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhDBFile.IWPaint;
{$ELSE}
procedure TArcIWEnhDBFile.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhDBFile.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhDBFile.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhDBFile.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhDBFile.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhDBFile.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhDBLabel }

constructor TArcIWEnhDBLabel.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhDBLabel.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhDBLabel.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhDBLabel.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhDBLabel.IWPaint;
{$ELSE}
procedure TArcIWEnhDBLabel.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhDBLabel.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhDBLabel.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhDBLabel.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhDBLabel.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhDBLabel.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhDBListbox }

constructor TArcIWEnhDBListbox.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhDBListbox.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhDBListbox.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhDBListbox.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhDBListbox.IWPaint;
{$ELSE}
procedure TArcIWEnhDBListbox.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhDBListbox.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhDBListbox.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhDBListbox.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhDBListbox.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhDBListbox.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhDBLookupComboBox }

constructor TArcIWEnhDBLookupComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhDBLookupComboBox.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhDBLookupComboBox.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhDBLookupComboBox.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhDBLookupComboBox.IWPaint;
{$ELSE}
procedure TArcIWEnhDBLookupComboBox.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhDBLookupComboBox.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhDBLookupComboBox.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhDBLookupComboBox.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhDBLookupComboBox.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhDBLookupComboBox.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhDBLookupListBox }

constructor TArcIWEnhDBLookupListBox.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhDBLookupListBox.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhDBLookupListBox.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhDBLookupListBox.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhDBLookupListBox.IWPaint;
{$ELSE}
procedure TArcIWEnhDBLookupListBox.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhDBLookupListBox.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhDBLookupListBox.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhDBLookupListBox.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhDBLookupListBox.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhDBLookupListBox.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhDBMemo }

constructor TArcIWEnhDBMemo.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhDBMemo.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhDBMemo.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhDBMemo.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhDBMemo.IWPaint;
{$ELSE}
procedure TArcIWEnhDBMemo.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhDBMemo.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhDBMemo.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhDBMemo.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhDBMemo.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhDBMemo.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhDBNavigator }

constructor TArcIWEnhDBNavigator.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhDBNavigator.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhDBNavigator.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhDBNavigator.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhDBNavigator.IWPaint;
{$ELSE}
procedure TArcIWEnhDBNavigator.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhDBNavigator.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhDBNavigator.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhDBNavigator.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhDBNavigator.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhDBNavigator.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

{ TArcIWEnhDBText }

constructor TArcIWEnhDBText.Create(AOwner: TComponent);
begin
  inherited;
  FEnhancer := TArcIWEnhancer.Create(Self, (csDesigning in ComponentState));
end;

destructor TArcIWEnhDBText.Destroy;
begin
  FEnhancer.Free;
  inherited;
end;

function TArcIWEnhDBText.get_ShouldRenderTabOrder : boolean;
begin
  Result := FEnhancer.get_ShouldRenderTabOrder;
end;

procedure TArcIWEnhDBText.Loaded;
begin
  inherited;
  FEnhancer.Loaded;
end;

{$IFNDEF INTRAWEB51}
procedure TArcIWEnhDBText.IWPaint;
{$ELSE}
procedure TArcIWEnhDBText.Paint;
{$ENDIF}
begin
  FEnhancer.Paint;
end;

{$IFDEF INTRAWEB51}
function TArcIWEnhDBText.RenderHTML: TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB60}
function TArcIWEnhDBText.RenderHTML(AContext : TIWBaseComponentContext): TIWHTMLTag;
{$ENDIF}
{$IFDEF INTRAWEB70}
function TArcIWEnhDBText.RenderHTML(AContext : TIWBaseHTMLComponentContext): TIWHTMLTag;
{$ENDIF}
begin
  Result := FEnhancer.Render({$IFNDEF INTRAWEB51}AContext,{$ENDIF}inherited RenderHTML{$IFNDEF INTRAWEB51}(AContext){$ENDIF});
end;

procedure TArcIWEnhDBText.Resize;
begin
  inherited;
  if Assigned(FEnhancer) then
    FEnhancer.Resize;
end;

procedure TArcIWEnhDBText.SetEnhancer(const Value: TArcIWEnhancer);
begin
  FEnhancer.Assign(Value);
end;

end.
