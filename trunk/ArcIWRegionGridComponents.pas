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

unit ArcIWRegionGridComponents;

{$I IntraWebVersion.inc}
{$I Eval.inc}

{$IFDEF INTRAWEB51}
  ERROR: This unit is only valid for IW 7
{$ENDIF}

interface

uses
  SysUtils, Classes, Controls, Graphics, IWHTMLTag, IWControl, IWColor, IWTypes,
  IWVCLBaseControl, IWBaseControl, IWBaseHTMLControl, IWRenderContext, IWApplication,
  ArcIWGridCommon, ArcIWCustomGrid, IWRegion, DB, TypInfo, IWBaseRenderContext,
  IWBaseInterfaces, IWBaseContainerLayout, IWLayoutMgrForm, IWVCLComponent,
  IWMarkupLanguageTag, Forms, IWContainerLayout, IWBaseHTMLInterfaces,
  ArcIWRegionGrid, IWCompButton, IWHTMLControls
  {$IFDEF INTRAWEB120}, IWRenderStream, IWCompExtCtrls, {$ELSE}, IWStreams, IWExtCtrls, {$ENDIF} ArcCommon;

type
  TArcIWRGLink = class(TIWLink, IIWSubmitInvisible)
  private
    Region : TIWGridRegion;
    Grid : TArcIWRegionGrid;
    FMoveDataset: boolean;
  protected
    procedure DoClick; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MoveDataset : boolean read FMoveDataset write FMoveDataset;
  end;

  TArcIWRGImage = class(TIWImage, IIWSubmitInvisible)
  private
    Region : TIWGridRegion;
    Grid : TArcIWRegionGrid;
    FMoveDataset: boolean;
  protected
    procedure DoClick; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MoveDataset : boolean read FMoveDataset write FMoveDataset;
  end;

  TArcIWRGImageFile = class(TIWImageFile, IIWSubmitInvisible)
  private
    Region : TIWGridRegion;
    Grid : TArcIWRegionGrid;
    FMoveDataset: boolean;
  protected
    procedure DoClick; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MoveDataset : boolean read FMoveDataset write FMoveDataset;
  end;

  TArcIWRGButton = class(TIWButton, IIWSubmitInvisible)
  private
    Region : TIWGridRegion;
    Grid : TArcIWRegionGrid;
    FMoveDataset: boolean;
  protected
    procedure DoClick; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MoveDataset : boolean read FMoveDataset write FMoveDataset;
  end;

implementation

{ TArcIWRGButton }

constructor TArcIWRGButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);// This is dangerous I know but needs to happen to fix "Cannot find submit component"...
  if (not (csDesigning in ComponentState)) and
     (AOwner is TIWGridRegion) then
  begin
    Region := TIWGridRegion(AOwner);
    Grid := Region.Grid;
  end else
  begin
    Region := nil;
    Grid := nil;
  end;
end;

procedure TArcIWRGButton.DoClick;
begin
  if Region <> nil then
  begin
    if FMoveDataset and (Grid.DataSource.DataSet.Bookmark <> Region.Bookmark) then
      Grid.DataSource.DataSet.Bookmark := Region.Bookmark;
  end;
  inherited;
end;

{ TArcIWRGLink }

constructor TArcIWRGLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);// This is dangerous I know but needs to happen to fix "Cannot find submit component"...
  if (not (csDesigning in ComponentState)) and
     (AOwner is TIWGridRegion) then
  begin
    Region := TIWGridRegion(AOwner);
    Grid := Region.Grid;
  end else
  begin
    Region := nil;
    Grid := nil;
  end;
end;

procedure TArcIWRGLink.DoClick;
begin
  if Region <> nil then
  begin
    if FMoveDataset and (Grid.DataSource.DataSet.Bookmark <> Region.Bookmark) then
      Grid.DataSource.DataSet.Bookmark := Region.Bookmark;
  end;
  inherited;
end;

{ TArcIWRGImage }

constructor TArcIWRGImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);// This is dangerous I know but needs to happen to fix "Cannot find submit component"...
  if (not (csDesigning in ComponentState)) and
     (AOwner is TIWGridRegion) then
  begin
    Region := TIWGridRegion(AOwner);
    Grid := Region.Grid;
  end else
  begin
    Region := nil;
    Grid := nil;
  end;
end;

procedure TArcIWRGImage.DoClick;
begin
  if Region <> nil then
  begin
    if FMoveDataset and (Grid.DataSource.DataSet.Bookmark <> Region.Bookmark) then
      Grid.DataSource.DataSet.Bookmark := Region.Bookmark;
  end;
  inherited;
end;

{ TArcIWRGImageFile }

constructor TArcIWRGImageFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);// This is dangerous I know but needs to happen to fix "Cannot find submit component"...
  if (not (csDesigning in ComponentState)) and
     (AOwner is TIWGridRegion) then
  begin
    Region := TIWGridRegion(AOwner);
    Grid := Region.Grid;
  end else
  begin
    Region := nil;
    Grid := nil;
  end;
end;

procedure TArcIWRGImageFile.DoClick;
begin
  if Region <> nil then
  begin
    if FMoveDataset and (Grid.DataSource.DataSet.Bookmark <> Region.Bookmark) then
      Grid.DataSource.DataSet.Bookmark := Region.Bookmark;
  end;
  inherited;
end;

end.
