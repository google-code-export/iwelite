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

unit ArcIWEnhancedReg;

interface

{$I IntraWebVersion.inc}

uses Classes, SysUtils, {ArcIWEnhancedPropertyEditors, DesignIntf, ColnEdit,}
  ArcIWEnhancedComps, ArcIWEnhancedDBComps, ArcIWEnhancedInterface;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('IWES Visual',[TArcIWEnhEdit, TArcIWEnhCalendar,
    TArcIWEnhEdit, TArcIWEnhCalendar, TArcIWEnhActiveX, TArcIWEnhButton,
    TArcIWEnhCheckBox, TArcIWEnhFlash, TArcIWEnhLabel, TArcIWEnhListbox,
    TArcIWEnhComboBox, TArcIWEnhMemo, TArcIWEnhMPEG, TArcIWEnhProgressBar,
    TArcIWEnhQuicktime, TArcIWEnhText, TArcIWEnhRadioGroup, TArcIWEnhImage,
    TArcIWEnhImageFile, TArcIWEnhHRule, TArcIWEnhLink, TArcIWEnhURL,
    TArcIWEnhURLWindow, TArcIWEnhApplet, TArcIWEnhList
    {$IFDEF INTRAWEB71}, TArcIWEnhTimeEdit, TArcIWEnhOrderedListBox{$ENDIF}]);

  RegisterComponents('IWES Visual DB',[TArcIWEnhDBImage, TArcIWEnhDBRadioGroup,
    TArcIWEnhDBCheckBox, TArcIWEnhDBComboBox, TArcIWEnhDBEdit, TArcIWEnhDBFile,
    TArcIWEnhDBLabel, TArcIWEnhDBListbox, TArcIWEnhDBLookupComboBox,
    TArcIWEnhDBLookupListBox, TArcIWEnhDBMemo, TArcIWEnhDBNavigator,
    TArcIWEnhDBText]);

  //RegisterPropertyEditor(TypeInfo(TArcIWShadows), nil, '', TIWShadowsPropertyEditor);
end;

end.

