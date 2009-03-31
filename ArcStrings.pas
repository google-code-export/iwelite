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
//  unit ArcStrings                                                           //
//    Copyright 2009 by EDV-Beratung Monien                                   //
//    Written By Olaf Monien                                                  //
//                                                                            //
//  Description:                                                              //
//    ArcStrings is a wrapper for string functions from ArcFastStrings.       //
//    All functionality is mapped to SysUtils and StrUtils, as ArcFastStrings //
//    has not been ported to Delphi 2009 / Unicode.                           //
//    StrUtils should be much fast since D2009 but be aware of speed bumps.   //
//  Updates:                                                                  //
//    03/31/2009 - Finished initial release of ArcStrings                     //
//                                                                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit ArcStrings;

interface

function FastReplace(const AText, AFromText, AToText: string; ACaseSensitive: boolean = false): string;

function FastCharPos(const ASource: string; const C: Char; AStartPos: Integer): Integer;

function FastPosNoCase(const aSourceString, aFindString: string; const aSourceLen, aFindLen, StartPos: Integer): Integer;

function FastPos(const aSourceString, aFindString: string; const aSourceLen, aFindLen, StartPos: Integer): Integer;

implementation
uses
  SysUtils,
  StrUtils;

function FastReplace(const AText, AFromText, AToText: string; ACaseSensitive: boolean = false): string;
begin
  if ACaseSensitive then
    result := ReplaceStr(AText, AFromText, AToText)
  else
    result := ReplaceText(AText, AFromText, AToText)
end;

function FastCharPos(const ASource: string; const C: Char; AStartPos: Integer): Integer;
begin
  result := StrUtils.PosEx(C, ASource, AStartPos);
end;

function FastPosNoCase(const ASourceString, AFindString: string; const ASourceLen, AFindLen, StartPos: Integer): Integer;
begin
  result := PosEx(LowerCase(aFindString), LowerCase(ASourceString), StartPos);
end;

function FastPos(const aSourceString, aFindString: string; const aSourceLen, aFindLen, StartPos: Integer): Integer;
begin
  result := PosEx(aFindString, aSourceString, StartPos);
end;

end.

