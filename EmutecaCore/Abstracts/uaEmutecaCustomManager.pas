unit uaEmutecaCustomManager;
{< caEmutecaCustomManagerTxt class unit.

  This file is part of Emuteca Core.

  Copyright (C) 2006-2018 Chixpy
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LazFileUtils, LazUTF8,
  // CHX abstracts
  uaCHXStorable,
  // Emuteca units
  uEmutecaConst;

type

  { caEmutecaCustomManagerTxt }

  caEmutecaCustomManagerTxt = class(caCHXStorableTxt)
  private
    FProgressCallBack: TEmutecaProgressCallBack;

  protected
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack); virtual;

  public
    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressCallBack;

    procedure ImportFromFile(const aFilename: string); virtual;
    procedure ImportFromStrLst(aTxtFile: TStrings); virtual; abstract;
    procedure ExportToFile(const aFilename: string; ClearFile: boolean); virtual;
    procedure ExportToStrLst(aTxtFile: TStrings); virtual; abstract;
  end;

  { caEmutecaCustomManagerIni }

  caEmutecaCustomManagerIni = class(caCHXStorableIni)
  private
    FProgressCallBack: TEmutecaProgressCallBack;

  protected
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack); virtual;

  public
    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressCallBack;

    procedure ImportFromFile(const aFilename: string); virtual;
    procedure ImportFromIni(aIniFile: TMemIniFile); virtual; abstract;
    procedure ExportToFile(const aFilename: string; ClearFile: boolean); virtual;
    procedure ExportToIni(aIniFile: TMemIniFile); virtual; abstract;
  end;

implementation

{ caEmutecaCustomManagerIni }

procedure caEmutecaCustomManagerIni.SetProgressCallBack(
  AValue: TEmutecaProgressCallBack);
begin
  if FProgressCallBack = AValue then
    Exit;
  FProgressCallBack := AValue;
end;

procedure caEmutecaCustomManagerIni.ImportFromFile(const aFilename: string);
begin
  DoFileOpen(aFilename, @ImportFromIni, True, False, False);
end;

procedure caEmutecaCustomManagerIni.ExportToFile(const aFilename: string;
  ClearFile: boolean);
begin
  DoFileOpen(aFilename, @ExportToIni, False, ClearFile, True);
end;

{ caEmutecaCustomManagerTxt }

procedure caEmutecaCustomManagerTxt.SetProgressCallBack(
  AValue: TEmutecaProgressCallBack);
begin
  if FProgressCallBack = AValue then
    Exit;
  FProgressCallBack := AValue;
end;

procedure caEmutecaCustomManagerTxt.ImportFromFile(const aFilename: string);
begin
  DoFileOpen(aFilename, @ImportFromStrLst, True, False, False);
end;

procedure caEmutecaCustomManagerTxt.ExportToFile(const aFilename: string;
  ClearFile: boolean);
begin
  DoFileOpen(aFilename, @ExportToStrLst, False, ClearFile, True);
end;

end.
{
This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3 of the License, or (at your option)
any later version.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

A copy of the GNU General Public License is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA.
}
