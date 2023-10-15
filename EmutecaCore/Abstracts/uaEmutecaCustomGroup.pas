unit uaEmutecaCustomGroup;

{< caEmutecaCustomGroup abstact class unit.

  This file is part of Emuteca Core.

  Copyright (C) 2006-2020 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, LazFileUtils,
  // Emuteca units
  uEmutecaConst,
  // Emuteca abstract classes
  uaEmutecaCustomSGItem,
  // Emuteca classes
  ucEmutecaPlayingStats;

type

  { caEmutecaCustomGroup }

  caEmutecaCustomGroup = class(caEmutecaCustomSGItem)
  private
    FDeveloper: string;
    procedure SetDeveloper(AValue: string);

  protected
    procedure DoSaveToStrLst(aTxtFile: TStrings;
      ExportMode: boolean); virtual;

  public
    procedure LoadFromStrLst(aTxtFile: TStrings); override;
    {< Loads Group properties from StringList. }
    procedure ExportToStrLst(aTxtFile: TStrings); virtual;
    {< Saves Group properties to StringList (without custom user data). }
    procedure SaveToStrLst(aTxtFile: TStrings); override;
    {< Saves all Group properties to StringList (to be overrided). }

    function ExportCommaText: string;
    {< Saves Group properties to a comma separated string. }

    procedure ImportFrom(aGroup: caEmutecaCustomGroup);
    {< Copies properties from another Group. }

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published

    property Developer: string read FDeveloper write SetDeveloper;
    {< Developer. }
  end;

  {< This class defines an abstract basic group.

    It stores with all basic properties, but without a software list to avoid
    circular reference with ucEmutecaSoftware, using this class in
    cEmutecaSoftware.CachedGroup property. }

implementation

procedure caEmutecaCustomGroup.SetDeveloper(AValue: string);
begin
  AValue := CleanInfo(AValue);
  if FDeveloper = AValue then
    Exit;
  FDeveloper := AValue;
end;

procedure caEmutecaCustomGroup.DoSaveToStrLst(aTxtFile: TStrings;
  ExportMode: boolean);
begin
  if not assigned(aTxtFile) then
    Exit;

  aTxtFile.Add(Self.ID);
  aTxtFile.Add(GetActualTitle);
  aTxtFile.Add(GetActualSortTitle);
  aTxtFile.Add(Self.Date);
  aTxtFile.Add(Self.Developer);
  aTxtFile.Add(''); // Removed aTxtFile.Add(MediaFilename);

  Stats.WriteToStrLst(aTxtFile, ExportMode);
end;

procedure caEmutecaCustomGroup.ImportFrom(aGroup: caEmutecaCustomGroup);
begin
  if not assigned(aGroup) then
    Exit;

  if aGroup.ID <> krsImportKeepValueKey then
    Self.ID := aGroup.ID;
  if aGroup.Title <> krsImportKeepValueKey then
    Self.Title := aGroup.GetActualTitle;
  if aGroup.SortTitle <> krsImportKeepValueKey then
    Self.SortTitle := aGroup.GetActualSortTitle;
  if aGroup.Date <> krsImportKeepValueKey then
    Self.Date := aGroup.Date;
  if aGroup.Developer <> krsImportKeepValueKey then
    Self.Developer := aGroup.Developer;
  // if aGroup.MediaFileName <> krsImportKeepValueKey then
  //   MediaFileName := aGroup.MediaFileName;
end;

procedure caEmutecaCustomGroup.LoadFromStrLst(aTxtFile: TStrings);
begin
  if not assigned(aTxtFile) then
    Exit;

  while aTxtFile.Count < 6 do
    aTxtFile.Add(krsImportKeepValueKey);

  Self.ID := aTxtFile[0];
  Self.Title := aTxtFile[1];
  Self.SortTitle := aTxtFile[2];
  Self.Date := aTxtFile[3];
  Self.Developer := aTxtFile[4];
  // MediaFileName := aTxtFile[5];

  Stats.LoadFromStrLst(aTxtFile, 6);
  // Next := aTxtFile[9]
end;

procedure caEmutecaCustomGroup.ExportToStrLst(aTxtFile: TStrings);
begin
  DoSaveToStrLst(aTxtFile, True);
end;

procedure caEmutecaCustomGroup.SaveToStrLst(aTxtFile: TStrings);
begin
  DoSaveToStrLst(aTxtFile, False);
end;

function caEmutecaCustomGroup.ExportCommaText: string;
var
  aStringList: TStringList;
begin
  aStringList := TStringList.Create;
  try
    ExportToStrLst(aStringList);
  finally
    Result := aStringList.CommaText;
    FreeAndNil(aStringList);
  end;
end;

constructor caEmutecaCustomGroup.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor caEmutecaCustomGroup.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(caEmutecaCustomGroup);

finalization
  UnRegisterClass(caEmutecaCustomGroup);
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
