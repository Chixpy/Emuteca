unit uaEmutecaCustomGroup;

{< caEmutecaCustomGroup abstact class unit.

  This file is part of Emuteca Core.

  Copyright (C) 2006-2018 Chixpy

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
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, LazFileUtils,
  // CHX units
  uCHXStrUtils,
  // CHX abstract classes
  uaCHXStorable,
  // Emuteca units
  uEmutecaConst,
  // Emuteca classes
  ucEmutecaPlayingStats;

type

  { caEmutecaCustomGroup }

  caEmutecaCustomGroup = class(caCHXStorableTxt)
  private
    FDeveloper: string;
    FID: string;
    FMediaFileName: string;
    FSortTitle: string;
    FStats: cEmutecaPlayingStats;
    FTitle: string;
    FYear: string;
    function GetMediaFileName: string;
    function GetSortTitle: string;
    function GetTitle: string;
    procedure SetDeveloper(AValue: string);
    procedure SetID(AValue: string);
    procedure SetMediaFileName(AValue: string);
    procedure SetSortTitle(AValue: string);
    procedure SetTitle(AValue: string);
    procedure SetYear(AValue: string);

  protected
    procedure DoSaveToStrLst(aTxtFile: TStrings;
      ExportMode: boolean); virtual;

  public
    function GetActualTitle: string;
    //< Gets actual Title string, not automade
    function GetActualSortTitle: string;
    //< Gets actual SortTitle string, not automade
    function GetActualMediaFilename: string;

    function CompareID(aID: string): integer;
    function MatchID(aID: string): boolean;

    procedure LoadFromStrLst(aTxtFile: TStrings); override;
    procedure ExportToStrLst(aTxtFile: TStrings); virtual;
    procedure SaveToStrLst(aTxtFile: TStrings); override;

    function ExportCommaText: string;
    procedure ImportFrom(aGroup: caEmutecaCustomGroup);

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property ID: string read FID write SetID;
    {< ID. }
    property Title: string read GetTitle write SetTitle;
    {< Name of the group. }
    property SortTitle: string read GetSortTitle write SetSortTitle;
    {< Sort title. }
    property Year: string read FYear write SetYear;
    {< Development year. }
    property Developer: string read FDeveloper write SetDeveloper;
    {< Developer. }
    property MediaFileName: string read GetMediaFileName
      write SetMediaFileName;
    {< Name of media files. }

    property Stats: cEmutecaPlayingStats read FStats;
  end;

  {< This class defines an abstract basic group.

    It stores with all basic properties, but without a software list to avoid
    circular reference with ucEmutecaSoftware, using this in class
    cEmutecaSoftware.CachedGroup property. }

implementation

procedure caEmutecaCustomGroup.SetDeveloper(AValue: string);
begin
  AValue := UTF8Trim(AValue);
  if FDeveloper = AValue then
    Exit;
  FDeveloper := AValue;
end;

function caEmutecaCustomGroup.GetTitle: string;
begin
  if FTitle = '' then
    Result := ID
  else
    Result := FTitle;
end;

function caEmutecaCustomGroup.GetSortTitle: string;
begin
  if FSortTitle = '' then
    Result := Title
  else
    Result := FSortTitle;
end;

function caEmutecaCustomGroup.GetMediaFileName: string;
begin
  if FMediaFileName = '' then
  begin
    Result := CleanFileName(SortTitle, True, False);

    // Removing last dot "Super Mario Bros.", Windows have problems with
    //   removing folders ended with dot
    if Utf8EndsText('.', Result) then
      Result[UTF8LengthFast(Result)] := '_';
  end
  else
    Result := FMediaFileName;
end;

procedure caEmutecaCustomGroup.SetID(AValue: string);
begin
  AValue := UTF8Trim(AValue);
  if FID = AValue then
    Exit;
  FID := AValue;

  FPONotifyObservers(Self, ooChange, nil);
end;

procedure caEmutecaCustomGroup.SetMediaFileName(AValue: string);
begin
  AValue := CleanFileName(AValue, True, False);

  // Removing last dot "Super Mario Bros.", Windows have problems with
  //   removing folders ended with dot
  if Utf8EndsText('.', AValue) then
    AValue[UTF8LengthFast(AValue)] := '_';

  if CompareFilenames(AValue, SortTitle) = 0 then
    FMediaFileName := ''
  else
    FMediaFileName := AValue;
end;

procedure caEmutecaCustomGroup.SetSortTitle(AValue: string);
begin
  AValue := UTF8Trim(AValue);
  if FSortTitle = AValue then
    Exit;

  if UTF8CompareStr(AValue, Title) = 0 then
    FSortTitle := ''
  else
    FSortTitle := AValue;
end;

procedure caEmutecaCustomGroup.SetTitle(AValue: string);
begin
  AValue := UTF8Trim(AValue);

  if FTitle = AValue then
    Exit;

  if UTF8CompareStr(AValue, ID) = 0 then
    FTitle := ''
  else
    FTitle := AValue;
end;

procedure caEmutecaCustomGroup.SetYear(AValue: string);
begin
  AValue := UTF8Trim(AValue);

  if FYear = AValue then
    Exit;
  FYear := AValue;
end;

procedure caEmutecaCustomGroup.DoSaveToStrLst(aTxtFile: TStrings;
  ExportMode: boolean);
begin
  if not assigned(aTxtFile) then
    Exit;

  aTxtFile.Add(ID);
  aTxtFile.Add(GetActualTitle);
  aTxtFile.Add(GetActualSortTitle);
  aTxtFile.Add(Year);
  aTxtFile.Add(Developer);
  aTxtFile.Add(GetActualMediaFilename);

  Stats.WriteToStrLst(aTxtFile, ExportMode);
end;

procedure caEmutecaCustomGroup.ImportFrom(aGroup: caEmutecaCustomGroup);
begin
  if not assigned(aGroup) then
    Exit;

  if aGroup.ID <> krsImportKeepValueKey then
    ID := aGroup.ID;
  if aGroup.Title <> krsImportKeepValueKey then
    Title := aGroup.Title;
  if aGroup.SortTitle <> krsImportKeepValueKey then
    SortTitle := aGroup.SortTitle;
  if aGroup.Year <> krsImportKeepValueKey then
    Year := aGroup.Year;
  if aGroup.Developer <> krsImportKeepValueKey then
    Developer := aGroup.Developer;
  if aGroup.MediaFileName <> krsImportKeepValueKey then
    MediaFileName := aGroup.MediaFileName;
end;

function caEmutecaCustomGroup.GetActualTitle: string;
begin
  Result := FTitle;
end;

function caEmutecaCustomGroup.GetActualSortTitle: string;
begin
  Result := FSortTitle;
end;

function caEmutecaCustomGroup.GetActualMediaFilename: string;
begin
  Result := FMediaFileName;
end;

function caEmutecaCustomGroup.CompareID(aID: string): integer;
begin
  Result := UTF8CompareText(Self.ID, aID);
end;

function caEmutecaCustomGroup.MatchID(aID: string): boolean;
begin
  Result := CompareID(aID) = 0;
end;

procedure caEmutecaCustomGroup.LoadFromStrLst(aTxtFile: TStrings);
begin
  if not assigned(aTxtFile) then
    Exit;

  while aTxtFile.Count < 6 do
    aTxtFile.Add('');

  ID := aTxtFile[0];
  Title := aTxtFile[1];
  SortTitle := aTxtFile[2];
  Year := aTxtFile[3];
  Developer := aTxtFile[4];
  MediaFileName := aTxtFile[5];

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
  FStats := cEmutecaPlayingStats.Create(Self);
end;

destructor caEmutecaCustomGroup.Destroy;
begin
  Stats.Free;
  inherited Destroy;
end;

initialization
  RegisterClass(caEmutecaCustomGroup);

finalization
  UnRegisterClass(caEmutecaCustomGroup);
end.
