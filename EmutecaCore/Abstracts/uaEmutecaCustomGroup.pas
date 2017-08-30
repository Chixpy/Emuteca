{ This file is part of Emuteca

  Copyright (C) 2006-2017 Chixpy

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
unit uaEmutecaCustomGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LazUTF8,
  uaCHXStorable, uCHXStrUtils,
  uEmutecaCommon,
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


  public
    function GetActualTitle: string;
    //< Gets actual Title string, not automade
    function GetActualSortTitle: string;
    //< Gets actual SortTitle string, not automade

    function CompareID(aID: string): integer;
    function MatchID(aID: string): boolean;

    procedure LoadFromIni(aIniFile: TIniFile); override;
    procedure LoadFromStrLst(aTxtFile: TStrings); override;
    procedure SaveToIni(aIniFile: TMemIniFile; const ExportMode: boolean);
      override;
    procedure SaveToStrLst(aTxtFile: TStrings; const ExportMode: boolean);
      override;

    procedure ImportFrom(aGroup: caEmutecaCustomGroup);

    procedure SearchAllRelatedFiles(OutFileList: TStrings;
      aFolder: string; Extensions: TStrings; AutoExtract: boolean); virtual;
    function SearchFirstRelatedFile(aFolder: string;
      Extensions: TStrings; AutoExtract: boolean): string; virtual;

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
    property MediaFileName: string read GetMediaFileName write SetMediaFileName;
    {< Name of media files. }


    property Stats: cEmutecaPlayingStats read FStats;
  end;

implementation

{ caEmutecaCustomGroup }

procedure caEmutecaCustomGroup.SetDeveloper(AValue: string);
begin
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
    MediaFileName := Title;
  Result := FMediaFileName;
end;

procedure caEmutecaCustomGroup.SetID(AValue: string);
begin
  if FID = AValue then
    Exit;
  FID := AValue;

  FPONotifyObservers(Self, ooChange, nil);
end;

procedure caEmutecaCustomGroup.SetMediaFileName(AValue: string);
begin
  if AValue = '' then
    FMediaFileName := CleanFileName(Title, True, False)
  else
    FMediaFileName := CleanFileName(AValue, True, False);
end;

procedure caEmutecaCustomGroup.SetSortTitle(AValue: string);
begin
  if FSortTitle = AValue then
    Exit;

  if UTF8CompareStr(AValue, Title) = 0 then
    FSortTitle := ''
  else
    FSortTitle := AValue;
end;

procedure caEmutecaCustomGroup.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;

  if UTF8CompareStr(AValue, ID) = 0 then
    FTitle := ''
  else
    FTitle := AValue;
end;

procedure caEmutecaCustomGroup.SetYear(AValue: string);
begin
  if FYear = AValue then
    Exit;
  FYear := AValue;
end;

procedure caEmutecaCustomGroup.ImportFrom(aGroup: caEmutecaCustomGroup);
begin
  if not assigned(aGroup) then
    Exit;

  ID := aGroup.ID;
  Title := aGroup.Title;
  SortTitle := aGroup.SortTitle;
  Year := aGroup.Year;
  Developer := aGroup.Developer;
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

function caEmutecaCustomGroup.CompareID(aID: string): integer;
begin
  Result := UTF8CompareText(ID, aID);
end;

function caEmutecaCustomGroup.MatchID(aID: string): boolean;
begin
  Result := CompareID(aID) = 0;
end;

procedure caEmutecaCustomGroup.LoadFromIni(aIniFile: TIniFile);
begin
  if aIniFile = nil then
    Exit;

  Title := aIniFile.ReadString(ID, krsIniKeyTitle, GetActualTitle);
  SortTitle := aIniFile.ReadString(ID, krsIniKeySortTitle, GetActualSortTitle);
  Year := aIniFile.ReadString(ID, krsIniKeyYear, Year);
  Developer := aIniFile.ReadString(ID, krsIniKeyDeveloper, Developer);
  MediaFileName := aIniFile.ReadString(ID, krsIniKeyFileName,
    MediaFileName);

  Stats.LoadFromIni(aIniFile, ID);
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

procedure caEmutecaCustomGroup.SaveToIni(aIniFile: TMemIniFile;
  const ExportMode: boolean);
begin
  if aIniFile = nil then
    Exit;

  aIniFile.WriteString(ID, krsIniKeyTitle, GetActualTitle);
  aIniFile.WriteString(ID, krsIniKeySortTitle, GetActualSortTitle);
  aIniFile.WriteString(ID, krsIniKeyYear, Year);
  aIniFile.WriteString(ID, krsIniKeyDeveloper, Developer);
  aIniFile.WriteString(ID, krsIniKeyFileName, MediaFileName);

  Stats.WriteToIni(aIniFile, ID, ExportMode);
end;

procedure caEmutecaCustomGroup.SaveToStrLst(aTxtFile: TStrings;
  const ExportMode: boolean);
begin
  if not assigned(aTxtFile) then
    Exit;

  aTxtFile.Add(ID);
  aTxtFile.Add(GetActualTitle);
  aTxtFile.Add(GetActualSortTitle);
  aTxtFile.Add(Year);
  aTxtFile.Add(Developer);
  aTxtFile.Add(MediaFileName);

  Stats.WriteToStrLst(aTxtFile, ExportMode);
end;

procedure caEmutecaCustomGroup.SearchAllRelatedFiles(OutFileList: TStrings;
  aFolder: string; Extensions: TStrings; AutoExtract: boolean);
begin
  // HACK: Dot added to MediaFileName, to preserve dots in ids like "Super Mario Bros."
  EmuTKSearchAllRelatedFiles(OutFileList, aFolder, MediaFileName + '.', Extensions,
    False, '');
end;

function caEmutecaCustomGroup.SearchFirstRelatedFile(aFolder: string;
  Extensions: TStrings; AutoExtract: boolean): string;
begin
  // HACK: Dot added to MediaFileName, to preserve dots in ids like "Super Mario Bros."
  Result := EmuTKSearchFirstRelatedFile(aFolder, MediaFileName + '.',
    Extensions, False, False, '');
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
