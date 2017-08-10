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
  uaCHXStorable,
  uEmutecaCommon,
  ucEmutecaPlayingStats;

const
  krsCSVGroupHeader = '"ID","Title","System","Year","Developer"';
  krsCSVGroupStatsHeader = krsCSVGroupHeader + ',' + krsCSVStatsHeader;

type

  { caEmutecaCustomGroup }

  caEmutecaCustomGroup = class(caCHXStorableTxt)
  private
    FDeveloper: string;
    FID: string;
    FStats: cEmutecaPlayingStats;
    FTitle: string;
    FYear: string;
    function GetTitle: string;
    procedure SetDeveloper(AValue: string);
    procedure SetID(AValue: string);
    procedure SetTitle(AValue: string);
    procedure SetYear(AValue: string);

  public
    function GetActualTitle: string;
    //< Gets actual Title string, not automade

    function MatchID(aID: string): boolean;

    procedure LoadFromIni(aIniFile: TMemIniFile); override;
    procedure LoadFromStrLst(aTxtFile: TStrings); override;
    procedure SaveToIni(aIniFile: TMemIniFile; const ExportMode: boolean);
      override;
    procedure SaveToStrLst(aTxtFile: TStrings; const ExportMode: boolean);
      override;

    procedure SearchAllRelatedFiles(OutFileList: TStrings;
      aFolder: string; Extensions: TStrings; AutoExtract: boolean); virtual;
    function SearchFirstRelatedFile(aFolder: string;
      Extensions: TStrings; AutoExtract: boolean): string; virtual;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property ID: string read FID write SetID;
    {< ID, Sort and Media filename. }
    property Title: string read GetTitle write SetTitle;
    {< Name of the group. }
    property Year: string read FYear write SetYear;
    {< Development year. }
    property Developer: string read FDeveloper write SetDeveloper;
    {< Developer. }

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

procedure caEmutecaCustomGroup.SetID(AValue: string);
begin
  if FID = AValue then
    Exit;
  FID := AValue;

  FPONotifyObservers(Self, ooChange, nil);
end;

procedure caEmutecaCustomGroup.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;

  if UTF8CompareText(AValue, ID) = 0 then
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

function caEmutecaCustomGroup.GetActualTitle: string;
begin
  Result := FTitle;
end;

function caEmutecaCustomGroup.MatchID(aID: string): boolean;
begin
  Result := UTF8CompareText(ID, aID) = 0;
end;

procedure caEmutecaCustomGroup.LoadFromIni(aIniFile: TMemIniFile);
begin
  if aIniFile = nil then
    Exit;

  Title := aIniFile.ReadString(ID, krsIniKeyTitle, GetActualTitle);
  Year := aIniFile.ReadString(ID, krsIniKeyYear, Year);
  Developer := aIniFile.ReadString(ID, krsIniKeyDeveloper, Developer);

  Stats.LoadFromIni(aIniFile, ID);
end;

procedure caEmutecaCustomGroup.LoadFromStrLst(aTxtFile: TStrings);
begin
  if not assigned(aTxtFile) then
    Exit;

  while aTxtFile.Count < 4 do
    aTxtFile.Add('');

  ID := aTxtFile[0];
  Title := aTxtFile[1];
  Year := aTxtFile[2];
  Developer := aTxtFile[3];

  Stats.LoadFromStrLst(aTxtFile, 4);
  // Next := aTxtFile[7]
end;

procedure caEmutecaCustomGroup.SaveToIni(aIniFile: TMemIniFile;
  const ExportMode: boolean);
begin
  if aIniFile = nil then
    Exit;

  aIniFile.WriteString(ID, krsIniKeyTitle, GetActualTitle);
  aIniFile.WriteString(ID, krsIniKeyYear, Year);
  aIniFile.WriteString(ID, krsIniKeyDeveloper, Developer);

  Stats.WriteToIni(aIniFile, ID, ExportMode);
end;

procedure caEmutecaCustomGroup.SaveToStrLst(aTxtFile: TStrings;
  const ExportMode: boolean);
begin
  if not assigned(aTxtFile) then
    Exit;

  aTxtFile.Add(ID);
  aTxtFile.Add(GetActualTitle);
  aTxtFile.Add(Year);
  aTxtFile.Add(Developer);

  Stats.WriteToStrLst(aTxtFile, ExportMode);
end;

procedure caEmutecaCustomGroup.SearchAllRelatedFiles(OutFileList: TStrings;
  aFolder: string; Extensions: TStrings; AutoExtract: boolean);
begin
  // HACK: Dot added to ID, to preserve dots in ids like "Super Mario Bros."
     EmuTKSearchAllRelatedFiles(OutFileList, aFolder, ID + '.', Extensions,
      False,  '');
end;

function caEmutecaCustomGroup.SearchFirstRelatedFile(aFolder: string;
  Extensions: TStrings; AutoExtract: boolean): string;
begin
  // HACK: Dot added to ID, to preserve dots in ids like "Super Mario Bros."
      Result := EmuTKSearchFirstRelatedFile(aFolder, ID + '.', Extensions,
      False, False,'');
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
