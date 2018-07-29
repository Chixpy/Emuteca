{ Basic abstract group class of Emuteca

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
  Classes, SysUtils, LazUTF8,
  // CHX units
  uCHXStrUtils,
  // CHX abstract objects
  uaCHXStorable,
  // Emuteca units
  uEmutecaCommon,
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
     procedure DoSaveToIni(aTxtFile: TStrings; ExportMode: Boolean); virtual;

  public
    function GetActualTitle: string;
    //< Gets actual Title string, not automade
    function GetActualSortTitle: string;
    //< Gets actual SortTitle string, not automade

    function CompareID(aID: string): integer;
    function MatchID(aID: string): boolean;

    procedure LoadFromStrLst(aTxtFile: TStrings); override;
    procedure ExportToStrLst(aTxtFile: TStrings); virtual;
    procedure SaveToStrLst(aTxtFile: TStrings); override;

    function ExportCommaText: string;
    procedure ImportFrom(aGroup: caEmutecaCustomGroup);

    procedure SearchAllRelatedFiles(OutFileList: TStrings;
      aFolder: string; Extensions: TStrings; SearchInComp: boolean; AutoExtract: boolean); virtual;
    function SearchFirstRelatedFile(aFolder: string;
      Extensions: TStrings; SearchInComp: boolean; AutoExtract: boolean): string; virtual;

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
  Result := FMediaFileName;
  if Result <> '' then Exit;

  // Opps, it's empty

  Result := CleanFileName(SortTitle, True, False);
  // Removing last dots "Super Mario Bros.", Windows have problems with
  //   removing folders ended with dot
  while Utf8EndsText('.', Result) do
    Result := Copy(Result, 1, Length(Result) - 1);
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
    FMediaFileName := CleanFileName(SortTitle, True, False)
  else
    FMediaFileName := CleanFileName(AValue, True, False);

  // Removing last dot "Super Mario Bros.", Windows have problems with
  //   removing folders ended with dot
  if Utf8EndsText('.', FMediaFileName) then
    FMediaFileName[UTF8LengthFast(FMediaFileName)] := '_';
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

procedure caEmutecaCustomGroup.DoSaveToIni(aTxtFile: TStrings;
  ExportMode: Boolean);
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
  DoSaveToIni(aTxtFile, True);
end;

procedure caEmutecaCustomGroup.SaveToStrLst(aTxtFile: TStrings);
begin
  DoSaveToIni(aTxtFile, False);
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

procedure caEmutecaCustomGroup.SearchAllRelatedFiles(OutFileList: TStrings;
  aFolder: string; Extensions: TStrings; SearchInComp: boolean; AutoExtract: boolean);
begin
  EmuTKSearchAllRelatedFiles(OutFileList, aFolder, MediaFileName, Extensions,
    SearchInComp, AutoExtract, '');
end;

function caEmutecaCustomGroup.SearchFirstRelatedFile(aFolder: string;
  Extensions: TStrings; SearchInComp: boolean; AutoExtract: boolean): string;
begin
  Result := EmuTKSearchFirstRelatedFile(aFolder, MediaFileName, Extensions, SearchInComp, AutoExtract, '');
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
