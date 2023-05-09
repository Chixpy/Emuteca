unit uaEmutecaCustomSoft;

{< caEmutecaCustomSoft abstract class unit.

  This file is part of Emuteca Core.

  Copyright (C) 2006-2020 Chixpy

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
  Classes, SysUtils, sha1, LazUTF8, LazFileUtils,
  // CHX units
  uCHXStrUtils, uCHXFileUtils,
  // Emuteca Core units
  uEmutecaConst, uEmutecaCommon,
  // Emuteca abstract classes
  uaEmutecaCustomSGItem,
  // Emuteca Core classes
  ucEmutecaPlayingStats;

type

  { caEmutecaCustomSoft }

  caEmutecaCustomSoft = class(caEmutecaCustomSGItem)
  private
    FCracked: string;
    FDumpInfo: string;
    FDumpStatus: TEmutecaDumpStatus;
    FExtraParameters: TStringList;
    FFileName: string;
    FFixed: string;
    FFolder: string;
    FGroupKey: string;
    FHack: string;
    FModified: string;
    FPirate: string;
    FPublisher: string;
    FSHA1: TSHA1Digest;
    FStats: cEmutecaPlayingStats;
    FTrainer: string;
    FTranslation: string;
    FVersion: string;
    FZone: string;
    procedure SetCracked(AValue: string);
    procedure SetDumpInfo(AValue: string);
    procedure SetDumpStatus(AValue: TEmutecaDumpStatus);
    procedure SetFileName(AValue: string);
    procedure SetFixed(AValue: string);
    procedure SetFolder(AValue: string);
    procedure SetGroupKey(AValue: string);
    procedure SetHack(AValue: string);
    procedure SetModified(AValue: string);
    procedure SetPirate(AValue: string);
    procedure SetPublisher(AValue: string);
    procedure SetSHA1(AValue: TSHA1Digest);
    procedure SetTrainer(AValue: string);
    procedure SetTranslation(AValue: string);
    procedure SetVersion(AValue: string);
    procedure SetZone(AValue: string);

  protected
    function GetTitle: string; override;
    procedure SetTitle(AValue: string); override;
    function GetID: string; override;
    procedure SetID(AValue: string); override;

    procedure DoSaveToStrLst(aTxtFile: TStrings; ExportMode: boolean); virtual;

  public
    property SHA1: TSHA1Digest read FSHA1 write SetSHA1;
    {< SHA1 of the file. For searching in SHA1 DB. }

    function SHA1IsEmpty: boolean;
    function MatchSHA1(aSHA1: TSHA1Digest): boolean;
    function CompareFile(const aFolder, aFile: string): integer;
    function MatchFile(const aFolder, aFile: string): boolean;
    function MatchGroupKey(const aGroupID: string): boolean;
    function CompareGroupKey(const aGroupID: string): integer;
    function MatchGroupFile: boolean; virtual;

    procedure LoadFromStrLst(aTxtFile: TStrings); override;
    procedure SaveToStrLst(aTxtFile: TStrings); override;
    procedure ExportToStrLst(aTxtFile: TStrings); virtual;
    function ExportCommaText: string;

    procedure ImportFrom(aSoft: caEmutecaCustomSoft);

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;


  published
    // Basic data
    // ----------

    property Folder: string read FFolder write SetFolder;
    {< Folder or archive where the file is in. }
    property FileName: string read FFileName write SetFileName;
    {< Filename (or file inside and archive). }

    property GroupKey: string read FGroupKey write SetGroupKey;
    {< ID of the CachedGroup. }


    // Release data
    // ------------
    property Version: string read FVersion write SetVersion;
    {< Version (v1.0; PRG1) or release type info (demo; prototype),
         or both (v1.0RC2; demo). }

    property Publisher: string read FPublisher write SetPublisher;
    {< Publisher.

    Don't confuse with Developer.}

    property Zone: string read FZone write SetZone;
    {< Zone. }


    // Version flags. (Based on Cowering + TOSEC)
    // ------------------------------------------
    property DumpStatus: TEmutecaDumpStatus
      read FDumpStatus write SetDumpStatus;
    {< Merged Verified, good, bad dump, etc.}

    property DumpInfo: string read FDumpInfo write SetDumpInfo;
    {< Number of alternate, bad dump, etc. }

    property Fixed: string read FFixed write SetFixed;
    {< The game has been fixed to make it to work with emulators. }

    property Trainer: string read FTrainer write SetTrainer;
    {< The game has been modified to add cheats. }

    property Translation: string read FTranslation write SetTranslation;
    {< The game is fan-traslated.

        Lenguage abreviation of translation. }

    property Pirate: string read FPirate write SetPirate;
    {< The game was released physically breaking some IP laws.

       This is for not licensed copies of the games, mainly in China,
       Korea, etc. }

    property Cracked: string read FCracked write SetCracked;
    {< The game was modified for breaking some segurity check. }

    property Modified: string read FModified write SetModified;
    {< The game was modified by use (hiscores or saves). }

    property Hack: string read FHack write SetHack;
    {< The game was modified for changing something (intros, sprites).

      Only used if not covered by previous properties.}

    property ExtraParameters: TStringList read FExtraParameters;
    {< Extra parameters for use in emulator command line.

      For example: Number of map to use with Doom; line to execute in some
        MSX/CPC emulators, etc.}

    property Stats: cEmutecaPlayingStats read FStats;

  end;

implementation

{ caEmutecaCustomSoft }

function caEmutecaCustomSoft.GetID: string;
begin
  Result := FID;
  if Result <> '' then
    Exit;

  Result := SHA1Print(SHA1);
end;

function caEmutecaCustomSoft.GetTitle: string;
begin
  Result := FTitle;
  if Result <> '' then
    Exit;

  Result := GroupKey;
end;

procedure caEmutecaCustomSoft.SetCracked(AValue: string);
begin
  AValue := CleanInfo(AValue);
  if FCracked = AValue then
    Exit;
  FCracked := AValue;
end;

procedure caEmutecaCustomSoft.SetDumpInfo(AValue: string);
begin
  AValue := CleanInfo(AValue);
  if FDumpInfo = AValue then
    Exit;
  FDumpInfo := AValue;
end;

procedure caEmutecaCustomSoft.SetDumpStatus(AValue: TEmutecaDumpStatus);
begin
  if FDumpStatus = AValue then
    Exit;
  FDumpStatus := AValue;
end;

procedure caEmutecaCustomSoft.SetFileName(AValue: string);
begin
  FFileName := SetAsFile(UTF8Trim(AValue));
end;

procedure caEmutecaCustomSoft.SetFixed(AValue: string);
begin
  AValue := CleanInfo(AValue);
  if FFixed = AValue then
    Exit;
  FFixed := AValue;
end;

procedure caEmutecaCustomSoft.SetFolder(AValue: string);
begin
  FFolder := SetAsFolder(UTF8Trim(AValue));
end;

procedure caEmutecaCustomSoft.SetGroupKey(AValue: string);
begin
  AValue := UTF8Trim(AValue);

  if FGroupKey = AValue then
    Exit;
  FGroupKey := AValue;
end;

procedure caEmutecaCustomSoft.SetHack(AValue: string);
begin
  AValue := CleanInfo(AValue);

  if FHack = AValue then
    Exit;
  FHack := AValue;
end;

procedure caEmutecaCustomSoft.SetID(AValue: string);
begin
  AValue := UTF8Trim(AValue);

  if AValue = ID then
    Exit;

  if SHA1IsEmpty then
  begin
    // If SHA1 is not calculated, use inherited method
    inherited SetID(AValue);
    Exit;
  end;

  if UTF8CompareText(AValue, SHA1Print(SHA1)) = 0 then
    FID := ''
  else
    FID := AValue;

  FPONotifyObservers(Self, ooChange, nil);
end;

procedure caEmutecaCustomSoft.SetModified(AValue: string);
begin
  AValue := CleanInfo(AValue);
  if FModified = AValue then
    Exit;
  FModified := AValue;
end;

procedure caEmutecaCustomSoft.SetPirate(AValue: string);
begin
  AValue := CleanInfo(AValue);
  if FPirate = AValue then
    Exit;
  FPirate := AValue;
end;

procedure caEmutecaCustomSoft.SetPublisher(AValue: string);
begin
  AValue := CleanInfo(AValue);
  if FPublisher = AValue then
    Exit;
  FPublisher := AValue;
end;

procedure caEmutecaCustomSoft.SetSHA1(AValue: TSHA1Digest);
begin
  if SHA1Match(FSHA1, AValue) then
    Exit;
  FSHA1 := AValue;
end;


procedure caEmutecaCustomSoft.SetTitle(AValue: string);
begin
  AValue := UTF8Trim(AValue);

  if AValue = GroupKey then
    FTitle := ''
  else
    FTitle := AValue;
end;

procedure caEmutecaCustomSoft.DoSaveToStrLst(aTxtFile: TStrings;
  ExportMode: boolean);
begin
  if not assigned(aTxtFile) then
    Exit;

  aTxtFile.Add(GroupKey);

  if ExportMode then
  begin
    aTxtFile.Add('');
    aTxtFile.Add(ID); // Exporting only ID
    aTxtFile.Add('');
    aTxtFile.Add('');
  end
  else
  begin
    aTxtFile.Add(SHA1Print(SHA1));
    aTxtFile.Add(GetActualID); // If SHA1 = ID then FID = ''
    aTxtFile.Add(Folder);
    aTxtFile.Add(FileName);
  end;

  aTxtFile.Add(GetActualTitle);
  aTxtFile.Add(''); // TranslitTitle and MediaFilename removed
  aTxtFile.Add(GetActualSortTitle);

  aTxtFile.Add(Version);
  aTxtFile.Add(Date);
  aTxtFile.Add(Publisher);
  aTxtFile.Add(Zone);

  aTxtFile.Add(DumpSt2Key(DumpStatus));
  aTxtFile.Add(DumpInfo);
  aTxtFile.Add(Fixed);
  aTxtFile.Add(Trainer);
  aTxtFile.Add(Translation);
  aTxtFile.Add(Pirate);
  aTxtFile.Add(Cracked);
  aTxtFile.Add(Modified);
  aTxtFile.Add(Hack);
  aTxtFile.Add(ExtraParameters.CommaText);

  Stats.WriteToStrLst(aTxtFile, ExportMode);
end;


procedure caEmutecaCustomSoft.SetTrainer(AValue: string);
begin
  AValue := CleanInfo(AValue);
  if FTrainer = AValue then
    Exit;
  FTrainer := AValue;
end;

procedure caEmutecaCustomSoft.SetTranslation(AValue: string);
begin
  AValue := CleanInfo(AValue);
  if FTranslation = AValue then
    Exit;
  FTranslation := AValue;
end;

procedure caEmutecaCustomSoft.SetVersion(AValue: string);
begin
  AValue := CleanInfo(AValue);

  if FVersion = AValue then
    Exit;
  FVersion := AValue;
end;

procedure caEmutecaCustomSoft.SetZone(AValue: string);
begin
  AValue := UTF8TextReplace(AValue, '-', ',');
  AValue := UTF8TextReplace(AValue, ' ', '');
  AValue := UTF8LowerString(UTF8Trim(AValue));

  // Validating: 'en' or 'en,es[,pt]'...
  // Maybe we don't want lost info...
  //if Length(AValue) > 2 then
  //  if AValue[3] <> ',' then
  //    Exit;

  FZone := AValue
end;

function caEmutecaCustomSoft.SHA1IsEmpty: boolean;
begin
  Result := SHA1Match(SHA1, kCHXSHA1Empty);
end;

function caEmutecaCustomSoft.MatchSHA1(aSHA1: TSHA1Digest): boolean;
begin
  Result := SHA1Match(SHA1, aSHA1);
end;

function caEmutecaCustomSoft.CompareFile(
  const aFolder, aFile: string): integer;
begin
  Result := CompareFilenames(SetAsAbsoluteFile(Folder + Filename, ''),
    SetAsAbsoluteFile(SetAsFolder(aFolder) + aFile, ''));
end;

function caEmutecaCustomSoft.MatchFile(const aFolder, aFile: string): boolean;
begin
  Result := CompareFile(aFolder, aFile) = 0;
end;

function caEmutecaCustomSoft.MatchGroupKey(const aGroupID: string): boolean;
begin
  Result := CompareGroupKey(aGroupID) = 0;
end;

function caEmutecaCustomSoft.CompareGroupKey(const aGroupID: string): integer;
begin
  Result := UTF8CompareText(Self.GroupKey, aGroupID);
end;

function caEmutecaCustomSoft.MatchGroupFile: boolean;
begin
  Result := CompareFilenames(GroupKey, MediaFileName) = 0;
end;

procedure caEmutecaCustomSoft.LoadFromStrLst(aTxtFile: TStrings);
begin
  if not assigned(aTxtFile) then
    Exit;

  // Dirty error fix
  while aTxtFile.Count < 22 do
    aTxtFile.Add('');

  GroupKey := aTxtFile[0];
  SHA1 := StringToSHA1Digest(aTxtFile[1]);
  ID := aTxtFile[2];

  if aTxtFile[3] <> '' then
    Folder := aTxtFile[3];
  if aTxtFile[4] <> '' then
    FileName := aTxtFile[4];

  Title := aTxtFile[5];
  // MediaFileName := aTxtFile[6]; // MediaFileName and TranslitTitle removed
  SortTitle := aTxtFile[7];

  Version := aTxtFile[8];
  Date := aTxtFile[9];
  Publisher := aTxtFile[10];
  Zone := aTxtFile[11];

  DumpStatus := Key2DumpSt(aTxtFile[12]);
  DumpInfo := aTxtFile[13];
  Fixed := aTxtFile[14];
  Trainer := aTxtFile[15];
  Translation := aTxtFile[16];
  Pirate := aTxtFile[17];
  Cracked := aTxtFile[18];
  Modified := aTxtFile[19];
  Hack := aTxtFile[20];
  ExtraParameters.CommaText := aTxtFile[21];

  Stats.LoadFromStrLst(aTxtFile, 22);

  // Next := aTxtFile[25]
end;

procedure caEmutecaCustomSoft.ExportToStrLst(aTxtFile: TStrings);
begin
  DoSaveToStrLst(aTxtFile, True);
end;

function caEmutecaCustomSoft.ExportCommaText: string;
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

procedure caEmutecaCustomSoft.SaveToStrLst(aTxtFile: TStrings);
begin
  DoSaveToStrLst(aTxtFile, False);
end;

procedure caEmutecaCustomSoft.ImportFrom(aSoft: caEmutecaCustomSoft);
var
  i: integer;
begin
  if not Assigned(aSoft) then
    Exit;

  if aSoft.GroupKey <> krsImportKeepValueKey then
    Self.GroupKey := aSoft.GroupKey;

  if aSoft.Title <> krsImportKeepValueKey then
    Self.Title := aSoft.Title;
  // if aSoft.MediaFileName <> krsImportKeepValueKey then // MediaFileName and
  //   Self.MediaFileName := aSoft.MediaFileName;         // TranslitTitle changed
  if aSoft.SortTitle <> krsImportKeepValueKey then
    Self.SortTitle := aSoft.SortTitle;

  if aSoft.Version <> krsImportKeepValueKey then
    Self.Version := aSoft.Version;
  if aSoft.Date <> krsImportKeepValueKey then
    Self.Date := aSoft.Date;
  if aSoft.Publisher <> krsImportKeepValueKey then
    Self.Publisher := aSoft.Publisher;
  if aSoft.Zone <> krsImportKeepValueKey then
    Self.Zone := aSoft.Zone;

  // We well keep verified / favorited dumps
  if (Self.DumpStatus <> edsFavorite) and (aSoft.DumpStatus <> edsKeepValue) then
    Self.DumpStatus := aSoft.DumpStatus;
  if aSoft.DumpInfo <> krsImportKeepValueKey then
    Self.DumpInfo := aSoft.DumpInfo;
  if aSoft.Fixed <> krsImportKeepValueKey then
    Self.Fixed := aSoft.Fixed;
  if aSoft.Trainer <> krsImportKeepValueKey then
    Self.Trainer := aSoft.Trainer;
  if aSoft.Translation <> krsImportKeepValueKey then
    Self.Translation := aSoft.Translation;
  if aSoft.Pirate <> krsImportKeepValueKey then
    Self.Pirate := aSoft.Pirate;
  if aSoft.Cracked <> krsImportKeepValueKey then
    Self.Cracked := aSoft.Cracked;
  if aSoft.Modified <> krsImportKeepValueKey then
    Self.Modified := aSoft.Modified;
  if aSoft.Hack <> krsImportKeepValueKey then
    Self.Hack := aSoft.Hack;

  i := 0;
  while i < aSoft.ExtraParameters.Count do
  begin
    if aSoft.ExtraParameters[i] <> krsImportKeepValueKey then
    begin
      while Self.ExtraParameters.Count <= i do
        Self.ExtraParameters.Add('');
      Self.ExtraParameters[i] := aSoft.ExtraParameters[i];
    end;
    Inc(i);
  end;
end;

constructor caEmutecaCustomSoft.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FStats := cEmutecaPlayingStats.Create(Self);
  FExtraParameters := TStringList.Create;

  DumpStatus := edsUnknown;
end;

destructor caEmutecaCustomSoft.Destroy;
begin
  Stats.Free;
  ExtraParameters.Free;

  inherited Destroy;
end;

initialization
  RegisterClass(caEmutecaCustomSoft);

finalization
  UnRegisterClass(caEmutecaCustomSoft);
end.
