unit uaEmutecaCustomSoft;

{< caEmutecaCustomSoft abstract class unit.

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
  Classes, SysUtils, sha1, LazUTF8, LazFileUtils,
  // CHX units
  uCHXStrUtils, uCHXFileUtils,
  // CHX abstracts
  uaCHXStorable,
  // Emuteca Core units
  uEmutecaConst, uEmutecaCommon,
  // Emuteca Core classes
  ucEmutecaPlayingStats;

type

  { caEmutecaCustomSoft }

  caEmutecaCustomSoft = class(caCHXStorableTxt)
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
    FID: string;
    FModified: string;
    FPirate: string;
    FPublisher: string;
    FSHA1: TSHA1Digest;
    FStats: cEmutecaPlayingStats;
    FTrainer: string;
    FTranslation: string;
    FTranslitTitle: string;
    FVersion: string;
    FYear: string;
    FZone: string;
    function GetID: string;
    function GetTranslitTitle: string;
    procedure SetCracked(AValue: string);
    procedure SetDumpInfo(AValue: string);
    procedure SetDumpStatus(AValue: TEmutecaDumpStatus);
    procedure SetFileName(AValue: string);
    procedure SetFixed(AValue: string);
    procedure SetFolder(AValue: string);
    procedure SetGroupKey(AValue: string);
    procedure SetHack(AValue: string);
    procedure SetID(AValue: string);
    procedure SetModified(AValue: string);
    procedure SetPirate(AValue: string);
    procedure SetPublisher(AValue: string);
    procedure SetSHA1(AValue: TSHA1Digest);
    procedure SetTrainer(AValue: string);
    procedure SetTranslation(AValue: string);
    procedure SetTranslitTitle(AValue: string);
    procedure SetVersion(AValue: string);
    procedure SetYear(AValue: string);
    procedure SetZone(AValue: string);

  protected
    FTitle: string;
    FSortTitle: string;

    function GetTitle: string; virtual;
    procedure SetTitle(AValue: string); virtual;

    function GetSortTitle: string; virtual;
    procedure SetSortTitle(AValue: string); virtual;

    procedure DoSaveToStrLst(aTxtFile: TStrings; ExportMode: boolean); virtual;

  public
    property SHA1: TSHA1Digest read FSHA1 write SetSHA1;
    {< SHA1 of the file. For searching in SHA1 DB. }

    function GetActualID: string;
    //< Gets actual ID string, not automade.
    function GetActualTitle: string;
    //< Gets actual SortTitle string, not inherited from group or automade.
    function GetActualSortTitle: string;
    //< Gets actual SortTitle string, not inherited from group or automade.
    function GetActualTranslitTitle: string;
    //< Gets actual TranslitTitle string, not inherited from group or automade.
    function GetMediaFileName: string;

    function SHA1IsEmpty: boolean;
    function MatchSHA1(aSHA1: TSHA1Digest): boolean;
    function CompareFile(const aFolder, aFile: string): integer;
    function MatchFile(const aFolder, aFile: string): boolean;
    function MatchID(const aID: string): boolean;
    function CompareID(const aID: string): integer;
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
    property ID: string read GetID write SetID;
    {< ID of the file. Usually SHA1, some systems is filename (MAME) or
      a custom one (PSX)}
    property Folder: string read FFolder write SetFolder;
    {< Folder or archive where the file is in. }
    property FileName: string read FFileName write SetFileName;
    {< Filename (or file inside and archive). }

    property GroupKey: string read FGroupKey write SetGroupKey;
    {< ID of the CachedGroup. }

    property Title: string read GetTitle write SetTitle;
    {< Title.

      If empty, then it's same as group ID. }
    property TranslitTitle: string read GetTranslitTitle
      write SetTranslitTitle;
    {< Trasliterated name in english (ASCII7) characters.

    If TranslitTitle = '' then
      If GetActualTitle <> '' then
        TranslitTitle = GetActualTitle
      else
        TranslitTitle = CachedGroup.ID

    }
    property SortTitle: string read GetSortTitle write SetSortTitle;
    {< Title formated for sorting purposes.
     If SortTitle = '' then
      If GetActualTranslitTitle <> '' then
        SortTitle = GetActualTranslitTitle
      else
        SortTitle = CachedGroup.ID
    }

    // Release data
    // ------------
    property Version: string read FVersion write SetVersion;
    {< Version (v1.0; PRG1) or release type info (demo, prototype),
         or both (v1.0RC2, alpha 3). }

    property Year: string read FYear write SetYear;
    {< Date of the release (no development one).

    Format: YYYY/MM/DD }

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
  if FID = '' then
    Result := SHA1Print(SHA1)
  else
    Result := FID;
end;

function caEmutecaCustomSoft.GetSortTitle: string;
begin
  if FSortTitle = '' then
    Result := UTF8LowerString(TranslitTitle)
  else
    Result := FSortTitle;
end;

function caEmutecaCustomSoft.GetTitle: string;
begin
  if FTitle = '' then
    Result := GroupKey
  else
    Result := FTitle;
end;

function caEmutecaCustomSoft.GetTranslitTitle: string;
begin
  if FTranslitTitle = '' then
    Result := Title
  else
    Result := FTranslitTitle;
end;

procedure caEmutecaCustomSoft.SetCracked(AValue: string);
begin
  AValue := UTF8Trim(AValue);
  if FCracked = AValue then
    Exit;
  FCracked := AValue;
end;

procedure caEmutecaCustomSoft.SetDumpInfo(AValue: string);
begin
  AValue := UTF8Trim(AValue);
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
  AValue := UTF8Trim(AValue);
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
  AValue := UTF8Trim(AValue);
  if FHack = AValue then
    Exit;
  FHack := AValue;
end;

procedure caEmutecaCustomSoft.SetID(AValue: string);
begin
  AValue := UTF8Trim(AValue);
  if FID = AValue then
    Exit;

  if UTF8CompareText(AValue, SHA1Print(SHA1)) = 0 then
    FID := ''
  else
    FID := AValue;

  FPONotifyObservers(Self, ooChange, nil);
end;

procedure caEmutecaCustomSoft.SetModified(AValue: string);
begin
  AValue := UTF8Trim(AValue);
  if FModified = AValue then
    Exit;
  FModified := AValue;
end;

procedure caEmutecaCustomSoft.SetPirate(AValue: string);
begin
  AValue := UTF8Trim(AValue);
  if FPirate = AValue then
    Exit;
  FPirate := AValue;
end;

procedure caEmutecaCustomSoft.SetPublisher(AValue: string);
begin
  AValue := UTF8Trim(AValue);
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

procedure caEmutecaCustomSoft.SetSortTitle(AValue: string);
begin
  AValue := UTF8Trim(AValue);
  if UTF8CompareText(AValue, TranslitTitle) = 0 then
    FSortTitle := ''
  else
    FSortTitle := AValue;
end;

procedure caEmutecaCustomSoft.SetTitle(AValue: string);
begin
  AValue := UTF8Trim(AValue);
  if FTitle = AValue then
    Exit;

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
  aTxtFile.Add(GetActualTranslitTitle);
  aTxtFile.Add(GetActualSortTitle);

  aTxtFile.Add(Version);
  aTxtFile.Add(Year);
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
  AValue := UTF8Trim(AValue);
  if FTrainer = AValue then
    Exit;
  FTrainer := AValue;
end;

procedure caEmutecaCustomSoft.SetTranslation(AValue: string);
begin
  AValue := UTF8Trim(AValue);
  if FTranslation = AValue then
    Exit;
  FTranslation := AValue;
end;

procedure caEmutecaCustomSoft.SetTranslitTitle(AValue: string);
begin
  AValue := UTF8Trim(AValue);
  if AValue = Title then
    FTranslitTitle := ''
  else
    FTranslitTitle := AValue;
end;

procedure caEmutecaCustomSoft.SetVersion(AValue: string);
begin
  AValue := UTF8Trim(AValue);
  if FVersion = AValue then
    Exit;
  FVersion := AValue;
end;

procedure caEmutecaCustomSoft.SetYear(AValue: string);
begin
  AValue := UTF8Trim(AValue);
  if FYear = AValue then
    Exit;
  FYear := AValue;
end;

procedure caEmutecaCustomSoft.SetZone(AValue: string);
begin
  FZone := UTF8LowerString(UTF8Trim(AValue));
end;

function caEmutecaCustomSoft.GetActualID: string;
begin
  Result := FID;
end;

function caEmutecaCustomSoft.GetActualTitle: string;
begin
  Result := FTitle;
end;

function caEmutecaCustomSoft.GetActualSortTitle: string;
begin
  Result := FSortTitle;
end;

function caEmutecaCustomSoft.GetActualTranslitTitle: string;
begin
  Result := FTranslitTitle;
end;

function caEmutecaCustomSoft.GetMediaFileName: string;
begin
  Result := RemoveFromBrackets(ExtractFileNameOnly(FileName));

  // Removing last dots "Super Mario Bros.",
  // Windows have problems with removing folders ended with dot...
  if Utf8EndsText('.', Result) then
    Result[UTF8LengthFast(Result)] := '_';
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

function caEmutecaCustomSoft.MatchID(const aID: string): boolean;
begin
  Result := CompareID(aID) = 0;
end;

function caEmutecaCustomSoft.CompareID(const aID: string): integer;
begin
  Result := UTF8CompareText(Self.ID, aID);
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
  Result := CompareFilenames(GroupKey, GetMediaFileName) = 0;
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
  TranslitTitle := aTxtFile[6];
  SortTitle := aTxtFile[7];

  Version := aTxtFile[8];
  Year := aTxtFile[9];
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
  if aSoft.TranslitTitle <> krsImportKeepValueKey then
    Self.TranslitTitle := aSoft.TranslitTitle;
  if aSoft.SortTitle <> krsImportKeepValueKey then
    Self.SortTitle := aSoft.SortTitle;

  if aSoft.Version <> krsImportKeepValueKey then
    Self.Version := aSoft.Version;
  if aSoft.Year <> krsImportKeepValueKey then
    Self.Year := aSoft.Year;
  if aSoft.Publisher <> krsImportKeepValueKey then
    Self.Publisher := aSoft.Publisher;
  if aSoft.Zone <> krsImportKeepValueKey then
    Self.Zone := aSoft.Zone;

  if aSoft.DumpStatus <> edsKeepValue then
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
