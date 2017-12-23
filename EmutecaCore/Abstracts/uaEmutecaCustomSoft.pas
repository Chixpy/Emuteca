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
unit uaEmutecaCustomSoft;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, sha1, LazUTF8, LazFileUtils,
  uCHXStrUtils, uCHXFileUtils,
  uaCHXStorable,
  uEmutecaCommon,
  ucEmutecaPlayingStats;

type

  { caEmutecaCustomSoft }

  caEmutecaCustomSoft = class(caCHXStorableTxt)
  private
    FCracked: string;
    FDumpInfo: string;
    FDumpStatus: TEmutecaDumpStatus;
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
    FSortTitle: string;
    FStats: cEmutecaPlayingStats;
    FTrainer: string;
    FTranslation: string;
    FTranslitTitle: string;
    FVersion: string;
    FYear: string;
    FZone: string;
    function GetID: string;
    function GetSortTitle: string;
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
    procedure SetSortTitle(AValue: string);
    procedure SetTrainer(AValue: string);
    procedure SetTranslation(AValue: string);
    procedure SetTranslitTitle(AValue: string);
    procedure SetVersion(AValue: string);
    procedure SetYear(AValue: string);
    procedure SetZone(AValue: string);

  protected
    FTitle: string;

    function GetTitle: string; virtual;
    procedure SetTitle(AValue: string); virtual;

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
    function MatchFile(aFolder, aFile: string): boolean;
    function MatchID(aID: string): boolean;
    function CompareID(aID: string): integer;
    function MatchGroupKey(aGroupID: string): boolean;
    function CompareGroupKey(aGroupID: string): integer;
    function MatchGroupFile: boolean; virtual;

    procedure LoadFromStrLst(aTxtFile: TStrings); override;
    procedure SaveToStrLst(aTxtFile: TStrings; const ExportMode: boolean);
      override;
    procedure LoadFromIni(aIniFile: TIniFile); override;
    procedure SaveToIni(aIniFile: TMemIniFile; const ExportMode: boolean);
      override;

    procedure ImportFrom(aSoft: caEmutecaCustomSoft);

    procedure SearchAllRelatedFiles(OutFileList: TStrings;
      aFolder: string; Extensions: TStrings; SearchInComp: boolean; AutoExtract: boolean); virtual;
    function SearchFirstRelatedFile(aFolder: string;
      Extensions: TStrings; SearchInComp: boolean; AutoExtract: boolean): string; virtual;


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

          Only if not covered by previous properties.}

    property Stats: cEmutecaPlayingStats read FStats;

  end;

implementation

{ caEmutecaCustomSoft }

function caEmutecaCustomSoft.GetID: string;
begin
  if (FID = '') then
    Result := SHA1Print(SHA1)
  else
    Result := FID;
end;

function caEmutecaCustomSoft.GetSortTitle: string;
begin
  Result := FSortTitle;
  if Result <> '' then
    Exit;

  Result := UTF8LowerString(TranslitTitle);
end;

function caEmutecaCustomSoft.GetTitle: string;
begin
  if FTitle <> '' then
    Result := FTitle
  else
    Result := GroupKey;
end;

function caEmutecaCustomSoft.GetTranslitTitle: string;
begin
  Result := FTranslitTitle;
  if Result <> '' then
    Exit;

  Result := Title;
end;

procedure caEmutecaCustomSoft.SetCracked(AValue: string);
begin
  if FCracked = AValue then
    Exit;
  FCracked := AValue;
end;

procedure caEmutecaCustomSoft.SetDumpInfo(AValue: string);
begin
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
  FFileName := SetAsFile(AValue);
end;

procedure caEmutecaCustomSoft.SetFixed(AValue: string);
begin
  if FFixed = AValue then
    Exit;
  FFixed := AValue;
end;

procedure caEmutecaCustomSoft.SetFolder(AValue: string);
begin
  FFolder := SetAsFolder(AValue);
end;

procedure caEmutecaCustomSoft.SetGroupKey(AValue: string);
begin
  if FGroupKey = AValue then
    Exit;
  FGroupKey := AValue;
end;

procedure caEmutecaCustomSoft.SetHack(AValue: string);
begin
  if FHack = AValue then
    Exit;
  FHack := AValue;
end;

procedure caEmutecaCustomSoft.SetID(AValue: string);
begin
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
  if FModified = AValue then
    Exit;
  FModified := AValue;
end;

procedure caEmutecaCustomSoft.SetPirate(AValue: string);
begin
  if FPirate = AValue then
    Exit;
  FPirate := AValue;
end;

procedure caEmutecaCustomSoft.SetPublisher(AValue: string);
begin
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
  if (UTF8CompareText(AValue, TranslitTitle) = 0) then
    FSortTitle := ''
  else
    FSortTitle := AValue;
end;

procedure caEmutecaCustomSoft.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;

  if (UTF8CompareText(AValue, GroupKey) = 0) then
    FTitle := ''
  else
    FTitle := AValue;
end;

procedure caEmutecaCustomSoft.SetTrainer(AValue: string);
begin
  if FTrainer = AValue then
    Exit;
  FTrainer := AValue;
end;

procedure caEmutecaCustomSoft.SetTranslation(AValue: string);
begin
  if FTranslation = AValue then
    Exit;
  FTranslation := AValue;
end;

procedure caEmutecaCustomSoft.SetTranslitTitle(AValue: string);
begin
  if (UTF8CompareText(AValue, Title) = 0) then
    FTranslitTitle := ''
  else
    FTranslitTitle := AValue;
end;

procedure caEmutecaCustomSoft.SetVersion(AValue: string);
begin
  if FVersion = AValue then
    Exit;
  FVersion := AValue;
end;

procedure caEmutecaCustomSoft.SetYear(AValue: string);
begin
  if FYear = AValue then
    Exit;
  FYear := AValue;
end;

procedure caEmutecaCustomSoft.SetZone(AValue: string);
begin
  FZone := UTF8LowerString(AValue);
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

  // Removing last dots "Super Mario Bros.", Windows have problems with
  //   removing folders ended with dot
  while Utf8EndsText('.', Result) do
    Result := Copy(Result, 1, Length(Result) - 1);
end;

function caEmutecaCustomSoft.SHA1IsEmpty: boolean;
begin
  Result := SHA1Match(SHA1, kCHXSHA1Empty);
end;

function caEmutecaCustomSoft.MatchSHA1(aSHA1: TSHA1Digest): boolean;
begin
  Result := SHA1Match(SHA1, aSHA1);
end;

function caEmutecaCustomSoft.MatchFile(aFolder, aFile: string): boolean;
begin
  Result := (CompareFilenames(FileName, SetAsFile(aFile)) = 0) and
    (CompareFilenames(Folder, SetAsFolder(aFolder)) = 0);
end;

function caEmutecaCustomSoft.MatchID(aID: string): boolean;
begin
  Result := CompareID(aID) = 0;
end;

function caEmutecaCustomSoft.CompareID(aID: string): integer;
begin
  Result := UTF8CompareText(Self.ID, aID);
end;

function caEmutecaCustomSoft.MatchGroupKey(aGroupID: string): boolean;
begin
  Result := CompareGroupKey(aGroupID) = 0;
end;

function caEmutecaCustomSoft.CompareGroupKey(aGroupID: string): integer;
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

  while aTxtFile.Count < 21 do
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

  Stats.LoadFromStrLst(aTxtFile, 21);

  // Next := aTxtFile[24]

end;

procedure caEmutecaCustomSoft.SaveToStrLst(aTxtFile: TStrings;
  const ExportMode: boolean);
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

  Stats.WriteToStrLst(aTxtFile, ExportMode);
end;

procedure caEmutecaCustomSoft.LoadFromIni(aIniFile: TIniFile);
var
  Section: string;
begin
  if not assigned(aIniFile) then
    Exit;

  Section := ID;

  // Basic data
  // ----------

  // Don't overwrite SHA1.
  // If SHA1 = ID -> whe don't need it
  //   else -> SHA1 is from actual file anyways.
  // It's saved for custom purpouses.
  // SHA1 := StringToSHA1Digest(aIniFile.ReadString(Section, krsIniKeySHA1, SHA1Print(SHA1)));

  // We can export by SHA1 systems with custom ID...
  if GetActualID = '' then
    ID := aIniFile.ReadString(Section, krsIniKeyID, GetActualID);

  GroupKey := aIniFile.ReadString(Section, krsIniKeyGroup, GroupKey);
  Title := aIniFile.ReadString(Section, krsIniKeyTitle, GetActualTitle);
  TranslitTitle := aIniFile.ReadString(Section, krsIniKeyTranslitTitle,
    GetActualTranslitTitle);
  SortTitle := aIniFile.ReadString(Section, krsIniKeySortTitle,
    GetActualSortTitle);

  // Release data
  // ------------
  Version := aIniFile.ReadString(Section, krsIniKeyVersion, Version);
  Year := aIniFile.ReadString(Section, krsIniKeyYear, Year);
  Publisher := aIniFile.ReadString(Section, krsIniKeyPublisher, Publisher);
  Zone := aIniFile.ReadString(Section, krsIniKeyZone, Zone);

  // Version Flags
  // ---------------
  DumpStatus := Key2DumpSt(aIniFile.ReadString(Section,
    krsIniKeyDumpStatus, DumpSt2Key(DumpStatus)));
  DumpInfo := aIniFile.ReadString(Section, krsIniKeyDumpInfo, DumpInfo);
  Fixed := aIniFile.ReadString(Section, krsIniKeyFixed, Fixed);
  Trainer := aIniFile.ReadString(Section, krsIniKeyTrainer, Trainer);
  Translation := aIniFile.ReadString(Section, krsIniKeyTranslation,
    Translation);
  Pirate := aIniFile.ReadString(Section, krsIniKeyPirate, Pirate);
  Cracked := aIniFile.ReadString(Section, krsIniKeyCracked, Cracked);
  Modified := aIniFile.ReadString(Section, krsIniKeyModified, Modified);
  Hack := aIniFile.ReadString(Section, krsIniKeyHack, Hack);

  Folder := aIniFile.ReadString(Section, krsIniKeyFolder, Folder);
  FileName := aIniFile.ReadString(Section, krsIniKeyFileName, FileName);

  Stats.LoadFromIni(aIniFile, Section);
end;

procedure caEmutecaCustomSoft.SaveToIni(aIniFile: TMemIniFile;
  const ExportMode: boolean);
var
  Section: string;
begin
  if not assigned(aIniFile) then
    Exit;

  Section := ID;

  // Basic data
  // ----------
  aIniFile.WriteString(Section, krsIniKeySHA1, SHA1Print(SHA1));
  aIniFile.WriteString(Section, krsIniKeyID, GetActualID);
  aIniFile.WriteString(Section, krsIniKeyGroup, GroupKey);
  aIniFile.WriteString(Section, krsIniKeyTitle, GetActualTitle);
  aIniFile.WriteString(Section, krsIniKeyTranslitTitle,
    GetActualTranslitTitle);
  aIniFile.WriteString(Section, krsIniKeySortTitle, GetActualSortTitle);

  // Release data
  // ------------
  aIniFile.WriteString(Section, krsIniKeyVersion, Version);
  aIniFile.WriteString(Section, krsIniKeyYear, Year);
  aIniFile.WriteString(Section, krsIniKeyPublisher, Publisher);
  aIniFile.WriteString(Section, krsIniKeyZone, Zone);

  // Version Flags
  // ---------------
  aIniFile.WriteString(Section,
    krsIniKeyDumpStatus, DumpSt2Key(DumpStatus));
  aIniFile.WriteString(Section, krsIniKeyDumpInfo, DumpInfo);
  aIniFile.WriteString(Section, krsIniKeyFixed, Fixed);
  aIniFile.WriteString(Section, krsIniKeyTrainer, Trainer);
  aIniFile.WriteString(Section, krsIniKeyTranslation, Translation);
  aIniFile.WriteString(Section, krsIniKeyPirate, Pirate);
  aIniFile.WriteString(Section, krsIniKeyCracked, Cracked);
  aIniFile.WriteString(Section, krsIniKeyModified, Modified);
  aIniFile.WriteString(Section, krsIniKeyHack, Hack);

  if ExportMode then
  begin
    aIniFile.DeleteKey(Section, krsIniKeyFolder);
    aIniFile.DeleteKey(Section, krsIniKeyFileName);
  end
  else
  begin
    aIniFile.WriteString(Section, krsIniKeyFolder, Folder);
    aIniFile.WriteString(Section, krsIniKeyFileName, FileName);
  end;

  Stats.WriteToIni(aIniFile, Section, ExportMode);
end;

procedure caEmutecaCustomSoft.ImportFrom(aSoft: caEmutecaCustomSoft);
begin
  if not Assigned(aSoft) then
    Exit;

  if aSoft.GroupKey <> krsImportKeepValue then
    GroupKey := aSoft.GroupKey;

  if aSoft.Title <> krsImportKeepValue then
    Title := aSoft.Title;
  if aSoft.TranslitTitle <> krsImportKeepValue then
    TranslitTitle := aSoft.TranslitTitle;
  if aSoft.SortTitle <> krsImportKeepValue then
    SortTitle := aSoft.SortTitle;

  if aSoft.Version <> krsImportKeepValue then
    Version := aSoft.Version;
  if aSoft.Year <> krsImportKeepValue then
    Year := aSoft.Year;
  if aSoft.Publisher <> krsImportKeepValue then
    Publisher := aSoft.Publisher;
  if aSoft.Zone <> krsImportKeepValue then
    Zone := aSoft.Zone;

  if aSoft.DumpStatus <> edsKeepValue then
    DumpStatus := aSoft.DumpStatus;
  if aSoft.DumpInfo <> krsImportKeepValue then
    DumpInfo := aSoft.DumpInfo;
  if aSoft.Fixed <> krsImportKeepValue then
    Fixed := aSoft.Fixed;
  if aSoft.Trainer <> krsImportKeepValue then
    Trainer := aSoft.Trainer;
  if aSoft.Translation <> krsImportKeepValue then
    Translation := aSoft.Translation;
  if aSoft.Pirate <> krsImportKeepValue then
    Pirate := aSoft.Pirate;
  if aSoft.Cracked <> krsImportKeepValue then
    Cracked := aSoft.Cracked;
  if aSoft.Modified <> krsImportKeepValue then
    Modified := aSoft.Modified;
  if aSoft.Hack <> krsImportKeepValue then
    Hack := aSoft.Hack;

end;

procedure caEmutecaCustomSoft.SearchAllRelatedFiles(OutFileList: TStrings;
  aFolder: string; Extensions: TStrings; SearchInComp: boolean; AutoExtract: boolean);
begin
  EmuTKSearchAllRelatedFiles(OutFileList, aFolder, GetMediaFileName, Extensions,
    SearchInComp, AutoExtract, '');
end;

function caEmutecaCustomSoft.SearchFirstRelatedFile(aFolder: string;
  Extensions: TStrings; SearchInComp: boolean; AutoExtract: boolean): string;
begin
  Result := EmuTKSearchFirstRelatedFile(aFolder, GetMediaFileName,
    Extensions, SearchInComp, AutoExtract, '');
end;

constructor caEmutecaCustomSoft.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FStats := cEmutecaPlayingStats.Create(Self);

  DumpStatus := edsGood;
end;

destructor caEmutecaCustomSoft.Destroy;
begin
  Stats.Free;

  inherited Destroy;
end;

initialization
  RegisterClass(caEmutecaCustomSoft);

finalization
  UnRegisterClass(caEmutecaCustomSoft);
end.
