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
unit ucEmutecaSoftware;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8, contnrs, IniFiles, sha1, fgl,
  uCHXStrUtils, strutils,
  uaCHXStorable,
  uEmutecaCommon,
  ucEmutecaSystem, ucEmutecaGroup, ucEmutecaPlayingStats;

const
  // Constant for DumpStatus, fixed (for filenames)
  krsedsVerified = 'Verified';
  krsedsGood = 'GoodDump';
  krsedsAlternate = 'Alternate';
  krsedsOverDump = 'OverDump';
  krsedsBadDump = 'BadDump';
  krsedsUnderDump = 'UnderDump';

resourcestring
  // Strings for DumpStatus, translatable
  rsedsVerified = 'Verified';
  rsedsGood = 'GoodDump';
  rsedsAlternate = 'Alternate';
  rsedsOverDump = 'OverDump';
  rsedsBadDump = 'BadDump';
  rsedsUnderDump = 'UnderDump';


type
  TEmutecaDumpStatus = (edsVerified, edsGood, edsAlternate, edsOverDump,
    edsBadDump, edsUnderDump);

const
  EmutecaDumpStatusKeys: array [TEmutecaDumpStatus] of string =
    ('!', '', 'a', 'o', 'b', 'u');
  //< Keys for DumpStatus
  EmutecaDumpStatusStrs: array [TEmutecaDumpStatus] of string =
    (rsedsVerified, rsedsGood, rsedsAlternate, rsedsOverDump,
    rsedsBadDump, rsedsUnderDump);
  //< Strings for DumpStatus (localizable)
  EmutecaDumpStatusStrsK: array [TEmutecaDumpStatus] of string =
    (krsedsVerified, krsedsGood, krsedsAlternate, krsedsOverDump,
    krsedsBadDump, krsedsUnderDump);
//< Strings for DumpStatus (fixed constants, used for icon filenames, etc. )


function Key2EmutecaDumpSt(aString: string): TEmutecaDumpStatus;
// Result := EmutecaDumpStatusKeys[DumpStatus];
// Result := EmutecaDumpStatusStrs[DumpStatus];
// Result := EmutecaDumpStatusStrsK[DumpStatus];

type
  { cEmutecaSoftware. }

  cEmutecaSoftware = class(caCHXStorable, IFPObserver)
  private
    FCracked: string;
    FDumpInfo: string;
    FDumpStatus: TEmutecaDumpStatus;
    FFileName: string;
    FFixed: string;
    FFolder: string;
    FGroup: cEmutecaGroup;
    FGroupKey: string;
    FHack: string;
    FID: string;
    FModified: string;
    FPirate: string;
    FPublisher: string;
    FSHA1: TSHA1Digest;
    FSortTitle: string;
    FStats: cEmutecaPlayingStats;
    FSystem: cEmutecaSystem;
    FSystemKey: string;
    FTitle: string;
    FTrainer: string;
    FTranslation: string;
    FTranslitTitle: string;
    FVersion: string;
    FYear: string;
    FZone: string;
    function GetID: string;
    function GetSortTitle: string;
    function GetTitle: string;
    function GetTranslitTitle: string;
    procedure SetCracked(AValue: string);
    procedure SetDumpInfo(AValue: string);
    procedure SetDumpStatus(AValue: TEmutecaDumpStatus);
    procedure SetFileName(AValue: string);
    procedure SetFixed(AValue: string);
    procedure SetFolder(AValue: string);
    procedure SetGroup(AValue: cEmutecaGroup);
    procedure SetGroupKey(AValue: string);
    procedure SetHack(AValue: string);
    procedure SetID(AValue: string);
    procedure SetModified(AValue: string);
    procedure SetPirate(AValue: string);
    procedure SetPublisher(AValue: string);
    procedure SetSHA1(AValue: TSHA1Digest);
    procedure SetSortTitle(AValue: string);
    procedure SetSystem(AValue: cEmutecaSystem);
    procedure SetSystemKey(AValue: string);
    procedure SetTitle(AValue: string);
    procedure SetTrainer(AValue: string);
    procedure SetTranslation(AValue: string);
    procedure SetTranslitTitle(AValue: string);
    procedure SetVersion(AValue: string);
    procedure SetYear(AValue: string);
    procedure SetZone(AValue: string);

  protected


  public
    property SHA1: TSHA1Digest read FSHA1 write SetSHA1;
    {< SHA1 of the file. For searching in SHA1 DB. }

    property System: cEmutecaSystem read FSystem write SetSystem;
    {< Link to System. }
    property Group: cEmutecaGroup read FGroup write SetGroup;
    {< Link to Group. }

    function SHA1IsEmpty: boolean;
    function MatchSHA1(aSHA1: TSHA1Digest): boolean;
    function MatchSystem(aSystem: cEmutecaSystem): boolean;
    function MatchGroup(aGroup: cEmutecaGroup): boolean;
    function MatchMFile(aSoft: cEmutecaSoftware): Boolean;

    function GetActualID: string;
    function GetActualTitle: string;
    //< Gets actual SortTitle string, not inherited from group or automade
    function GetActualSortTitle: string;
    //< Gets actual SortTitle string, not inherited from group or automade
    function GetActualTranslitTitle: string;
    //< Gets actual TranslitTitle string, not inherited from group or automade

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);

    procedure LoadFromStrLst(aTxtFile: TStrings); override;
    procedure SaveToStrLst(aTxtFile: TStrings; const ExportMode: boolean);
      override;
    procedure LoadFromIni(aIniFile: TCustomIniFile); override;
    procedure SaveToIni(aIniFile: TCustomIniFile; const ExportMode: boolean);
      override;


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

    property SystemKey: string read FSystemKey write SetSystemKey;
    {< ID of the System. }
    property GroupKey: string read FGroupKey write SetGroupKey;
    {< ID of the Group. }

    property Title: string read GetTitle write SetTitle;
    {< Title.

      If empty, then it's same as group. }
    property TranslitTitle: string read GetTranslitTitle
      write SetTranslitTitle;
    {< Trasliterated name in english (ASCII7) characters.

    If TranslitTitle = '' then
      If GetActualTitle <> '' then
        TranslitTitle = GetActualTitle
      else
        TranslitTitle = Group.ID

    }
    property SortTitle: string read GetSortTitle write SetSortTitle;
    {< Title formated for sorting purposes.
     If SortTitle = '' then
      If GetActualTranslitTitle <> '' then
        SortTitle = GetActualTranslitTitle
      else
        SortTitle = Group.ID
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

    Don't confuse with developer.}

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

  { cEmutecaSoftList }

  cEmutecaGenSoftList = specialize TFPGObjectList<cEmutecaSoftware>;
  cEmutecaSoftList = class(cEmutecaGenSoftList);

  TEmutecaReturnSoftCB = function(aSoft: cEmutecaSoftware): boolean of object;

implementation

function Key2EmutecaDumpSt(aString: string): TEmutecaDumpStatus;
begin
  aString := UTF8Trim(UTF8LowerString(aString));

  if (aString = EmutecaDumpStatusKeys[edsGood]) then // krsedsGoodKey = ''
    Result := edsGood
  else if (aString[1] = EmutecaDumpStatusKeys[edsVerified]) then
    Result := edsVerified
  else if (aString[1] = EmutecaDumpStatusKeys[edsAlternate]) then
    Result := edsAlternate
  else if (aString[1] = EmutecaDumpStatusKeys[edsOverDump]) then
    Result := edsOverDump
  else if (aString[1] = EmutecaDumpStatusKeys[edsBadDump]) then
    Result := edsBadDump
  else if (aString[1] = EmutecaDumpStatusKeys[edsUnderDump]) then
    Result := edsUnderDump
  else
    Result := edsGood;
end;

{ cEmutecaSoftware }

function cEmutecaSoftware.GetID: string;
begin
  if (FID = '') then
    Result := SHA1Print(SHA1)
  else
    Result := FID;
end;

procedure cEmutecaSoftware.LoadFromIni(aIniFile: TCustomIniFile);
var
  SHA1Str: string;
begin
  if not assigned(aIniFile) then
    Exit;

  SHA1Str := SHA1Print(SHA1);

  // TODO: Load from ini file


  Stats.LoadFromIni(aIniFile,SHA1Str);
end;

procedure cEmutecaSoftware.SaveToIni(aIniFile: TCustomIniFile;
  const ExportMode: boolean);
var
  SHA1Str: string;
begin
  if not assigned(aIniFile) then
    Exit;

  SHA1Str := SHA1Print(SHA1);

  // Basic data
  // ----------
  aIniFile.WriteString(SHA1Str, 'ID', GetActualID);
  aIniFile.WriteString(SHA1Str, 'Title', GetActualTitle);
  aIniFile.WriteString(SHA1Str, 'TranslitTitle', GetActualTranslitTitle);
  aIniFile.WriteString(SHA1Str, 'SortTitle', GetActualSortTitle);

  // System and Group
  // ----------------
  if assigned(System) then
    aIniFile.WriteString(SHA1Str, 'System', System.ID)
  else
    aIniFile.WriteString(SHA1Str, 'System', SystemKey);

  if assigned(Group) then
    aIniFile.WriteString(SHA1Str, 'Group', Group.ID)
  else
    aIniFile.WriteString(SHA1Str, 'Group', GroupKey);

  // Release data
  // ------------
  aIniFile.WriteString(SHA1Str, 'Version', Version);
  aIniFile.WriteString(SHA1Str, 'Year', Year);
  aIniFile.WriteString(SHA1Str, 'Publisher', Publisher);
  aIniFile.WriteString(SHA1Str, 'Zone', Zone);

  // Version Flags
  // ---------------
  aIniFile.WriteString(SHA1Str,
    'DumpStatus', EmutecaDumpStatusKeys[DumpStatus]);
  aIniFile.WriteString(SHA1Str, 'DumpInfo', DumpInfo);
  aIniFile.WriteString(SHA1Str, 'Fixed', Fixed);
  aIniFile.WriteString(SHA1Str, 'Trainer', Trainer);
  aIniFile.WriteString(SHA1Str, 'Translation', Translation);
  aIniFile.WriteString(SHA1Str, 'Pirate', Pirate);
  aIniFile.WriteString(SHA1Str, 'Cracked', Cracked);
  aIniFile.WriteString(SHA1Str, 'Modified', Modified);
  aIniFile.WriteString(SHA1Str, 'Hack', Hack);

  if not ExportMode then
  begin
    aIniFile.WriteString(SHA1Str, 'Folder', Folder);
    aIniFile.WriteString(SHA1Str, 'FileName', FileName);
  end;

  Stats.WriteToIni(aIniFile,SHA1Str, ExportMode);
end;

procedure cEmutecaSoftware.SetFileName(AValue: string);
begin
  FFileName := SetAsFile(AValue);
end;

procedure cEmutecaSoftware.SetID(AValue: string);
begin
  if FID = AValue then
    Exit;

  if UTF8CompareText(AValue, SHA1Print(SHA1)) = 0 then
    FID := ''
  else
    FID := AValue;

  FPONotifyObservers(Self, ooChange, nil);
end;

procedure cEmutecaSoftware.SetSHA1(AValue: TSHA1Digest);
begin
  if SHA1Match(FSHA1, AValue) then
    Exit;
  FSHA1 := AValue;
end;

procedure cEmutecaSoftware.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;

  if Assigned(FSystem) then
    FSystem.FPODetachObserver(Self);

  FSystem := AValue;

  if Assigned(System) then
  begin
    System.FPOAttachObserver(Self);
    FSystemKey := System.ID;
  end;

  //else FSystemKey := ''; We don't want to delete the old SystemKey
end;

procedure cEmutecaSoftware.SetSystemKey(AValue: string);
begin
  if FSystemKey = AValue then Exit;
  FSystemKey := AValue;
end;

procedure cEmutecaSoftware.SetCracked(AValue: string);
begin
  if FCracked = AValue then
    Exit;
  FCracked := AValue;
end;

function cEmutecaSoftware.GetSortTitle: string;
begin
  Result := FSortTitle;
  if Result <> '' then
    Exit;

  Result := UTF8LowerString(TranslitTitle);
  if Result <> '' then
    Exit;

  // Surely we never execute this...
  Result := UTF8LowerString(Title);
end;

function cEmutecaSoftware.GetTitle: string;
begin
  Result := FTitle;
  if Result <> '' then
    exit;

  if Assigned(Group) then
    Result := Group.Title
  else
    Result := GroupKey;
end;

function cEmutecaSoftware.GetTranslitTitle: string;
begin
  Result := FTranslitTitle;
  if Result <> '' then
    exit;

  Result := GetActualTitle;
  if Result <> '' then
    exit;

  // Not needed Group.ID = GroupKey
  // if Assigned(Group) then
  //   Result := Group.ID
  // else
  Result := GroupKey;
end;

procedure cEmutecaSoftware.SetDumpInfo(AValue: string);
begin
  if FDumpInfo = AValue then
    Exit;
  FDumpInfo := AValue;
end;

procedure cEmutecaSoftware.SetDumpStatus(AValue: TEmutecaDumpStatus);
begin
  if FDumpStatus = AValue then
    Exit;
  FDumpStatus := AValue;
end;

procedure cEmutecaSoftware.SetFixed(AValue: string);
begin
  if FFixed = AValue then
    Exit;
  FFixed := AValue;
end;

procedure cEmutecaSoftware.SetGroup(AValue: cEmutecaGroup);
begin
  if FGroup = AValue then
    Exit;

  if Assigned(FGroup) then
    FGroup.FPODetachObserver(Self);

  FGroup := AValue;

  if Assigned(Group) then
  begin
    Group.FPOAttachObserver(Self);
    FGroupKey := Group.ID;
  end;
  // else FGroupKey := ''; We don't want to delete old GroupKey
end;

procedure cEmutecaSoftware.SetGroupKey(AValue: string);
begin
  if FGroupKey = AValue then Exit;
  FGroupKey := AValue;
end;

procedure cEmutecaSoftware.SetHack(AValue: string);
begin
  if FHack = AValue then
    Exit;
  FHack := AValue;
end;

procedure cEmutecaSoftware.SetModified(AValue: string);
begin
  if FModified = AValue then
    Exit;
  FModified := AValue;
end;

procedure cEmutecaSoftware.SetPirate(AValue: string);
begin
  if FPirate = AValue then
    Exit;
  FPirate := AValue;
end;

procedure cEmutecaSoftware.SetPublisher(AValue: string);
begin
  if FPublisher = AValue then
    Exit;
  FPublisher := AValue;
end;

procedure cEmutecaSoftware.SetSortTitle(AValue: string);
begin
  if (UTF8CompareText(AValue, TranslitTitle) = 0) or (TranslitTitle = '') then
    FSortTitle := ''
  else
    FSortTitle := UTF8LowerString(AValue);
end;

procedure cEmutecaSoftware.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;

  if Assigned(Group) and (UTF8CompareText(AValue, Group.Title) = 0) then
    FTitle := ''
  else
    FTitle := AValue;
end;

procedure cEmutecaSoftware.SetTrainer(AValue: string);
begin
  if FTrainer = AValue then
    Exit;
  FTrainer := AValue;
end;

procedure cEmutecaSoftware.SetTranslation(AValue: string);
begin
  if FTranslation = AValue then
    Exit;
  FTranslation := AValue;
end;

procedure cEmutecaSoftware.SetTranslitTitle(AValue: string);
begin
  if (AValue = Title) or (Title = '') then
    FTranslitTitle := ''
  else
    FTranslitTitle := AValue;
end;

procedure cEmutecaSoftware.SetVersion(AValue: string);
begin
  if FVersion = AValue then
    Exit;
  FVersion := AValue;
end;

procedure cEmutecaSoftware.SetYear(AValue: string);
begin
  if FYear = AValue then
    Exit;
  FYear := AValue;
end;

procedure cEmutecaSoftware.SetZone(AValue: string);
begin
  if FZone = AValue then
    Exit;
  FZone := AValue;
end;

function cEmutecaSoftware.SHA1IsEmpty: boolean;
begin
  Result := SHA1Match(SHA1, kEmuTKSHA1Empty);
end;

function cEmutecaSoftware.MatchSHA1(aSHA1: TSHA1Digest): boolean;
begin
  Result := SHA1Match(Self.SHA1, aSHA1);
end;

function cEmutecaSoftware.MatchSystem(aSystem: cEmutecaSystem): boolean;
begin
  if Assigned(aSystem) then
    Result := UTF8CompareText(Self.SystemKey, aSystem.ID) = 0
    else
      Result := False;
end;

function cEmutecaSoftware.MatchGroup(aGroup: cEmutecaGroup): boolean;
begin
  if Assigned(aGroup) then
  Result := UTF8CompareText(Self.GroupKey, aGroup.ID) = 0
  else
     Result := False;
end;

function cEmutecaSoftware.MatchMFile(aSoft: cEmutecaSoftware): Boolean;
begin
  Result := (CompareFilenames(Self.FileName, aSoft.FileName) = 0) and
        (CompareFilenames(Self.Folder, aSoft.Folder) = 0);
end;

function cEmutecaSoftware.GetActualID: string;
begin
  Result := FID;
end;

function cEmutecaSoftware.GetActualTitle: string;
begin
  Result := FTitle;
end;

function cEmutecaSoftware.GetActualSortTitle: string;
begin
  Result := FSortTitle;
end;

function cEmutecaSoftware.GetActualTranslitTitle: string;
begin
  Result := FTranslitTitle;
end;

procedure cEmutecaSoftware.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if not assigned(ASender) then
    Exit;

  if ASender = System then
  begin
    case Operation of
      ooFree: System := nil;
      else
        FSystemKey := cEmutecaSystem(ASender).ID;
    end;
  end
  else if ASender = Group then
  begin
    case Operation of
      ooFree: Group := nil;
      else
        FGroupKey := cEmutecaGroup(ASender).ID;
    end;
  end;
end;

procedure cEmutecaSoftware.SetFolder(AValue: string);
begin
  FFolder := SetAsFolder(AValue);
end;

constructor cEmutecaSoftware.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FStats := cEmutecaPlayingStats.Create(Self);

  DumpStatus := edsGood;
end;

destructor cEmutecaSoftware.Destroy;
begin
  if Assigned(System) then
    System.FPODetachObserver(Self);
  if Assigned(Group) then
    Group.FPODetachObserver(Self);

  Stats.Destroy;

  inherited Destroy;
end;

procedure cEmutecaSoftware.LoadFromStrLst(aTxtFile: TStrings);
var
  i: integer;
begin
  if not assigned(aTxtFile) then
    Exit;

  while aTxtFile.Count < 25 do
    aTxtFile.Add('');

  FSystemKey := aTxtFile[0];
  FGroupKey := aTxtFile[1];
  HexToBin(PChar(aTxtFile[2]), @SHA1, 20);
  ID := aTxtFile[3];

  if aTxtFile[4] <> '' then
    Folder := aTxtFile[4];
  if aTxtFile[5] <> '' then
    FileName := aTxtFile[5];

  Title := aTxtFile[6];
  TranslitTitle := aTxtFile[7];
  SortTitle := aTxtFile[8];

  Version := aTxtFile[9];
  Year := aTxtFile[10];
  Publisher := aTxtFile[11];
  Zone := aTxtFile[12];

  DumpStatus := Key2EmutecaDumpSt(aTxtFile[13]);
  DumpInfo := aTxtFile[14];
  Fixed := aTxtFile[15];
  Trainer := aTxtFile[16];
  Translation := aTxtFile[17];
  Pirate := aTxtFile[18];
  Cracked := aTxtFile[19];
  Modified := aTxtFile[20];
  Hack := aTxtFile[21];

  Stats.LoadFromStrLst(aTxtFile, 22);

  // Next := aTxtFile[25]
end;

procedure cEmutecaSoftware.SaveToStrLst(aTxtFile: TStrings;
  const ExportMode: boolean);
begin
  if not assigned(aTxtFile) then
    Exit;

  if assigned(System) then
    aTxtFile.Add(System.ID)
  else
    aTxtFile.Add(SystemKey);
  if assigned(Group) then
    aTxtFile.Add(Group.ID)
  else
    aTxtFile.Add(GroupKey);
  aTxtFile.Add(SHA1Print(SHA1));
  aTxtFile.Add(GetActualID); // If SHA1 = ID then FID = ''

  if ExportMode then
  begin
    aTxtFile.Add('');
    aTxtFile.Add('');
  end
  else
  begin
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

  aTxtFile.Add(EmutecaDumpStatusKeys[DumpStatus]);
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

initialization
  RegisterClass(cEmutecaSoftware);

finalization
  UnRegisterClass(cEmutecaSoftware);
end.
