{ This file is part of Emuteca

  Copyright (C) 2006-2013 Chixpy

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

{ cGame unit. }
unit uEmutecaGame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fileutil, IniFiles, LazUTF8, fgl,
  uEmutecaConst, uCHXStrUtils, uEmutecaPlayingStats;

type

  { @name.

    @definitionList(
    @itemLabel(NOTE:)
    @item(Because PascalScript don't suport overloaded methods,
      we don't use them right here.)
    )
  }

  { cEmutecaGame }

  cEmutecaGame = class(cEmutecaPlayingStats)
  private
    FAlternate: String;
    FBadDump: String;
    FCracked: String;
    FGameGroup: String;
    FFixed: String;
    FKey: String;
    FModified: String;
    FPublisher: String;
    FFileName: String;
    FFolder: String;
    FHack: String;
    FLanguages: TStringList;
    FLicense: String;
    FGameName: String;
    FPirate: String;
    FReleaseType: String;
    FSortKey: String;
    FTags: TStringList;
    FTrainer: String;
    FTranslation: String;
    FTransliteratedName: string;
    FVerified: boolean;
    FVersion: String;
    FYear: String;
    FZones: TStringList;
    function GetDataString: String;
    procedure SetAlternate(const AValue: String);
    procedure SetBadDump(const AValue: String);
    procedure SetCracked(const AValue: String);
    procedure SetDataString(const AValue: String);
    procedure SetGameGroup(const AValue: String);
    procedure SetFixed(const AValue: String);
    procedure SetKey(const AValue: String);
    procedure SetModified(const AValue: String);
    procedure SetPublisher(const AValue: String);
    procedure SetFileName(const AValue: String);
    procedure SetFolder(const AValue: String);
    procedure SetHack(const AValue: String);
    procedure SetLicense(const AValue: String);
    procedure SetGameName(const AValue: String);
    procedure SetPirate(const AValue: String);
    procedure SetReleaseType(const AValue: String);
    procedure SetSortKey(const AValue: String);
    procedure SetTrainer(const AValue: String);
    procedure SetTranslation(const AValue: String);
    procedure SetTransliteratedName(AValue: string);
    procedure SetVerified(const AValue: boolean);
    procedure SetVersion(const AValue: String);
    procedure SetYear(const AValue: String);

  protected

  public
    property DataString: String read GetDataString write SetDataString;
    { Converts or load game info from string (CSV formatted).
    }
    procedure ExportData(aFilename: String; ExportMode: boolean);
    procedure ExportDataIni(aIniFile: TCustomIniFile; ExportMode: boolean);
    procedure ImportData(aFilename: String);
    procedure ImportDataIni(aIniFile: TCustomIniFile);

    constructor Create(const aFolder: String;
      const aFileName: String; const aKey: String);
    destructor Destroy; override;

  published
    // Basic data
    // ----------
    property Key: String read FKey write SetKey;
    {< Key used to identify data (CRC32, Keyname, etc).

      It's system dependant. Some systems will use CRC32 (maybe in the
      future SHA1 or MD5 will be used), other ones have a common key
      as reference (arcade = MAME key).

      Always converted to lowercase.
    }
    property GameName: String read FGameName write SetGameName;
    {< Original title of the game version.

    Ideally it will contain cyrilic, korean and other fancy characters. Until
      someone make a database with original names it will contain
      transliterated names.
    }

    property TransliteratedName: string read FTransliteratedName write SetTransliteratedName;

    property SortKey: String read FSortKey write SetSortKey;
    {< Title formated for sorting purposes.

    It contains the name for sorting:
    @unorderedList(
      @item(Articles @italic(The) or @italic(A) at the end of the name.
        "@italic(The Emuteca collection)" -> "@italic(Emuteca collection,
        The)". If title is in another language, its rules will be used.)
      @item(The fancy characters will be trasliterated to latin ones.
        Obviusly, if original name is a trasliteration from words in
        latin, original words will be used. As example: Final Fantasy in
        Japanese is @italic(ファイナルファンタジー), trasliteration is
        @italic(Fainaru Fantajī) but here will be @italic(Final Fantasy).)
    )
    }
    property GameGroup: String read FGameGroup write SetGameGroup;
    {< Gamegroup key. }
    property Folder: String read FFolder write SetFolder;
    {< Folder or compressed file where the game is. }
    property FileName: String read FFileName write SetFileName;
    {< Actual file of then game.

    If the game have many files, it's the file used as reference.
    }

    // Release data
    property Version: String read FVersion write SetVersion;
    {< Version, revision or update of the game. }
    property Year: String read FYear write SetYear;
    {< Date of the release (no development one).

    Format: YYYY/MM/DD}
    property Publisher: String read FPublisher write SetPublisher;
    {< Publisher company.

    Don't confuse with developer.}
    property Zones: TStringList read FZones;
    {< Zones. }
    property Languages: TStringList read FLanguages;
    {< Languages used in the game. }
    property License: String read FLicense write SetLicense;
    {< Original license.

    Empty means a commercial one, but better if it's stored.}
    property ReleaseType: String read FReleaseType write SetReleaseType;
    {< Type of release (prototype, demo, etc).
    }

    property Tags: TStringList read FTags;
    {< Tags for grouping games.

    Format: Generic/Especific (Players/1 player; Series/Batman;
      Genre/Shot'm up; Perspective/2.5D; Location/Europe/Spain)

    }

    // Cowering/TOSEC flags
    // --------------------

    property Verified: boolean read FVerified write SetVerified;
    {< Verified good dump. }

    property Alternate: String read FAlternate write SetAlternate;
    {< Alternate dump.

      Usually have an ID number, but its better to have the main difference.}
    property BadDump: String read FBadDump write SetBadDump;
    {< Bad dump, underdump or overdump.

    @definitionList(
    @itemLabel(Bad dumps)
    @item(ID number or problem.)
    @itemLabel(Underdump)
    @item('-' + ID number or problem.)
    @itemLabel(Overdump)
    @item('+' + ID number or problem.)
    )}
    property Fixed: String read FFixed write SetFixed;
    {< The game has been fixed for make it to work. }
    property Trainer: String read FTrainer write SetTrainer;
    {< The game has been modified to add cheats. }
    property Translation: String read FTranslation write SetTranslation;
    {< The game is fan-traslated.

    Lenguage abreviation of traslatration.}
    property Pirate: String read FPirate write SetPirate;
    {< The game was released physically breaking some IP laws.

    This is for not licensed copies of the games, mainly in China, Korea, etc.
    }
    property Cracked: String read FCracked write SetCracked;
    {< The game was modified for breaking some segurity check. }
    property Modified: String read FModified write SetModified;
    {< The game was modified by use (hiscores or saves). }
    property Hack: String read FHack write SetHack;
    {< The game was modified for changing something (intros, sprites).

      If not covered by previous properties.}

  end;


  cGameOwnedList = class (specialize TFPGObjectList<cEmutecaGame>);
  cGameList = class (specialize TFPGList<cEmutecaGame>);

implementation

{ cEmutecaGame }

procedure cEmutecaGame.SetKey(const AValue: String);
begin
  FKey := Trim(UTF8LowerCase(AValue));
end;

procedure cEmutecaGame.SetModified(const AValue: String);
begin
  FModified := Trim(AValue);
end;

function cEmutecaGame.GetDataString: String;
var
  Tmp: TStringList;
begin
  Result := '';
  Tmp := TStringList.Create;
  try
    Tmp.BeginUpdate;

    // ORDER IS IMPORTANT
    Tmp.Add(Key);
    Tmp.Add(GameName);
    Tmp.Add(SortKey);
    Tmp.Add(GameGroup);
    Tmp.Add(Folder);
    Tmp.Add(FileName);
    Tmp.Add(''); // Reserved for future use
    Tmp.Add(DateTimeToStr(LastTime)); // These must be done by cEmutecaPlayingStats...
    Tmp.Add(IntToStr(TimesPlayed));
    Tmp.Add(IntToStr(PlayingTime));
    Tmp.Add('');
    Tmp.Add(Version);
    Tmp.Add(Year);
    Tmp.Add(Publisher);
    Tmp.Add(Zones.CommaText);
    Tmp.Add(Languages.CommaText);
    Tmp.Add(License);
    Tmp.Add(ReleaseType);
    Tmp.Add('');
    Tmp.Add(Tags.CommaText);
    Tmp.Add('');
    Tmp.Add(BoolToStr(Verified));
    Tmp.Add(Alternate);
    Tmp.Add(BadDump);
    Tmp.Add(Fixed);
    Tmp.Add(Trainer);
    Tmp.Add(Translation);
    Tmp.Add(Pirate);
    Tmp.Add(Cracked);
    Tmp.Add(Modified);
    Tmp.Add(Hack);

    tmp.EndUpdate;
    Result := Tmp.CommaText;
  finally
    FreeAndNil(Tmp);
  end;
end;

procedure cEmutecaGame.SetAlternate(const AValue: String);
begin
  FAlternate := Trim(AValue);
end;

procedure cEmutecaGame.SetBadDump(const AValue: String);
begin
  FBadDump := Trim(AValue);
end;

procedure cEmutecaGame.SetCracked(const AValue: String);
begin
  FCracked := Trim(AValue);
end;

procedure cEmutecaGame.SetDataString(const AValue: String);
var
  Tmp: TStringList;
  i: integer;
begin
  tmp := TStringList.Create;
  try
    tmp.CommaText := AValue;

    i := 0;
    while i < Tmp.Count do
    begin
      case i of
        0: Key := Tmp[i];
        1: GameName := Tmp[i];
        2: SortKey := Tmp[i];
        3: GameGroup := Tmp[i];
        4: Folder := Tmp[i];
        5: FileName := Tmp[i];
        // 6:
        7: LastTime := StrToDateTimeDef(Tmp[i], 0); // cEmutecaPlayingStats...
        8: TimesPlayed := StrToInt(Tmp[i]);
        9: PlayingTime := StrToCardinalDef(Tmp[i], 0);
        // 10:
        11: Version := Tmp[i];
        12: Year := Tmp[i];
        13: Publisher := Tmp[i];
        14: Zones.CommaText := Tmp[i];
        15: Languages.CommaText := Tmp[i];
        16: License := Tmp[i];
        17: ReleaseType := Tmp[i];
        // 18:
        19: Tags.CommaText := Tmp[i];
        // 20:
        21: Verified := StrToBoolDef(Tmp[i], False);
        22: Alternate := Tmp[i];
        23: BadDump := Tmp[i];
        24: Fixed := Tmp[i];
        25: Trainer := Tmp[i];
        26: Translation := Tmp[i];
        27: Pirate := Tmp[i];
        28: Cracked := Tmp[i];
        29: Modified := Tmp[i];
        30: Hack := Tmp[i];
      end;
      Inc(i);
    end;
  finally
    FreeAndNil(Tmp);
  end;
end;

procedure cEmutecaGame.SetGameGroup(const AValue: String);
begin
  FGameGroup := Trim(AValue);
end;

procedure cEmutecaGame.SetFixed(const AValue: String);
begin
  FFixed := Trim(AValue);
end;

procedure cEmutecaGame.SetPublisher(const AValue: String);
begin
  FPublisher := Trim(AValue);
end;

procedure cEmutecaGame.SetFileName(const AValue: String);
begin
  FFileName := AValue;
end;

procedure cEmutecaGame.SetFolder(const AValue: String);
begin
  FFolder := SetAsFolder(AValue);
end;

procedure cEmutecaGame.SetHack(const AValue: String);
begin
  FHack := Trim(AValue);
end;

procedure cEmutecaGame.SetLicense(const AValue: String);
begin
  FLicense := Trim(AValue);
end;

procedure cEmutecaGame.SetGameName(const AValue: String);
begin
  // If sortkey is equal to GameName change it too.
  if UTF8CompareText(SortKey, FGameName) = 0 then
    SortKey := Trim(AValue);

  // Same for transliterated GameName
  if UTF8CompareText(TransliteratedName, FGameName) = 0 then
    TransliteratedName := Trim(AValue);

  FGameName := Trim(AValue);
end;

procedure cEmutecaGame.SetPirate(const AValue: String);
begin
  FPirate := Trim(AValue);
end;

procedure cEmutecaGame.SetReleaseType(const AValue: String);
begin
  FReleaseType := Trim(AValue);
end;

procedure cEmutecaGame.SetSortKey(const AValue: String);
begin
  FSortKey := Trim(AValue);
end;

procedure cEmutecaGame.SetTrainer(const AValue: String);
begin
  FTrainer := Trim(AValue);
end;

procedure cEmutecaGame.SetTranslation(const AValue: String);
begin
  FTranslation := Trim(AValue);
end;

procedure cEmutecaGame.SetTransliteratedName(AValue: string);
begin
  FTransliteratedName := Trim(AValue);
end;

procedure cEmutecaGame.SetVerified(const AValue: boolean);
begin
  FVerified := AValue;
end;

procedure cEmutecaGame.SetVersion(const AValue: String);
begin
  FVersion := Trim(AValue);
end;

procedure cEmutecaGame.SetYear(const AValue: String);
begin
  FYear := Trim(AValue);
end;

procedure cEmutecaGame.ExportData(aFilename: String; ExportMode: boolean);
var
  F: TMemInifile;
begin
  F := TMemIniFile.Create(UTF8ToSys(aFilename));
  try
    ExportDataIni(F, ExportMode);
  finally
    FreeAndNil(F);
  end;
end;

procedure cEmutecaGame.ExportDataIni(aIniFile: TCustomIniFile; ExportMode: boolean);
begin
  if aIniFile = nil then
    Exit;
  aIniFile.WriteString(Key, 'Name', GameName);
  aIniFile.WriteString(Key, 'SortName', SortKey);
  aIniFile.WriteString(Key, 'Version', Version);
  aIniFile.WriteString(Key, 'GameGroup', GameGroup);
  aIniFile.WriteString(Key, 'Year', Year);
  aIniFile.WriteString(Key, 'Publisher', Publisher);
  aIniFile.WriteString(Key, 'Zones', Zones.CommaText);
  aIniFile.WriteString(Key, 'Languages', Languages.CommaText);

  aIniFile.WriteString(Key, 'License', License);
  aIniFile.WriteString(Key, 'ReleaseType', ReleaseType);
  aIniFile.WriteString(Key, 'Tags', Tags.CommaText);

  aIniFile.WriteBool(Key, 'Verified', Verified);
  aIniFile.WriteString(Key, 'Alternate', Alternate);
  aIniFile.WriteString(Key, 'BadDump', BadDump);
  aIniFile.WriteString(Key, 'Fixed', Fixed);
  aIniFile.WriteString(Key, 'Hack', Hack);
  aIniFile.WriteString(Key, 'Trainer', Trainer);
  aIniFile.WriteString(Key, 'Translation', Translation);
  aIniFile.WriteString(Key, 'Pirate', Pirate);
  aIniFile.WriteString(Key, 'Cracked', Cracked);
  aIniFile.WriteString(Key, 'Modified', Modified);

  // TODO 2: Esto debería hacerlo cEmutecaPlayingStats...
  if ExportMode then
  begin
    aIniFile.DeleteKey(Key, 'LastTime');
    aIniFile.DeleteKey(Key, 'TimesPlayed');
    aIniFile.DeleteKey(Key, 'PlayingTime');
  end
  else
  begin
    aIniFile.WriteDateTime(Key, 'LastTime', LastTime);
    aIniFile.WriteString(Key, 'TimesPlayed', IntToStr(TimesPlayed));
    aIniFile.WriteString(Key, 'PlayingTime', IntToStr(PlayingTime));
  end;
end;

procedure cEmutecaGame.ImportData(aFilename: String);
var
  F: TMemInifile;
begin
  if not FileExistsUTF8(aFilename) then
    Exit;
  F := TMemIniFile.Create(UTF8ToSys(aFilename));
  try
    ImportDataIni(F);
  finally
    FreeAndNil(F);
  end;
end;

procedure cEmutecaGame.ImportDataIni(aIniFile: TCustomIniFile);
var
  TempStr: String;
begin
  if aIniFile = nil then
    Exit;

  GameName := aIniFile.ReadString(Key, 'Name', GameName);
  SortKey := aIniFile.ReadString(Key, 'SortName', SortKey);
  Version := aIniFile.ReadString(Key, 'Version', Version);
  GameGroup := aIniFile.ReadString(Key, 'GameGroup', GameGroup);
  Year := aIniFile.ReadString(Key, 'Year', Year);
  Publisher := aIniFile.ReadString(Key, 'Publisher', Publisher);
  Zones.CommaText := aIniFile.ReadString(Key, 'Zones', Zones.CommaText);
  Languages.CommaText := aIniFile.ReadString(Key, 'Languages',
    Languages.CommaText);

  License := aIniFile.ReadString(Key, 'License', License);
  ReleaseType := aIniFile.ReadString(Key, 'ReleaseType', ReleaseType);
  Tags.CommaText := aIniFile.ReadString(Key, 'Tags', Tags.CommaText);

  TempStr := aIniFile.ReadString(Key, 'Verified', '');
  if TempStr <> '' then
    Verified := StrToBoolDef(TempStr, Verified);
  Alternate := aIniFile.ReadString(Key, 'Alternate', Alternate);
  BadDump := aIniFile.ReadString(Key, 'BadDump', BadDump);
  Fixed := aIniFile.ReadString(Key, 'Fixed', Fixed);
  Hack := aIniFile.ReadString(Key, 'Hack', Hack);
  Trainer := aIniFile.ReadString(Key, 'Trainer', Trainer);
  Translation := aIniFile.ReadString(Key, 'Translation', Translation);
  Pirate := aIniFile.ReadString(Key, 'Pirate', Pirate);
  Cracked := aIniFile.ReadString(Key, 'Cracked', Cracked);
  Modified := aIniFile.ReadString(Key, 'Modified', Modified);


  // TODO 2: Esto lo debería hacer uGameStats.
  TempStr := aIniFile.ReadString(Key, 'LastTime', '');
  if TempStr <> '' then
    LastTime := StrToDateTimeDef(TempStr, LastTime);
  TempStr := aIniFile.ReadString(Key, 'PlayingTime', '');
  PlayingTime := StrToCardinalDef(TempStr, PlayingTime);
  TempStr := aIniFile.ReadString(Key, 'TimesPlayed', '');
  TimesPlayed := StrToCardinalDef(TempStr, TimesPlayed);
end;

constructor cEmutecaGame.Create(const aFolder: String;
  const aFileName: String; const aKey: String);
begin
  inherited Create;

  GameName := Trim(ExtractFileNameOnly(aFileName));
  SortKey := GameName;
  Folder := SetAsFolder(aFolder);
  FileName := aFileName;
  Key := aKey;
  FLanguages := TStringList.Create;
  FTags := TStringList.Create;
  FZones := TStringList.Create;
  GameGroup := Trim(RemoveFromBrackets(GameName + kEmutecaVirtualGroupExt));
end;

destructor cEmutecaGame.Destroy;
begin
  FreeAndNil(FLanguages);
  FreeAndNil(FTags);
  FreeAndNil(FZones);
  inherited Destroy;
end;

end.
