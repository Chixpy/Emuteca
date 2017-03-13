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

{ cSystem unit. }
unit ucEmutecaSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LazFileUtils, LazUTF8, contnrs, fgl,
  uCHXStrUtils,
  uEmutecaCommon, uaCHXStorable,
  ucEmutecaGroupManager, ucEmutecaPlayingStats;

const
  // Ini file Keys
  // -------------
  krsIniKeyEnabled = 'Enabled';  // TODO: uEmutecaCommon.pas?
  krsIniKeyTitle = 'Title';
  krsIniKeyFileName = 'FileName';
  krsIniKeyExtensions = 'Extensions';
  krsIniKeyBaseFolder = 'BaseFolder';
  krsIniKeyTempFolder = 'TempFolder';
  krsIniKeyMainEmulator = 'MainEmulator';
  krsIniKeyOtherEmulators = 'OtherEmulators';
  krsIniKeyIcon = 'Icon';
  krsIniKeyImage = 'Image';
  krsIniKeyBackImage = 'BackImage';
  krsIniKeyIconFolder = 'IconFolder';
  krsIniKeyImageFolders = 'ImageFolders';
  krsIniKeyImageCaptions = 'ImageCaptions';
  krsIniKeyText = 'Text';
  krsIniKeyTextFolders = 'TextFolders';
  krsIniKeyTextCaptions = 'TextCaptions';
  krsIniKeyGamesKey = 'GamesKey';
  krsIniKeyExtractAll = 'ExtractAll';

  // Constants for file keys
  krsCRC32 = 'CRC32';
  krsSHA1 = 'SHA1';
  krsFileName = 'FileName';
  krsCustom = 'Custom';

resourcestring
  rsLoadingSystemList = 'Loading system list...';
  rsSavingSystemList = 'Saving system list...';
  rsAllSystems = 'All Systems';
  rsSelectSystem = 'Select a System';


type
  TEmutecaFileKey = (TEFKSHA1, TEFKCRC32, TEFKFileName, TEFKCustom);

const
  EmutecaFileKeyStrsK: array [TEmutecaFileKey] of string =
    (krsSHA1, krsCRC32, krsFileName, krsCustom);
//< Strings for FileKeys (fixed constants, used for ini files, etc. )

type
  { cEmutecaSystem }

  cEmutecaSystem = class(caCHXStorable)
  private
    FBackImage: string;
    FBaseFolder: string;
    FEnabled: boolean;
    FExtensions: TStringList;
    FExtractAll: boolean;
    FFileName: string;
    FGameKey: TEmutecaFileKey;
    FIcon: string;
    FIconFolder: string;
    FID: string;
    FImage: string;
    FImageCaptions: TStringList;
    FImageFolders: TStringList;
    FInfoText: string;
    FMainEmulator: string;
    FOtherEmulators: TStringList;
    FGroupManager: cEmutecaGroupManager;
    FStats: cEmutecaPlayingStats;
    FTempFolder: string;
    FTextCaptions: TStringList;
    FTextFolders: TStringList;
    FTitle: string;
    procedure SetBackImage(AValue: string);
    procedure SetBaseFolder(AValue: string);
    procedure SetEnabled(AValue: boolean);
    procedure SetExtractAll(AValue: boolean);
    procedure SetFileName(AValue: string);
    procedure SetGameKey(AValue: TEmutecaFileKey);
    procedure SetIcon(AValue: string);
    procedure SetIconFolder(AValue: string);
    procedure SetID(AValue: string);
    procedure SetImage(AValue: string);
    procedure SetInfoText(AValue: string);
    procedure SetMainEmulator(AValue: string);
    procedure SetTempFolder(AValue: string);
    procedure SetTitle(AValue: string);

  protected
    procedure FixFolderListData(FolderList, CaptionList: TStrings);
    {< Try to fix some incosistences in folders and its captions.

    Who knows if somebody edited the .ini file by hand...
    }

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromIni(aIniFile: TCustomIniFile); override;
    procedure SaveToIni(aIniFile: TCustomIniFile;
      const ExportMode: boolean); override;

    procedure LoadGroups(aFile: string);
    procedure SaveGroups(aFile: string; ExportMode: Boolean);

  published
    property GroupManager: cEmutecaGroupManager read FGroupManager;

    // Basic Info
    // ----------
    property ID: string read FID write SetID;
    {< ID of the system. }

    property Title: string read FTitle write SetTitle;
    {< Visible name (Usually "%Company%: %Model% %(info)%"}

    property FileName: string read FFileName write SetFileName;
    {< Name used for files or folders }

    property Enabled: boolean read FEnabled write SetEnabled;
    {< Is the system visible }

    property ExtractAll: boolean read FExtractAll write SetExtractAll;
    {< Must all files be extracted from compressed archives? }

    property BaseFolder: string read FBaseFolder write SetBaseFolder;
    {< System base folder

       Used by default to store some data if the file isn't defined
         (System image or text).
    }

    property TempFolder: string read FTempFolder write SetTempFolder;
    {< Temp folder for decompress Software, it's recommended leave it empty
         so Emuteca will use OS Temp folder.

       Some cases of use it:
       @unorderedList(
         @item(File must extracted to a specific folder. So emulator
           can recognize it.)
         @item(Emulator can't run from command line and you must browse to the
           file with the emulator... :-@ )
         @item(Lazy testing... >_<U )
       )
     }

    // Emulator related
    // ----------------
    property MainEmulator: string read FMainEmulator write SetMainEmulator;
    {< Main emulator ID. }
    property OtherEmulators: TStringList read FOtherEmulators;
    {< Ids of other emulators for the system. }

    // Images
    // ------
    property Icon: string read FIcon write SetIcon;
    {< Path to the icon of the system. }
    property Image: string read FImage write SetImage;
    {< Path to image of the system. }
    property BackImage: string read FBackImage write SetBackImage;
    {< Image used for as background. }

    property IconFolder: string read FIconFolder write SetIconFolder;
    {< Folder for the icons of the games. }
    property ImageFolders: TStringList read FImageFolders;
    {< Folders for the game images. }
    property ImageCaptions: TStringList read FImageCaptions;
    {< Captions for the folders of game's images. }

    // Texts
    // -----
    property InfoText: string read FInfoText write SetInfoText;
    {< Path to text file of the system. }

    property TextFolders: TStringList read FTextFolders;
    {< Folders for game texts. }
    property TextCaptions: TStringList read FTextCaptions;
    {< Captions for the folders of game's texts. }

    // Import
    // ------
    property GameKey: TEmutecaFileKey read FGameKey write SetGameKey;
    {< Default key (CRC/SHA) to be used as game identifiers
       (when importing/exporting data). }
    property Extensions: TStringList read FExtensions;
    {< Extensions used by the system.

    Only one extension in every string, without dot.
    }

    // Usage statitics
    // ---------------
    property Stats: cEmutecaPlayingStats read FStats;
  end;

  cEmutecaGenSystemList = specialize TFPGObjectList<cEmutecaSystem>;
  cEmutecaSystemList = class(cEmutecaGenSystemList);

  TEmutecaReturnSystemCB = function(aSystem: cEmutecaSystem): boolean of
    object;

{< For CallBack functions }

function Str2EmutecaFileKey(aString: string): TEmutecaFileKey;
// Result := EmutecaFileKeyStrsK[TEmutecaFileKey];

implementation

function Str2EmutecaFileKey(aString: string): TEmutecaFileKey;
begin
  // In Emuteca <= 0.7, True => CRC32 / False => FileName
  aString := UTF8UpperCase(aString);

  // I don't like this "else if" format but it's clearer...
  if (aString = UTF8UpperCase(krsCRC32)) or
    (StrToBoolDef(aString, False)) then
    Result := TEFKCRC32
  else if (aString = UTF8UpperCase(krsFileName)) or
    (not StrToBoolDef(aString, True)) then
    Result := TEFKFileName
  else if (aString = UTF8UpperCase(krsSHA1)) then
    Result := TEFKSHA1
  else if (aString = UTF8UpperCase(krsCustom)) then
    Result := TEFKCustom
  else // Default
    Result := TEFKSHA1;
end;

{ cEmutecaSystem }

procedure cEmutecaSystem.LoadFromIni(aIniFile: TCustomIniFile);
begin
  if aIniFile = nil then
    Exit;

  // Basic data
  Title := aIniFile.ReadString(ID, krsIniKeyTitle, Title);

  FileName := aIniFile.ReadString(ID, krsIniKeyFileName, FileName);
  if FileName = '' then
    FileName := Title;

  Enabled := aIniFile.ReadBool(ID, krsIniKeyEnabled, Enabled);

  ExtractAll := aIniFile.ReadBool(ID, krsIniKeyExtractAll, ExtractAll);

  BaseFolder := aIniFile.ReadString(ID, krsIniKeyBaseFolder, BaseFolder);
  TempFolder := aIniFile.ReadString(ID, krsIniKeyTempFolder, TempFolder);

  // Emulators
  MainEmulator := aIniFile.ReadString(ID, krsIniKeyMainEmulator, MainEmulator);
  OtherEmulators.CommaText :=
    aIniFile.ReadString(ID, krsIniKeyOtherEmulators, OtherEmulators.CommaText);

  // Images
  Icon := aIniFile.ReadString(ID, krsIniKeyIcon, Icon);
  Image := aIniFile.ReadString(ID, krsIniKeyImage, Image);
  BackImage := aIniFile.ReadString(ID, krsIniKeyBackImage, BackImage);

  IconFolder := aIniFile.ReadString(ID, krsIniKeyIconFolder, IconFolder);
  ImageFolders.CommaText :=
    aIniFile.ReadString(ID, krsIniKeyImageFolders, ImageFolders.CommaText);
  ImageCaptions.CommaText :=
    aIniFile.ReadString(ID, krsIniKeyImageCaptions, ImageCaptions.CommaText);

  // Texts
  InfoText := aIniFile.ReadString(ID, krsIniKeyText, InfoText);

  TextFolders.CommaText :=
    aIniFile.ReadString(ID, krsIniKeyTextFolders, TextFolders.CommaText);
  TextCaptions.CommaText :=
    aIniFile.ReadString(ID, krsIniKeyTextCaptions, TextCaptions.CommaText);

  // Import
  GameKey := Str2EmutecaFileKey(aIniFile.ReadString(ID,
    krsIniKeyGamesKey, EmutecaFileKeyStrsK[GameKey]));
  Extensions.CommaText := aIniFile.ReadString(ID, krsIniKeyExtensions,
    Extensions.CommaText);

  Stats.LoadFromIni(aIniFile, ID);

  // Fixing lists...
  FixFolderListData(ImageFolders, ImageCaptions);
  FixFolderListData(TextFolders, TextCaptions);
end;

procedure cEmutecaSystem.SaveToIni(aIniFile: TCustomIniFile;
  const ExportMode: boolean);
begin
  if aIniFile = nil then
    Exit;

  // Basic data
  aIniFile.WriteString(ID, krsIniKeyTitle, Title);
  aIniFile.WriteString(ID, krsIniKeyFileName, FileName);
  aIniFile.WriteBool(ID, krsIniKeyExtractAll, ExtractAll);

  // Emulators
  aIniFile.WriteString(ID, krsIniKeyMainEmulator, MainEmulator);
  aIniFile.WriteString(ID, krsIniKeyOtherEmulators, OtherEmulators.CommaText);

  // Import
  aIniFile.WriteString(ID, krsIniKeyGamesKey, EmutecaFileKeyStrsK[GameKey]);
  aIniFile.WriteString(ID, krsIniKeyExtensions, Extensions.CommaText);

  if ExportMode then
  begin // Las borramos por si acaso existen
    // Basic data
    aIniFile.DeleteKey(ID, krsIniKeyEnabled);
    aIniFile.DeleteKey(ID, krsIniKeyBaseFolder);
    aIniFile.DeleteKey(ID, krsIniKeyTempFolder);

    // Images
    aIniFile.DeleteKey(ID, krsIniKeyIcon);
    aIniFile.DeleteKey(ID, krsIniKeyImage);
    aIniFile.DeleteKey(ID, krsIniKeyBackImage);

    aIniFile.DeleteKey(ID, krsIniKeyIconFolder);
    aIniFile.DeleteKey(ID, krsIniKeyImageFolders);
    aIniFile.DeleteKey(ID, krsIniKeyImageCaptions);

    // Texts
    aIniFile.DeleteKey(ID, krsIniKeyText);

    aIniFile.DeleteKey(ID, krsIniKeyTextFolders);
    aIniFile.DeleteKey(ID, krsIniKeyTextCaptions);
  end
  else
  begin
    // Basic data
    aIniFile.WriteBool(ID, krsIniKeyEnabled, Enabled);
    aIniFile.WriteString(ID, krsIniKeyBaseFolder, BaseFolder);
    aIniFile.WriteString(ID, krsIniKeyTempFolder, TempFolder);

    // Images
    aIniFile.WriteString(ID, krsIniKeyIcon, Icon);
    aIniFile.WriteString(ID, krsIniKeyImage, Image);
    aIniFile.WriteString(ID, krsIniKeyBackImage, BackImage);

    aIniFile.WriteString(ID, krsIniKeyIconFolder, IconFolder);
    aIniFile.WriteString(ID, krsIniKeyImageFolders, ImageFolders.CommaText);
    aIniFile.WriteString(ID, krsIniKeyImageCaptions, ImageCaptions.CommaText);

    // Texts
    aIniFile.WriteString(ID, krsIniKeyText, InfoText);

    aIniFile.WriteString(ID, krsIniKeyTextFolders, TextFolders.CommaText);
    aIniFile.WriteString(ID, krsIniKeyTextCaptions, TextCaptions.CommaText);
  end;

  Stats.WriteToIni(aIniFile, ID, ExportMode);
end;

procedure cEmutecaSystem.LoadGroups(aFile: string);
begin
  GroupManager.Clear;
  if not FileExistsUTF8(aFile) then Exit;

  GroupManager.LoadFromFileTxt(aFile);
end;

procedure cEmutecaSystem.SaveGroups(aFile: string; ExportMode: Boolean);
begin
  if aFile = '' then Exit;
  if not DirectoryExistsUTF8(ExtractFileDir(aFile)) then
    ForceDirectoriesUTF8(ExtractFileDir(aFile));
  GroupManager.SaveToFileTxt(aFile, ExportMode);
end;

procedure cEmutecaSystem.SetBaseFolder(AValue: string);
begin
  FBaseFolder := SetAsFolder(AValue);
end;

procedure cEmutecaSystem.SetBackImage(AValue: string);
begin
  FBackImage := SetAsFile(AValue);
end;

procedure cEmutecaSystem.SetEnabled(AValue: boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
end;

procedure cEmutecaSystem.SetExtractAll(AValue: boolean);
begin
  if FExtractAll = AValue then
    Exit;
  FExtractAll := AValue;
end;

procedure cEmutecaSystem.SetFileName(AValue: string);
begin
  FFileName := CleanFileName(AValue);
end;

procedure cEmutecaSystem.SetGameKey(AValue: TEmutecaFileKey);
begin
  if FGameKey = AValue then
    Exit;
  FGameKey := AValue;
end;

procedure cEmutecaSystem.SetIcon(AValue: string);
begin
  FIcon := SetAsFile(AValue);
end;

procedure cEmutecaSystem.SetIconFolder(AValue: string);
begin
  FIconFolder := SetAsFolder(AValue);
end;

procedure cEmutecaSystem.SetID(AValue: string);
begin
  if FID = AValue then
    Exit;
  FID := AValue;

  FPONotifyObservers(Self, ooChange, nil);
end;

procedure cEmutecaSystem.SetImage(AValue: string);
begin
  FImage := SetAsFile(AValue);
end;

procedure cEmutecaSystem.SetInfoText(AValue: string);
begin
  FInfoText := SetAsFile(AValue);
end;

procedure cEmutecaSystem.SetMainEmulator(AValue: string);
begin
  if FMainEmulator = AValue then
    Exit;
  FMainEmulator := AValue;

  if OtherEmulators.IndexOf(MainEmulator) = -1 then
    OtherEmulators.Add(MainEmulator);
end;

procedure cEmutecaSystem.SetTempFolder(AValue: string);
begin
  FTempFolder := SetAsFolder(AValue);
end;

procedure cEmutecaSystem.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
end;

procedure cEmutecaSystem.FixFolderListData(FolderList, CaptionList: TStrings);
var
  i: integer;
begin
  // Removing empty Folders and asociated captions.
  i := 0;
  while i < FolderList.Count do
  begin
    FolderList[i] := SetAsFolder(FolderList[i]);
    if FolderList[i] = '' then
    begin
      FolderList.Delete(i);
      if i < CaptionList.Count then
        CaptionList.Delete(i);
    end
    else
      Inc(i);
  end;

  // Adding text (folder name) to empty Captions
  if FolderList.Count > CaptionList.Count then
  begin
    i := CaptionList.Count;
    while i < FolderList.Count do
    begin
      CaptionList.Add(ExtractFileName(ExcludeTrailingPathDelimiter(
        FolderList[i])));
      Inc(i);
    end;
  end;

  // Removing exceed of captions
  while FolderList.Count < CaptionList.Count do
    CaptionList.Delete(CaptionList.Count - 1);
end;

constructor cEmutecaSystem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FStats := cEmutecaPlayingStats.Create(Self);
  FGroupManager := cEmutecaGroupManager.Create(Self);

  Self.Enabled := False;
  Self.GameKey := TEFKSHA1;

  Self.FExtensions := TStringList.Create;
  Self.Extensions.CaseSensitive := False;
  self.Extensions.Sorted := True;

  Self.FOtherEmulators := TStringList.Create;
  Self.OtherEmulators.CaseSensitive := False;
  Self.OtherEmulators.Sorted := True;

  Self.FImageCaptions := TStringList.Create;
  Self.FImageFolders := TStringList.Create;

  Self.FTextCaptions := TStringList.Create;
  Self.FTextFolders := TStringList.Create;
end;

destructor cEmutecaSystem.Destroy;
begin
  FreeAndNil(Self.FExtensions);
  FreeAndNil(Self.FOtherEmulators);

  FreeAndNil(Self.FImageCaptions);
  FreeAndNil(Self.FImageFolders);

  FreeAndNil(Self.FTextCaptions);
  FreeAndNil(Self.FTextFolders);

  FreeAndNil(FStats);
  FreeAndNil(FGroupManager);

  inherited Destroy;
end;

end.
