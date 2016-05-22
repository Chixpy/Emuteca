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

{ cConfig unit. }
unit ucEmutecaConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8,
  uCHXStrUtils, uCHXRscStr, u7zWrapper;

const
  // Sections and keys for ini file
  // [Images]
  krsIniSectionImages = 'Images';
  krsIniKeyImagesFolder = 'ImagesFolder';
  krsIniKeyDefaultImagesSubfolder = 'DefaultImagesSubfolder';
  krsIniKeyDefaultSystemImage = 'DefaultSystemImage';
  krsIniKeyDefaultSystemIcon = 'DefaultSystemIcon';
  krsIniKeyDefaultEmulatorImage = 'DefaultEmulatorImage';
  krsIniKeyDefaultEmulatorIcon = 'DefaultEmulatorIcon';
  krsIniKeyDefaultGameImage = 'DefaultGameImage';
  krsIniKeyDefaultGameIcon = 'DefaultGameIcon';

  // [Config]
  krsIniSectionConfig = 'Config';

type

  { cEmutecaConfig: Class wich has all general options and configurations
      for Emuteca.

    All folder paths (absolute and relative) have the trailing path separator.
  }
  cEmutecaConfig = class(TComponent)
  private
    FCompanySubFolder: string;
    FCompressedExtensions: TStringList;
    FConfigFile: string;
    FDataFolder: string;
    FDefaultEmulatorIcon: string;
    FDefaultEmulatorImage: string;
    FDefaultGameIcon: string;
    FDefaultGameImage: string;
    FDefaultImagesSubfolder: string;
    FDefaultSystemIcon: string;
    FDefaultSystemImage: string;
    FEmulatorsFile: string;
    FEmulatorSubFolder: string;
    FFlagsSubfolder: string;
    FGameDataExt: string;
    FGameGroupExt: string;
    FCommonMediaFolder: string;
    FGameScriptsSubFolder: string;
    FGeneralScriptsSubFolder: string;
    FGroupScriptsSubFolder: string;
    FHelpFolder: string;
    FIconsIniFile: string;
    FIconsSubfolder: string;
    FImageExtensions: TStringList;
    FImagesFolder: string;
    FMinTime: cardinal;
    FmPlayerExecutable: string;
    FmPlayerSubfolder: string;
    FMusicExtensions: TStringList;
    FParentFile: string;
    FScriptsFolder: string;
    FSearchFile: string;
    FSystemsIniFile: string;
    FTagSubFolder: string;
    FTempFile: string;
    FTempFolder: string;
    FTextExtensions: TStringList;
    FToolsFolder: string;
    FVersionsFile: string;
    FVideoExtensions: TStringList;
    FVIIconsSubfolder: string;
    FYearSubFolder: string;
    Fz7CMExecutable: string;
    Fz7GExecutable: string;
    Fz7Subfolder: string;
    procedure SetCompanySubFolder(const AValue: string);
    procedure SetConfigFile(AValue: string);
    procedure SetDataFolder(const AValue: string);
    procedure SetDefaultEmulatorIcon(const AValue: string);
    procedure SetDefaultEmulatorImage(const AValue: string);
    procedure SetDefaultGameIcon(const AValue: string);
    procedure SetDefaultGameImage(const AValue: string);
    procedure SetDefaultImagesSubfolder(const AValue: string);
    procedure SetDefaultSystemIcon(const AValue: string);
    procedure SetDefaultSystemImage(const AValue: string);
    procedure SetEmulatorsFile(const AValue: string);
    procedure SetEmulatorSubFolder(const AValue: string);
    procedure SetFlagsSubfolder(AValue: string);
    procedure SetGameDataExt(const AValue: string);
    procedure SetGameGroupExt(const AValue: string);
    procedure SetCommonMediaFolder(const AValue: string);
    procedure SetGameScriptsSubFolder(const AValue: string);
    procedure SetGeneralScriptsSubFolder(const AValue: string);
    procedure SetGroupScriptsSubFolder(const AValue: string);
    procedure SetHelpFolder(const AValue: string);
    procedure SetIconsIniFile(const AValue: string);
    procedure SetIconsSubfolder(const AValue: string);
    procedure SetImagesFolder(const AValue: string);
    procedure SetMinTime(const AValue: cardinal);
    procedure SetmPlayerExecutable(AValue: string);
    procedure SetmPlayerSubfolder(AValue: string);
    procedure SetParentFile(AValue: string);
    procedure SetScriptsFolder(const AValue: string);
    procedure SetSearchFile(const AValue: string);
    procedure SetSystemsIniFile(const AValue: string);
    procedure SetTagSubFolder(const AValue: string);
    procedure SetTempFile(const AValue: string);
    procedure SetTempFolder(const AValue: string);
    procedure SetToolsFolder(const AValue: string);
    procedure SetVersionsFile(AValue: string);
    procedure SetVIIconsSubfolder(AValue: string);
    procedure SetYearSubFolder(const AValue: string);
    procedure Setz7CMExecutable(const AValue: string);
    procedure Setz7GExecutable(const AValue: string);
    procedure Setz7Subfolder(const AValue: string);

  published

    // Default images
    property ImagesFolder: string read FImagesFolder write SetImagesFolder;
    property DefaultImagesSubfolder: string
      read FDefaultImagesSubfolder write SetDefaultImagesSubfolder;
    property DefaultSystemImage: string
      read FDefaultSystemImage write SetDefaultSystemImage;
    property DefaultSystemIcon: string
      read FDefaultSystemIcon write SetDefaultSystemIcon;
    property DefaultEmulatorImage: string
      read FDefaultEmulatorImage write SetDefaultEmulatorImage;
    property DefaultEmulatorIcon: string
      read FDefaultEmulatorIcon write SetDefaultEmulatorIcon;
    property DefaultGameImage: string read FDefaultGameImage
      write SetDefaultGameImage;
    property DefaultGameIcon: string read FDefaultGameIcon
      write SetDefaultGameIcon;


    // Tools folder
    property ToolsFolder: string read FToolsFolder write SetToolsFolder;
    property z7Subfolder: string read Fz7Subfolder write Setz7Subfolder;
    property z7CMExecutable: string read Fz7CMExecutable
      write Setz7CMExecutable;
    property z7GExecutable: string read Fz7GExecutable write Setz7GExecutable;


    // CommonMedia folder
    { TODO : Only GUI? }
    property CommonMediaFolder: string
      read FCommonMediaFolder write SetCommonMediaFolder;
    property CompanySubFolder: string read FCompanySubFolder
      write SetCompanySubFolder;
    property YearSubFolder: string read FYearSubFolder write SetYearSubFolder;
    property TagSubFolder: string read FTagSubFolder write SetTagSubFolder;
    property EmulatorSubFolder: string
      read FEmulatorSubFolder write SetEmulatorSubFolder;

    // Scripts
    property ScriptsFolder: string read FScriptsFolder write SetScriptsFolder;
    property GeneralScriptsSubFolder: string
      read FGeneralScriptsSubFolder write SetGeneralScriptsSubFolder;
    property GameScriptsSubFolder: string
      read FGameScriptsSubFolder write SetGameScriptsSubFolder;
    property GroupScriptsSubFolder: string
      read FGroupScriptsSubFolder write SetGroupScriptsSubFolder;

    // Config/Data
    property DataFolder: string read FDataFolder write SetDataFolder;
    property ParentsFile: string read FParentFile write SetParentFile;
    property VersionsFile: string read FVersionsFile write SetVersionsFile;
    property EmulatorsFile: string read FEmulatorsFile write SetEmulatorsFile;
    property SystemsFile: string read FSystemsIniFile write SetSystemsIniFile;

    // File extensions
    property GameDataExt: string read FGameDataExt write SetGameDataExt;
    property GameGroupExt: string read FGameGroupExt write SetGameGroupExt;
    property CompressedExtensions: TStringList read FCompressedExtensions;
    property TextExtensions: TStringList read FTextExtensions;
    property ImageExtensions: TStringList read FImageExtensions;
    property MusicExtensions: TStringList read FMusicExtensions;
    property VideoExtensions: TStringList read FVideoExtensions;

    // Temp
    property TempSubfolder: string read FTempFolder write SetTempFolder;
    property TempFile: string read FTempFile write SetTempFile;


  public
    property ConfigFile: string read FConfigFile write SetConfigFile;
    procedure LoadConfig(aFileName: string);
    procedure SaveConfig(aFilename: string);
    procedure SetDefaultConfig;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  IniFiles;

{ cEmutecaConfig }

procedure cEmutecaConfig.SetDataFolder(const AValue: string);
begin
  FDataFolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetCompanySubFolder(const AValue: string);
begin
  FCompanySubFolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetConfigFile(AValue: string);
begin
  FConfigFile := SetAsFile(AValue);
end;

procedure cEmutecaConfig.SetDefaultEmulatorIcon(const AValue: string);
begin
  FDefaultEmulatorIcon := SetAsFile(AValue);
end;

procedure cEmutecaConfig.SetDefaultEmulatorImage(const AValue: string);
begin
  FDefaultEmulatorImage := SetAsFile(AValue);
end;

procedure cEmutecaConfig.SetDefaultGameIcon(const AValue: string);
begin
  FDefaultGameIcon := SetAsFile(AValue);
end;

procedure cEmutecaConfig.SetDefaultGameImage(const AValue: string);
begin
  FDefaultGameImage := SetAsFile(AValue);
end;

procedure cEmutecaConfig.SetDefaultImagesSubfolder(const AValue: string);
begin
  FDefaultImagesSubfolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetDefaultSystemIcon(const AValue: string);
begin
  FDefaultSystemIcon := SetAsFile(AValue);
end;

procedure cEmutecaConfig.SetDefaultSystemImage(const AValue: string);
begin
  FDefaultSystemImage := SetAsFile(AValue);
end;

procedure cEmutecaConfig.SetEmulatorsFile(const AValue: string);
begin
  FEmulatorsFile := SetAsFile(AValue);
end;

procedure cEmutecaConfig.SetEmulatorSubFolder(const AValue: string);
begin
  FEmulatorSubFolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetFlagsSubfolder(AValue: string);
begin
  FFlagsSubfolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetGameDataExt(const AValue: string);
begin
  FGameDataExt := AValue;
end;

procedure cEmutecaConfig.SetGameGroupExt(const AValue: string);
begin
  FGameGroupExt := AValue;
end;

procedure cEmutecaConfig.SetCommonMediaFolder(const AValue: string);
begin
  FCommonMediaFolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetGameScriptsSubFolder(const AValue: string);
begin
  FGameScriptsSubFolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetGeneralScriptsSubFolder(const AValue: string);
begin
  FGeneralScriptsSubFolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetGroupScriptsSubFolder(const AValue: string);
begin
  FGroupScriptsSubFolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetHelpFolder(const AValue: string);
begin
  FHelpFolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetIconsIniFile(const AValue: string);
begin
  FIconsIniFile := SetAsFile(AValue);
end;

procedure cEmutecaConfig.SetIconsSubfolder(const AValue: string);
begin
  FIconsSubfolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetImagesFolder(const AValue: string);
begin
  FImagesFolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetMinTime(const AValue: cardinal);
begin
  FMinTime := AValue;
end;

procedure cEmutecaConfig.SetmPlayerExecutable(AValue: string);
begin
  FmPlayerExecutable := SetAsFile(AValue);
end;

procedure cEmutecaConfig.SetmPlayerSubfolder(AValue: string);
begin
  FmPlayerSubfolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetParentFile(AValue: string);
begin
  FParentFile := SetAsFile(AValue);
end;

procedure cEmutecaConfig.SetScriptsFolder(const AValue: string);
begin
  FScriptsFolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetSearchFile(const AValue: string);
begin
  FSearchFile := SetAsFile(AValue);
end;

procedure cEmutecaConfig.SetSystemsIniFile(const AValue: string);
begin
  FSystemsIniFile := SetAsFile(AValue);
end;

procedure cEmutecaConfig.SetTagSubFolder(const AValue: string);
begin
  FTagSubFolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetTempFile(const AValue: string);
begin
  FTempFile := SetAsFile(AValue);
end;

procedure cEmutecaConfig.SetTempFolder(const AValue: string);
begin
  FTempFolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetToolsFolder(const AValue: string);
begin
  FToolsFolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetVersionsFile(AValue: string);
begin
  FVersionsFile := SetAsFile(AValue);
end;

procedure cEmutecaConfig.SetVIIconsSubfolder(AValue: string);
begin
  FVIIconsSubfolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetYearSubFolder(const AValue: string);
begin
  FYearSubFolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.Setz7CMExecutable(const AValue: string);
begin
  Fz7CMExecutable := SetAsFile(AValue);
end;

procedure cEmutecaConfig.Setz7GExecutable(const AValue: string);
begin
  Fz7GExecutable := SetAsFile(AValue);
end;

procedure cEmutecaConfig.Setz7Subfolder(const AValue: string);
begin
  Fz7Subfolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.LoadConfig(aFileName: string);
var
  IniFile: TMemIniFile;

begin
  if aFilename = '' then
    aFilename := ConfigFile;
  if aFilename = '' then
    raise EInOutError.Create(self.ClassName + '.ReadConfig: ' +
      rsENotFilename);
  ;
  { TODO : Raise exception? Warning? create file always? Exit?}
  //if not FileExistsUTF8(aFilename) then


  ConfigFile := aFilename;

  IniFile := TMemIniFile.Create(UTF8ToSys(ConfigFile));
  try
    // Images
    ImagesFolder := IniFile.ReadString(krsIniSectionImages,
      krsIniKeyImagesFolder, ImagesFolder);

    DefaultImagesSubfolder :=
      IniFile.ReadString(krsIniSectionImages, krsIniKeyDefaultImagesSubfolder,
      DefaultImagesSubfolder);
    DefaultSystemImage := IniFile.ReadString(krsIniSectionImages,
      krsIniKeyDefaultSystemImage, DefaultSystemImage);
    DefaultSystemIcon := IniFile.ReadString(krsIniSectionImages,
      krsIniKeyDefaultSystemIcon, DefaultSystemIcon);
    DefaultEmulatorImage :=
      IniFile.ReadString(krsIniSectionImages, krsIniKeyDefaultEmulatorImage,
      DefaultEmulatorImage);
    DefaultEmulatorIcon :=
      IniFile.ReadString(krsIniSectionImages, krsIniKeyDefaultEmulatorIcon,
      DefaultEmulatorIcon);
    DefaultGameImage := IniFile.ReadString(krsIniSectionImages,
      krsIniKeyDefaultGameImage, DefaultGameImage);
    DefaultGameIcon := IniFile.ReadString(krsIniSectionImages,
      krsIniKeyDefaultGameIcon, DefaultGameIcon);

    // Config/Data
    DataFolder := IniFile.ReadString('Config', 'DataFolder', DataFolder);
    ParentsFile := IniFile.ReadString('Config', 'ParentsFile', ParentsFile);
    VersionsFile := IniFile.ReadString('Config', 'VersionsFile', VersionsFile);
    EmulatorsFile := IniFile.ReadString('Config', 'EmulatorsFile',
      EmulatorsFile);
    SystemsFile := IniFile.ReadString('Config', 'SystemsFile', SystemsFile);

    // Tools
    ToolsFolder := IniFile.ReadString('Tools', 'ToolsFolder', ToolsFolder);
    z7Subfolder := IniFile.ReadString('Tools', '7zSubfolder', z7Subfolder);
    z7CMExecutable := IniFile.ReadString('Tools', '7zCMExecutable',
      z7CMExecutable);
    z7GExecutable := IniFile.ReadString('Tools', '7zGExecutable',
      z7GExecutable);

    // CommonMedia folders
    CommonMediaFolder := IniFile.ReadString('CommonMedia',
      'CommonMediaFolder', CommonMediaFolder);
    CompanySubFolder := IniFile.ReadString('CommonMedia',
      'CompanySubFolder', CompanySubFolder);
    YearSubFolder := IniFile.ReadString('CommonMedia',
      'YearSubFolder', YearSubFolder);
    TagSubFolder := IniFile.ReadString('CommonMedia', 'TagSubFolder',
      TagSubFolder);
    EmulatorSubFolder := IniFile.ReadString('CommonMedia',
      'EmulatorSubFolder', EmulatorSubFolder);

    // Scripts folders
    ScriptsFolder := IniFile.ReadString('Scripts', 'ScriptsFolder',
      ScriptsFolder);
    GeneralScriptsSubFolder :=
      IniFile.ReadString('Scripts', 'GeneralScriptsSubFolder',
      GeneralScriptsSubFolder);
    GameScriptsSubFolder :=
      IniFile.ReadString('Scripts', 'GameScriptsSubFolder',
      GameScriptsSubFolder);
    GroupScriptsSubFolder :=
      IniFile.ReadString('Scripts', 'GroupScriptsSubFolder',
      GroupScriptsSubFolder);

    // File extensions
    GameDataExt := Trim(UTF8LowerCase(
      IniFile.ReadString('Extensions', 'GameDataExt', GameDataExt)));
    GameGroupExt := Trim(UTF8LowerCase(
      IniFile.ReadString('Extensions', 'GameGroupExt', GameGroupExt)));
    CompressedExtensions.CommaText :=
      Trim(UTF8LowerCase(IniFile.ReadString('Extensions',
      'CompressedExtensions', CompressedExtensions.CommaText)));
    TextExtensions.CommaText :=
      Trim(UTF8LowerCase(IniFile.ReadString('Extensions',
      'TextExtensions', TextExtensions.CommaText)));
    ImageExtensions.CommaText :=
      Trim(UTF8LowerCase(IniFile.ReadString('Extensions',
      'ImageExtensions', ImageExtensions.CommaText)));
    MusicExtensions.CommaText :=
      Trim(UTF8LowerCase(IniFile.ReadString('Extensions',
      'MusicExtensions', MusicExtensions.CommaText)));
    VideoExtensions.CommaText :=
      Trim(UTF8LowerCase(IniFile.ReadString('Extensions',
      'VideoExtensions', VideoExtensions.CommaText)));

    // Temp
    TempSubfolder := IniFile.ReadString('Temp', 'TempSubfolder',
      TempSubfolder);
    TempFile := IniFile.ReadString('Temp', 'TempFile', TempFile);

  finally
    FreeAndNil(IniFile);
  end;
end;

procedure cEmutecaConfig.SaveConfig(aFilename: string);
var
  IniFile: TMemIniFile;
begin
  if aFilename = '' then
    aFilename := ConfigFile;
  if aFilename = '' then
    raise EInOutError.Create(self.ClassName + '.SaveConfig: ' +
      rsENotFilename);
  ConfigFile := aFilename;

  IniFile := TMemIniFile.Create(UTF8ToSys(ConfigFile));
  try

    // Config/Data

    IniFile.WriteString('Config', 'DataFolder', DataFolder);
    IniFile.WriteString('Config', 'ParentsFile', ParentsFile);
    IniFile.WriteString('Config', 'VersionsFile', VersionsFile);
    IniFile.WriteString('Config', 'EmulatorsIniFile', EmulatorsFile);
    IniFile.WriteString('Config', 'SystemsIniFile', SystemsFile);

    // Tools
    IniFile.WriteString('Tools', 'ToolsFolder', ToolsFolder);
    IniFile.WriteString('Tools', '7zSubfolder', z7Subfolder);
    IniFile.WriteString('Tools', '7zCMExecutable', z7CMExecutable);
    IniFile.WriteString('Tools', '7zGExecutable', z7GExecutable);


    // CommonMedia folders
    IniFile.WriteString('CommonMedia', 'CommonMediaFolder', CommonMediaFolder);
    IniFile.WriteString('CommonMedia', 'CompanySubFolder', CompanySubFolder);
    IniFile.WriteString('CommonMedia', 'YearSubFolder', YearSubFolder);
    IniFile.WriteString('CommonMedia', 'TagSubFolder', TagSubFolder);
    IniFile.WriteString('CommonMedia', 'EmulatorSubFolder', EmulatorSubFolder);

    // Scripts folders
    IniFile.WriteString('Scripts', 'ScriptsFolder', ScriptsFolder);
    IniFile.WriteString('Scripts', 'GeneralScriptsSubFolder',
      GeneralScriptsSubFolder);
    IniFile.WriteString('Scripts', 'GameScriptsSubFolder',
      GameScriptsSubFolder);
    IniFile.WriteString('Scripts', 'GroupScriptsSubFolder',
      GroupScriptsSubFolder);

    // File extensions
    IniFile.WriteString('Extensions', 'GameDataExt',
      Trim(UTF8LowerCase(GameDataExt)));
    IniFile.WriteString('Extensions', 'GameGroupExt',
      Trim(UTF8LowerCase(GameGroupExt)));
    IniFile.WriteString('Extensions', 'CompressedExtensions',
      Trim(UTF8LowerCase(CompressedExtensions.CommaText)));
    IniFile.WriteString('Extensions', 'TextExtensions',
      Trim(UTF8LowerCase(TextExtensions.CommaText)));
    IniFile.WriteString('Extensions', 'ImageExtensions',
      Trim(UTF8LowerCase(ImageExtensions.CommaText)));
    IniFile.WriteString('Extensions', 'MusicExtensions',
      Trim(UTF8LowerCase(MusicExtensions.CommaText)));
    IniFile.WriteString('Extensions', 'VideoExtensions',
      Trim(UTF8LowerCase(VideoExtensions.CommaText)));

    // Temp
    IniFile.WriteString('Temp', 'TempSubfolder', TempSubfolder);
    IniFile.WriteString('Temp', 'TempFile', TempFile);

  finally
    FreeAndNil(IniFile);
  end;
end;

procedure cEmutecaConfig.SetDefaultConfig;
begin
  // Images
  ImagesFolder := 'Images';

  DefaultImagesSubfolder := 'Defaults';
  DefaultSystemImage := 'SystemImage.png';
  DefaultSystemIcon := 'SystemIcon.png';
  DefaultEmulatorImage := 'EmulatorImage.png';
  DefaultEmulatorIcon := 'EmulatorIcon.png';
  DefaultGameImage := 'GameImage.png';
  DefaultGameIcon := 'GameIcon.png';


  // Config/Data
  DataFolder := 'Data';
  TagSubFolder := 'Tags';
  ParentsFile := 'Parents.csv';
  VersionsFile := 'Versions.csv';
  EmulatorsFile := 'Emulators.ini';
  SystemsFile := 'Systems.ini';

  // CommonMedia
  CommonMediaFolder := 'Common';
  CompanySubFolder := 'Companies';
  YearSubFolder := 'Years';
  EmulatorSubFolder := 'Emulators';

  // Scripts
  ScriptsFolder := 'Scripts';
  GeneralScriptsSubFolder := 'General';
  GameScriptsSubFolder := 'Game';
  GroupScriptsSubFolder := 'Group';

  // Tools
  ToolsFolder := 'Tools';
  z7Subfolder := '7zip';
  z7CMExecutable := '7z.exe';
  z7GExecutable := '7zG.exe';

  // File extensions
  GameDataExt := 'gam';
  GameGroupExt := 'fam';

  TextExtensions.CommaText := 'txt,nfo';
  ImageExtensions.CommaText :=
    'png,gif,jpg,jpe,jpeg,jfif,bmp,' + 'xpm,pbm,pgm,ppm,ico,icns,cur,tif,tiff';
  MusicExtensions.CommaText := 'ogg,mp3,mid,midi';
  VideoExtensions.CommaText := 'mp4,avi,mpg';
  CompressedExtensions.CommaText := w7zFileExts;

  // Temp
  TempSubfolder := 'tEMpUTECA';
  TempFile := 'Emuteca.tmp';

end;

constructor cEmutecaConfig.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FTextExtensions := TStringList.Create;
  FImageExtensions := TStringList.Create;
  FMusicExtensions := TStringList.Create;
  FVideoExtensions := TStringList.Create;
  FCompressedExtensions := TStringList.Create;

  SetDefaultConfig;
end;

destructor cEmutecaConfig.Destroy;
begin
  FreeAndNil(FTextExtensions);
  FreeAndNil(FImageExtensions);
  FreeAndNil(FMusicExtensions);
  FreeAndNil(FVideoExtensions);
  FreeAndNil(FCompressedExtensions);
  inherited Destroy;
end;

end.
