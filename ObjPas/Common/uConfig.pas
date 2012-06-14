{ This file is part of Emuteca

  Copyright (C) 2006-2012 Chixpy

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

{cConfig unit}
unit uConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazUTF8, lazutf8classes,
  uCustomUtils;

resourcestring
  rsENotFilename = 'Not defined filename.';

type

  { cConfig: Class wich has all general options and configurations.

    All folder paths (absolute and relative) have the trailing path separator.
  }
  cConfig = class (TObject)
  private
    FCompanySubFolder: String;
    FCompressedExtensions: TStringList;
    FConfigFile: string;
    FDataFolder: String;
    FDefaultEmulatorIcon: String;
    FDefaultEmulatorImage: String;
    FDefaultGameIcon: String;
    FDefaultGameImage: String;
    FDefaultImagesSubfolder: String;
    FDefaultSystemIcon: String;
    FDefaultSystemImage: String;
    FEmulatorsIniFile: String;
    FEmulatorSubFolder: String;
    FFlagsSubfolder: String;
    FGameDataExt: String;
    FGameGroupExt: String;
    FCommonMediaFolder: String;
    FGameScriptsSubFolder: String;
    FGeneralScriptsSubFolder: String;
    FGroupScriptsSubFolder: String;
    FHelpFolder: String;
    FIconsIniFile: String;
    FIconsSubfolder: String;
    FImageExtensions: TStringList;
    FImagesFolder: String;
    FMinTime: Cardinal;
    FmPlayerExecutable: String;
    FmPlayerSubfolder: String;
    FMusicExtensions: TStringList;
    FScriptsFolder: String;
    FSearchFile: String;
    FSystemsIniFile: String;
    FTagSubFolder: String;
    FTempFile: String;
    FTempFolder: String;
    FTextExtensions: TStringList;
    FToolsFolder: String;
    FVideoExtensions: TStringList;
    FVIIconsSubfolder: String;
    FYearSubFolder: String;
    Fz7CMExecutable: String;
    Fz7GExecutable: String;
    Fz7Subfolder: String;
    procedure SetCompanySubFolder(const AValue: String);
    procedure SetConfigFile(AValue: string);
    procedure SetDataFolder(const AValue: String);
    procedure SetDefaultEmulatorIcon(const AValue: String);
    procedure SetDefaultEmulatorImage(const AValue: String);
    procedure SetDefaultGameIcon(const AValue: String);
    procedure SetDefaultGameImage(const AValue: String);
    procedure SetDefaultImagesSubfolder(const AValue: String);
    procedure SetDefaultSystemIcon(const AValue: String);
    procedure SetDefaultSystemImage(const AValue: String);
    procedure SetEmulatorsIniFile(const AValue: String);
    procedure SetEmulatorSubFolder(const AValue: String);
    procedure SetFlagsSubfolder(AValue: String);
    procedure SetGameDataExt(const AValue: String);
    procedure SetGameGroupExt(const AValue: String);
    procedure SetCommonMediaFolder(const AValue: String);
    procedure SetGameScriptsSubFolder(const AValue: String);
    procedure SetGeneralScriptsSubFolder(const AValue: String);
    procedure SetGroupScriptsSubFolder(const AValue: String);
    procedure SetHelpFolder(const AValue: String);
    procedure SetIconsIniFile(const AValue: String);
    procedure SetIconsSubfolder(const AValue: String);
    procedure SetImagesFolder(const AValue: String);
    procedure SetMinTime(const AValue: Cardinal);
    procedure SetmPlayerExecutable(AValue: String);
    procedure SetmPlayerSubfolder(AValue: String);
    procedure SetScriptsFolder(const AValue: String);
    procedure SetSearchFile(const AValue: String);
    procedure SetSystemsIniFile(const AValue: String);
    procedure SetTagSubFolder(const AValue: String);
    procedure SetTempFile(const AValue: String);
    procedure SetTempFolder(const AValue: String);
    procedure SetToolsFolder(const AValue: String);
    procedure SetVIIconsSubfolder(AValue: String);
    procedure SetYearSubFolder(const AValue: String);
    procedure Setz7CMExecutable(const AValue: String);
    procedure Setz7GExecutable(const AValue: String);
    procedure Setz7Subfolder(const AValue: String);

  public
    // Default images
    property ImagesFolder: String read FImagesFolder write SetImagesFolder;
    property DefaultImagesSubfolder: String
      read FDefaultImagesSubfolder write SetDefaultImagesSubfolder;
    property DefaultSystemImage: String
      read FDefaultSystemImage write SetDefaultSystemImage;
    property DefaultSystemIcon: String
      read FDefaultSystemIcon write SetDefaultSystemIcon;
    property DefaultEmulatorImage: String
      read FDefaultEmulatorImage write SetDefaultEmulatorImage;
    property DefaultEmulatorIcon: String
      read FDefaultEmulatorIcon write SetDefaultEmulatorIcon;
    property DefaultGameImage: String
      read FDefaultGameImage write SetDefaultGameImage;
    property DefaultGameIcon: String
      read FDefaultGameIcon write SetDefaultGameIcon;
    property IconsSubfolder: String
      read FIconsSubfolder write SetIconsSubfolder;
    property IconsIniFile: String read FIconsIniFile write SetIconsIniFile;
    property FlagsSubfolder: String read FFlagsSubfolder write SetFlagsSubfolder;
    property VIIconsSubfolder:String read FVIIconsSubfolder write SetVIIconsSubfolder;

    // Tools folder
    property ToolsFolder: String read FToolsFolder write SetToolsFolder;
    property z7Subfolder: String read Fz7Subfolder write Setz7Subfolder;
    property z7CMExecutable: String read Fz7CMExecutable write Setz7CMExecutable;
    property z7GExecutable: String read Fz7GExecutable write Setz7GExecutable;
    property mPlayerSubfolder: String read FmPlayerSubfolder write SetmPlayerSubfolder;
    property mPlayerExecutable: String read FmPlayerExecutable write SetmPlayerExecutable;

    // CommonMedia folder
    property CommonMediaFolder: String
      read FCommonMediaFolder write SetCommonMediaFolder;
    property CompanySubFolder: String
      read FCompanySubFolder write SetCompanySubFolder;
    property YearSubFolder: String
      read FYearSubFolder write SetYearSubFolder;
    property TagSubFolder: String read FTagSubFolder write SetTagSubFolder;
    property EmulatorSubFolder: String
      read FEmulatorSubFolder write SetEmulatorSubFolder;

    // Scripts
    property ScriptsFolder: String read FScriptsFolder
      write SetScriptsFolder;
    property GeneralScriptsSubFolder: String
      read FGeneralScriptsSubFolder write SetGeneralScriptsSubFolder;
    property GameScriptsSubFolder: String
      read FGameScriptsSubFolder write SetGameScriptsSubFolder;
    property GroupScriptsSubFolder: String
      read FGroupScriptsSubFolder write SetGroupScriptsSubFolder;

    // Config/Data
    property HelpFolder: String read FHelpFolder write SetHelpFolder;
    property SearchFile: String read FSearchFile write SetSearchFile;
    property DataFolder: String read FDataFolder write SetDataFolder;
    property EmulatorsIniFile: String
      read FEmulatorsIniFile write SetEmulatorsIniFile;
    property SystemsIniFile: String
      read FSystemsIniFile write SetSystemsIniFile;

    // File extensions
    property GameDataExt: String read FGameDataExt write SetGameDataExt;
    property GameGroupExt: String read FGameGroupExt write SetGameGroupExt;
    property CompressedExtensions: TStringList read FCompressedExtensions;
    property TextExtensions: TStringList read FTextExtensions;
    property ImageExtensions: TStringList read FImageExtensions;
    property MusicExtensions: TStringList read FMusicExtensions;
    property VideoExtensions: TStringList read FVideoExtensions;

    // Temp
    property TempSubfolder: String read FTempFolder write SetTempFolder;
    property TempFile: String read FTempFile write SetTempFile;


    property ConfigFile: string read FConfigFile write SetConfigFile;
    procedure ReadConfig(aFileName: String);
    procedure SaveConfig(aFilename: String);

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  IniFiles;

{ cConfig }

procedure cConfig.SetDataFolder(const AValue: String);
begin
  FDataFolder := SetAsFolder(AValue);
end;

procedure cConfig.SetCompanySubFolder(const AValue: String);
begin
  FCompanySubFolder := SetAsFolder(AValue);
end;

procedure cConfig.SetConfigFile(AValue: string);
begin
  FConfigFile := AValue;
end;

procedure cConfig.SetDefaultEmulatorIcon(const AValue: String);
begin
  FDefaultEmulatorIcon := AValue;
end;

procedure cConfig.SetDefaultEmulatorImage(const AValue: String);
begin
  FDefaultEmulatorImage := AValue;
end;

procedure cConfig.SetDefaultGameIcon(const AValue: String);
begin
  FDefaultGameIcon := AValue;
end;

procedure cConfig.SetDefaultGameImage(const AValue: String);
begin
  FDefaultGameImage := AValue;
end;

procedure cConfig.SetDefaultImagesSubfolder(const AValue: String);
begin
  FDefaultImagesSubfolder := SetAsFolder(AValue);
end;

procedure cConfig.SetDefaultSystemIcon(const AValue: String);
begin
  FDefaultSystemIcon := AValue;
end;

procedure cConfig.SetDefaultSystemImage(const AValue: String);
begin
  FDefaultSystemImage := AValue;
end;

procedure cConfig.SetEmulatorsIniFile(const AValue: String);
begin
  FEmulatorsIniFile :=AValue;
end;

procedure cConfig.SetEmulatorSubFolder(const AValue: String);
begin
  FEmulatorSubFolder := SetAsFolder(AValue);
end;

procedure cConfig.SetFlagsSubfolder(AValue: String);
begin
  FFlagsSubfolder := SetAsFolder(AValue);
end;

procedure cConfig.SetGameDataExt(const AValue: String);
begin
  FGameDataExt := AValue;
end;

procedure cConfig.SetGameGroupExt(const AValue: String);
begin
  FGameGroupExt := AValue;
end;

procedure cConfig.SetCommonMediaFolder(const AValue: String);
begin
  FCommonMediaFolder := SetAsFolder(AValue);
end;

procedure cConfig.SetGameScriptsSubFolder(const AValue: String);
begin
  FGameScriptsSubFolder := SetAsFolder(AValue);
end;

procedure cConfig.SetGeneralScriptsSubFolder(const AValue: String);
begin
  FGeneralScriptsSubFolder := SetAsFolder(AValue);
end;

procedure cConfig.SetGroupScriptsSubFolder(const AValue: String);
begin
  FGroupScriptsSubFolder := SetAsFolder(AValue);
end;

procedure cConfig.SetHelpFolder(const AValue: String);
begin
  FHelpFolder := SetAsFolder(AValue);
end;

procedure cConfig.SetIconsIniFile(const AValue: String);
begin
  FIconsIniFile := AValue;
end;

procedure cConfig.SetIconsSubfolder(const AValue: String);
begin
  FIconsSubfolder := SetAsFolder(AValue);
end;

procedure cConfig.SetImagesFolder(const AValue: String);
begin
  FImagesFolder := SetAsFolder(AValue);
end;

procedure cConfig.SetMinTime(const AValue: Cardinal);
begin
  FMinTime := AValue;
end;

procedure cConfig.SetmPlayerExecutable(AValue: String);
begin
  FmPlayerExecutable := AValue;
end;

procedure cConfig.SetmPlayerSubfolder(AValue: String);
begin
  FmPlayerSubfolder := SetAsFolder(AValue);
end;

procedure cConfig.SetScriptsFolder(const AValue: String);
begin
  FScriptsFolder := SetAsFolder(AValue);
end;

procedure cConfig.SetSearchFile(const AValue: String);
begin
  FSearchFile := AValue;
end;

procedure cConfig.SetSystemsIniFile(const AValue: String);
begin
  FSystemsIniFile := AValue;
end;

procedure cConfig.SetTagSubFolder(const AValue: String);
begin
  FTagSubFolder := SetAsFolder(AValue);
end;

procedure cConfig.SetTempFile(const AValue: String);
begin
  FTempFile := AValue;
end;

procedure cConfig.SetTempFolder(const AValue: String);
begin
  FTempFolder := SetAsFolder(AValue);
end;

procedure cConfig.SetToolsFolder(const AValue: String);
begin
  FToolsFolder := SetAsFolder(AValue);
end;

procedure cConfig.SetVIIconsSubfolder(AValue: String);
begin
  FVIIconsSubfolder := SetAsFolder(AValue);
end;

procedure cConfig.SetYearSubFolder(const AValue: String);
begin
  FYearSubFolder := SetAsFolder(AValue);
end;

procedure cConfig.Setz7CMExecutable(const AValue: String);
begin
  Fz7CMExecutable := AValue;
end;

procedure cConfig.Setz7GExecutable(const AValue: String);
begin
  Fz7GExecutable := AValue;
end;

procedure cConfig.Setz7Subfolder(const AValue: String);
begin
  Fz7Subfolder := SetAsFolder(AValue);
end;

procedure cConfig.ReadConfig(aFileName: String);
var
  IniFile: TMemIniFile;

  // Yeah, I know that using IniFile variable in ReadValue is wrong...
  function ReadValue(const Section, Key, sDefault: String): String;
  begin
    Result := IniFile.ReadString(Section, Key, '');
    if Result = '' then
    begin
      Result := sDefault;
      IniFile.WriteString(Section, Key, sDefault);
      IniFile.UpdateFile;
    end;
  end;

begin
  if aFilename = '' then
    aFilename := ConfigFile;
  if aFilename = '' then Exit;
  if not FileExistsUTF8(aFilename) then
    raise EInOutError.Create(self.ClassName + '.ReadConfig: ' + rsENotFilename);
  ConfigFile := aFilename;

  IniFile := TMemIniFile.Create(UTF8ToSys(ConfigFile));
  try
    // Images
    ImagesFolder := ReadValue('Images', 'ImagesFolder', ImagesFolder);

    DefaultImagesSubfolder :=
      ReadValue('Images', 'DefaultImagesSubfolder', DefaultImagesSubfolder);
    DefaultSystemImage := ReadValue('Images', 'DefaultSystemImage',
      DefaultSystemImage);
    DefaultSystemIcon := ReadValue('Images', 'DefaultSystemIcon',
      DefaultSystemIcon);
    DefaultEmulatorImage :=
      ReadValue('Images', 'DefaultEmulatorImage', DefaultEmulatorImage);
    DefaultEmulatorIcon :=
      ReadValue('Images', 'DefaultEmulatorIcon', DefaultEmulatorIcon);
    DefaultGameImage := ReadValue('Images', 'DefaultGameImage',
      DefaultGameImage);
    DefaultGameIcon := ReadValue('Images', 'DefaultGameIcon', DefaultGameIcon);

    FlagsSubfolder := ReadValue('Images', 'FlagsSubfolder', FlagsSubfolder);
    VIIconsSubfolder := ReadValue('Images', 'VIIconsSubfolder', VIIconsSubfolder);
    IconsSubfolder := ReadValue('Images', 'IconsSubfolder', IconsSubfolder);
    IconsIniFile := ReadValue('Images', 'IconsIniFile', IconsIniFile);

    // Config/Data
    HelpFolder := ReadValue('Config', 'HelpFolder', SearchFile);
    SearchFile := ReadValue('Config', 'SearchFile', SearchFile);
    DataFolder := ReadValue('Config', 'DataFolder', DataFolder);
    EmulatorsIniFile := ReadValue('Config', 'EmulatorsIniFile',
      EmulatorsIniFile);
    SystemsIniFile := ReadValue('Config', 'SystemsIniFile', SystemsIniFile);

    // Tools
    ToolsFolder := ReadValue('Tools', 'ToolsFolder', ToolsFolder);
    z7Subfolder := ReadValue('Tools', '7zSubfolder', z7Subfolder);
    z7CMExecutable := ReadValue('Tools', '7zCMExecutable', z7CMExecutable);
    z7GExecutable := ReadValue('Tools', '7zGExecutable', z7GExecutable);
    mPlayerSubfolder := ReadValue('Tools', 'mPlayerSubfolder', mPlayerSubfolder);
    mPlayerExecutable := ReadValue('Tools', 'mPlayerExecutable', mPlayerExecutable);

    // CommonMedia folders
    CommonMediaFolder := ReadValue('CommonMedia', 'CommonMediaFolder',
      CommonMediaFolder);
    CompanySubFolder := ReadValue('CommonMedia', 'CompanySubFolder',
      CompanySubFolder);
    YearSubFolder := ReadValue('CommonMedia', 'YearSubFolder',
      YearSubFolder);
    TagSubFolder := ReadValue('CommonMedia', 'TagSubFolder', TagSubFolder);
    EmulatorSubFolder := ReadValue('CommonMedia', 'EmulatorSubFolder',
      EmulatorSubFolder);

    // Scripts folders
    ScriptsFolder := ReadValue('Scripts', 'ScriptsFolder', ScriptsFolder);
    GeneralScriptsSubFolder :=
      ReadValue('Scripts', 'GeneralScriptsSubFolder', GeneralScriptsSubFolder);
    GameScriptsSubFolder :=
      ReadValue('Scripts', 'GameScriptsSubFolder', GameScriptsSubFolder);
    GroupScriptsSubFolder :=
      ReadValue('Scripts', 'GroupScriptsSubFolder', GroupScriptsSubFolder);

    // File extensions
    GameDataExt := Trim(UTF8LowerCase(
      ReadValue('Extensions', 'GameDataExt', GameDataExt)));
    GameGroupExt := Trim(UTF8LowerCase(
      ReadValue('Extensions', 'GameGroupExt', GameGroupExt)));
    CompressedExtensions.CommaText :=
      Trim(UTF8LowerCase(ReadValue('Extensions', 'CompressedExtensions',
      CompressedExtensions.CommaText)));
    TextExtensions.CommaText :=
      Trim(UTF8LowerCase(ReadValue('Extensions', 'TextExtensions',
      TextExtensions.CommaText)));
    ImageExtensions.CommaText :=
      Trim(UTF8LowerCase(ReadValue('Extensions', 'ImageExtensions',
      ImageExtensions.CommaText)));
    MusicExtensions.CommaText :=
      Trim(UTF8LowerCase(ReadValue('Extensions', 'MusicExtensions',
      MusicExtensions.CommaText)));
    VideoExtensions.CommaText :=
      Trim(UTF8LowerCase(ReadValue('Extensions', 'VideoExtensions',
      VideoExtensions.CommaText)));

    // Temp
    TempSubfolder := ReadValue('Temp', 'TempSubfolder', TempSubfolder);
    TempFile := ReadValue('Temp', 'TempFile', TempFile);

  finally
    FreeAndNil(IniFile);
  end;
end;

procedure cConfig.SaveConfig(aFilename: String);
var
  IniFile: TMemIniFile;
begin
  if aFilename = '' then
    aFilename := ConfigFile;
  if aFilename = '' then
    raise EInOutError.Create(self.ClassName + '.SaveConfig: ' + rsENotFilename);
  ConfigFile := aFilename;

  IniFile := TMemIniFile.Create(UTF8ToSys(ConfigFile));
  try
    // Images
    IniFile.WriteString('Images', 'ImagesFolder', ImagesFolder);

    IniFile.WriteString('Images', 'DefaultImagesSubfolder', DefaultImagesSubfolder);
    IniFile.WriteString('Images', 'DefaultSystemImage', DefaultSystemImage);
    IniFile.WriteString('Images', 'DefaultSystemIcon', DefaultSystemIcon);
    IniFile.WriteString('Images', 'DefaultEmulatorImage', DefaultEmulatorImage);
    IniFile.WriteString('Images', 'DefaultEmulatorIcon', DefaultEmulatorIcon);
    IniFile.WriteString('Images', 'DefaultGameImage', DefaultGameImage);
    IniFile.WriteString('Images', 'DefaultGameIcon', DefaultGameIcon);

    IniFile.WriteString('Images', 'FlagsSubfolder', FlagsSubfolder);
    IniFile.WriteString('Images', 'VIIconsSubfolder', VIIconsSubfolder);
    IniFile.WriteString('Images', 'IconsSubfolder', IconsSubfolder);
    IniFile.WriteString('Images', 'IconsIniFile', IconsIniFile);

    // Config/Data
    IniFile.WriteString('Config', 'SearchFile', SearchFile);
    IniFile.WriteString('Config', 'DataFolder', DataFolder);
    IniFile.WriteString('Config', 'EmulatorsIniFile', EmulatorsIniFile);
    IniFile.WriteString('Config', 'SystemsIniFile', SystemsIniFile);

    // Tools
    IniFile.WriteString('Tools', 'ToolsFolder', ToolsFolder);
    IniFile.WriteString('Tools', '7zSubfolder', z7Subfolder);
    IniFile.WriteString('Tools', '7zCMExecutable', z7CMExecutable);
    IniFile.WriteString('Tools', '7zGExecutable', z7GExecutable);
    IniFile.WriteString('Tools', 'mPlayerSubfolder', mPlayerSubfolder);
    IniFile.WriteString('Tools', 'mPlayerExecutable', mPlayerExecutable);

    // CommonMedia folders
    IniFile.WriteString('CommonMedia', 'CommonMediaFolder', CommonMediaFolder);
    IniFile.WriteString('CommonMedia', 'CompanySubFolder', CompanySubFolder);
    IniFile.WriteString('CommonMedia', 'YearSubFolder', YearSubFolder);
    IniFile.WriteString('CommonMedia', 'TagSubFolder', TagSubFolder);
    IniFile.WriteString('CommonMedia', 'EmulatorSubFolder', EmulatorSubFolder);

    // Scripts folders
    IniFile.WriteString('Scripts', 'ScriptsFolder', ScriptsFolder);
    IniFile.WriteString('Scripts', 'GeneralScriptsSubFolder', GeneralScriptsSubFolder);
    IniFile.WriteString('Scripts', 'GameScriptsSubFolder', GameScriptsSubFolder);
    IniFile.WriteString('Scripts', 'GroupScriptsSubFolder', GroupScriptsSubFolder);

    // File extensions
    IniFile.WriteString('Extensions', 'GameDataExt', Trim(UTF8LowerCase(GameDataExt)));
    IniFile.WriteString('Extensions', 'GameGroupExt', Trim(UTF8LowerCase(GameGroupExt)));
    IniFile.WriteString('Extensions', 'CompressedExtensions', Trim(UTF8LowerCase(CompressedExtensions.CommaText)));
    IniFile.WriteString('Extensions', 'TextExtensions', Trim(UTF8LowerCase(TextExtensions.CommaText)));
    IniFile.WriteString('Extensions', 'ImageExtensions', Trim(UTF8LowerCase(ImageExtensions.CommaText)));
    IniFile.WriteString('Extensions', 'MusicExtensions', Trim(UTF8LowerCase(MusicExtensions.CommaText)));
    IniFile.WriteString('Extensions', 'VideoExtensions', Trim(UTF8LowerCase(VideoExtensions.CommaText)));

    // Temp
    IniFile.WriteString('Temp', 'TempSubfolder', TempSubfolder);
    IniFile.WriteString('Temp', 'TempFile', TempFile);

  finally
    FreeAndNil(IniFile);
  end;
end;

constructor cConfig.Create;
begin
  inherited;
  FTextExtensions := TStringList.Create;
  FImageExtensions := TStringList.Create;
  FMusicExtensions := TStringList.Create;
  FVideoExtensions := TStringList.Create;
  FCompressedExtensions := TStringList.Create;
end;

destructor cConfig.Destroy;
begin
  FreeAndNil(FTextExtensions);
  FreeAndNil(FImageExtensions);
  FreeAndNil(FMusicExtensions);
  FreeAndNil(FVideoExtensions);
  FreeAndNil(FCompressedExtensions);
  inherited Destroy;
end;

end.

