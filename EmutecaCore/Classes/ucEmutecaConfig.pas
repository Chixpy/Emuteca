unit ucEmutecaConfig;
{< cEmutecaConfig class unit.

  This file is part of Emuteca

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
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8, Graphics,
  uCHXStrUtils, uCHXRscStr, uCHX7zWrapper;

const
  // [Config]
  krsIniSecConfig = 'Config';
  krsIniKeyDataFolder = 'DataFolder';
  krsIniKeyEmulatorsFile = 'EmulatorsFile';
  krsIniKeyAutoSysFolders = 'AutoSysFolders';
  krsIniKeySystemsFile = 'SystemsFile';
  krsIniKeySysDataFolder = 'SysDataFolder';
  krsIniKeyTagsFolder = 'TagsFolder';

  // [Tools]
  krsIniSecTools = 'Tools';
  krsIniKey7zCMExecutable = '7zCMExecutable';
  krsIniKey7zGExecutable = '7zGExecutable';

  // [Extensions]
  krsIniSecExtensions = 'Extensions';
  krsIniKeyCompressedExtensions = 'CompressedExtensions';

  // [Temp]
  krsIniSecTemp = 'Temp';
  krsIniKeyTempSubfolder = 'TempSubfolder';
  krsIniKeyTempFile = 'TempFile';

  // [Misc]
  krsIniSecMisc = 'Misc';
  krsIniKeyMinPlayTime = 'MinPlayTime';

type

  { cEmutecaConfig }
  // TODO: Use caCHXConfig
  cEmutecaConfig = class(TComponent)
  private
    FAutoSysFolder: string;
    FCompressedExtensions: TStringList;
    FConfigFile: string;
    FEmulatorsFile: string;
    FSysDataFolder: string;
    FMinPlayTime: integer;
    FSystemsFile: string;
    FTagsFolder: string;
    FTempFile: string;
    FTempSubFolder: string;
    FSoftFile: string;
    Fz7CMExecutable: string;
    Fz7GExecutable: string;
    procedure SetAutoSysFolder(AValue: string);
    procedure SetConfigFile(AValue: string);
    procedure SetEmulatorsFile(const AValue: string);
    procedure SetSysDataFolder(AValue: string);
    procedure SetMinPlayTime(AValue: integer);
    procedure SetSystemsFile(const AValue: string);
    procedure SetTagsFolder(AValue: string);
    procedure SetTempFile(const AValue: string);
    procedure SetTempSubFolder(const AValue: string);
    procedure SetSoftFile(AValue: string);
    procedure Setz7CMExecutable(const AValue: string);
    procedure Setz7GExecutable(const AValue: string);

  protected

  published
    // Tools
    property z7CMExecutable: string read Fz7CMExecutable write Setz7CMExecutable;
    {< 7z.exe path. }
    property z7GExecutable: string read Fz7GExecutable write Setz7GExecutable;
    {< 7zG.exe path. }

    // Config/Data
    property EmulatorsFile: string read FEmulatorsFile write SetEmulatorsFile;
    property SystemsFile: string read FSystemsFile write SetSystemsFile;
    property SysDataFolder: string read FSysDataFolder write SetSysDataFolder;
    property AutoSysFolder: string read FAutoSysFolder write SetAutoSysFolder;
    property TagsFolder: string read FTagsFolder write SetTagsFolder;

    // File extensions
    property CompressedExtensions: TStringList read FCompressedExtensions;
    {< List of compressed file extensions. }

    // Temp folder/file
    property TempSubfolder: string read FTempSubFolder write SetTempSubFolder;
    {< Subfolder for temporal files.

      It can be a full path folder, or if a relative path is stored
        a subfolder for system's temporal folder. }
    property TempFile: string read FTempFile write SetTempFile;

    // Misc
    property MinPlayTime: integer read FMinPlayTime write SetMinPlayTime;
    {< Minimal time playing needed to store in statistics. }

  public
    property ConfigFile: string read FConfigFile write SetConfigFile;
    procedure LoadConfig(aFileName: string);
    procedure SaveConfig(aFilename: string);
    procedure SetDefaultConfig;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;
  {< Class wich has all general options and configurations for Emuteca Core.

    All folder paths (absolute and relative) have the trailing path separator.
  }
implementation

uses
  IniFiles;

{ cEmutecaConfig }
procedure cEmutecaConfig.SetConfigFile(AValue: string);
begin
  FConfigFile := SetAsFile(AValue);
end;

procedure cEmutecaConfig.SetAutoSysFolder(AValue: string);
begin
  if FAutoSysFolder = AValue then
    Exit;
  FAutoSysFolder := AValue;
end;

procedure cEmutecaConfig.SetEmulatorsFile(const AValue: string);
begin
  FEmulatorsFile := SetAsFile(AValue);
end;

procedure cEmutecaConfig.SetSysDataFolder(AValue: string);
begin
  FSysDataFolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetMinPlayTime(AValue: integer);
begin
  if FMinPlayTime = AValue then
    Exit;
  FMinPlayTime := AValue;

  if MinPlayTime < 1 then
    FMinPlayTime := 60;
end;

procedure cEmutecaConfig.SetSystemsFile(const AValue: string);
begin
  FSystemsFile := SetAsFile(AValue);
end;

procedure cEmutecaConfig.SetTagsFolder(AValue: string);
begin
  FTagsFolder :=  SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetTempFile(const AValue: string);
begin
  FTempFile := SetAsFile(AValue);
end;

procedure cEmutecaConfig.SetTempSubFolder(const AValue: string);
begin
  FTempSubFolder := SetAsFolder(AValue);
end;

procedure cEmutecaConfig.SetSoftFile(AValue: string);
begin
  FSoftFile := SetAsFile(AValue);
end;

procedure cEmutecaConfig.Setz7CMExecutable(const AValue: string);
begin
  Fz7CMExecutable := SetAsFile(AValue);

   // TODO: This must be done by cEmuteca and use cEmuteca.BaseDir as base
  if not FilenameIsAbsolute(z7CMExecutable) then
    w7zSetPathTo7zexe(CreateAbsoluteSearchPath(z7CMExecutable, GetCurrentDirUTF8));
end;

procedure cEmutecaConfig.Setz7GExecutable(const AValue: string);
begin
  Fz7GExecutable := SetAsFile(AValue);

  // TODO: This must be done by cEmuteca and use cEmuteca.BaseDir as base
  if not FilenameIsAbsolute(z7GExecutable) then
    w7zSetPathTo7zGexe(CreateAbsoluteSearchPath(z7GExecutable, GetCurrentDirUTF8));
end;

procedure cEmutecaConfig.LoadConfig(aFileName: string);
var
  IniFile: TMemIniFile;

begin
  if aFilename = '' then
    aFilename := ConfigFile;
  if aFilename = '' then
    raise EInOutError.Create(ClassName + '.ReadConfig: ' +
      rsENotFilename);
  { TODO : Raise exception? Warning? create file always? Exit?}
  //if not FileExistsUTF8(aFilename) then


  ConfigFile := aFilename;

  IniFile := TMemIniFile.Create(UTF8ToSys(ConfigFile));
  try
    // Config/Data
    EmulatorsFile := IniFile.ReadString(krsIniSecConfig,
      krsIniKeyEmulatorsFile, EmulatorsFile);
    SystemsFile := IniFile.ReadString(krsIniSecConfig,
      krsIniKeySystemsFile, SystemsFile);
    AutoSysFolder := IniFile.ReadString(krsIniSecConfig,
      krsIniKeyAutoSysFolders, AutoSysFolder);
    SysDataFolder := IniFile.ReadString(krsIniSecConfig,
      krsIniKeySysDataFolder, SysDataFolder);
    TagsFolder := IniFile.ReadString(krsIniSecConfig,
      krsIniKeyTagsFolder, TagsFolder);

    // Tools
    z7CMExecutable := IniFile.ReadString(krsIniSecTools,
      krsIniKey7zCMExecutable, z7CMExecutable);
    z7GExecutable := IniFile.ReadString(krsIniSecTools,
      krsIniKey7zGExecutable, z7GExecutable);

    // File extensions
    CompressedExtensions.CommaText :=
      Trim(UTF8LowerCase(IniFile.ReadString(krsIniSecExtensions,
      krsIniKeyCompressedExtensions, CompressedExtensions.CommaText)));

    // Temp
    TempSubfolder := IniFile.ReadString(krsIniSecTemp,
      krsIniKeyTempSubfolder, TempSubfolder);
    TempFile := IniFile.ReadString(krsIniSecTemp,
      krsIniKeyTempFile, TempFile);

    // Misc
    MinPlayTime := IniFile.ReadInteger(krsIniSecMisc,
      krsIniKeyMinPlayTime, MinPlayTime);

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
    raise EInOutError.Create(ClassName + '.SaveConfig: ' +
      rsENotFilename);
  ConfigFile := aFilename;

  IniFile := TMemIniFile.Create(UTF8ToSys(ConfigFile));
  try

    // Config/Data
    IniFile.WriteString(krsIniSecConfig, krsIniKeyEmulatorsFile,
      EmulatorsFile);
    IniFile.WriteString(krsIniSecConfig, krsIniKeyAutoSysFolders,
      AutoSysFolder);
    IniFile.WriteString(krsIniSecConfig, krsIniKeySystemsFile, SystemsFile);
    IniFile.WriteString(krsIniSecConfig, krsIniKeySysDataFolder,
      SysDataFolder);
    IniFile.WriteString(krsIniSecConfig, krsIniKeyTagsFolder, TagsFolder);

    // Tools
    IniFile.WriteString(krsIniSecTools, krsIniKey7zCMExecutable,
      z7CMExecutable);
    IniFile.WriteString(krsIniSecTools, krsIniKey7zGExecutable,
      z7GExecutable);

    // File extensions
    IniFile.WriteString(krsIniSecExtensions, krsIniKeyCompressedExtensions,
      Trim(UTF8LowerCase(CompressedExtensions.CommaText)));

    // Temp
    IniFile.WriteString(krsIniSecTemp, krsIniKeyTempSubfolder, TempSubfolder);
    IniFile.WriteString(krsIniSecTemp, krsIniKeyTempFile, TempFile);

    // Misc
    IniFile.WriteInteger(krsIniSecMisc, krsIniKeyMinPlayTime, MinPlayTime);

  finally
    FreeAndNil(IniFile);
  end;
end;

procedure cEmutecaConfig.SetDefaultConfig;
  procedure DeleteComprExt(const aExt: string);
  var
    aPos: Integer;
  begin
    aPos := CompressedExtensions.IndexOf(aExt);
    if aPos <> -1 then
      CompressedExtensions.Delete(aPos);
  end;

begin
  // Config/Data
  EmulatorsFile := 'Data/Emulators.ini';
  AutoSysFolder := 'Data/SysFolders.csv';
  SystemsFile := 'Data/Systems.ini';
  SysDataFolder := 'Systems/';
  TagsFolder := 'Tags/';

  // Tools
  z7CMExecutable := 'Tools/7zip/7z.exe';
  z7GExecutable := 'Tools/7zip/7zG.exe';
  // CompressedExtensions.CommaText := w7zGetFileExts;
  // Removed ext: cab,chm,dll,iso,img,jar,msi,swf,ntfs,ppt,doc,,xls,xpi,vhd,
  //   deb,rpm,cpio,cramfs,dmg,fat,flv,mbr,nsis,sys,bpl,hfs,hxi,hxq,hxr,hxs,
  //   chi,chq,chw,hxw,msp,scap,squashfs,swm,wim,exe,
  CompressedExtensions.CommaText :='001,7z,arj,bz2,bzip2,cba,cb7,cbr,cbz,' +
    'gz,gzip,lha,lit,lzh,lzma,lzma86,r00,rar,tar,taz,tbz,tbz2,tgz,tpz,txz,' +
    'xar,xz,z,zip';

  // Temp
  TempSubfolder := 'tEMpUTECA/';
  TempFile := 'Emuteca.tmp';

  // Misc
  MinPlayTime := 60;

end;

constructor cEmutecaConfig.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FCompressedExtensions := TStringList.Create;
  CompressedExtensions.Sorted := True;
  CompressedExtensions.CaseSensitive := False;

  SetDefaultConfig;
end;

destructor cEmutecaConfig.Destroy;
begin
  FreeAndNil(FCompressedExtensions);
  inherited Destroy;
end;

end.
