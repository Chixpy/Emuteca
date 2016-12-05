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

{ cEmuteca unit. }
unit ucEmuteca;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazUTF8, LazFileUtils, dateutils,
  u7zWrapper,
  uCHXStrUtils,
  uEmutecaCommon,
  ucEmutecaConfig, ucEmutecaEmulatorManager, ucEmutecaSystemManager,
  ucEmutecaGroupManager, ucEmutecaSoftManager,
  ucEmutecaSoftware, ucEmutecaGroup, ucEmutecaSystem, ucEmutecaEmulator;

type

  { cEmuteca }

  cEmuteca = class(TComponent)
  private
    FConfig: cEmutecaConfig;
    FEmulatorManager: cEmutecaEmulatorManager;
    FGroupManager: cEmutecaGroupManager;
    FProgressCallBack: TEmutecaProgressCallBack;
    FVersionManager: cEmutecaSoftManager;
    FSystemManager: cEmutecaSystemManager;
    FTempFolder: string;
    procedure SetConfig(AValue: cEmutecaConfig);
    procedure SetProgressBar(AValue: TEmutecaProgressCallBack);
    procedure SetTempFolder(AValue: string);

  protected


  public
    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressBar;

    { TODO: Maybe be protected. Remove all references to this

      Y traer los procedimientos que lo usen aquí. }
    property TempFolder: string read FTempFolder write SetTempFolder;

    procedure LoadConfig(aFile: string);

    function SearchGroup(aID: string): cEmutecaGroup;
    function SearchSystem(aID: string): cEmutecaSystem;
    function SearchMainEmulator(aID: string): cEmutecaEmulator;

    function RunSoftware(const aSoftware: cEmutecaSoftware): integer;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Config: cEmutecaConfig read FConfig write SetConfig;

    property SoftManager: cEmutecaSoftManager read FVersionManager;
    property GroupManager: cEmutecaGroupManager read FGroupManager;
    property EmulatorManager: cEmutecaEmulatorManager read FEmulatorManager;
    property SystemManager: cEmutecaSystemManager read FSystemManager;


  end;

implementation

{ cEmuteca }


procedure cEmuteca.SetTempFolder(AValue: string);
begin
  FTempFolder := SetAsFolder(AValue);
end;

procedure cEmuteca.LoadConfig(aFile: string);
begin
  Config.LoadConfig(aFile);

  // Temp folder
  TempFolder := SetAsFolder(GetTempDir) + Config.TempSubfolder;
  ForceDirectories(TempFolder);

  // Creating Data Folder...
  ForceDirectories(Config.DataFolder);

  // Setting EmulatorManager
  EmulatorManager.DataFile := Config.DataFolder + Config.EmulatorsFile;
  EmulatorManager.LoadFromFile('');

  // Setting SystemManager
  SystemManager.DataFile := Config.DataFolder + Config.SystemsFile;
  SystemManager.LoadFromFile('');

  // Setting GroupManager
  SoftManager.SystemManager := SystemManager;
  GroupManager.DataFile := Config.DataFolder + Config.GroupsFile;
  GroupManager.LoadFromFile('');

  // Setting SoftManager
  SoftManager.SystemManager := SystemManager;
  SoftManager.GroupManager := GroupManager;
  SoftManager.DataFile := Config.DataFolder + Config.SoftFile;
  SoftManager.LoadFromFile('');
end;

function cEmuteca.SearchGroup(aID: string): cEmutecaGroup;
begin
  Result := GroupManager.ItemById(aID);
end;

function cEmuteca.SearchSystem(aID: string): cEmutecaSystem;
begin
  Result := SystemManager.ItemById(aID);
end;

function cEmuteca.SearchMainEmulator(aID: string): cEmutecaEmulator;
begin
  Result := EmulatorManager.ItemById(aID);
end;

function cEmuteca.RunSoftware(const aSoftware: cEmutecaSoftware): integer;
var
  aEmulator: cEmutecaEmulator;
  CompressedFile, aFolder, RomFile: string;
  Compressed, NewDir: boolean;
  CompError: integer;
  StartTime: TDateTime;
  TimePlaying: Int64;
begin
  // Trying to document step by step with my bad english

  // Uhm. If things go bad from the start, they only can improve :-D
  // TODO: Remove this after Exception/Error codes are implemented :-P
  Result := kEmutecaExecErrorNoGame;

  if not assigned(aSoftware) then
    { TODO : Exception or return Comperror code? }
    exit;

  // 1. Searching for current emulator.
  aEmulator := SearchMainEmulator(aSoftware.System.MainEmulator);

  if not assigned(aEmulator) then
    { TODO : Exception or return Comperror code? }
    exit;

  // TODO: 1.1 Test if emulator support aSoftware extension...

  // TODO: 1.2 If not, ask if try to open, else ask for an emulator...

  if not assigned(aEmulator) then
    { TODO : Exception or return Comperror code? }
    Exit;

  // 2. Setting temp folder.
  if Trim(ExcludeTrailingPathDelimiter(aSoftware.System.TempFolder)) <> '' then
    aFolder := aSoftware.System.TempFolder
  else
    aFolder := Self.TempFolder + krsEmutecaGameSubFolder;

  //   2.1. If don't exists create new, and mark it to delete at the end.
  NewDir := not DirectoryExists(aFolder);
  if NewDir then
    ForceDirectoriesUTF8(aFolder);

  // 3. Decompressing archives if needed...

  //   3.1. Testing if aSoftware.Folder is an archive
  //     I don't test extensions... only if the "game's folder" is actually a
  //     file and not a folder.
  CompressedFile := ExcludeTrailingPathDelimiter(aSoftware.Folder);
  Compressed := FileExistsUTF8(CompressedFile);

  //   3.2 Actual extracting, and setting RomFile
  if Compressed then
  begin
    if aSoftware.System.ExtractAll then
      CompError := w7zExtractFile(CompressedFile, AllFilesMask,
        aFolder, True, '')
    else
      CompError := w7zExtractFile(CompressedFile, aSoftware.FileName,
        aFolder, True, '');
    if CompError <> 0 then
      { TODO : Exception or return Comperror code? }
      Exit;
    RomFile := aFolder + aSoftware.FileName;
  end
  else
  begin
    RomFile := aSoftware.Folder + aSoftware.FileName;

    { TODO : I don't remember why I implemened this.
        When it is a normal "decompressed" ROM, if it is a 7z and
        ExtractAll is True then decompress it.

        RomFile := ZipFileName anyways. }
    if aSoftware.System.ExtractAll and
      SupportedExt(aSoftware.FileName, Config.CompressedExtensions) then
    begin
      // The ROM is a compressed file but must be extracted anyways
      CompError := w7zExtractFile(RomFile, AllFilesMask, aFolder, True, '');
      if CompError <> 0 then
       { TODO : Exception or return Comperror code? }
        Exit;
      Compressed := True;
    end;
  end;

  // Last test if extracting goes wrong...
  if (RomFile = '') or not FileExistsUTF8(RomFile) then
    // CompError code already set...
    Exit;

  StartTime := Now; // Stats

  Result := aEmulator.Execute(RomFile);

  TimePlaying := SecondsBetween(Now, StartTime);

  // if Emulator returns no error and passed at least MinTime...
  if (Result = 0) and (TimePlaying >=  Config.MinPlayTime) then
  begin
    { TODO : This are not saved if lists are not saved on exit }
    aSoftware.Stats.AddPlayingTime(StartTime, TimePlaying);
    aSoftware.Group.Stats.AddPlayingTime(StartTime, TimePlaying);

    { TODO : System and Emulator are not saved unless you save it
      in their managers }
    aSoftware.System.Stats.AddPlayingTime(StartTime, TimePlaying);
    aEmulator.Stats.AddPlayingTime(StartTime, TimePlaying);
  end;

  // X. Kill them all
  if Compressed then
  begin
    if aSoftware.System.ExtractAll then
    begin
      DeleteDirectory(aFolder, not NewDir);
    end
    else
    begin
      if FileExistsUTF8(RomFile) then
        DeleteFileUTF8(RomFile);
    end;
  end;
end;

procedure cEmuteca.SetConfig(AValue: cEmutecaConfig);
begin
  if FConfig = AValue then
    Exit;
  FConfig := AValue;
end;

procedure cEmuteca.SetProgressBar(AValue: TEmutecaProgressCallBack);
begin
  FProgressCallBack := AValue;
  EmulatorManager.ProgressCallBack := self.ProgressCallBack;
  SystemManager.ProgressCallBack := self.ProgressCallBack;
  GroupManager.ProgressCallBack := self.ProgressCallBack;
  SoftManager.ProgressCallBack := Self.ProgressCallBack;
end;

constructor cEmuteca.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  // Creating components
  FConfig := cEmutecaConfig.Create(Self);

  FSystemManager := cEmutecaSystemManager.Create(Self);
  FEmulatorManager := cEmutecaEmulatorManager.Create(Self);
  FGroupManager := cEmutecaGroupManager.Create(Self);
  FVersionManager := cEmutecaSoftManager.Create(Self);
end;

destructor cEmuteca.Destroy;
begin
  // Deleting temp folder
  // TODO: Crappy segurity check... :-(
  if (Length(TempFolder) > Length(Config.TempSubfolder) + 5) and
    DirectoryExistsUTF8(TempFolder) then
    DeleteDirectory(TempFolder, False);

  Config.SaveConfig('');

  FreeAndNil(FVersionManager);
  FreeAndNil(FGroupManager);
  FreeAndNil(FEmulatorManager);
  FreeAndNil(FSystemManager);
  FreeAndNil(FConfig);
  inherited Destroy;
end;

end.
