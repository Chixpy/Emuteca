{ This file is part of Emuteca

  Copyright (C) 2006-2016 Chixpy

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
  Classes, SysUtils, FileUtil, LazUTF8, LazFileUtils,
  u7zWrapper,
  uCHXStrUtils,
  uEmutecaCommon,
  ucEmutecaConfig, ucEmutecaEmulatorManager, ucEmutecaSystemManager,
  ucEmutecaParentManager, ucEmutecaVersionManager,
  ucEmutecaVersion, ucEmutecaParent, ucEmutecaSystem, ucEmutecaEmulator;

type

  { cEmuteca }

  cEmuteca = class(TComponent)
  private
    FConfig: cEmutecaConfig;
    FCurrentEmulator: cEmutecaEmulator;
    FCurrentParent: cEmutecaParent;
    FCurrentSoft: cEmutecaVersion;
    FCurrentSystem: cEmutecaSystem;
    FEmulatorManager: cEmutecaEmulatorManager;
    FParentManager: cEmutecaParentManager;
    FProgressCallBack: TEmutecaProgressCallBack;
    FVersionManager: cEmutecaVersionManager;
    FSystemManager: cEmutecaSystemManager;
    FTempFolder: string;
    procedure SetConfig(AValue: cEmutecaConfig);
    procedure SetCurrentEmulator(AValue: cEmutecaEmulator);
    procedure SetCurrentParent(AValue: cEmutecaParent);
    procedure SetCurrentSoft(AValue: cEmutecaVersion);
    procedure SetCurrentSystem(AValue: cEmutecaSystem);
    procedure SetProgressBar(AValue: TEmutecaProgressCallBack);
    procedure SetTempFolder(AValue: string);

  protected
    property TempFolder: string read FTempFolder write SetTempFolder;

  public
    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressBar;

    procedure LoadConfig(aFile: string);

    function SearchParent(aSoftware: cEmutecaVersion): cEmutecaParent;
    function SearchSystem(aParent: cEmutecaParent): cEmutecaSystem;
    function SearchMainEmulator(aSystem: cEmutecaSystem): cEmutecaEmulator;

    function RunSoftware(const aSoftware: cEmutecaVersion): integer;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Config: cEmutecaConfig read FConfig write SetConfig;

    property SoftManager: cEmutecaVersionManager read FVersionManager;
    property ParentManager: cEmutecaParentManager read FParentManager;
    property EmulatorManager: cEmutecaEmulatorManager read FEmulatorManager;
    property SystemManager: cEmutecaSystemManager read FSystemManager;

    property CurrentSoft: cEmutecaVersion
      read FCurrentSoft write SetCurrentSoft;
    property CurrentParent: cEmutecaParent
      read FCurrentParent write SetCurrentParent;
    property CurrentEmulator: cEmutecaEmulator
      read FCurrentEmulator write SetCurrentEmulator;
    property CurrentSystem: cEmutecaSystem
      read FCurrentSystem write SetCurrentSystem;
  end;

implementation

{ cEmuteca }


procedure cEmuteca.SetTempFolder(AValue: string);
begin
  if FTempFolder = AValue then
    Exit;
  FTempFolder := AValue;
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

  // Setting ParentManager
  ParentManager.DataFile := Config.DataFolder + Config.ParentsFile;
  ParentManager.LoadDataFile;

  // Setting SoftManager
  SoftManager.DataFile := Config.DataFolder + Config.VersionsFile;
  SoftManager.LoadDataFile;
end;

function cEmuteca.SearchParent(aSoftware: cEmutecaVersion): cEmutecaParent;
begin
  Result := ParentManager.ItemById(aSoftware.Parent);
end;

function cEmuteca.SearchSystem(aParent: cEmutecaParent): cEmutecaSystem;
begin
  Result := SystemManager.ItemById(aParent.System);
end;

function cEmuteca.SearchMainEmulator(aSystem: cEmutecaSystem): cEmutecaEmulator;
begin
  Result := EmulatorManager.ItemById(aSystem.MainEmulator);
end;

function cEmuteca.RunSoftware(const aSoftware: cEmutecaVersion): integer;
var
  aEmulator: cEmutecaEmulator;
  aParent: cEmutecaParent;
  aSystem: cEmutecaSystem;
  CompressedFile, aFolder, RomFile: string;
  Compressed, NewDir: Boolean;
  CompError: Integer;
begin

  // Ough... Here are Dragons
  // ------------------------
  // Trying to document step by step with my bad english

  // Uhm. If things go bad from the start, they only can improve :-D
  Result := kEmutecaExecErrorNoGame;

  if not assigned(aSoftware) then
    { TODO : Exception or return Comperror code? }
    exit;

  // First of all, we will asume than aSoftware <> CurrentSoft or
  //   CurrentXs are not coherent, althougth we will test coherency...;
  // NOTE: Really testing to CurrentX is not needed, maybe testing
  //   CurrentParent is somewhat faster...

  // 1. Searching for parent to know the system (test CurrentParent first
  //  for speed,  but it can not be true)
  if (not Assigned(CurrentParent)) or
    (AnsiCompareText(CurrentParent.SortName, aSoftware.Parent) <> 0) then
  begin
    aParent := SearchParent(aSoftware);
  end
  else
    aParent := CurrentParent;
  if not assigned(aParent) then
    { TODO : Exception or return Comperror code? }
    Exit;

  // 2. Searching for system to know the emulator(s) (test CurrentSystem first,
  //  for speed,  but it can not be true)
  if (not Assigned(CurrentSystem)) or
    (AnsiCompareText(CurrentSystem.ID, aParent.System) <> 0) then
  begin
    aSystem := SearchSystem(aParent);
  end
  else
    aSystem := CurrentSystem;
  if not assigned(aSystem) then
    { TODO : Exception or return Comperror code? }
    Exit;

  // 3. Test if emulator support aSoftware extension... (test if CurrentEmulator
  //   belongs to software system first or select main emulator of software)...

  { TODO : Compare with list of emulators }
  if (not Assigned(CurrentEmulator)) or
    (AnsiCompareText(CurrentEmulator.ID, aSystem.MainEmulator) <> 0) then
  begin
    aEmulator := SearchMainEmulator(aSystem);
  end
  else
    aEmulator := CurrentEmulator;

  //   3.a ... if not test for other emulators.
  if not assigned(aEmulator) then
    { TODO : Exception or return Comperror code? }
    Exit;

  // 4. Decompressing archives if needed...

  //   4.1. Testing if aSoftware.Folder is an archive
  //     I don't test extensions... only if the "game's folder" is actually a
  //     file and not a folder.
  CompressedFile := ExcludeTrailingPathDelimiter(aSoftware.Folder);
  Compressed := FileExistsUTF8(CompressedFile);

  //   4.2.
  if Trim(ExcludeTrailingPathDelimiter(aSystem.TempFolder)) <> '' then
    aFolder := SetAsFolder(aSystem.TempFolder)
  else
    aFolder := SetAsFolder(TempFolder);
  aFolder := SetAsFolder(aFolder + krsEmutecaGameSubFolder);

  NewDir := not DirectoryExists(aFolder);
  if NewDir then
    ForceDirectoriesUTF8(aFolder);

  if Compressed then
  begin
    if aSystem.ExtractAll then
      CompError := w7zExtractFile(CompressedFile, AllFilesMask, aFolder, True, '')
    else
      CompError := w7zExtractFile(CompressedFile, aSoftware.FileName,
        aFolder, True, '');
        if CompError <> 0 then
      Exit;
    RomFile := aFolder + aSoftware.FileName;
  end
  else
  begin
     RomFile := SetAsFolder(aSoftware.Folder) + aSoftware.FileName;

     { TODO : I don't remeber this case }
     if (aSystem.ExtractAll) and
      (Config.CompressedExtensions.IndexOf(UTF8Copy(ExtractFileExt(aSoftware.FileName),
      2, MaxInt)) <> -1) then
    begin
      // The ROM is a compressed file but must be extracted anyways
      CompError := w7zExtractFile(RomFile, '*', aFolder, True, '');
      if CompError <> 0 then
        Exit;
      Compressed := True;
    end;
  end;

    // Last test if extracting goes wrong...
  if (RomFile = '') or not FileExistsUTF8(RomFile) then
    // CompError code already set...
    Exit;

  // TempTime := Now;

  Result := aEmulator.Execute(RomFile);

    // if Emulator returns no Comperror and passed at least one minute...
    {
  if (Result = 0) and (Now > (EncodeTime(0, 1, 0, 0) + TempTime)) then
  begin
    aGame.AddPlayingTime(Now, TempTime);
    aGame.LastTime := TempTime;
    aGame.TimesPlayed := aGame.TimesPlayed + 1;
  end;
  }

  // Kill them all
  if Compressed then
  begin
    if aSystem.ExtractAll then
    begin
      DeleteDirectory(aFolder, False);
      if not NewDir then
        ForceDirectoriesUTF8(aFolder);
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

procedure cEmuteca.SetCurrentEmulator(AValue: cEmutecaEmulator);
begin
  if FCurrentEmulator = AValue then
    Exit;
  FCurrentEmulator := AValue;
end;

procedure cEmuteca.SetCurrentParent(AValue: cEmutecaParent);
begin
  if FCurrentParent = AValue then
    Exit;
  FCurrentParent := AValue;
end;

procedure cEmuteca.SetCurrentSoft(AValue: cEmutecaVersion);
begin
  if FCurrentSoft = AValue then
    Exit;
  FCurrentSoft := AValue;
end;

procedure cEmuteca.SetCurrentSystem(AValue: cEmutecaSystem);
begin
  if FCurrentSystem = AValue then
    Exit;
  FCurrentSystem := AValue;
end;

procedure cEmuteca.SetProgressBar(AValue: TEmutecaProgressCallBack);
begin
  FProgressCallBack := AValue;
  EmulatorManager.ProgressCallBack := self.ProgressCallBack;
  SystemManager.ProgressCallBack := self.ProgressCallBack;
  ParentManager.ProgressCallBack := self.ProgressCallBack;
  SoftManager.ProgressCallBack := Self.ProgressCallBack;
end;

constructor cEmuteca.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  // Creating components
  FConfig := cEmutecaConfig.Create(Self);

  FSystemManager := cEmutecaSystemManager.Create(Self);
  FEmulatorManager := cEmutecaEmulatorManager.Create(Self);
  FParentManager := cEmutecaParentManager.Create(Self);
  FVersionManager := cEmutecaVersionManager.Create(Self);
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
  FreeAndNil(FParentManager);
  FreeAndNil(FEmulatorManager);
  FreeAndNil(FSystemManager);
  FreeAndNil(FConfig);
  inherited Destroy;
end;

end.
