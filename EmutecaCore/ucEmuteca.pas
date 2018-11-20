unit ucEmuteca;
{< cEmuteca class unit.

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
  Classes, SysUtils, fgl, FileUtil, LazUTF8, LazFileUtils, dateutils,
  // CHX units
  uCHX7zWrapper, uCHXStrUtils,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr,
  // Emuteca Core abstracts
  uaEmutecaCustomGroup,
  // Emuteca Core units
  ucEmutecaConfig, ucEmutecaEmulatorManager, ucEmutecaSystemManager,
  ucEmutecaSoftware, ucEmutecaSystem, ucEmutecaEmulator,
  // Emuteca Core threads
  utEmutecaGetSoftSHA1;

type

  { cEmuteca }

  cEmuteca = class(TComponent)
  private
    FBaseFolder: string;
    FGetSoftSHA1Thread: ctEmutecaGetSoftSHA1;
    FConfig: cEmutecaConfig;
    FEmulatorManager: cEmutecaEmulatorManager;
    FProgressCallBack: TEmutecaProgressCallBack;
    FSystemManager: cEmutecaSystemManager;
    FTempFolder: string;
    procedure SetBaseFolder(AValue: string);
    procedure SetGetSoftSHA1Thread(AValue: ctEmutecaGetSoftSHA1);
    procedure SetProgressBar(AValue: TEmutecaProgressCallBack);

  protected
    property GetSoftSHA1Thread: ctEmutecaGetSoftSHA1
      read FGetSoftSHA1Thread write SetGetSoftSHA1Thread;
    procedure GetSoftSHA1ThreadThreadTerminated(Sender: TObject);
    {< Used with ctEmutecaGetSoftSHA1.OnTerminate to change GetSoftSHA1Thread
      to @nil. }

    procedure SetTempFolder(AValue: string);

  public
    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressBar;
    {< Callback function to show progress. }

    property TempFolder: string read FTempFolder;
    {< Emuteca's TempFolder. }

    procedure LoadConfig(aFile: string);

    procedure ClearAllData;
    {< Removes all loaded data. }
    procedure LoadAllData;
    {< Loads all data. }
    procedure SaveAllData;
    {< Saves all data. }

    procedure CleanSystems;
    {< Removes Groups without soft, and Soft not found of all systems. }

    procedure CacheData;
    {< Runs background threads.

      Now only calculates SHA1 of files in background. It was used to
        load icons too, but it's a Emuteca GUI job. }
    procedure UpdateSysEmulators;
    {< (Re)Loads emulators assigned to systems. }

    function RunSoftware(const aSoftware: cEmutecaSoftware): integer;
    {< Runs a software with its current system emulator. }

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property BaseFolder: string read FBaseFolder write SetBaseFolder;
    {< Base folder for relative config paths. }

    property Config: cEmutecaConfig read FConfig;
    {< Config component. }

    property SystemManager: cEmutecaSystemManager read FSystemManager;
    {< System Manager component. }

    property EmulatorManager: cEmutecaEmulatorManager read FEmulatorManager;
    {< Emulator Manager component. }
  end;
  {< Main Emuteca Core class.

  It manages and stores all data and relations. }

implementation

{ cEmuteca }
procedure cEmuteca.CacheData;
begin
  // Teminate thread if it's running
  if assigned(GetSoftSHA1Thread) then
  begin
    // Don't auto nil GetSoftSHA1Thread, only terminate it.
    // GetSoftSHA1Thread will be reused.
    GetSoftSHA1Thread.OnTerminate := nil;
    GetSoftSHA1Thread.Terminate;
    //GetSoftSHA1Thread.WaitFor; Don't wait
  end;
  // GetSoftSHA1Thread.Free; Auto freed with FreeOnTerminate

  if (TempFolder = '') or (not Assigned(SystemManager)) then
    Exit;

  // Caching data in background
  FGetSoftSHA1Thread := ctEmutecaGetSoftSHA1.Create;
  if Assigned(GetSoftSHA1Thread.FatalException) then
    raise GetSoftSHA1Thread.FatalException;
  GetSoftSHA1Thread.OnTerminate := @GetSoftSHA1ThreadThreadTerminated;
  //< Autonil

  GetSoftSHA1Thread.TempFolder := TempFolder;
  GetSoftSHA1Thread.SystemManager := SystemManager;
  GetSoftSHA1Thread.Start;
end;

procedure cEmuteca.UpdateSysEmulators;
begin
    SystemManager.UpdateSystemsEmulators(EmulatorManager.EnabledList);
end;

procedure cEmuteca.LoadConfig(aFile: string);
begin
  Config.LoadConfig(aFile); // if empty, then last config file.

  // Temp folder
  if FilenameIsAbsolute(Config.TempSubfolder) then
    SetTempFolder(Config.TempSubfolder)
  else
    SetTempFolder(SetAsFolder(GetTempDir) + Config.TempSubfolder);
  ForceDirectories(TempFolder);

  // Setting EmulatorManager
  EmulatorManager.DefaultFileName :=
    SetAsAbsoluteFile(Config.EmulatorsFile, BaseFolder);

  // Setting SystemManager
  SystemManager.TempFolder := TempFolder;
  SystemManager.DefaultFileName :=
    SetAsAbsoluteFile(Config.SystemsFile, BaseFolder);
  SystemManager.SysDataFolder :=
    SetAsAbsoluteFile(Config.SysDataFolder, BaseFolder);

  LoadAllData;
end;

procedure cEmuteca.SaveAllData;
begin
  // If we do a cancelable loading, ClearData must be False
  SystemManager.SaveToFile('', True);
  SystemManager.SaveAllEnabledSystemsData;
  EmulatorManager.SaveToFile('', True);
end;

procedure cEmuteca.CleanSystems;
var
  i: integer;
  aSystem: cEmutecaSystem;
  SysPCB: TEmutecaProgressCallBack;
  Continue: boolean;
begin
  i := 0;
  Continue := True;
  while Continue and (i < SystemManager.EnabledList.Count) do
  begin
    aSystem := SystemManager.EnabledList[i];

    // Show only one global progress bar
    SysPCB := aSystem.ProgressCallBack;
    aSystem.ProgressCallBack := nil;

    if assigned(ProgressCallBack) then
      Continue := ProgressCallBack(rsCleaningSystemData,
        aSystem.Title, i, SystemManager.EnabledList.Count, True);

    aSystem.CleanSoftGroupLists;

    // Restoring aSystem progress bar
    aSystem.ProgressCallBack := SysPCB;

    Inc(i);
  end;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

procedure cEmuteca.ClearAllData;
begin
  // If we are still caching...
  if Assigned(GetSoftSHA1Thread) then
    GetSoftSHA1Thread.Terminate;

  EmulatorManager.ClearData;
  SystemManager.ClearData;
end;

procedure cEmuteca.LoadAllData;
begin
  ClearAllData;

  EmulatorManager.LoadFromFile('');
  SystemManager.LoadFromFile('');
  UpdateSysEmulators;

  CacheData;
end;

function cEmuteca.RunSoftware(const aSoftware: cEmutecaSoftware): integer;
var
  aEmulator: cEmutecaEmulator;
  CompressedFile, aFolder, RomFile: string;
  Compressed, NewDir: boolean;
  CompError: integer;
  StartTime: TDateTime;
  TimePlaying: int64;
begin
  // Trying to document step by step with my bad english

  // Uhm. If things go bad from the start, they only can improve :-D
  Result := kErrorRunSoftUnknown;

  if not assigned(aSoftware) then
  begin
    Result := kErrorRunSoftNoSoft;
    Exit;
  end;

  // 1. Searching for current emulator.
  aEmulator := EmulatorManager.EnabledList.ItemById(
    aSoftware.CachedSystem.MainEmulator);

  // TODO: 1.1 Test if emulator support aSoftware extension...

  // TODO: 1.2 If not, ask if try to open, else ask for an emulator...

  if not assigned(aEmulator) then
  begin
    Result := kErrorRunSoftNoEmu;
    Exit;
  end;

  // 2. Setting temp folder.
  //    (Overrided if it's in a compressed file with a subfolder)
  if Trim(ExcludeTrailingPathDelimiter(
    aSoftware.CachedSystem.WorkingFolder)) <> '' then
    aFolder := aSoftware.CachedSystem.WorkingFolder
  else
    aFolder := SetAsFolder(TempFolder + aSoftware.CachedSystem.ListFileName) + krsTempGameSubFolder;

  //   2.1. If don't exists create new, and delete it at the end.
  NewDir := not DirectoryExistsUTF8(aFolder);
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

    if aSoftware.CachedSystem.ExtractAll then
    begin
      // If ExtractAll then make a subfolder inside the temp folder
      aFolder := SetAsFolder(aFolder + ExtractFileNameOnly(CompressedFile));

      NewDir := not DirectoryExistsUTF8(aFolder);
      if NewDir then
        ForceDirectoriesUTF8(aFolder);

      CompError := w7zExtractFile(CompressedFile, AllFilesMask,
        aFolder, True, '')
    end
    else
    begin
      CompError := w7zExtractFile(CompressedFile, aSoftware.FileName,
        aFolder, True, '');
    end;

    if CompError <> 0 then
    begin
      Result := kError7zDecompress - CompError;
      Exit;
    end;
    RomFile := aFolder + aSoftware.FileName;
  end
  else
  begin
    RomFile := aSoftware.Folder + aSoftware.FileName;
  end;

  // Last test if extracting goes wrong...
  if (RomFile = '') or not FileExistsUTF8(RomFile) then
  begin
    Result := kErrorRunSoftNoSoftFile;
    Exit;
  end;

  StartTime := Now; // Stats

  Result := aEmulator.Execute(RomFile, aSoftware.ExtraParameters);

  { TODO : Windows shorcuts don't work }
  TimePlaying := SecondsBetween(Now, StartTime);

  // if Emulator returns no error and passed at least MinTime...
  if (Result = 0) and (TimePlaying >= Config.MinPlayTime) then
  begin
    aSoftware.Stats.AddPlayingTime(StartTime, TimePlaying);
    aSoftware.CachedGroup.Stats.AddPlayingTime(StartTime, TimePlaying);
    aSoftware.CachedSystem.Stats.AddPlayingTime(StartTime, TimePlaying);
    aEmulator.Stats.AddPlayingTime(StartTime, TimePlaying);
  end;

  // X. Kill them all
  if Compressed then
  begin
    if aSoftware.CachedSystem.ExtractAll then
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

procedure cEmuteca.SetProgressBar(AValue: TEmutecaProgressCallBack);
begin
  FProgressCallBack := AValue;
  EmulatorManager.ProgressCallBack := ProgressCallBack;
  SystemManager.ProgressCallBack := ProgressCallBack;
end;

procedure cEmuteca.GetSoftSHA1ThreadThreadTerminated(Sender: TObject);
begin
  GetSoftSHA1Thread := nil;
end;

procedure cEmuteca.SetTempFolder(AValue: string);
begin
  FTempFolder := SetAsFolder(AValue);
end;

procedure cEmuteca.SetGetSoftSHA1Thread(AValue: ctEmutecaGetSoftSHA1);
begin
  if FGetSoftSHA1Thread = AValue then
    Exit;
  FGetSoftSHA1Thread := AValue;
end;

procedure cEmuteca.SetBaseFolder(AValue: string);
begin
  FBaseFolder := SetAsFolder(AValue);
end;

constructor cEmuteca.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  // Creating components
  FConfig := cEmutecaConfig.Create(Self);

  FSystemManager := cEmutecaSystemManager.Create(Self);
  FEmulatorManager := cEmutecaEmulatorManager.Create(Self);
end;

destructor cEmuteca.Destroy;
begin
  // If we are still caching...
  // Teminate thread if it's running
  if assigned(GetSoftSHA1Thread) then
  begin
    GetSoftSHA1Thread.OnTerminate := nil;
    GetSoftSHA1Thread.Terminate;
    GetSoftSHA1Thread.WaitFor;
  end;
  // GetSoftSHA1Thread.Free; Auto freed with FreeOnTerminate

  // Deleting temp folder
  // TODO: Crappy segurity check... :-(
  if (Length(TempFolder) > Length(Config.TempSubfolder) + 5) and
    DirectoryExistsUTF8(TempFolder) then
    DeleteDirectory(TempFolder, False);

  Config.SaveConfig('');

  FreeAndNil(FSystemManager);
  FreeAndNil(FEmulatorManager);
  FreeAndNil(FConfig);
  inherited Destroy;
end;

initialization
  RegisterClass(cEmuteca);

finalization
  UnRegisterClass(cEmuteca);
end.
