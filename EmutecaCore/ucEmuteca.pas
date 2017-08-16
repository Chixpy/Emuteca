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
  Classes, SysUtils, fgl, FileUtil, LazUTF8, LazFileUtils, dateutils, sha1,
  u7zWrapper,
  uCHXStrUtils,
  uEmutecaCommon,
  uaEmutecaCustomGroup,
  ucEmutecaConfig,
  ucEmutecaEmulatorManager, ucEmutecaSystemManager,
  ucEmutecaSoftList,
  ucEmutecaSoftware, ucEmutecaSystem, ucEmutecaEmulator;

type
  { Cache data Thread. }

  { cEmutecaCacheDataThread }

  cEmutecaCacheDataThread = class(TThread)
  private
    FSystemManager: cEmutecaSystemManager;
    FTempFolder: string;
    procedure SetSystemManager(AValue: cEmutecaSystemManager);
    procedure SetTempFolder(AValue: string);

  protected
    procedure Execute; override;

  public
    property SystemManager: cEmutecaSystemManager
      read FSystemManager write SetSystemManager;
    property TempFolder: string read FTempFolder write SetTempFolder;
    constructor Create;
  end;

  { cEmuteca }

  cEmuteca = class(TComponent)
  private
    FBaseFolder: string;
    FCacheDataThread: cEmutecaCacheDataThread;
    FConfig: cEmutecaConfig;
    FEmulatorManager: cEmutecaEmulatorManager;
    FProgressCallBack: TEmutecaProgressCallBack;
    FSystemManager: cEmutecaSystemManager;
    FTempFolder: string;
    procedure SetBaseFolder(AValue: string);
    procedure SetCacheDataThread(AValue: cEmutecaCacheDataThread);
    procedure SetProgressBar(AValue: TEmutecaProgressCallBack);

  protected
    property CacheDataThread: cEmutecaCacheDataThread
      read FCacheDataThread write SetCacheDataThread;

    procedure SetTempFolder(AValue: string);

  public
    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressBar;

    property TempFolder: string read FTempFolder;

    procedure LoadConfig(aFile: string);

    procedure ClearAllData;
    procedure LoadData;
    procedure SaveData;

    function SearchMainEmulator(aID: string): cEmutecaEmulator;

    procedure CacheData;

    function RunSoftware(const aSoftware: cEmutecaSoftware): integer;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property BaseFolder: string read FBaseFolder write SetBaseFolder;
    {< Base folder for relative config paths. }

    property Config: cEmutecaConfig read FConfig;

    property SystemManager: cEmutecaSystemManager read FSystemManager;

    property EmulatorManager: cEmutecaEmulatorManager read FEmulatorManager;
  end;

implementation

{ cEmutecaCacheDataThread }

procedure cEmutecaCacheDataThread.SetSystemManager(AValue:
  cEmutecaSystemManager);
begin
  if FSystemManager = AValue then
    Exit;
  FSystemManager := AValue;
end;

procedure cEmutecaCacheDataThread.SetTempFolder(AValue: string);
begin
  FTempFolder := SetAsFolder(AValue);
end;

procedure cEmutecaCacheDataThread.Execute;
var
  aSoft: cEmutecaSoftware;
  // aGroup: cEmutecaGroup;
  aFolder, aFile: string;
  aSha1: TSHA1Digest;
  CurrSysPos, CurrSoftPos: integer;
  SoftList: cEmutecaSoftList;
begin
  if not Assigned(SystemManager) then
    Exit;

  // Already done by system itself.
  //  // Caching groups
  //  CurrSoftPos := 0;
  //  aGroup := nil;
  //  while (not Terminated) and (CurrSoftPos < SoftList.Count) do
  //  begin
  //    aSoft := SoftList[CurrSoftPos];

  //    if not Assigned(aSoft.Group) then
  //    begin
  //      if not aSoft.MatchGroupKey(aGroup) then
  //      begin
  //        aGroup := aSoft.System.GroupManager.ItemById(aSoft.GroupKey, True);
  //      end;
  //      if not terminated then
  //        aSoft.Group := aGroup;
  //    end;

  //    Inc(FCurrSoftPos);
  //  end;

  //  if Terminated then
  //    Exit;

  if TempFolder = '' then
    Exit;

  // Caching SHA1
  try
    CurrSysPos := 0;
    while (not Terminated) and (CurrSysPos < SystemManager.FullList.Count) do
    begin
      SoftList := SystemManager.FullList[CurrSysPos].SoftManager.FullList;

      CurrSoftPos := 0;
      while (not Terminated) and (CurrSoftPos < SoftList.Count) do
      begin
        aSoft := SoftList[CurrSoftPos];
        aFolder := aSoft.Folder;
        aFile := aSoft.FileName;

        if aSoft.SHA1IsEmpty then
        begin
          if DirectoryExistsUTF8(aFolder) then
          begin
            aSha1 := SHA1File(aFolder + aFile);
            if not terminated then
              aSoft.SHA1 := aSha1;
          end
          else
          begin
            w7zExtractFile(aFolder, aFile, TempFolder + 'SHA1Cache/', False, '');
            aSha1 := SHA1File(TempFolder + 'SHA1Cache/' + aFile);
            if not terminated then
              aSoft.SHA1 := aSha1;
            DeleteFileUTF8(TempFolder + 'SHA1Cache/' + aFile);
          end;
        end;
        Inc(CurrSoftPos);
      end;
      Inc(CurrSysPos);
    end;
  finally
    // Catch exception if aSoft/SoftList is deleted while catching...
    // Dirty, nothing is lossed...
    ;
  end;
end;

constructor cEmutecaCacheDataThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

{ cEmuteca }
procedure cEmuteca.CacheData;
begin
  if Assigned(CacheDataThread) then
    CacheDataThread.Terminate; // FreeOnTerminate := true;

  // Caching data in background
  FCacheDataThread := cEmutecaCacheDataThread.Create;
  if Assigned(CacheDataThread.FatalException) then
    raise CacheDataThread.FatalException;
  CacheDataThread.TempFolder := TempFolder;
  CacheDataThread.SystemManager := SystemManager;
  CacheDataThread.Start;
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
  EmulatorManager.IniFileName :=
    SetAsAbsoluteFile(Config.EmulatorsFile, BaseFolder);

  // Setting SystemManager
  SystemManager.TempFolder := TempFolder;
  SystemManager.IniFileName :=
    SetAsAbsoluteFile(Config.SystemsFile, BaseFolder);
  SystemManager.SysDataFolder :=
    SetAsAbsoluteFile(Config.SysDataFolder, BaseFolder);

  LoadData;
end;

procedure cEmuteca.SaveData;
begin
  SystemManager.SaveToFileIni('', False);
  EmulatorManager.SaveToFileIni('', False);
end;

procedure cEmuteca.ClearAllData;
begin
  // If we are still caching...
  if Assigned(CacheDataThread) then
    CacheDataThread.Terminate;

  EmulatorManager.ClearData;
  SystemManager.ClearData;
end;

procedure cEmuteca.LoadData;
begin
  ClearAllData;

  EmulatorManager.LoadData;
  SystemManager.LoadData;

  CacheData;
end;

function cEmuteca.SearchMainEmulator(aID: string): cEmutecaEmulator;
begin
  Result := EmulatorManager.FullList.ItemById(aID);
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
  // TODO: Remove this after Exception/Error codes are implemented :-P
  Result := kEmutecaExecErrorNoGame;

  if not assigned(aSoftware) then
    { TODO : Exception or return Comperror code? }
    exit;

  // 1. Searching for current emulator.
  aEmulator := SearchMainEmulator(aSoftware.CachedSystem.MainEmulator);

  if not assigned(aEmulator) then
    { TODO : Exception or return Comperror code? }
    exit;

  // TODO: 1.1 Test if emulator support aSoftware extension...

  // TODO: 1.2 If not, ask if try to open, else ask for an emulator...

  if not assigned(aEmulator) then
    { TODO : Exception or return Comperror code? }
    Exit;

  // 2. Setting temp folder.
  if Trim(ExcludeTrailingPathDelimiter(aSoftware.CachedSystem.WorkingFolder)) <>
    '' then
    aFolder := aSoftware.CachedSystem.WorkingFolder
  else
    aFolder := TempFolder + krsEmutecaTempGameSubFolder;

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
    if aSoftware.CachedSystem.ExtractAll then
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
    //if aSoftware.CachedSystem.ExtractAll and
    //  SupportedExt(aSoftware.FileName, Config.CompressedExtensions) then
    //begin
    //  // The ROM is a compressed file but must be extracted anyways
    //  CompError := w7zExtractFile(RomFile, AllFilesMask, aFolder, True, '');
    //  if CompError <> 0 then
    //    { TODO : Exception or return Comperror code? }
    //    Exit;
    //  Compressed := True;
    //end;
  end;

  // Last test if extracting goes wrong...
  if (RomFile = '') or not FileExistsUTF8(RomFile) then
    // CompError code already set...
    Exit;

  StartTime := Now; // Stats

  Result := aEmulator.Execute(RomFile);

  { TODO : Windows shorcuts don't work }
  TimePlaying := SecondsBetween(Now, StartTime);

  // if Emulator returns no error and passed at least MinTime...
  if (Result = 0) and (TimePlaying >= Config.MinPlayTime) then
  begin
    { TODO : This are not saved if lists are not saved on exit }
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

procedure cEmuteca.SetTempFolder(AValue: string);
begin
  FTempFolder := SetAsFolder(AValue);
end;

procedure cEmuteca.SetCacheDataThread(AValue: cEmutecaCacheDataThread);
begin
  if FCacheDataThread = AValue then
    Exit;
  FCacheDataThread := AValue;
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
  if Assigned(CacheDataThread) then
    CacheDataThread.Terminate;

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

end.
