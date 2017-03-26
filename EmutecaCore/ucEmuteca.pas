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
  u7zWrapper, sha1,
  uCHXStrUtils,
  uEmutecaCommon,
  ucEmutecaConfig, ucEmutecaEmulatorManager, ucEmutecaSystemManager,
  ucEmutecaGroupManager, ucEmutecaSoftManager,
  ucEmutecaSoftware, ucEmutecaGroup, ucEmutecaSystem, ucEmutecaEmulator;

type
  { Cache data Thread. }

  { cEmutecaCacheDataThread }

  cEmutecaCacheDataThread = class(TThread)
  private
    FCurrSoftPos: integer;
    FSoftList: cEmutecaSoftList;
    FSystemManager: cEmutecaSystemManager;
    FTempFolder: string;
    procedure SetCurrSoftPos(AValue: integer);
    procedure SetSoftList(AValue: cEmutecaSoftList);
    procedure SetSystemManager(AValue: cEmutecaSystemManager);
    procedure SetTempFolder(AValue: string);

  protected
    property CurrSoftPos: integer read FCurrSoftPos write SetCurrSoftPos;
    procedure Execute; override;

  public
    property SoftList: cEmutecaSoftList read FSoftList write SetSoftList;
    property SystemManager: cEmutecaSystemManager
      read FSystemManager write SetSystemManager;
    property TempFolder: string read FTempFolder write SetTempFolder;
    constructor Create;
  end;

  { cEmuteca }

  cEmuteca = class(TComponent)
  private
    FCacheDataThread: cEmutecaCacheDataThread;
    FConfig: cEmutecaConfig;
    FEmulatorManager: cEmutecaEmulatorManager;
    FProgressCallBack: TEmutecaProgressCallBack;
    FSoftManager: cEmutecaSoftManager;
    FSystemManager: cEmutecaSystemManager;
    FTempFolder: string;
    procedure SetCacheDataThread(AValue: cEmutecaCacheDataThread);
    procedure SetProgressBar(AValue: TEmutecaProgressCallBack);
    procedure SetTempFolder(AValue: string);

  protected
    property CacheDataThread: cEmutecaCacheDataThread
      read FCacheDataThread write SetCacheDataThread;

  public
    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressBar;

    { TODO: Maybe be protected. Remove all references to this

      Y traer los procedimientos que lo usen aquí. }
    property TempFolder: string read FTempFolder write SetTempFolder;

    procedure LoadConfig(aFile: string);
    procedure SaveConfig;

    procedure SelectSystem(aSystem: cEmutecaSystem);

    function SearchMainEmulator(aID: string): cEmutecaEmulator;

    procedure SearchMediaFiles(OutFileList: TStrings;
      aFolder: string; aFileName: string; Extensions: TStrings);
    procedure SearchSoftFiles(OutFileList: TStrings;
      aFolder: string; aSoft: cEmutecaSoftware; Extensions: TStrings);
    procedure SearchGroupFiles(OutFileList: TStrings;
      aFolder: string; aGroup: cEmutecaGroup; Extensions: TStrings);

    function SearchFirstMediaFile(aFolder: string;
      aFileName: string; Extensions: TStrings): string;
    function SearchFirstSoftFile(aFolder: string;
      aSoft: cEmutecaSoftware; Extensions: TStrings;
      UseGroup: boolean = True): string;
    function SearchFirstGroupFile(aFolder: string;
      aGroup: cEmutecaGroup; Extensions: TStrings): string;

    procedure CacheData;

    function RunSoftware(const aSoftware: cEmutecaSoftware): integer;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Config: cEmutecaConfig read FConfig;

    property SoftManager: cEmutecaSoftManager read FSoftManager;

    property EmulatorManager: cEmutecaEmulatorManager read FEmulatorManager;

    property SystemManager: cEmutecaSystemManager read FSystemManager;
  end;

implementation

{ cEmutecaCacheDataThread }
procedure cEmutecaCacheDataThread.SetSoftList(AValue: cEmutecaSoftList);
begin
  if FSoftList = AValue then
    Exit;
  FSoftList := AValue;
end;

procedure cEmutecaCacheDataThread.SetSystemManager(AValue:
  cEmutecaSystemManager);
begin
  if FSystemManager = AValue then
    Exit;
  FSystemManager := AValue;
end;

procedure cEmutecaCacheDataThread.SetTempFolder(AValue: string);
begin
  if FTempFolder = AValue then
    Exit;
  FTempFolder := AValue;
end;

procedure cEmutecaCacheDataThread.SetCurrSoftPos(AValue: integer);
begin
  if FCurrSoftPos = AValue then
    Exit;
  FCurrSoftPos := AValue;
end;

procedure cEmutecaCacheDataThread.Execute;
var
  aSoft: cEmutecaSoftware;
  aSystem: cEmutecaSystem;
  aGroup: cEmutecaGroup;
  aFolder, aFile: string;
  aSha1: TSHA1Digest;
begin
  if (not Assigned(SystemManager)) or (not Assigned(SoftList)) then
    Exit;

  // Caching Systems and groups
  CurrSoftPos := 0;
  aSystem := nil;
  while (not Terminated) and (CurrSoftPos < SoftList.Count) do
  begin
    aSoft := SoftList[CurrSoftPos];

    if not Assigned(aSoft.System) then
    begin
      if not aSoft.MatchSystem(aSystem) then
      begin
        aSystem := SystemManager.ItemById(aSoft.SystemKey, True);
        aGroup := nil;
      end;
      if not terminated then
        aSoft.System := aSystem;
    end;

    if not Assigned(aSoft.Group) then
    begin
      if not aSoft.MatchGroup(aGroup) then
      begin
        aGroup := aSoft.System.GroupManager.ItemById(aSoft.GroupKey, True);
      end;
      if not terminated then
        aSoft.Group := aGroup;
    end;

    Inc(FCurrSoftPos);
  end;

  if Terminated then
    Exit;

  if TempFolder = '' then
    Exit;

  // Caching SHA1
  try
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
      Inc(FCurrSoftPos);
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

procedure cEmuteca.SetTempFolder(AValue: string);
begin
  FTempFolder := SetAsFolder(AValue);
end;

procedure cEmuteca.CacheData;
begin
  if Assigned(CacheDataThread) then
    CacheDataThread.Terminate; // FreeOnTerminate := true;

  // Caching data in background
  FCacheDataThread := cEmutecaCacheDataThread.Create;
  if Assigned(CacheDataThread.FatalException) then
    raise CacheDataThread.FatalException;
  CacheDataThread.TempFolder := Self.TempFolder;
  CacheDataThread.SystemManager := Self.SystemManager;
  CacheDataThread.SoftList := Self.SoftManager.FullList;
  CacheDataThread.Start;
end;

procedure cEmuteca.SearchMediaFiles(OutFileList: TStrings;
  aFolder: string; aFileName: string; Extensions: TStrings);

  procedure SearchFilesByExt(aFileList: TStrings; aBaseFileName: string;
    aExtList: TStrings);
  var
    i: integer;
    aFile: string;
  begin
    i := 0;
    while i < aExtList.Count do
    begin
      aFile := aBaseFileName + ExtensionSeparator + aExtList[i];
      if FileExistsUTF8(aFile) then
        aFileList.Add(aFile);
      Inc(i);
    end;
  end;

var
  CacheFolder: string;
  CompressedArchives: TStringList;
  i, j: integer;
  Info: TSearchRec;
begin
  if not assigned(OutFileList) then
    Exit;

  aFolder := SetAsFolder(aFolder);
  aFileName := RemoveFromBrackets(ExtractFileNameOnly(aFileName));
  if (aFileName = '') or (aFolder = '') or
    (not DirectoryExistsUTF8(aFolder)) or (Extensions = nil) or
    (Extensions.Count = 0) then
    Exit;

  // 1. Basic search
  // Folder/aFileName.mext
  SearchFilesByExt(OutFileList, aFolder + aFileName, Extensions);

  // 2. Search in folder
  // Folder/aFileName/[*]/*.mext
  { TODO: What is faster? FindAllFiles or modified
    cEmuteca.SearchFirstMediaFile.SearchFileInFolder ?}
  FindAllFiles(OutFileList, aFolder + aFileName,
    FileMaskFromStringList(Extensions), True);

  // 3. Search in zip files
  //   Folder/aFileName.zip/*.mext (extract to CacheFolder/*.mext
  { TODO : Diferenciate systems }
  CacheFolder := TempFolder +
    SetAsFolder(ExtractFileName(ExcludeTrailingPathDelimiter(aFolder))) +
    SetAsFolder(aFileName);


  // 3.a. If not CacheFolder exists, then search Folder/aFileName.zip/*.mext
  //   and extract to CacheFolder
  if not DirectoryExistsUTF8(CacheFolder) then
  begin
    CompressedArchives := TStringList.Create;
    try
      SearchFilesByExt(CompressedArchives, aFolder + aFileName,
        Config.CompressedExtensions);

      i := 0;
      j := CompressedArchives.Count;
      while i < j do
      begin
        w7zExtractFile(CompressedArchives[i], AllFilesMask, CacheFolder,
          False, '');
        Inc(i);
      end;
    finally
      FreeAndNil(CompressedArchives);
    end;
  end;

  // 3.b. Actually searching in CacheFolder
  FindAllFiles(OutFileList, CacheFolder,
    FileMaskFromStringList(Extensions), True);

  // Found something then Exit
  if OutFileList.Count > 0 then
    Exit;

  // 4. If nothing found, search ONLY ONE from every compressed archive.
  // Folder/*.zip/aFileName.mext
  if FindFirstUTF8(aFolder + AllFilesMask, 0, Info) = 0 then
    // TODO: change to 3.X way :-P
    try
      repeat
        if SupportedExt(Info.Name, Config.CompressedExtensions) then
        begin
          // AllFilesMask... Maybe is a good idea...
          w7zExtractFile(aFolder + Info.Name, aFileName + '.*',
            CacheFolder + Info.Name, False, '');
        end;
      until (FindNextUTF8(Info) <> 0);
    finally
      FindCloseUTF8(Info);
    end;
  FindAllFiles(OutFileList, CacheFolder,
    FileMaskFromStringList(Extensions), True);

end;

function cEmuteca.SearchFirstMediaFile(aFolder: string;
  aFileName: string; Extensions: TStrings): string;

  function SearchFileByExt(aBaseFileName: string;
    aExtList: TStrings): string;
  var
    i: integer;
  begin
    Result := '';
    i := 0;
    while (Result = '') and (i < aExtList.Count) do
    begin
      if FileExistsUTF8(aBaseFileName + ExtensionSeparator +
        aExtList[i]) then
        Result := aBaseFileName + ExtensionSeparator + aExtList[i];
      Inc(i);
    end;
  end;

  function SearchFileInFolder(aFolder: string;
    Extensions: TStrings): string;
  var
    Info: TSearchRec;
  begin
    Result := '';
    aFolder := SetAsFolder(aFolder);
    if (aFolder = '') or (not DirectoryExistsUTF8(aFolder)) then
      Exit;

    if FindFirstUTF8(aFolder + AllFilesMask, faAnyFile, Info) = 0 then
      try
        repeat
          if SupportedExt(Info.Name, Extensions) then
            Result := aFolder + Info.Name;
        until (Result <> '') or (FindNextUTF8(Info) <> 0);
      finally
        FindCloseUTF8(Info);
      end;

    if Result <> '' then
      Exit;

    if FindFirstUTF8(aFolder + AllFilesMask, faDirectory, Info) = 0 then
      try
        repeat
          if (Info.Name <> '.') and (Info.Name <> '') and
            (Info.Name <> '..') and
            ((Info.Attr and faDirectory) <> 0) then
            Result := SearchFileInFolder(aFolder + Info.Name, Extensions);
        until (Result <> '') or (FindNextUTF8(Info) <> 0);
      finally
        FindCloseUTF8(Info);
      end;
  end;

  //function cEmuteca.SearchFirstMediaFile(aFolder: string;
  //  aFileName: string; Extensions: TStrings): string;
var
  CacheFolder: string;
  CompressedArchives: TStringList;
  i, j: integer;
  Info: TSearchRec;
begin
  Result := '';

  aFolder := SetAsFolder(aFolder);
  aFileName := RemoveFromBrackets(ExtractFileNameOnly(aFileName));
  if (aFileName = '') or (aFolder = '') or
    (not DirectoryExistsUTF8(aFolder)) or (Extensions = nil) or
    (Extensions.Count = 0) then
    Exit;

  // 1. Basic search
  // Folder/aFileName.mext
  Result := SearchFileByExt(aFolder + aFileName, Extensions);
  if Result <> '' then
    Exit;

  // 2. Search in folder
  // Folder/aFileName/[*]/*.mext
  Result := SearchFileInFolder(aFolder + aFileName, Extensions);
  if Result <> '' then
    Exit;

  // 3. Search in zip files
  //   Folder/aFileName.zip/*.mext (extract to CacheFolder/*.mext
  { TODO : Diferenciate systems }
  CacheFolder := TempFolder +
    SetAsFolder(ExtractFileName(ExcludeTrailingPathDelimiter(aFolder))) +
    SetAsFolder(aFileName);

  // 3.a. If not CacheFolder exists, then search Folder/aFileName.zip/*.mext
  //   and extract to CacheFolder (WE EXTRACT ALL FILES)
  if not DirectoryExistsUTF8(CacheFolder) then
  begin
    CompressedArchives := TStringList.Create;
    try
      FindAllFiles(CompressedArchives, aFolder + aFileName,
        FileMaskFromStringList(Config.CompressedExtensions), True);

      i := 0;
      j := CompressedArchives.Count;
      while i < j do
      begin
        w7zExtractFile(CompressedArchives[i], AllFilesMask, CacheFolder,
          False, '');
        Inc(i);
      end;
    finally
      FreeAndNil(CompressedArchives);
    end;
  end;

  // 3.b. Actually searching the file in CacheFolder
  Result := SearchFileInFolder(CacheFolder, Extensions);
  if Result <> '' then
    Exit;

  // 4. If nothing found, search ONLY ONE from every compressed archive.
  // Folder/*.zip/aFileName.mext
  if FindFirstUTF8(aFolder + AllFilesMask, 0, Info) = 0 then
    // TODO: change to 3.X way :-P
    try
      repeat
        if SupportedExt(Info.Name, Config.CompressedExtensions) then
        begin
          // AllFilesMask... Maybe is a good idea...
          w7zExtractFile(aFolder + Info.Name, aFileName + '.*',
            CacheFolder + Info.Name, False, '');
        end;
      until (FindNextUTF8(Info) <> 0);
    finally
      FindCloseUTF8(Info);
    end;
  Result := SearchFileInFolder(CacheFolder, Extensions);
end;

procedure cEmuteca.LoadConfig(aFile: string);
begin
  Config.LoadConfig(aFile);

  // Temp folder
  TempFolder := SetAsFolder(GetTempDir) + Config.TempSubfolder;
  ForceDirectories(TempFolder);

  // Setting EmulatorManager
  EmulatorManager.DataFile := Config.EmulatorsFile;
  EmulatorManager.LoadFromFileIni('');

  // Setting SystemManager
  SystemManager.DataFile := Config.SystemsFile;
  SystemManager.LoadFromFileIni('');

  // Setting SoftManager
  SoftManager.DataFile := Config.SoftFile;
  SoftManager.LoadFromFileTxt('');

  CacheData;
end;

procedure cEmuteca.SaveConfig;
begin
  SoftManager.SaveToFileTxt('', False);
  SystemManager.SaveToFileIni('', False);
  EmulatorManager.SaveToFileIni('', False);
end;

procedure cEmuteca.SelectSystem(aSystem: cEmutecaSystem);
begin
  SoftManager.SelectSystem(aSystem);
end;

function cEmuteca.SearchMainEmulator(aID: string): cEmutecaEmulator;
begin
  Result := EmulatorManager.ItemById(aID);
end;

procedure cEmuteca.SearchSoftFiles(OutFileList: TStrings;
  aFolder: string; aSoft: cEmutecaSoftware; Extensions: TStrings);
var
  i: integer;
begin
  i := OutFileList.Count;
  SearchMediaFiles(OutFileList, aFolder, aSoft.FileName, Extensions);
  if OutFileList.Count = i then
    SearchGroupFiles(OutFileList, aFolder, aSoft.Group, Extensions);
end;

function cEmuteca.SearchFirstSoftFile(aFolder: string;
  aSoft: cEmutecaSoftware; Extensions: TStrings; UseGroup: boolean): string;
begin
  Result := SearchFirstMediaFile(aFolder, aSoft.FileName, Extensions);
  if (Result = '') and UseGroup then
    Result := SearchFirstGroupFile(aFolder, aSoft.Group, Extensions);
end;

procedure cEmuteca.SearchGroupFiles(OutFileList: TStrings;
  aFolder: string; aGroup: cEmutecaGroup; Extensions: TStrings);
begin
  SearchMediaFiles(OutFileList, aFolder, aGroup.ID, Extensions);
end;

function cEmuteca.SearchFirstGroupFile(aFolder: string;
  aGroup: cEmutecaGroup; Extensions: TStrings): string;
begin
  Result := SearchFirstMediaFile(aFolder, aGroup.ID, Extensions);
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

  { TODO : Windows shorcuts don't work }
  TimePlaying := SecondsBetween(Now, StartTime);

  // if Emulator returns no error and passed at least MinTime...
  if (Result = 0) and (TimePlaying >= Config.MinPlayTime) then
  begin
    { TODO : This are not saved if lists are not saved on exit }
    //   aSoftware.Stats.AddPlayingTime(StartTime, TimePlaying);
    //    aSoftware.Info.Group.Stats.AddPlayingTime(StartTime, TimePlaying);

    { TODO : System and Emulator are not saved unless you save it
      in their managers }
    //    aSoftware.Info.System.Stats.AddPlayingTime(StartTime, TimePlaying);
    //    aEmulator.Stats.AddPlayingTime(StartTime, TimePlaying);

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

procedure cEmuteca.SetProgressBar(AValue: TEmutecaProgressCallBack);
begin
  FProgressCallBack := AValue;
  EmulatorManager.ProgressCallBack := Self.ProgressCallBack;
  SystemManager.ProgressCallBack := Self.ProgressCallBack;
  SoftManager.ProgressCallBack := Self.ProgressCallBack;
end;

procedure cEmuteca.SetCacheDataThread(AValue: cEmutecaCacheDataThread);
begin
  if FCacheDataThread = AValue then
    Exit;
  FCacheDataThread := AValue;
end;

constructor cEmuteca.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  // Creating components
  FConfig := cEmutecaConfig.Create(Self);

  FSystemManager := cEmutecaSystemManager.Create(Self);
  SystemManager.Config := Self.Config;
  FEmulatorManager := cEmutecaEmulatorManager.Create(Self);
  FSoftManager := cEmutecaSoftManager.Create(Self);
end;

destructor cEmuteca.Destroy;
begin
  // Deleting temp folder
  // TODO: Crappy segurity check... :-(
  if (Length(TempFolder) > Length(Config.TempSubfolder) + 5) and
    DirectoryExistsUTF8(TempFolder) then
    DeleteDirectory(TempFolder, False);

  // If we are still caching...
  if Assigned(CacheDataThread) and not CacheDataThread.Finished then
    CacheDataThread.Terminate;

  Config.SaveConfig('');

  FreeAndNil(FSoftManager);
  FreeAndNil(FSystemManager);
  FreeAndNil(FEmulatorManager);
  FreeAndNil(FConfig);
  inherited Destroy;
end;

end.
