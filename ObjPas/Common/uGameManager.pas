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

{cGameManager unit}
unit uGameManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, FileUtil, IniFiles, LazUTF8, LConvEncoding,
  uSystem, uGame, uGameGroup, uEmulator, uCustomUtils, u7zWrapper;

const
  // Praying for no emulator use these exit codes.
  CGMExecErrorNoGame = 300;
  CGMDecompressError = 301;

  CGMGameSubFolder = 'Game';

type
  TGMCallBackType = (GMCBAddFile, //< Adding a file.
    GMCBImportData, //< Importing data.
    GMCBExportData, //< Exporting data.
    GMCBSaveList, //< Saving game list.
    GMCBLoadList, //< Loading game list.
    GMCBDecompress //< Decompressing a file
    );
  //< Type of action doing the callback.

  TGMProgressCallBack = function(const TypeCB: TGMCallBackType;
    const Info1, Info2: string; const Value, MaxValue: int64): boolean of
    object;
  {< Type for calling back a function a show the progress.

    Used as cGameManager.ProgressCallBack, this function is called in some
      iterations that can be slow.

    @param(TypeCB TGMCallBackType indicating the current action.)
    @param(Info1 First string of info @(i.e. Folder@).)
    @param(Info2 Second string of info @(i.e. Filename@).)
    @param(Value Current value in the iteration.)
    @param(MaxValue Max value in the iteration.)
    @return(Abort the iteration? (Although it can be ignored :P))
  }

  { Manager of the games and groups (families).

    This entity manages the lists of games and group internally regaderless
      of the Form, GUI o interface used.

    @definitionList(
      @itemLabel(NOTE:)
      @item(Because PascalScript don't suport overloaded methods,
        we don't use them right here.)
    )
  }

  cGameManager = class
  private
    FCompressedExt: TStringList;
    FCRCMaxSize: cardinal;
    FEmulator: cEmulator;
    FEmulatorsFile: string;
    FGameDataFileExt: string;
    FGroupDataFileExt: string;
    FGameList: TFPObjectList;
    FGroupList: TFPObjectList;
    FProgressCallBack: TGMProgressCallBack;
    FSystem: cSystem;
    FSystemsFile: string;
    FTempFolder: string;
    FTempFile: string;
    procedure SetCRCMaxSize(const AValue: cardinal);
    procedure SetEmulatorsFile(const AValue: string);
    procedure SetGameDataFileExt(const AValue: string);
    procedure SetGroupDataFileExt(const AValue: string);
    procedure SetProgressCallBack(const AValue: TGMProgressCallBack);
    procedure SetSystemsFile(const AValue: string);
    procedure SetTempFolder(const AValue: string);
    procedure SetTempFile(const AValue: string);

  protected
    property GroupList: TFPObjectList read FGroupList;
    {< Actual list where the groups of the current system are stored. }
    property GameList: TFPObjectList read FGameList;
    {< Actual list where the games of the current system are stored. }

  public
    property SystemsFile: string read FSystemsFile write SetSystemsFile;
    {< Path of the file where system configurations are stored. }
    property EmulatorsFile: string read FEmulatorsFile write SetEmulatorsFile;
    {< Path of the file where emulator configurations are stored. }

    property GameDataFileExt: string read FGameDataFileExt
      write SetGameDataFileExt;
    {< Extension used for files with game data. }
    property GroupDataFileExt: string
      read FGroupDataFileExt write SetGroupDataFileExt;
    {< Extension used for files with group data. }

    property System: cSystem read FSystem;
    {< Current loaded system. }
    property Emulator: cEmulator read FEmulator;
    {< Current selected emulator. }

    property TempFolder: string read FTempFolder write SetTempFolder;
    //< Temp folder used for decompress and other dirty things.
    property TempFile: string read FTempFile write SetTempFile;
    //< Temp file for... nothing. May be for export/import when updating.
    property CRCMaxSize: cardinal read FCRCMaxSize write SetCRCMaxSize;
    //< Max file size for calculate the CRC32 of the file.

    property CompressedExt: TStringList read FCompressedExt;
    //< File extensions of compressed archives suported.

    property ProgressCallBack: TGMProgressCallBack
      read FProgressCallBack write SetProgressCallBack;
    //< CallBack function to show the progress in actions.

    function GameAtPos(const aIndex: integer): cGame;
    //< Return the game at a position.
    function Game(aGameKey: string): cGame;
    //< Return the game with have aGameKey key.
    function GameCount: longint;
    //< Return the number of games.

    function GroupAtPos(const aIndex: integer): cGameGroup;
    //< Return the group at a position.
    function Group(aGroupKey: string): cGameGroup;
    //< Return the game with have aGroupKey key.
    function GroupCount: longint;
    //< Return the number of groups.

    procedure SaveSystem;
    //< Save current system configuration.
    procedure ChangeSystem(const SystemName: string);
    //< Change the current system.
    procedure ChangeEmulator(const EmulatorName: string);
    //< Change the current emultor.
    procedure PurgeGameData;
    //< Removes al games and groups from the system.

    procedure UpdateGameList;
    procedure SoftUpdateGameList;
    procedure UpdateGroupList;

    function AddFile(aFolder: string; Info: TSearchRec): boolean;
    {< Add a file (or all files if it's a compressed archive) to the game list.

      Automatically searchs if the file is a game using the current system
        extension filter.

      if the file is not in the system extesion filter, this function search
        if the file is a compressed archive and adds every game found in it.

      This funtion is for IterateFolderObj use, if you want to add a game or
        group straight use AddGame/AddGroup.

      @param(aFolder Folder where the file is in.)
      @param(Info TSearchRec with the file info.)
      @return(@false => Abort.)
    }

    function AddGame(const aFolder: string; const aFileName: string;
      const aKey: string): cGame;
    //< Add a game.
    function AddGroup(aGameGroupID: string): cGameGroup;
    //< Add a group.

    function GameMediaExists(aFolder: string; aGameVersion: cGame;
      Extensions: TStrings): boolean;
    function GroupMediaExists(aFolder: string; aGameGroup: cGameGroup;
      Extensions: TStrings): boolean;

    procedure SearchGameMedia(FileList: TStrings; aFolder: string;
      aGameVersion: cGame; Extensions: TStrings);
    procedure SearchGroupMedia(FileList: TStrings; aFolder: string;
      aGameGroup: cGameGroup; Extensions: TStrings);
    procedure SearchMediaFiles(FileList: TStrings; aFolder: string;
      aFileName: string; Extensions: TStrings);


    procedure SaveSystemGameList;
    //< Save the current system game list (and groups).
    procedure LoadSystemGameList;
    //< Load the current system game list (and groups).
    procedure ExportGameData(const aFileName: string;
      const ExportMode: boolean);
    {< Export current system game list to a .ini file.

     @param(ExportMode If false saves some data for internal
        purpourses: times played, last time, total time,...)
    }
    procedure ExportGameDataIni(const aIniFile: TCustomIniFile;
      const ExportMode: boolean);
    {< Export current system game list to an already opened .ini file.

     @param(ExportMode Boolean. If false saves some data for internal
        purpourses: times played, last time, total time,... )
    }
    procedure ImportGameData(const aFileName: string);
    procedure ImportGameDataIni(const aIniFile: TCustomIniFile);

    function Execute(aGame: cGame): integer;
    //< Execute a Game.

    constructor Create(const aSystemsFile: string;
      const aTempFolder: string; const aTempFile: string);
    destructor Destroy; override;
  end;

implementation

{ cGameManager }

procedure cGameManager.SetEmulatorsFile(const AValue: string);
begin
  FEmulatorsFile := AValue;
end;

procedure cGameManager.SetGameDataFileExt(const AValue: string);
var
  TrValue: string;
begin
  TrValue := Trim(AValue);
  if (UTF8Length(TrValue) > 0) and
    (UTF8CompareText(UTF8Copy(TrValue, 1, 1), ExtensionSeparator) = 0) then
    FGameDataFileExt := TrValue
  else
    FGameDataFileExt := ExtensionSeparator + TrValue;
end;

procedure cGameManager.SetGroupDataFileExt(const AValue: string);
var
  TrValue: string;
begin
  TrValue := Trim(AValue);
  if (UTF8Length(TrValue) > 0) and
    (UTF8CompareText(UTF8Copy(TrValue, 1, 1), ExtensionSeparator) = 0) then
    FGroupDataFileExt := TrValue
  else
    FGroupDataFileExt := ExtensionSeparator + TrValue;
end;

procedure cGameManager.SetProgressCallBack(const AValue: TGMProgressCallBack);
begin
  FProgressCallBack := AValue;
end;

procedure cGameManager.SetCRCMaxSize(const AValue: cardinal);
begin
  FCRCMaxSize := AValue;
end;

procedure cGameManager.SetSystemsFile(const AValue: string);
begin
  FSystemsFile := AValue;
end;

procedure cGameManager.SetTempFolder(const AValue: string);
begin
  FTempFolder := SetAsFolder(AValue);
end;

procedure cGameManager.SetTempFile(const AValue: string);
begin
  FTempFile := AValue;
end;

function cGameManager.GameAtPos(const aIndex: integer): cGame;
begin
  if (aIndex < GameList.Count) and (aIndex >= 0) then
    Result := cGame(GameList.Items[aIndex])
  else
    Result := nil;
end;

function cGameManager.Game(aGameKey: string): cGame;
var
  i: integer;
  aGame: cGame;
begin
  Result := nil;
  aGameKey := Trim(UTF8LowerCase(aGameKey));

  // Maybe backwards is better for batch operations and
  //   we don't need to access GameCount every iteration...
  i := GameCount - 1;
  while (i >= 0) do
  begin
    aGame := GameAtPos(i);
    if UTF8CompareText(aGame.Key, aGameKey) = 0 then
    begin
      Result := aGame;
      Break; // ... dirty exit, but we don't need to check: Result <> nil
    end;
    Dec(i);
  end;
end;

function cGameManager.GameCount: longint;
begin
  Result := GameList.Count;
end;

function cGameManager.GroupAtPos(const aIndex: integer): cGameGroup;
begin
  if (aIndex < GroupCount) and (aIndex >= 0) then
    Result := cGameGroup(GroupList.Items[aIndex])
  else
    Result := nil;
end;

function cGameManager.Group(aGroupKey: string): cGameGroup;
var
  i: integer;
  aGroup: cGameGroup;
begin
  Result := nil;
  aGroupKey := Trim(UTF8LowerCase(aGroupKey));

  // Backwards and we don't access GroupCount every iteration...
  //   Meh, see cGameManager.Game(aGameKey: String): cGame;
  i := GroupCount - 1;
  while (i >= 0) do
  begin
    aGroup := GroupAtPos(i);
    if UTF8CompareText(aGroup.Key, aGroupKey) = 0 then
    begin
      Result := aGroup;
      Break;
    end;
    Dec(i);
  end;
end;

function cGameManager.GroupCount: longint;
begin
  Result := GroupList.Count;
end;

procedure cGameManager.SaveSystem;
begin
  if System = nil then
    Exit;
  System.SaveToFile(SystemsFile);
end;

procedure cGameManager.ChangeSystem(const SystemName: string);
begin
  if System <> nil then
    if UTF8CompareText(System.ID, SystemName) = 0 then
      Exit
    else
      SaveSystemGameList;

  DeleteDirectory(TempFolder, True);

  FreeAndNil(FSystem);
  FreeAndNil(FEmulator);
  PurgeGameData;

  FSystem := cSystem.Create(SystemName);
  System.LoadFromFile(SystemsFile);
  if not System.Enabled then
  begin
    FreeAndNil(FSystem);
    Exit;
  end;

  ChangeEmulator(System.MainEmulator);

  LoadSystemGameList;
end;

procedure cGameManager.ChangeEmulator(const EmulatorName: string);
begin
  if Emulator <> nil then
    Emulator.SaveToFile(EmulatorsFile);
  FreeAndNil(FEmulator);

  FEmulator := cEmulator.Create(EmulatorName);
  Emulator.LoadFromFile(EmulatorsFile);
  if not Emulator.Enabled then
    FreeAndNil(FEmulator);
end;

procedure cGameManager.PurgeGameData;
begin
  GameList.Clear;
  GroupList.Clear;
end;

procedure cGameManager.UpdateGameList;
var
  DataFile: string;
begin
  if System = nil then
    Exit;
  DataFile := TempFolder + TempFile;
  if FileExistsUTF8(DataFile) then
    DeleteFileUTF8(DataFile);
  ExportGameData(DataFile, False);
  PurgeGameData;
  IterateFolderObj(System.GameFolder, @Self.AddFile,
    System.RecursiveGameFolder);
  ImportGameData(DataFile);
  if FileExistsUTF8(DataFile) then
    DeleteFileUTF8(DataFile);
end;

procedure cGameManager.SoftUpdateGameList;
begin
  // TODO 2: SoftUpdateGameList
  //   1. Remove Games wich file don't exists
  //   2. Add the new files
  // Is it faster than UpdateGameList?
  //   YES, because not Export/Import is needed
  UpdateGameList;
end;

procedure cGameManager.UpdateGroupList;
var
  i, j: integer;
begin
  GroupList.Clear;
  i := 0;
  j := Self.GameCount;
  while i < j do
  begin
    Self.AddGroup(GameAtPos(i).GameGroup);
    Inc(i);
  end;
end;

function cGameManager.AddFile(aFolder: string; Info: TSearchRec): boolean;
var
  Extension: string;
  i, j: integer;
  CompFiles, aFile: TStringList;
begin
  Result := True;
  if (Info.Attr and faDirectory) <> 0 then
    Exit;

  Extension := ExtractFileExt(UTF8LowerCase(Info.Name));
  if UTF8Copy(Extension, 1, 1) = ExtensionSeparator then
    Extension := UTF8Copy(Extension, 2, MaxInt);

  if (System.Extensions.IndexOf(Extension) <> -1) then
  begin // It has a extension recognised by the system
    if System.UseCRC and (Info.Size < CRCMaxSize) then
      AddGame(aFolder, Info.Name, IntToHex(CRC32File(aFolder + Info.Name), 8))
    else
      AddGame(aFolder, Info.Name, ExtractFileNameOnly(Info.Name));
    if ProgressCallBack <> nil then
      Result := ProgressCallBack(GMCBAddFile, aFolder, Info.Name, 0, 1);
  end
  else
  if (CompressedExt.IndexOf(Extension) <> -1) then
  begin // It's a extensión of a compressed archive
    CompFiles := TStringList.Create;
    try
      w7zListFiles(aFolder + Info.Name, CompFiles, False);

      i := 0;
      j := CompFiles.Count;
      while Result and (i < j) do
      begin
        aFile := TStringList.Create;
        try
          aFile.CommaText := CompFiles[i];

          if aFile.Count >= 5 then
          begin
            Extension := UTF8LowerCase(ExtractFileExt(aFile[0]));
            if UTF8Length(Extension) > 1 then
              if UTF8CompareText(UTF8Copy(Extension, 1, 1),
                ExtensionSeparator) = 0 then
                Extension := UTF8Copy(Extension, 2, MaxInt);

            if (System.Extensions.IndexOf(Extension) <> -1) then
            begin
              if System.UseCRC then
                AddGame(aFolder + Info.Name, aFile[0], aFile[4])
              else
                AddGame(aFolder + Info.Name, aFile[0],
                  ExtractFileNameOnly(aFile[0]));
              if ProgressCallBack <> nil then
                Result := ProgressCallBack(GMCBAddFile, aFolder +
                  Info.Name, aFile[0], i, j);
            end;
          end;
        finally
          FreeAndNil(aFile);
        end;
        Inc(i);
      end;
    finally
      FreeAndNil(CompFiles);
    end;
  end;
end;

function cGameManager.AddGame(const aFolder: string;
  const aFileName: string; const aKey: string): cGame;
var
  aGame: cGame;
begin
  aGame := cGame.Create(aFolder, aFileName, aKey);
  GameList.Add(aGame);
  AddGroup(aGame.GameGroup);
  Result := aGame;
end;

function cGameManager.AddGroup(aGameGroupID: string): cGameGroup;
begin
  aGameGroupID := Trim(aGameGroupID);
  Result := Group(aGameGroupID);

  if Result = nil then
  begin
    Result := cGameGroup.Create(aGameGroupID);
    GroupList.Add(Result);
  end;
end;

function cGameManager.GameMediaExists(aFolder: string;
  aGameVersion: cGame; Extensions: TStrings): boolean;
var
  TmpStrList: TStringList;
  aGameGroup: cGameGroup;
begin
  Result := False;
  TmpStrList := TStringList.Create;
  try
    SearchMediaFiles(TmpStrList, aFolder,
      RemoveFromBrackets(aGameVersion.FileName) + kCUVirtualGameExt,
      Extensions);
    if TmpStrList.Count = 0 then
    begin
      aGameGroup := Group(aGameVersion.GameGroup);
      Result := GroupMediaExists(aFolder, aGameGroup, Extensions);
    end
    else
      Result := True;
  finally
    FreeAndNil(TmpStrList);
  end;
end;

function cGameManager.GroupMediaExists(aFolder: string;
  aGameGroup: cGameGroup; Extensions: TStrings): boolean;
var
  TmpStrList: TStringList;
begin
  Result := False;
  TmpStrList := TStringList.Create;
  try
    SearchMediaFiles(TmpStrList, aFolder, aGameGroup.MediaFileName,
      Extensions);
    if TmpStrList.Count <> 0 then
      Result := True;
  finally
    FreeAndNil(TmpStrList);
  end;
end;

procedure cGameManager.SearchGameMedia(FileList: TStrings;
  aFolder: string; aGameVersion: cGame; Extensions: TStrings);
begin
  SearchMediaFiles(FileList, aFolder,
    RemoveFromBrackets(aGameVersion.FileName) + kCUVirtualGameExt, Extensions);
  if FileList.Count = 0 then
    SearchGroupMedia(FileList, aFolder, Group(aGameVersion.GameGroup),
      Extensions);
end;

procedure cGameManager.SearchGroupMedia(FileList: TStrings;
  aFolder: string; aGameGroup: cGameGroup; Extensions: TStrings);
begin
  SearchMediaFiles(FileList, aFolder, aGameGroup.MediaFileName, Extensions);
end;

procedure cGameManager.SearchMediaFiles(FileList: TStrings;
  aFolder: string; aFileName: string; Extensions: TStrings);

  procedure SearchFileByExt(aFileList: TStrings; aBaseFileName: string;
    aExtList: TStrings);
  var
    i, j: integer;
  begin
    i := 0;
    j := aExtList.Count;
    while i < j do
    begin
      if FileExistsUTF8(aBaseFileName + ExtensionSeparator +
        aExtList[i]) then
        aFileList.Add(aBaseFileName + ExtensionSeparator + aExtList[i]);
      Inc(i);
    end;
  end;

  procedure AddFilesFromFolder(FileList: TStrings; aFolder: string;
    Extensions: TStrings);
  var
    Info: TSearchRec;
  begin
    aFolder := SetAsFolder(aFolder);
    if (aFolder = '') or (not DirectoryExistsUTF8(aFolder)) then
      Exit;

    if FindFirstUTF8(aFolder + AllFilesMask, faAnyFile, Info) = 0 then
      try
        repeat
          if Extensions.IndexOf(UTF8LowerCase(UTF8Copy(
            ExtractFileExt(Info.Name), 2, MaxInt))) <> -1 then
            FileList.Add(aFolder + Info.Name);
        until (FindNextUTF8(Info) <> 0);
      finally
        FindCloseUTF8(Info);
      end;


    if FindFirstUTF8(aFolder + AllFilesMask, faDirectory, Info) = 0 then
      try
        repeat
          if (Info.Name <> '.') and (Info.Name <> '') and
            (Info.Name <> '..') and
            ((Info.Attr and faDirectory) <> 0) then
            AddFilesFromFolder(FileList, aFolder + Info.Name, Extensions);
        until (FindNextUTF8(Info) <> 0);
      finally
        FindCloseUTF8(Info);
      end;
  end;

var
  TempTypeSubFolder: string;
  CompressedArchives: TStringList;
  i, j: integer;

  Info: TSearchRec;
begin
  if FileList = nil then
    FileList := TStringList.Create
  else
    FileList.Clear;

  aFolder := SetAsFolder(aFolder);
  aFileName := RemoveFromBrackets(aFileName);
  if (aFileName = '') or (aFolder = '') or
    (not DirectoryExistsUTF8(aFolder)) or (Extensions = nil) or
    (Extensions.Count = 0) then
    Exit;

  // 1. Basic search
  // Folder/aFileName.mext
  SearchFileByExt(FileList, aFolder + aFileName, Extensions);

  // 2. Search in folder
  // Folder/aFileName/*.mext
  AddFilesFromFolder(FileList, aFolder + SetAsFolder(aFileName), Extensions);

  // 3.a Search in cache folder
  // TempFolder/Type/aFileName/*.mext
  TempTypeSubFolder := TempFolder +
    SetAsFolder(ExtractFileName(ExcludeTrailingPathDelimiter(aFolder))) +
    SetAsFolder(aFileName);

  if DirectoryExistsUTF8(TempTypeSubFolder) then
    AddFilesFromFolder(FileList, TempTypeSubFolder, Extensions)
  else
  begin
    // 3.b Search in compressed archive
    // Folder/aFileName.zip/*.mext (extract to TempFolder/Type/aFileName/*.mext)

    CompressedArchives := TStringList.Create;
    try
      SearchFileByExt(CompressedArchives, aFolder + aFileName, CompressedExt);

      i := 0;
      j := CompressedArchives.Count;
      while i < j do
      begin
        w7zExtractFile(CompressedArchives[i], AllFilesMask, TempTypeSubFolder,
          False, '');
        Inc(i);
      end;

      AddFilesFromFolder(FileList, TempTypeSubFolder, Extensions);
    finally
      FreeAndNil(CompressedArchives);
    end;
  end;

  if FileList.Count > 0 then
    Exit;

  // 4. If none found, search ONLY ONE from every compressed archive.
  // Folder/*.zip/aFileName.mext
  if FindFirstUTF8(aFolder + AllFilesMask, 0, Info) = 0 then
    try
      repeat
        // Ough, we really need a easy way to check extensions
        if CompressedExt.IndexOf(UTF8LowerCase(UTF8Copy(
          ExtractFileExt(Info.Name), 2, MaxInt))) <> -1 then
        begin
          // AllFilesMask... Maybe is a good idea...
          w7zExtractFile(aFolder + Info.Name, aFileName + '.*',
            TempTypeSubFolder, False, '');
          AddFilesFromFolder(FileList, TempTypeSubFolder, Extensions);
        end;
      until (FileList.Count > 0) or (FindNextUTF8(Info) <> 0);
    finally
      FindCloseUTF8(Info);
    end;
end;

procedure cGameManager.SaveSystemGameList;
var
  i, j: integer;
  aFileName: string;
  aStringList: TStringList;
  aGame: cGame;
  aGroup: cGameGroup;
begin
  if System = nil then
    Exit;
  aFileName := ExtractFilePath(SystemsFile) + System.DataFile +
    GroupDataFileExt;
  if FileExistsUTF8(aFileName) then
    DeleteFileUTF8(aFileName);

  aStringList := TStringList.Create;
  try
    aStringList.BeginUpdate;
    i := 0;
    j := FGroupList.Count;
    while i < j do
    begin
      aGroup := GroupAtPos(i);
      if ProgressCallBack <> nil then
        ProgressCallBack(GMCBSaveList, aGroup.Key, aGroup.Name, i, j);
      aStringList.Add(aGroup.DataString);
      Inc(i);
    end;
  finally
    aStringList.EndUpdate;
    aStringList.SaveToFile(aFileName);
  end;
  FreeAndNil(aStringList);

  aFileName := ExtractFilePath(SystemsFile) + System.DataFile +
    GameDataFileExt;
  if FileExistsUTF8(aFileName) then
    DeleteFileUTF8(aFileName);

  aStringList := TStringList.Create;
  try
    aStringList.BeginUpdate;
    i := 0;
    j := FGameList.Count;
    while i < j do
    begin
      aGame := GameAtPos(i);
      if ProgressCallBack <> nil then
        ProgressCallBack(GMCBSaveList, aGame.Name, aGame.Version, i, j);
      aStringList.Add(aGame.DataString);
      Inc(i);
    end;
  finally
    aStringList.EndUpdate;
    aStringList.SaveToFile(aFileName);
  end;
  FreeAndNil(aStringList);
end;

procedure cGameManager.LoadSystemGameList;
var
  i, j: integer;
  aStringList: TStringList;
  aGame: cGame;
  aGroup: cGameGroup;
  aFilename: string;
begin
  if System = nil then
    Exit;

  FGameList.Clear;
  FGroupList.Clear;

  aFilename := ExtractFilePath(SystemsFile) + System.DataFile +
    GroupDataFileExt;
  if not FileExistsUTF8(aFilename) then
    Exit;

  aStringList := TStringList.Create;
  try
    aStringList.LoadFromFile(aFilename);
    i := 0;
    j := aStringList.Count;
    while i < j do
    begin
      aGroup := cGameGroup.Create('');
      aGroup.DataString := aStringList[i];
      if ProgressCallBack <> nil then
        ProgressCallBack(GMCBLoadList, aGroup.Key, aGroup.Name, i, j);
      FGroupList.Add(aGroup);
      Inc(i);
    end;
  finally
    FreeAndNil(aStringList);
  end;

  aFilename := ExtractFilePath(SystemsFile) + System.DataFile +
    GameDataFileExt;
  if not FileExistsUTF8(aFilename) then
    Exit;

  aStringList := TStringList.Create;
  try
    aStringList.LoadFromFile(aFilename);
    i := 0;
    j := aStringList.Count;
    while i < j do
    begin
      aGame := cGame.Create('', '', '');
      aGame.DataString := aStringList[i];
      if ProgressCallBack <> nil then
        ProgressCallBack(GMCBLoadList, aGame.Name, aGame.Version, i, j);
      FGameList.Add(aGame);
      AddGroup(aGame.GameGroup);
      Inc(i);
    end;
  finally
    FreeAndNil(aStringList);
  end;
end;

procedure cGameManager.ExportGameData(const aFileName: string;
  const ExportMode: boolean);
var
  F: TMemInifile;
begin
  F := TMemIniFile.Create(UTF8ToSys(aFilename));
  try
    ExportGameDataIni(F, ExportMode);
  finally
    FreeAndNil(F);
  end;
end;

procedure cGameManager.ExportGameDataIni(const aIniFile: TCustomIniFile;
  const ExportMode: boolean);
var
  i, j: integer;
  aGame: cGame;
  aGameGroup: cGameGroup;
  Continue: boolean;
begin
  i := 0;
  j := GameCount;
  Continue := True;
  while (i < j) and Continue do
  begin
    aGame := GameAtPos(i);
    aGame.ExportDataIni(aIniFile, ExportMode);
    if ProgressCallBack <> nil then
      Continue := ProgressCallBack(GMCBExportData, aGame.Name,
        aGame.Version, i, j);
    Inc(i);
  end;

  i := 0;
  j := GroupCount;
  Continue := True;
  while (i < j) and Continue do
  begin
    aGameGroup := GroupAtPos(i);
    aGameGroup.ExportDataIni(aIniFile, ExportMode);
    if ProgressCallBack <> nil then
      Continue := ProgressCallBack(GMCBExportData, aGameGroup.Name,
        kCUVirtualGroupExt, i, j);
    Inc(i);
  end;
end;

procedure cGameManager.ImportGameData(const aFileName: string);
var
  F: TMemInifile;
begin
  if not FileExistsUTF8(aFilename) then
    Exit;
  F := TMemIniFile.Create(UTF8ToSys(aFilename));
  try
    ImportGameDataIni(F);
  finally
    FreeAndNil(F);
  end;
end;

procedure cGameManager.ImportGameDataIni(const aIniFile: TCustomIniFile);
var
  i, j: integer;
  aGame: cGame;
  aGameGroup: cGameGroup;
  Continue: boolean;
begin
  i := 0;
  j := GameCount;
  Continue := True;
  while (i < j) and Continue do
  begin
    aGame := GameAtPos(i);
    aGame.ImportDataIni(aIniFile);
    if ProgressCallBack <> nil then
      Continue := ProgressCallBack(GMCBImportData, aGame.Name,
        aGame.Version, i, j);
    Inc(i);
  end;

  UpdateGroupList;

  i := 0;
  j := GroupCount;
  Continue := True;
  while (i < j) and Continue do
  begin
    aGameGroup := GroupAtPos(i);
    aGameGroup.ImportDataIni(aIniFile);
    if Assigned(ProgressCallBack) then
      Continue := ProgressCallBack(GMCBImportData, aGameGroup.Name,
        kCUVirtualGroupExt, i, j);
    Inc(i);
  end;
end;

function cGameManager.Execute(aGame: cGame): integer;
var
  RomFile, CompressedFile: string;
  Error: integer;
  Compressed: boolean;
  NewDir: boolean;
  TempTime: TTime;
  aFolder: string;
begin
  // Uhm. If things go bad from the begin, they only can improve :-D
  Result := CGMExecErrorNoGame;

  // No, they don't :-|
  if aGame = nil then
    Exit;
  if Emulator = nil then
    Exit;
  if System = nil then
    Exit;

  // Testing if it's in a compressed archive.
  // I don't test extensions... only if the "game's folder" is actually a
  //   file and not a folder.
  CompressedFile := ExcludeTrailingPathDelimiter(aGame.Folder);
  Compressed := FileExistsUTF8(CompressedFile);

  if Trim(ExcludeTrailingPathDelimiter(System.TempFolder)) <> '' then
    aFolder := SetAsFolder(System.TempFolder)
  else
    aFolder := SetAsFolder(TempFolder);
  aFolder := SetAsFolder(aFolder + CGMGameSubFolder);

  NewDir := not DirectoryExists(aFolder);
  if NewDir then
    ForceDirectoriesUTF8(aFolder);

  if Compressed then
  begin
    if System.ExtractAll then
      Error := w7zExtractFile(CompressedFile, AllFilesMask, aFolder, True, '')
    else
      Error := w7zExtractFile(CompressedFile, aGame.FileName,
        aFolder, True, '');
    if Error <> 0 then
      Exit;
    RomFile := aFolder + aGame.FileName;
  end
  else
  begin
    RomFile := SetAsFolder(aGame.Folder) + aGame.FileName;
    if (System.ExtractAll) and
      (CompressedExt.IndexOf(UTF8Copy(ExtractFileExt(aGame.FileName),
      2, MaxInt)) <> -1) then
    begin // The ROM is a compressed file but must be extracted anyways
      Error := w7zExtractFile(RomFile, '*', aFolder, True, '');
      if Error <> 0 then
        Exit;
      Compressed := True;
    end;
  end;

  // Last test if extracting goes wrong...
  if (RomFile = '') or not FileExistsUTF8(RomFile) then
    // Error code already set...
    Exit;

  TempTime := Now;

  Result := Emulator.Execute(RomFile);

  // if Emulator returns no error and passed at least one minute...
  if (Result = 0) and (Now > (EncodeTime(0, 1, 0, 0) + TempTime)) then
  begin
    aGame.AddPlayingTime(Now, TempTime);
    aGame.LastTime := TempTime;
    aGame.IncTimesPlayed;
  end;

  // Kill all them
  if Compressed then
  begin
    if System.ExtractAll then
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

constructor cGameManager.Create(const aSystemsFile: string;
  const aTempFolder: string; const aTempFile: string);
begin
  SystemsFile := aSystemsFile;
  TempFolder := aTempFolder;
  TempFile := aTempFile;
  CRCMaxSize := 1024 * 1024 * 1024;

  FGameList := TFPObjectList.Create(True);
  FGroupList := TFPObjectList.Create(True);

  FCompressedExt := TStringList.Create;
  CompressedExt.CommaText := w7zFileExts;
end;

destructor cGameManager.Destroy;
begin
  SaveSystemGameList;
  FreeAndNil(FEmulator);
  FreeAndNil(FSystem);
  FreeAndNil(FGroupList);
  FreeAndNil(FGameList);
  FreeAndNil(FCompressedExt);
  inherited Destroy;
end;

end.
