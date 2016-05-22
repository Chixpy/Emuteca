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

{ cGameManager unit. }
unit ucEmutecaParentManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, IniFiles,
  LazUTF8, LConvEncoding,
  LResources,
  // Emuteca core
  uEmutecaCommon, ucEmutecaParent,
  // Utils
  u7zWrapper;

type
  { cEmutecaParentManager }

  cEmutecaParentManager = class(TComponent)
  private
    FCurrentList: cEmutecaParentList;
    FDataFile: string;
    FList: cEmutecaParentList;
    FProgressCallBack: TEmutecaProgressCallBack;
    procedure SetDataFile(AValue: string);
    procedure SetProgressCallBack(const AValue: TEmutecaProgressCallBack);

  protected


  public
    property List: cEmutecaParentList read FList;
    {< Actual list where the parents are stored. }
    property CurrentList: cEmutecaParentList read FCurrentList;
    {< Current loaded games list. }

    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressCallBack;
    //< CallBack function to show the progress in actions.

    procedure LoadDataFile;
    //< Loads current data file
    procedure SaveDataFile;
    //< Saves current data file

    procedure ImportDataFile(const aFileName: string);
    //< Imports data to the current list
    procedure ExportDataFile(const aFileName: string);
    //< Exports data of the current list


    function ItemById(aId: string): cEmutecaParent;
    //< Return the game with have aParentKey key

    {function GameAtPos(const aIndex: integer): cEmutecaGameVersion;
    //< Return the game at a position.
    function Game(aGameKey: string): cEmutecaGameVersion;
    //< Return the game with have aGameKey key.
    function GameCount: longint;
    //< Return the number of games.

    function GroupAtPos(const aIndex: integer): cEmutecaParent;
    //< Return the group at a position.
    function Group(aGroupKey: string): cEmutecaParent;
    //< Return the game with have aGroupKey key.
    function GroupCount: longint;
    //< Return the number of groups.

    procedure SaveSystem;
    //< Save current system configuration.
    procedure ChangeSystem(const SystemName: string); deprecated;
    //< Change the current system. (By name)
    procedure ChangeSystem2(aSystem: cEmutecaSystem);
    //< Change the current system. (By reference)
    procedure ChangeEmulator(const aEmulatorName: string);
    //< Change the current emultor.
    }
    procedure ClearGameData;
    //< Removes al games and groups from the system.

    {
    procedure UpdateGameList;
    procedure SoftUpdateGameList;
    }
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

    {function AddGame(const aFolder: string; const aFileName: string;
      const aKey: string): cEmutecaGameVersion;
    //< Add a game.
    function AddGroup(aGameGroupID: string): cEmutecaGameFamily;
    //< Add a group.

    function GameMediaExists(aFolder: string;
      aGameVersion: cEmutecaGameVersion; Extensions: TStrings): boolean;
    function GroupMediaExists(aFolder: string; aGameGroup: cEmutecaGameFamily;
      Extensions: TStrings): boolean;

    procedure SearchGameMedia(FileList: TStrings; aFolder: string;
      aGameVersion: cEmutecaGameVersion; Extensions: TStrings);
    procedure SearchGroupMedia(FileList: TStrings; aFolder: string;
      aGameGroup: cEmutecaGameFamily; Extensions: TStrings);
    procedure SearchMediaFiles(FileList: TStrings; aFolder: string;
      aFileName: string; Extensions: TStrings);

    }
    procedure SaveSystemGameList;
    //< Save the current system game list (and groups).
    procedure LoadParentList(aParentFile: string);
    //< Load the parent list
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

    //function Execute(aGame: cEmutecaGameVersion): integer;
    //< Execute a Game.

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property DataFile: string read FDataFile write SetDataFile;
  end;

implementation

{ cEmutecaParentManager }
procedure cEmutecaParentManager.SetProgressCallBack(
  const AValue: TEmutecaProgressCallBack);
begin
  FProgressCallBack := AValue;
end;

procedure cEmutecaParentManager.LoadDataFile;
begin
  List.Clear;
  ImportDataFile(DataFile); // Dirty trick :-P
end;

procedure cEmutecaParentManager.SaveDataFile;
begin
  List.SaveToFile(DataFile);
end;

procedure cEmutecaParentManager.ImportDataFile(const aFileName: string);
var
  OldCF: string;
  i: integer;
begin
  OldCF := DataFile;
  DataFile := aFileName;
  List.LoadFromFile(DataFile);
  // As List changed, then add all games to visible list
  CurrentList.Clear;
  i := 0;
  while i < List.Count do
  begin
    CurrentList.Add(List[i]);
    Inc(i);
  end;
  DataFile := OldCF;
end;

procedure cEmutecaParentManager.ExportDataFile(const aFileName: string);
var
  OldCF: string;
begin
  OldCF := DataFile;
  DataFile := aFilename;
  { TODO : Don't save user data... (Path to the File and if it's enabled) }
  SaveDataFile;
  DataFile := OldCF;
end;

function cEmutecaParentManager.ItemById(aId: string): cEmutecaParent;
var
  i: integer;
  aItem: cEmutecaParent;
begin
  Result := nil;
  aId := Trim(UTF8LowerCase(aId));

  // Maybe backwards is better for batch operations...
  i := List.Count - 1;
  while (i >= 0) do
  begin
    aItem := List.Items[i];
    if UTF8CompareText(aItem.SortName, aId) = 0 then
    begin
      Result := aItem;
      Break; // ... dirty exit, but we don't need to check: Result <> nil
    end;
    Dec(i);
  end;
end;

procedure cEmutecaParentManager.SetDataFile(AValue: string);
begin
  if FDataFile = AValue then
    Exit;
  FDataFile := AValue;
end;

{
function cEmutecaParentManager.GameAtPos(const aIndex: integer): cEmutecaGameVersion;
begin
  if (aIndex < GameList.Count) and (aIndex >= 0) then
    Result := cEmutecaGameVersion(GameList.Items[aIndex])
  else
    Result := nil;
end;

function cEmutecaParentManager.Game(aGameKey: string): cEmutecaGameVersion;
var
  i: integer;
  aGame: cEmutecaGameVersion;
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

function cEmutecaParentManager.GameCount: longint;
begin
  Result := GameList.Count;
end;
}
{
function cEmutecaParentManager.GroupAtPos(const aIndex: integer):
cEmutecaGameFamily;
begin
  if (aIndex < GroupCount) and (aIndex >= 0) then
    Result := cEmutecaGameFamily(ParentList.Items[aIndex])
  else
    Result := nil;
end;

function cEmutecaParentManager.Group(aGroupKey: string): cEmutecaGameFamily;
var
  i: integer;
  aGroup: cEmutecaGameFamily;
begin
  Result := nil;
  aGroupKey := Trim(UTF8LowerCase(aGroupKey));

  // Backwards and we don't access GroupCount every iteration...
  //   Meh, see cEmutecaParentManager.Game(aGameKey: String): cEmutecaGameVersion;
  i := GroupCount - 1;
  while (i >= 0) do
  begin
    aGroup := GroupAtPos(i);
    if UTF8CompareText(aGroup.GameID, aGroupKey) = 0 then
    begin
      Result := aGroup;
      Break;
    end;
    Dec(i);
  end;
end;

function cEmutecaParentManager.GroupCount: longint;
begin
  Result := ParentList.Count;
end;
}

{procedure cEmutecaParentManager.SaveSystem;
begin
  if System = nil then
    Exit;
  System.SaveToFile(SystemsFile);
end;

procedure cEmutecaParentManager.ChangeSystem(const SystemName: string);
begin

  if System <> nil then
    // Changing to same system?
    if UTF8CompareText(System.ID, SystemName) = 0 then
      Exit
    else
      SaveSystemGameList;

  // Clearing temp folder
  DeleteDirectory(TempFolder, True);

  FreeAndNil(FSystem);
  FreeAndNil(FEmulator);
  ClearGameData;

  FSystem := cEmutecaSystem.Create(nil);
  System.id := SystemName;
  System.LoadFromFile(SystemsFile);
  if not System.Enabled then
  begin
    FreeAndNil(FSystem);
    Exit;
  end;

  ChangeEmulator(System.MainEmulator);

  LoadParentList;
end;

procedure cEmutecaParentManager.ChangeSystem2(aSystem: cEmutecaSystem);
begin
  if aSystem <> nil then
    // Changing to same system?
    if System = aSystem then
      Exit
    else
      SaveSystemGameList;

  FSystem := aSystem;

  // Clearing temp folder
  DeleteDirectory(TempFolder, True);

  ClearGameData;

  ChangeEmulator(System.MainEmulator);

  LoadParentList;
end;

procedure cEmutecaParentManager.ChangeEmulator(const aEmulatorName: string);
begin
  if Emulator <> nil then
    Emulator.SaveToFile(EmulatorsFile);
  FreeAndNil(FEmulator);

  FEmulator := cEmutecaEmulator.Create(nil);
  Emulator.EmulatorName := aEmulatorName;
  Emulator.LoadFromFile(EmulatorsFile);
  if not Emulator.Enabled then
    FreeAndNil(FEmulator);
end;
}
procedure cEmutecaParentManager.ClearGameData;
begin
  // GameList.Clear;
  List.Clear;
end;

{
procedure cEmutecaParentManager.UpdateGameList;
var
  DataFile: string;
begin
  if System = nil then
    Exit;
  DataFile := TempFolder + TempFile;
  if FileExistsUTF8(DataFile) then
    DeleteFileUTF8(DataFile);
  ExportGameData(DataFile, False);
  ClearGameData;
  IterateFolderObj(System.GameFolder, @Self.AddFile,
    System.RecursiveGameFolder);
  ImportGameData(DataFile);
  if FileExistsUTF8(DataFile) then
    DeleteFileUTF8(DataFile);
end;

procedure cEmutecaParentManager.SoftUpdateGameList;
begin
  // TODO 2: SoftUpdateGameList
  //   1. Remove Games wich file don't exists
  //   2. Add the new files
  // Is it faster than UpdateGameList?
  //   No Export/Import is needed...
  UpdateGameList;
end;
 }
procedure cEmutecaParentManager.UpdateGroupList;
//var
//i, j: integer;
begin
{
  List.Clear;
  i := 0;
  j := Self.GameCount;
  while i < j do
  begin
    Self.AddGroup(GameAtPos(i).GameGroup);
    Inc(i);
  end;
}

end;

function cEmutecaParentManager.AddFile(aFolder: string;
  Info: TSearchRec): boolean;
  //var
  //Extension: string;
  //i, j: integer;
  //CompFiles, aFile: TStringList;
begin

  Result := True;
  {
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
      Result := ProgressCallBack(EMMCBAddFile, aFolder, Info.Name, 0, 1);
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
                Result := ProgressCallBack(EMMCBAddFile, aFolder +
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
  }
end;

{
function cEmutecaParentManager.AddGame(const aFolder: string;
  const aFileName: string; const aKey: string): cEmutecaGameVersion;
var
  aGame: cEmutecaGameVersion;
begin

  aGame := cEmutecaGameVersion.Create(nil);
  aGame.Folder := aFolder;
  aGame.FileName := aFileName;
  aGame.Key := aKey;
  GameList.Add(aGame);
  AddGroup(aGame.GameGroup);
  Result := aGame;

end;

function cEmutecaParentManager.AddGroup(aGameGroupID: string):
cEmutecaGameFamily;
begin
  aGameGroupID := Trim(aGameGroupID);
  Result := Group(aGameGroupID);

  if Result = nil then
  begin
    Result := cEmutecaGameFamily.Create(nil);
    Result.GameID := aGameGroupID;
    ParentList.Add(Result);
  end;
end;

function cEmutecaParentManager.GameMediaExists(aFolder: string;
  aGameVersion: cEmutecaGameVersion; Extensions: TStrings): boolean;
var
  TmpStrList: TStringList;
  aGameGroup: cEmutecaGameFamily;
begin
  Result := False;
  TmpStrList := TStringList.Create;
  try
    SearchMediaFiles(TmpStrList, aFolder,
      RemoveFromBrackets(aGameVersion.FileName) + kEmutecaVirtualGameExt,
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

function cEmutecaParentManager.GroupMediaExists(aFolder: string;
  aGameGroup: cEmutecaGameFamily; Extensions: TStrings): boolean;
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

procedure cEmutecaParentManager.SearchGameMedia(FileList: TStrings;
  aFolder: string; aGameVersion: cEmutecaGameVersion; Extensions: TStrings);
begin
  SearchMediaFiles(FileList, aFolder,
    RemoveFromBrackets(aGameVersion.FileName) + kEmutecaVirtualGameExt,
    Extensions);
  if FileList.Count = 0 then
    SearchGroupMedia(FileList, aFolder, Group(aGameVersion.GameGroup),
      Extensions);
end;

procedure cEmutecaParentManager.SearchGroupMedia(FileList: TStrings;
  aFolder: string; aGameGroup: cEmutecaGameFamily; Extensions: TStrings);
begin
  SearchMediaFiles(FileList, aFolder, aGameGroup.MediaFileName, Extensions);
end;

procedure cEmutecaParentManager.SearchMediaFiles(FileList: TStrings;
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
}
procedure cEmutecaParentManager.SaveSystemGameList;
//var
//i, j: integer;
//aFileName: string;
//aFile: TFileStream;
//aGame: cEmutecaGameVersion;
//aGroup: cEmutecaParent;
begin
  { TODO 1 : DON'T CHANGE FILES WHILE TESTNG BY NOW }
{
  // if System = nil then
    Exit;

  // Saving Families
  aFileName := ExtractFilePath(SystemsFile) + System.DataFile +
    GroupDataFileExt;
  if FileExistsUTF8(aFileName) then
    DeleteFileUTF8(aFileName);

  aFile := TFileStream.Create(aFileName, fmCreate);
  try
    i := 0;
    j := FList.Count;
    while i < j do
    begin
      aGroup := GroupAtPos(i);
      if ProgressCallBack <> nil then
        ProgressCallBack(EMMCBSaveList, aGroup.GameID, aGroup.Title, i, j);

      WriteComponentAsBinaryToStream(aFile, aGroup);
      Inc(i);
    end;
  finally
    FreeAndNil(aFile);
  end;

  // Saving Game versions

  aFileName := ExtractFilePath(SystemsFile) + System.DataFile +
    GameDataFileExt;
  if FileExistsUTF8(aFileName) then
    DeleteFileUTF8(aFileName);

  TFileStream.Create(aFileName, fmCreate);
  try
    i := 0;
    j := FGameList.Count;
    while i < j do
    begin
      aGame := GameAtPos(i);
      if ProgressCallBack <> nil then
        ProgressCallBack(EMMCBSaveList, aGame.GameName, aGame.Version, i, j);
      WriteComponentAsBinaryToStream(aFile, aGame);
      Inc(i);
    end;
  finally
    FreeAndNil(aFile);
  end;
  }
end;

procedure cEmutecaParentManager.LoadParentList(aParentFile: string);
var
  i: integer;
begin
  List.Clear;
  List.LoadFromFile(aParentFile);

  // As List changed, then add all games to visible list
  { TODO : Maybe there is a one line statement }
  i := 0;
  while i < List.Count do
  begin
    CurrentList.Add(List[i]);
    Inc(i);
  end;
end;

procedure cEmutecaParentManager.ExportGameData(const aFileName: string;
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

procedure cEmutecaParentManager.ExportGameDataIni(
  const aIniFile: TCustomIniFile; const ExportMode: boolean);
//var
//i, j: integer;
//aGame: cEmutecaGameVersion;
//aGameGroup: cEmutecaParent;
//Continue: boolean;
begin
  {
  i := 0;
  j := GameCount;
  Continue := True;
  while (i < j) and Continue do
  begin
    aGame := GameAtPos(i);
    aGame.ExportDataIni(aIniFile, ExportMode);
    if ProgressCallBack <> nil then
      Continue := ProgressCallBack(EMMCBExportData, aGame.GameName,
        aGame.Version, i, j);
    Inc(i);
  end;

  i := 0;
  j := GroupCount;
  Continue := True;
  while (i < j) and Continue do
  begin
    aGameGroup := GroupAtPos(i);
    aGameGroup.ExportDataIni(aIniFile);
    if ProgressCallBack <> nil then
      Continue := ProgressCallBack(EMMCBExportData, aGameGroup.Title,
        kEmutecaVirtualGroupExt, i, j);
    Inc(i);
  end;
  }
end;

procedure cEmutecaParentManager.ImportGameData(const aFileName: string);
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

procedure cEmutecaParentManager.ImportGameDataIni(
  const aIniFile: TCustomIniFile);
//var
//i, j: integer;
//aGame: cEmutecaGameVersion;
//aGameGroup: cEmutecaParent;
//Continue: boolean;
begin
  {
  i := 0;
  j := GameCount;
  Continue := True;
  while (i < j) and Continue do
  begin
    aGame := GameAtPos(i);
    aGame.ImportDataIni(aIniFile);
    if ProgressCallBack <> nil then
      Continue := ProgressCallBack(EMMCBImportData, aGame.GameName,
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
      Continue := ProgressCallBack(EMMCBImportData, aGameGroup.Title,
        kEmutecaVirtualGroupExt, i, j);
    Inc(i);
  end;
  }
end;

{function cEmutecaParentManager.Execute(aGame: cEmutecaGameVersion): integer;
var
  RomFile, CompressedFile: string;
  Error: integer;
  Compressed: boolean;
  NewDir: boolean;
  TempTime: TTime;
  aFolder: string;
begin
  // Uhm. If things go bad from the begin, they only can improve :-D
  Result := kEmutecaExecErrorNoGame;

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
  aFolder := SetAsFolder(aFolder + krsEmutecaGameSubFolder);

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
    aGame.Statistics.AddPlayingTime(Now, TempTime);
    aGame.Statistics.LastTime := TempTime;
    aGame.Statistics.TimesPlayed := aGame.Statistics.TimesPlayed + 1;
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
end;}

constructor cEmutecaParentManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FList := cEmutecaParentList.Create(True);
  FCurrentList := cEmutecaParentList.Create(False);
end;

destructor cEmutecaParentManager.Destroy;
begin
  SaveDataFile;
  FreeAndNil(FCurrentList);
  FreeAndNil(FList);
  inherited Destroy;
end;

end.
