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
  Classes, SysUtils, FileUtil, LazFileUtils,
  LazUTF8, LConvEncoding,
  LResources,
  // Emuteca core
  uaEmutecaManager, ucEmutecaParent,
  // Utils
  u7zWrapper;

resourcestring
  rsLoadingParentList = 'Loading parent list...';
  rsSavingParentList = 'Saving parent list...';

type
  { cEmutecaParentManager }

  cEmutecaParentManager = class(caEmutecaManagerTxt)
  private
    FDataFile: string;
    FFullList: cEmutecaParentMap;
    procedure SetDataFile(AValue: string);

  protected


  public
    property FullList: cEmutecaParentMap read FFullList;
    {< Actual list where the parents are stored. }

    procedure LoadFromFileTxt(TxtFile: TStrings); override;
    procedure SaveToFileTxt(TxtFile: TStrings; const ExportMode: boolean);
      override;


    function Add(aId: string): cEmutecaParent;
    {< Creates a parent with aId key, if already exists returns it.

       @Result cEmutecaParent created or found.
    }
    function ItemById(aId: string): cEmutecaParent;
    {< Return the parent with have aId key.

       @Result cEmutecaParent found.
    }
    function Delete(aId: string): integer;
    {< Deletes a parent by Id.

       @Result Index of deleted item
    }

        procedure AssingAllTo(aList: TStrings);
    procedure AssingEnabledTo(aList: TStrings);

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
    //function Execute(aGame: cEmutecaGameVersion): integer;
    //< Execute a Game.

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property DataFile: string read FDataFile write SetDataFile;
  end;

implementation

{ cEmutecaParentManager }

function cEmutecaParentManager.ItemById(aId: string): cEmutecaParent;
var
  i: integer;
begin
  // FullList.TryGetData(aId, Result); Maybe do this???

  Result := nil;
  i := FullList.IndexOf(aId);

  if i >= 0 then
    Result := FullList.Data[i];
end;

function cEmutecaParentManager.Delete(aId: string): integer;
begin
    Result := FullList.Remove(aId);
end;

procedure cEmutecaParentManager.AssingAllTo(aList: TStrings);
var
  i: longint;
begin
  if not assigned(aList) then
    aList := TStringList.Create;

  aList.BeginUpdate;
  i := 0;
  while i < FullList.Count do
  begin
    aList.AddObject(FullList.Data[i].Title, FullList.Data[i]);
    Inc(i);
  end;
  aList.EndUpdate;
end;

procedure cEmutecaParentManager.AssingEnabledTo(aList: TStrings);
begin
  { TODO : Maybe search for enabled systems... }
  AssingAllTo(aList);
end;

procedure cEmutecaParentManager.SetDataFile(AValue: string);
begin
  if FDataFile = AValue then
    Exit;
  FDataFile := AValue;
end;

procedure cEmutecaParentManager.LoadFromFileTxt(TxtFile: TStrings);
var
  i: integer;
  TempParent: cEmutecaParent;
begin
  if not Assigned(TxtFile) then
    Exit;

  i := 1; // Skipping Header
  while i < TxtFile.Count do
  begin
    TempParent := cEmutecaParent.Create(nil);
    TempParent.DataString := TxtFile[i];
    FullList.AddOrSetData(TempParent.SortName, TempParent);
    Inc(i);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsLoadingParentList, TempParent.System,
        TempParent.Title, i , TxtFile.Count);
  end;
end;

procedure cEmutecaParentManager.SaveToFileTxt(TxtFile: TStrings;
  const ExportMode: boolean);
var
  i: Integer;
begin
  if not Assigned(TxtFile) then
    Exit;

  if not ExportMode then
  begin
    TxtFile.Clear;
    TxtFile.Add('"ID/Sort Name","System","Title"');
  end;

  i := 0;
  while i < FullList.Count do
  begin
    FullList.Data[i].SaveToFileTxt(TxtFile, ExportMode);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsSavingParentList, FullList.Data[i].System,
        FullList.Data[i].Title, i + 1, FullList.Count);
    Inc(i);
  end;
end;

function cEmutecaParentManager.Add(aId: string): cEmutecaParent;
begin
  Result := ItemById(aId);

  // If already exists, then return it
  if assigned(result) then
    Exit;

  // Creating new item
  Result := cEmutecaParent.Create(Self);
  Result.SortName := aId;
  Result.Title := aId;
  FullList.Add(Result.SortName, Result);
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
  FullList.Clear;
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

  FFullList := cEmutecaParentMap.Create(True);
end;

destructor cEmutecaParentManager.Destroy;
begin
  SaveToFile('', false);
  FreeAndNil(FFullList);
  inherited Destroy;
end;

end.
