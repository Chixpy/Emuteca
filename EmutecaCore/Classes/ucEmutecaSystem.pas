unit ucEmutecaSystem;
{< cEmutecaSystem class unit.

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
  Classes, SysUtils, fgl, LazFileUtils, LazUTF8,
  // CHX units
  uCHX7zWrapper,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr,
  // Emuteca Core abstracts
  uaEmutecaCustomSystem,
  // Emuteca Core classes
  ucEmutecaGroupManager, ucEmutecaGroupList, ucEmutecaGroup,
  ucEmutecaSoftManager, ucEmutecaSoftList, ucEmutecaSoftware;

type

  { cEmutecaSystem }

  cEmutecaSystem = class(caEmutecaCustomSystem)
  private
    FGroupManager: cEmutecaGroupManager;
    FSoftGroupLoaded: boolean;
    FProgressCallBack: TEmutecaProgressCallBack;
    FSoftManager: cEmutecaSoftManager;
    procedure SetSoftGroupLoaded(AValue: boolean);
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack);

  protected
    property SoftGroupLoaded: boolean read FSoftGroupLoaded
      write SetSoftGroupLoaded;
    {< Are system soft and groups allready loaded? }

  public

    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressCallBack;
    {< Progress callback for loading soft and groups. }

    procedure ClearData;
    procedure AddSoft(aSoft: cEmutecaSoftware);
    {< Safe way to add software (adds group if needed, and link them). }
    procedure AddGroup(aGroup: cEmutecaGroup);
    {< Safe way to add groups (adds group in full list and visile list). }

    procedure CacheGroups;
    {< Add groups to software. }
    procedure CleanSoftGroup;
    {< Removes parents without soft and Soft not found. }


    procedure LoadSoftGroupLists(const aFile: string);
    procedure SaveSoftGroupLists(const aFile: string; ClearFile: boolean);
    procedure UnloadSoftGroupLists;
    procedure ImportSoftGroupLists(const aFile: string);
    procedure ExportSoftGroupLists(const aFile: string; ClearFile: boolean);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property GroupManager: cEmutecaGroupManager read FGroupManager;
    property SoftManager: cEmutecaSoftManager read FSoftManager;

  end;

  TEmutecaReturnSystemCB = function(aSystem: cEmutecaSystem): boolean of
    object;
{< For CallBack functions }

implementation

{ cEmutecaSystem }

procedure cEmutecaSystem.LoadSoftGroupLists(const aFile: string);
begin
  if SoftGroupLoaded then
    Exit;

  SoftManager.LoadFromFile(aFile + krsFileExtSoft);

  { TODO 1 : Si el fichero 'aFile + krsFileExtGroup' no existe,
      Emuteca muestra un error de SIGEVN. Si se comenta la línea también.

      Si el fichero existe y está vacío funciona perfectamente.

      Con el SoftManager no sucede eso, funcionando si no existe su fichero.
  }
  GroupManager.LoadFromFile(aFile + krsFileExtGroup);

  SoftGroupLoaded := True;

  CacheGroups;
end;

procedure cEmutecaSystem.ImportSoftGroupLists(const aFile: string);
begin
  if not SoftGroupLoaded then
    Exit;

  if FileExistsUTF8(aFile + krsFileExtSoft) then
    SoftManager.ImportFromFile(aFile + krsFileExtSoft);

  // Updating groups and lists
  CacheGroups;

  if FileExistsUTF8(aFile + krsFileExtGroup) then
    GroupManager.ImportFromFile(aFile + krsFileExtGroup);
end;

procedure cEmutecaSystem.ExportSoftGroupLists(const aFile: string;
  ClearFile: boolean);
begin
  if not SoftGroupLoaded then
    Exit;

  if aFile = '' then
    Exit;

  if not DirectoryExistsUTF8(ExtractFileDir(aFile)) then
    ForceDirectoriesUTF8(ExtractFileDir(aFile));

  GroupManager.ExportToFile(aFile + krsFileExtGroup, ClearFile);
  SoftManager.ExportToFile(aFile + krsFileExtSoft, ClearFile);
end;

procedure cEmutecaSystem.SaveSoftGroupLists(const aFile: string;
  ClearFile: boolean);
begin
  // If not loaded, don't overwrite with empty file.
  if not SoftGroupLoaded then
    Exit;

  if aFile = '' then
    Exit;

  if not DirectoryExistsUTF8(ExtractFileDir(aFile)) then
    ForceDirectoriesUTF8(ExtractFileDir(aFile));

  GroupManager.SaveToFile(aFile + krsFileExtGroup, ClearFile);
  SoftManager.SaveToFile(aFile + krsFileExtSoft, ClearFile);
end;

procedure cEmutecaSystem.UnloadSoftGroupLists;
begin
  ClearData;
  SoftGroupLoaded := False;
end;

constructor cEmutecaSystem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SoftGroupLoaded := False;

  FGroupManager := cEmutecaGroupManager.Create(Self);
  GroupManager.System := Self;
  FSoftManager := cEmutecaSoftManager.Create(Self);
  SoftManager.System := Self;
end;

destructor cEmutecaSystem.Destroy;
begin
  SoftManager.Free;
  GroupManager.Free;

  inherited Destroy;
end;

procedure cEmutecaSystem.SetProgressCallBack(AValue: TEmutecaProgressCallBack);
begin
  if FProgressCallBack = AValue then
    Exit;
  FProgressCallBack := AValue;

  GroupManager.ProgressCallBack := ProgressCallBack;
  SoftManager.ProgressCallBack := ProgressCallBack;
end;

procedure cEmutecaSystem.ClearData;
begin
  SoftManager.ClearData;
  GroupManager.ClearData;
end;

procedure cEmutecaSystem.SetSoftGroupLoaded(AValue: boolean);
begin
  if FSoftGroupLoaded = AValue then
    Exit;
  FSoftGroupLoaded := AValue;
end;

procedure cEmutecaSystem.CacheGroups;
var
  i, j, aComp: integer;
  aGroup: cEmutecaGroup;
  aSoft: cEmutecaSoftware;
begin
  if not SoftGroupLoaded then
    Exit;

  // Cleaning aGroup.SoftList
  i := 0;
  while i < GroupManager.FullList.Count do
  begin
    aGroup := GroupManager.FullList[i];
    aGroup.SoftList.Clear;
    Inc(i);
  end;

  // Updating soft groups and groups softlists.
  // ------------------------------------------
  // Here are dragons
  GroupManager.FullList.Sort(@EmutecaCompareGroupsByID);
  SoftManager.FullList.Sort(@EmutecaCompareSoftByGroupKey);

  // Uhm? Backwards? B-P
  i := GroupManager.FullList.Count - 1;
  if i >= 0 then
    aGroup := GroupManager.FullList[i]
  else
    aGroup := nil;
  j := SoftManager.FullList.Count;
  while j >= 1 do
  begin
    Dec(j);
    aSoft := SoftManager.FullList[j];

    if assigned(aGroup) then
      aComp := aSoft.CompareGroupKey(aGroup.ID)
    else
      aComp := 1; // aSoft.CompareGroupKey('');

    // Group > Soft -> Try Previous group
    while aComp < 0 do
    begin
      Dec(i);
      if i >= 0 then
        aGroup := GroupManager.FullList[i]
      else
        aGroup := nil;

      if assigned(aGroup) then
        aComp := aSoft.CompareGroupKey(aGroup.ID)
      else
        aComp := 1; // aSoft.CompareGroupKey('');
    end;

    // (Group < Soft) -> Ops, group doesn't exist
    if (aComp > 0) then
      aGroup := GroupManager.FullList[GroupManager.AddGroup(aSoft.GroupKey)];

    aGroup.SoftList.Add(aSoft);
    aSoft.CachedGroup := aGroup;
  end;

  // Adding to visible list groups with soft
  GroupManager.VisibleList.Clear;
  i := 0;
  while i < GroupManager.FullList.Count do
  begin
    aGroup := GroupManager.FullList[i];
    if aGroup.SoftList.Count > 0 then
      GroupManager.VisibleList.Add(aGroup);
    Inc(i);
  end;
end;

procedure cEmutecaSystem.AddSoft(aSoft: cEmutecaSoftware);
var
  aGroup: cEmutecaGroup;
begin
  if not SoftGroupLoaded then
    Exit;

  // Is it already added?
  if SoftManager.FullList.IndexOf(aSoft) <> -1 then
    Exit;

  aSoft.CachedSystem := Self;
  SoftManager.FullList.Add(aSoft);

  if assigned(aSoft.CachedGroup) then
  begin
    aGroup := cEmutecaGroup(aSoft.CachedGroup);
    AddGroup(aGroup);
    aGroup.SoftList.Add(aSoft);
  end
  else
  begin
    aGroup := GroupManager.FullList.ItemById(aSoft.GroupKey);

    if not Assigned(aGroup) then
      aGroup := GroupManager.FullList[GroupManager.AddGroup(aSoft.GroupKey)];

    aSoft.CachedGroup := aGroup;
    aGroup.SoftList.Add(aSoft);
  end;

  // Faster than CacheGroups;
  if aGroup.SoftList.Count > 0 then
    if GroupManager.VisibleList.IndexOf(aGroup) = -1 then
      GroupManager.VisibleList.Add(aGroup);
end;

procedure cEmutecaSystem.AddGroup(aGroup: cEmutecaGroup);
begin
  if not SoftGroupLoaded then
    Exit;

  // Is it already added?
  if GroupManager.FullList.IndexOf(aGroup) <> -1 then
    Exit;

  aGroup.CachedSystem := Self;
  GroupManager.FullList.Add(aGroup);

  // Faster than CacheGroups;
  if aGroup.SoftList.Count > 0 then
    if GroupManager.VisibleList.IndexOf(aGroup) = -1 then
      GroupManager.VisibleList.Add(aGroup);
end;

procedure cEmutecaSystem.CleanSoftGroup;

  procedure CleanSoftList;
  var
    Last7z: string;
    CompFileList: TStringList;
    aSoft: cEmutecaSoftware;
    Found, Continue: boolean;
    i, j: integer;
  begin
    // Sorting by filename
    if assigned(ProgressCallBack) then
      ProgressCallBack('Sorting soft list by filename...',
        'This can take a while.', 1, 100, False);
    SoftManager.FullList.Sort(@EmutecaCompareSoftByFileName);

    i := 0;
    Continue := True;
    Last7z := '';
    CompFileList := TStringList.Create;
    try
      while Continue and (i < SoftManager.FullList.Count) do
      begin
        aSoft := SoftManager.FullList[i];

        if assigned(ProgressCallBack) then
          Continue := ProgressCallBack(rsCleaningSystemData,
            aSoft.Title, i, SoftManager.FullList.Count, True);

        Found := False;

        if DirectoryExistsUTF8(aSoft.Folder) then
          // Uncompressed, simple.
          Found := FileExistsUTF8(aSoft.Folder + aSoft.FileName)
        else
        begin
        { Old, simple and slow way... Calculates SHA1 of 7z with every soft!!!
        Found := w7zFileExists(aSoft.Folder, aSoft.FileName, '') = 0;
        }

          // New, complex and faster way

          if CompareFilenames(Last7z, ExcludeTrailingPathDelimiter(
            aSoft.Folder)) <> 0 then
          begin
            // It's a new non cached 7z
            Last7z := ExcludeTrailingPathDelimiter(aSoft.Folder);
            CompFileList.Clear;
            if FileExistsUTF8(Last7z) then // Hides file not found error.
              w7zListFiles(Last7z, CompFileList, True, '');
          end;

          // Searching aSoft.FileName en CompFileList
          j := 0;
          while (not Found) and (j < CompFileList.Count) do
          begin
            Found := CompareFilenames(CompFileList[j], aSoft.FileName) = 0;
            if Found then
              CompFileList.Delete(j);
            Inc(j);
          end;
        end;

        if not Found then
          SoftManager.FullList.Delete(i)
        else
          Inc(i);
      end;

    finally
      CompFileList.Free;
    end;

    if assigned(ProgressCallBack) then
      ProgressCallBack('', '', 0, 0, False);
  end;

  procedure CleanGroupList;
  var
    i: integer;
    aGroup: cEmutecaGroup;
  begin
    if assigned(ProgressCallBack) then
      ProgressCallBack(rsCleaningSystemData,
        'Cleaning empty groups.', 1, 2, False);

    i := 0;
    while i < GroupManager.FullList.Count do
    begin
      aGroup := GroupManager.FullList[i];

      if aGroup.SoftList.Count <= 0 then
        GroupManager.FullList.Delete(i)
      else
        Inc(i);
    end;

    if assigned(ProgressCallBack) then
      ProgressCallBack('', '', 0, 0, False);
  end;

begin
  if not SoftGroupLoaded then
    Exit;

  CleanSoftList;

  // Groups can be empty...
  CacheGroups;

  // ... so this must be executed.
  CleanGroupList;
end;

initialization
  RegisterClass(cEmutecaSystem);

finalization
  UnRegisterClass(cEmutecaSystem);
end.
