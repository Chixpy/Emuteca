{ This file is part of Emuteca

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

{ cSystem unit. }
unit ucEmutecaSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8,
  u7zWrapper,
  uEmutecaCommon,
  uaEmutecaCustomSystem,
  ucEmutecaGroupManager, ucEmutecaGroupList, ucEmutecaGroup,
  ucEmutecaSoftManager, ucEmutecaSoftList, ucEmutecaSoftware;

type

  { cEmutecaSystem }

  cEmutecaSystem = class(caEmutecaCustomSystem)
  private
    FGroupManager: cEmutecaGroupManager;
    FSoftGroupLoaded: Boolean;
    FProgressCallBack: TEmutecaProgressCallBack;
    FSoftManager: cEmutecaSoftManager;
    procedure SetSoftGroupLoaded(AValue: Boolean);
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack);

  protected

  public
    property SoftGroupLoaded: Boolean read FSoftGroupLoaded write SetSoftGroupLoaded;
    {< Are system soft and groups loaded? }

    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressCallBack;
    {< Progress callback for loading soft and groups. }

    procedure ClearData;
    procedure AddSoft(aSoft: cEmutecaSoftware);
    //< Safe way to add software (adds group if needed, and link them).
    procedure AddGroup(aGroup: cEmutecaGroup);
    //< Safe way to add groups (adds group in full list and visile list).

    procedure CacheGroups;
    procedure CleanSoftGroup;
    //< Removes parents without soft and Soft not found


    procedure LoadSoftGroupLists(const aFile: string);
    procedure SaveSoftGroupLists(const aFile: string; ClearFile: Boolean);
    procedure ImportSoftGroupLists(const aFile: string);
    procedure ExportSoftGroupLists(const aFile: string; ClearFile: Boolean);

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
  SoftManager.LoadFromFile(aFile + krsFileExtSoft);
  GroupManager.LoadFromFile(aFile + krsFileExtGroup);

  CacheGroups;

  SoftGroupLoaded := True;
end;

procedure cEmutecaSystem.ImportSoftGroupLists(const aFile: string);
begin
  if FileExistsUTF8(aFile + krsFileExtSoft) then
    SoftManager.ImportFromFile(aFile + krsFileExtSoft);

  // Updating groups and lists
  CacheGroups;

  if FileExistsUTF8(aFile + krsFileExtGroup) then
    GroupManager.ImportFromFile(aFile + krsFileExtGroup);
end;

procedure cEmutecaSystem.ExportSoftGroupLists(const aFile: string;
  ClearFile: Boolean);
begin
  if aFile = '' then
    Exit;

  if not SoftGroupLoaded then Exit;

  if not DirectoryExistsUTF8(ExtractFileDir(aFile)) then
    ForceDirectoriesUTF8(ExtractFileDir(aFile));

  GroupManager.ExportToFile(aFile + krsFileExtGroup, ClearFile);
  SoftManager.ExportToFile(aFile + krsFileExtSoft, ClearFile);
end;

procedure cEmutecaSystem.SaveSoftGroupLists(const aFile: string;
  ClearFile: Boolean);
begin
  if aFile = '' then
    Exit;

  // If not loaded, don't overwrite and empty file.
  if not SoftGroupLoaded then Exit;

  if not DirectoryExistsUTF8(ExtractFileDir(aFile)) then
    ForceDirectoriesUTF8(ExtractFileDir(aFile));


  GroupManager.SaveToFile(aFile + krsFileExtGroup, ClearFile);
  SoftManager.SaveToFile(aFile + krsFileExtSoft, ClearFile);
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

  SoftGroupLoaded := False;
end;

procedure cEmutecaSystem.SetSoftGroupLoaded(AValue: Boolean);
begin
  if FSoftGroupLoaded=AValue then Exit;
  FSoftGroupLoaded:=AValue;
end;

procedure cEmutecaSystem.CacheGroups;
var
  i, j, aComp: integer;
  aGroup: cEmutecaGroup;
  aSoft: cEmutecaSoftware;
begin
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

  SoftGroupLoaded := True;
end;

procedure cEmutecaSystem.AddGroup(aGroup: cEmutecaGroup);
begin
  // Is it already added?
  if GroupManager.FullList.IndexOf(aGroup) <> -1 then
    Exit;
  aGroup.CachedSystem := Self;
  GroupManager.FullList.Add(aGroup);

  // Faster than CacheGroups;
  if aGroup.SoftList.Count > 0 then
    if GroupManager.VisibleList.IndexOf(aGroup) = -1 then
      GroupManager.VisibleList.Add(aGroup);

  SoftGroupLoaded := True;
end;

procedure cEmutecaSystem.CleanSoftGroup;
var
  i: integer;
  aGroup: cEmutecaGroup;
  aSoft: cEmutecaSoftware;
  Found, Continue: Boolean;
begin
  if not SoftGroupLoaded then Exit;

  i := 0;
  Continue := True;
  while Continue and (i < SoftManager.FullList.Count) do
  begin
    aSoft := SoftManager.FullList[i];

    if assigned(ProgressCallBack) then
      Continue := ProgressCallBack(rsCleaningSystemData,
        aSoft.Title, i, SoftManager.FullList.Count, True);

    Found := False;
    if DirectoryExistsUTF8(aSoft.Folder) then
      // Uncompressed
      Found := FileExistsUTF8(aSoft.Folder + aSoft.FileName)
    else // Compressed
      Found := w7zFileExists(aSoft.Folder, aSoft.FileName, '') = 0;

    if not Found then
      SoftManager.FullList.Delete(i)
    else
      Inc(i);
  end;

  // Group can lose all soft...
  CacheGroups;

  i := 0;
  while i < GroupManager.FullList.Count do
  begin
    aGroup := GroupManager.FullList[i];

    // This is fast so no ProgressCallBack here

    if aGroup.SoftList.Count <= 0 then
      GroupManager.FullList.Delete(i)
    else
      Inc(i);
  end;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

initialization
  RegisterClass(cEmutecaSystem);

finalization
  UnRegisterClass(cEmutecaSystem);
end.
