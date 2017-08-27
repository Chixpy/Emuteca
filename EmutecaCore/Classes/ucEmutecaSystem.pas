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

{ cSystem unit. }
unit ucEmutecaSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8,
  uEmutecaCommon,
  uaEmutecaCustomSystem,
  ucEmutecaGroupManager, ucEmutecaGroupList, ucEmutecaGroup,
  ucEmutecaSoftManager, ucEmutecaSoftList, ucEmutecaSoftware;

type

  { cEmutecaSystem }

  cEmutecaSystem = class(caEmutecaCustomSystem)
  private
    FGroupManager: cEmutecaGroupManager;
    FProgressCallBack: TEmutecaProgressCallBack;
    FSoftManager: cEmutecaSoftManager;
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack);

  protected
    procedure CacheGroups;

  public
    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressCallBack;

    procedure AddSoft(aSoft: cEmutecaSoftware);
    //< Safe way to add software (add group if needed and link them)
    procedure LoadSoftGroupLists(aFile: string);
    procedure ImportSoftGroupLists(aFile: string);
    procedure SaveSoftGroupLists(aFile: string; ExportMode: boolean);

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

procedure cEmutecaSystem.LoadSoftGroupLists(aFile: string);
begin
  SoftManager.ClearData;
  if FileExistsUTF8(aFile + krsFileExtSoft) then
    SoftManager.LoadFromFileTxt(aFile + krsFileExtSoft);

  GroupManager.ClearData;
  if FileExistsUTF8(aFile + krsFileExtGroup) then
    GroupManager.LoadFromFileTxt(aFile + krsFileExtGroup);

  CacheGroups;
end;

procedure cEmutecaSystem.ImportSoftGroupLists(aFile: string);
begin
  if FileExistsUTF8(aFile + krsFileExtSoft) then
    SoftManager.ImportFromFileCSV(aFile + krsFileExtSoft);

  // Updating groups and lists
  CacheGroups;

  if FileExistsUTF8(aFile + krsFileExtGroup) then
    GroupManager.ImportFromFileCSV(aFile + krsFileExtGroup);
end;

procedure cEmutecaSystem.SaveSoftGroupLists(aFile: string;
  ExportMode: boolean);
begin
  if aFile = '' then
    Exit;

  if not DirectoryExistsUTF8(ExtractFileDir(aFile)) then
    ForceDirectoriesUTF8(ExtractFileDir(aFile));

  GroupManager.SaveToFileTxt(aFile + krsFileExtGroup, ExportMode);
  SoftManager.SaveToFileTxt(aFile + krsFileExtSoft, ExportMode);
end;

constructor cEmutecaSystem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

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
  SoftManager.FullList.Add(aSoft);

  aSoft.CachedSystem := Self;

  if assigned(aSoft.CachedGroup) then
  begin
    aGroup := cEmutecaGroup(aSoft.CachedGroup);
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

  // Add to visible count
  if aGroup.SoftList.Count = 1 then
    GroupManager.VisibleList.Add(aGroup);
end;

initialization
  RegisterClass(cEmutecaSystem);

finalization
  UnRegisterClass(cEmutecaSystem);
end.
