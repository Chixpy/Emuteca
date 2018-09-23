unit ucEmutecaGroupManager;
{< cEmutecaGroupManager class unit.

  ----

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
  Classes, SysUtils, FileUtil, LazFileUtils, LazUTF8, LConvEncoding, LResources,
  // CHX abstracts
  uaCHXStorable,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr, uEmutecaCommon,
  // Emuteca Core abstracts
  uaEmutecaCustomManager, uaEmutecaCustomSystem,
  // Emuteca Core classes
  ucEmutecaGroupList;

type
  { cEmutecaGroupManager }

  cEmutecaGroupManager = class(caEmutecaCustomManagerTxt)
  private
    FSystem: caEmutecaCustomSystem;
    FVisibleList: cEmutecaGroupList;
    FFullList: cEmutecaGroupList;
    procedure SetSystem(AValue: caEmutecaCustomSystem);

  protected
    procedure ActLoadStrLst(aGrpLst: cEmutecaGroupList; aTxtFile: TStrings);

  public
    property System: caEmutecaCustomSystem read FSystem write SetSystem;
    {< Owner, as a shorcut to access it. }

    property VisibleList: cEmutecaGroupList read FVisibleList;
    {< Parents with soft. Updated by System when loading. }

    procedure ClearData;
    function AddGroup(aID: string): integer;

    // Inherited abstracts
    // -------------------
    procedure LoadFromStrLst(aTxtFile: TStrings); override;
    procedure SaveToStrLst(aTxtFile: TStrings); override;
    procedure ImportFromStrLst(aTxtFile: TStrings); override;
    {< Updates groups' data from file. It don't add any group to the list.
    }
    procedure ExportToStrLst(aTxtFile: TStrings); override;
    {< Saves groups' common data for importing.
    }

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property FullList: cEmutecaGroupList read FFullList;
    {< Actual list where the parents are stored. }

  end;

implementation

uses uaEmutecaCustomGroup,
  ucEmutecaSystem, ucEmutecaGroup;

{ cEmutecaGroupManager }

procedure cEmutecaGroupManager.ClearData;
begin
  VisibleList.Clear;
  FullList.Clear;
end;

procedure cEmutecaGroupManager.SetSystem(AValue: caEmutecaCustomSystem);
var
  i: integer;
  aGroup: cEmutecaGroup;
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;

  i := 0;
  while i < FullList.Count do
  begin
    aGroup := cEmutecaGroup(FullList[i]);
    aGroup.CachedSystem := System;
  end;
end;

procedure cEmutecaGroupManager.ActLoadStrLst(aGrpLst: cEmutecaGroupList;
  aTxtFile: TStrings);
var
  aGroup: cEmutecaGroup;
  i: integer;

begin
  // aGrpLst.BeginUpdate;
  aGrpLst.Capacity := aGrpLst.Count + aTxtFile.Count + 1; // Speed Up?
  i := 1; // Skipping Header
  while i < aTxtFile.Count do
  begin
    aGroup := cEmutecaGroup.Create(nil);
    aGroup.CommaText := aTxtFile[i];
    aGroup.CachedSystem := System;

    if Assigned(ProgressCallBack) then
      ProgressCallBack(rsLoadingGroupList, aGroup.Title,
        i, aTxtFile.Count, False);

    aGrpLst.Add(aGroup);
    Inc(i);
  end;
  // aGrpLst.EndUpdate;

  if Assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

procedure cEmutecaGroupManager.ImportFromStrLst(aTxtFile: TStrings);
var
  aGrpLst: cEmutecaGroupList;
  i, j, aComp: integer;
  aGroup1, aGroup2: cEmutecaGroup;
begin
  if not Assigned(aTxtFile) then
    Exit;

  aGrpLst := cEmutecaGroupList.Create(True);
  try
    // Loading import group list
    ActLoadStrLst(aGrpLst, aTxtFile);

    aGrpLst.Sort(@EmutecaCompareGroupsByID);
    FullList.Sort(@EmutecaCompareGroupsByID);

    i := aGrpLst.Count - 1;
    if i >= 0 then
      aGroup2 := aGrpLst[i]
    else
      aGroup2 := nil;
    j := FullList.Count;
    while j > 0 do
    begin
      Dec(j);
      aGroup1 := FullList[j];

      if assigned(ProgressCallBack) then
        ProgressCallBack(rsImportingGroupList, aGroup1.Title,
          j, FullList.Count,
          False);

      if assigned(aGroup2) then
        aComp := aGroup1.CompareID(aGroup2.ID)
      else
        aComp := 1; // aGroup1.CompareID('');

      // aGroup1 < aGroup2 -> Try Previous group2
      while aComp < 0 do
      begin
        Dec(i);
        if i >= 0 then
          aGroup2 := aGrpLst[i]
        else
          aGroup2 := nil;

        if assigned(aGroup2) then
          aComp := aGroup1.CompareID(aGroup2.ID)
        else
          aComp := 1; // aGroup1.CompareID('');
      end;
      // aGroup1 > aGroup2 -> Not found.
      // aGroup1 = aGroup2 -> Match.
      if aComp = 0 then
        aGroup1.ImportFrom(aGroup2);
    end;

  finally
    aGrpLst.Free;
  end;
  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

procedure cEmutecaGroupManager.ExportToStrLst(aTxtFile: TStrings);
var
  ExpGroupList: cEmutecaGroupList;
  i, j, aComp: integer;
  aGroup, aExpGroup, NewGroup: cEmutecaGroup;
begin
  if not Assigned(aTxtFile) then
    Exit;

  ExpGroupList := cEmutecaGroupList.Create(True);
  try
    // Loading export soft in file
    ActLoadStrLst(ExpGroupList, aTxtFile);

    // Sorting by ID both lists
    FullList.Sort(@EmutecaCompareGroupsByID);
    ExpGroupList.Sort(@EmutecaCompareGroupsByID);

    // Dragons...
    i := 0;
    j := 0;
    if ExpGroupList.Count > 0 then
      aExpGroup := ExpGroupList[j]
    else
      aExpGroup := nil;
    while i < FullList.Count do
    begin
      aGroup := FullList[i];

      repeat // until found or aExpGroup > aGroup
        if Assigned(aExpGroup) then
        begin
          if Assigned(aGroup) then
            aComp := aExpGroup.CompareID(aGroup.ID)
          else
            aComp := 1; // This must not happen...
        end
        else
          aComp := 1;

        if aComp < 0 then
        begin
          Inc(j);
          if ExpGroupList.Count > j then
            aExpGroup := ExpGroupList[j]
          else
            aExpGroup := nil;
        end;
      until aComp >= 0;

      if Assigned(aGroup) then
      begin
        if aComp = 0 then
          aExpGroup.ImportFrom(aGroup)
        else
        begin
          NewGroup := cEmutecaGroup.Create(nil);
          NewGroup.ID := aGroup.ID;
          NewGroup.ImportFrom(aGroup);
          ExpGroupList.Add(NewGroup);
        end;
      end;

      Inc(i);
    end;

    // Actually saving to file
    ExpGroupList.Sort(@EmutecaCompareGroupsByID);
    aTxtFile.Clear; // Clearing to export merged list
    aTxtFile.BeginUpdate;

    aTxtFile.Capacity := ExpGroupList.Count + 1; // Speed up?
    aTxtFile.Add(krsCSVGroupHeader);

    i := 0;
    while i < ExpGroupList.Count do
    begin
      aGroup := ExpGroupList[i];
      if Assigned(ProgressCallBack) then
        ProgressCallBack(rsSavingGroupList, aGroup.Title, i,
          ExpGroupList.Count, False);

      aTxtFile.Add(aGroup.ExportCommaText);

      Inc(i);
    end;

  finally
    aTxtFile.EndUpdate;
    ExpGroupList.Free;
  end;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

function cEmutecaGroupManager.AddGroup(aID: string): integer;
var
  TempGroup: cEmutecaGroup;
begin
  TempGroup := cEmutecaGroup.Create(nil);
  TempGroup.ID := aID;
  TempGroup.CachedSystem := System;
  Result := FullList.Add(TempGroup);

  if TempGroup.SoftList.Count > 0 then
    VisibleList.Add(TempGroup);
end;

procedure cEmutecaGroupManager.LoadFromStrLst(aTxtFile: TStrings);
begin
  if not Assigned(aTxtFile) then
    Exit;

  ActLoadStrLst(FullList, aTxtFile);
end;

procedure cEmutecaGroupManager.SaveToStrLst(aTxtFile: TStrings);
var
  i: integer;
  aGroup: cEmutecaGroup;
begin
  if not assigned(aTxtFile) then Exit;

  aTxtFile.BeginUpdate;
  try
    aTxtFile.Capacity := FullList.Count + 1; // Speed up?
    aTxtFile.Add(krsCSVGroupStatsHeader);

    i := 0;
    while i < FullList.Count do
    begin
      aGroup := cEmutecaGroup(FullList[i]);

      if Assigned(ProgressCallBack) then
        ProgressCallBack(rsSavingGroupList, aGroup.Title,
          i, FullList.Count, False);

      aTxtFile.Add(aGroup.CommaText);
      Inc(i);
    end;

  finally
    aTxtFile.EndUpdate;
  end;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

constructor cEmutecaGroupManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FFullList := cEmutecaGroupList.Create(True);
  FVisibleList := cEmutecaGroupList.Create(False);
end;

destructor cEmutecaGroupManager.Destroy;
begin
  FreeAndNil(FVisibleList);
  FreeAndNil(FFullList);
  inherited Destroy;
end;

initialization
  RegisterClass(cEmutecaGroupManager);

finalization
  UnRegisterClass(cEmutecaGroupManager);
end.
