unit ucEmutecaGroupManager;

{< cEmutecaGroupManager class unit.

  This file is part of Emuteca Core.

  Copyright (C) 2006-2023 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, LazUTF8,
  LConvEncoding, LResources,
  // CHX abstracts
  uaCHXStorable,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr,
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
  aGrpLst.Capacity := aGrpLst.Count + aTxtFile.Count; // Speed Up?

  if Assigned(ProgressCallBack) then
    ProgressCallBack(rsLoadingGroupList, '',
      0, aTxtFile.Count, False);

  i := 1; // Skipping Header
  while i < aTxtFile.Count do
  begin
    aGroup := cEmutecaGroup.Create(nil);
    aGroup.CommaText := aTxtFile[i];
    aGroup.CachedSystem := System;
    aGrpLst.Add(aGroup);

    if Assigned(ProgressCallBack) then
      ProgressCallBack(rsLoadingGroupList, aGroup.Title,
        i, aTxtFile.Count, False);

    Inc(i);
  end;
  // aGrpLst.EndUpdate;

  if Assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

procedure cEmutecaGroupManager.ImportFromStrLst(aTxtFile: TStrings);
var
  aImpGroupLst: cEmutecaGroupList;
  i, j, aComp: integer;
  aGroup, aImpGroup: cEmutecaGroup;
begin
  if not Assigned(aTxtFile) then
    Exit;

  aImpGroupLst := cEmutecaGroupList.Create(True);
  try
    // Loading import group list
    ActLoadStrLst(aImpGroupLst, aTxtFile);

    // Sorting by ID both lists
    aImpGroupLst.Sort(@EmutecaCompareGroupsByID);
    FullList.Sort(@EmutecaCompareGroupsByID);

    i := 0;
    if aImpGroupLst.Count > 0 then
      aImpGroup := aImpGroupLst[i]
    else
      aImpGroup := nil;

    j := 0;
    while (j < FullList.Count) and assigned(aImpGroup) do
    begin
      aGroup := FullList[j];

      aComp := aGroup.CompareID(aImpGroup.ID);

      // aGroup > aImpGroup -> Test next aImpGroup
      while (aComp > 0) and assigned(aImpGroup) do
      begin
        Inc(i);

        if i < aImpGroupLst.Count then
          aImpGroup := aImpGroupLst[i]
        else
          aImpGroup := nil;

        if assigned(aImpGroup) then
          aComp := aGroup.CompareID(aImpGroup.ID)
        else
          aComp := -1;
      end;

      // aGroup < aImpGroup -> Not found.
      // aGroup = aImpGroup -> Match.

      if (aComp = 0) and assigned(aImpGroup) then // Match
        aGroup.ImportFrom(aImpGroup);

      Inc(j);

      if assigned(ProgressCallBack) then
         ProgressCallBack(rsImportingGroupList, aGroup.Title, j, FullList.Count,
           False);
    end;

  finally
    aImpGroupLst.Free;
  end;
  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

procedure cEmutecaGroupManager.ExportToStrLst(aTxtFile: TStrings);
var
  ExpGroupList: cEmutecaGroupList;
  i, j, aComp: integer;
  aGroup, aExpGroup: cEmutecaGroup;
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
    i := 0; // FullList item.
    j := 0; // ExportList item.

    while i < FullList.Count do
    begin
      aGroup := FullList[i];

      if Assigned(aGroup) then
      begin
        // Searching a match (0) or ExportList > FullList (1)
        repeat
          if j < ExpGroupList.Count then
            aExpGroup := ExpGroupList[j]
          else
            aExpGroup := nil;

          if Assigned(aExpGroup) then
            aComp := aExpGroup.CompareID(aGroup.ID)
          else
            aComp := 1;

          if aComp < 0 then
            Inc(j);
        until aComp >= 0;

        // Exporting data
        if aComp = 0 then // Match
        begin
          aExpGroup.ImportFrom(aGroup); // Importing first match
        end
        else
        begin // Creating new group
          aExpGroup := cEmutecaGroup.Create(nil);
          aExpGroup.ID := aGroup.ID;
          aExpGroup.ImportFrom(aGroup);
          ExpGroupList.Add(aExpGroup);
        end;

        Inc(i); // Next FullList group

        // Cherry picking repeated FullList items
        aComp := 0;
        while (i < FullList.Count) and (aComp = 0) do
        begin
          aGroup := FullList[i];
          if Assigned(aGroup) then
          begin
            aComp := aExpGroup.CompareID(aGroup.ID);
            if (aComp = 0) then
            begin
              //if (aExpGroup.GetActualTitle = '') then
              //  aExpGroup.Title := aGroup.GetActualTitle;
              if (aExpGroup.GetActualSortTitle = '') then
                aExpGroup.SortTitle := aGroup.GetActualSortTitle;
              if (aExpGroup.Date = '') then
                aExpGroup.Date := aGroup.Date;
              if (aExpGroup.Developer = '') then
                aExpGroup.Developer := aGroup.Developer;
              // if (aExpGroup.GetActualMediaFilename = '') then
              //  aExpGroup.MediaFileName := aGroup.GetActualMediaFilename;
              Inc(i);
            end;
          end
          else
            Inc(i);
        end;
      end
      else // Not assigned aGroup?
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

      // Cleaning file, skip repeated IDs.
      aComp := 0;
      while (i < ExpGroupList.Count) and (aComp = 0) do
      begin
        aComp := aGroup.CompareID(ExpGroupList[i].ID);
        if (aComp = 0) then
          Inc(i);
      end;
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
  if not assigned(aTxtFile) then
    Exit;

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
{
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
