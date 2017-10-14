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

{ cGameManager unit. }
unit ucEmutecaGroupManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, IniFiles,
  LazUTF8, LConvEncoding, LResources,
  uEmutecaCommon,
  uaCHXStorable, uaEmutecaCustomManager,
  uaEmutecaCustomSystem, ucEmutecaGroupList;

type
  { cEmutecaGroupManager }

  cEmutecaGroupManager = class(caEmutecaCustomManager)
  private
    FSystem: caEmutecaCustomSystem;
    FVisibleList: cEmutecaGroupList;
    FFullList: cEmutecaGroupList;
    procedure SetSystem(AValue: caEmutecaCustomSystem);

  protected

    procedure ActLoadStrLst(aGrpLst: cEmutecaGroupList; aTxtFile: TStrings);

  public
    property System: caEmutecaCustomSystem read FSystem write SetSystem;

    property FullList: cEmutecaGroupList read FFullList;
    {< Actual list where the parents are stored. }
    property VisibleList: cEmutecaGroupList read FVisibleList;
    {< Parents with soft. Updated by System thread on loading. }

    function AddGroup(aID: string): integer;

    procedure LoadFromStrLst(aTxtFile: TStrings); override;
    procedure ImportFromIni(aIniFile: TIniFile); override;
    procedure SaveToStrLst(aTxtFile: TStrings; const ExportMode: boolean);
      override;
    procedure LoadFromIni(aIniFile: TIniFile); override;
    procedure ImportFromStrLst(aTxtFile: TStrings); override;
    procedure SaveToIni(aIniFile: TMemIniFile; const ExportMode: boolean);
      override;

    procedure ClearData;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
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
    aGroup.TXTString := aTxtFile[i];
    aGroup.CachedSystem := System;

    if Assigned(ProgressCallBack) then
      ProgressCallBack(rsLoadingGroupList, aGroup.Title, aGroup.ID,
        i, aTxtFile.Count);

    aGrpLst.Add(aGroup);
    Inc(i);
  end;
  // aGrpLst.EndUpdate;
end;

procedure cEmutecaGroupManager.ImportFromIni(aIniFile: TIniFile);
begin

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
          aGroup1.ID, j, FullList.Count);

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
    ProgressCallBack('', '', '', 0, 0);
end;

function cEmutecaGroupManager.AddGroup(aID: string): integer;
var
  TempGroup: cEmutecaGroup;
begin
  TempGroup := cEmutecaGroup.Create(nil);
  TempGroup.ID := aID;
  TempGroup.CachedSystem := System;
  Result := FullList.Add(TempGroup);
end;

procedure cEmutecaGroupManager.LoadFromIni(aIniFile: TIniFile);
begin

end;

procedure cEmutecaGroupManager.SaveToIni(aIniFile: TMemIniFile;
  const ExportMode: boolean);
begin

end;

procedure cEmutecaGroupManager.LoadFromStrLst(aTxtFile: TStrings);
begin
  if not Assigned(aTxtFile) then
    Exit;

  ActLoadStrLst(FullList, aTxtFile);
end;

procedure cEmutecaGroupManager.SaveToStrLst(aTxtFile: TStrings;
  const ExportMode: boolean);

  procedure SaveList(aTxtFile: TStrings);
  var
    i: integer;
    aGroup: cEmutecaGroup;
  begin
    aTxtFile.Clear;
    aTxtFile.BeginUpdate;
    try
      aTxtFile.Capacity := FullList.Count + 1; // Speed up?
      aTxtFile.Add(krsCSVGroupStatsHeader);

      i := 0;
      while i < FullList.Count do
      begin
        aGroup := cEmutecaGroup(FullList[i]);

        if Assigned(ProgressCallBack) then
          ProgressCallBack(rsSavingGroupList, aGroup.Title, aGroup.ID,
            i, FullList.Count);

        aTxtFile.Add(aGroup.TXTString);
        Inc(i);
      end;

    finally
      aTxtFile.EndUpdate;
    end;

    if assigned(ProgressCallBack) then
      ProgressCallBack('', '', '', 0, 0);
  end;


  procedure ExportList(aTxtFile: TStrings);
  var
    i: integer;
    aGroup: cEmutecaGroup;
  begin

    { TODO: Read items in file, merge and save. }

    aTxtFile.Clear;
    aTxtFile.BeginUpdate;
    try
      aTxtFile.Capacity := FullList.Count + 1; // Speed up?
      aTxtFile.Add(krsCSVGroupHeader);

      i := 0;
      while i < FullList.Count do
      begin
        aGroup := cEmutecaGroup(FullList[i]);

        if Assigned(ProgressCallBack) then
          ProgressCallBack(rsSavingGroupList, aGroup.Title, aGroup.ID,
            i, FullList.Count);

        aTxtFile.Add(aGroup.TXTExportString);
        Inc(i);
      end;

    finally
      aTxtFile.EndUpdate;
    end;

    if assigned(ProgressCallBack) then
      ProgressCallBack('', '', '', 0, 0);
  end;

begin
  if not Assigned(aTxtFile) then
    Exit;

  if ExportMode then
    ExportList(aTxtFile)
  else
    SaveList(aTxtFile);
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

end.
