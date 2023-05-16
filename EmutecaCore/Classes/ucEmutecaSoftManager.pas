unit ucEmutecaSoftManager;

{< cEmutecaSoftManager class unit.

  This file is part of Emuteca Core.

  Copyright (C) 2011-2020 Chixpy

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
  Classes, SysUtils, fgl, LazUTF8, LazFileUtils,
  // Emuteca Core Units
  uEmutecaConst, uEmutecaRscStr,
  // Emuteca Core abstracts
  uaEmutecaCustomManager, uaEmutecaCustomSystem, uaEmutecaCustomGroup,
  // Emuteca Core classes
  ucEmutecaSoftList;

type

  { cEmutecaSoftManager }

  cEmutecaSoftManager = class(caEmutecaCustomManagerTxt)
  private
    FSystem: caEmutecaCustomSystem;
    FFilterGroup: caEmutecaCustomGroup;
    FVisibleList: cEmutecaSoftList;
    FFullList: cEmutecaSoftList;
    procedure SetSystem(AValue: caEmutecaCustomSystem);
    procedure SetFilterGroup(AValue: caEmutecaCustomGroup);

  protected
    procedure ActLoadStrLst(aSoftLst: cEmutecaSoftList; aTxtFile: TStrings);

  public

    property System: caEmutecaCustomSystem read FSystem write SetSystem;
    property FilterGroup: caEmutecaCustomGroup
      read FFilterGroup write SetFilterGroup;

    property VisibleList: cEmutecaSoftList read FVisibleList;
    {< Filtered soft list }

    procedure ClearData;
    //< Clears all data WITHOUT saving.

    // Inherited abstracts
    // -------------------
    procedure LoadFromStrLst(aTxtFile: TStrings); override;
    procedure SaveToStrLst(aTxtFile: TStrings); override;
    procedure ImportFromStrLst(aTxtFile: TStrings); override;
    {< Updates soft data from file. It don't add any soft to the list.
    }
    procedure ExportToStrLst(aTxtFile: TStrings); override;
    {< Saves soft common data for importing.
    }

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property FullList: cEmutecaSoftList read FFullList;
    {< Actual list where the software is stored. }
  end;

implementation

uses uaEmutecaCustomSoft, ucEmutecaSoftware;

{ cEmutecaSoftManager }

constructor cEmutecaSoftManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FFullList := cEmutecaSoftList.Create(True);
  FVisibleList := cEmutecaSoftList.Create(False);
  // TODO: OnCompare FullList.OnCompare := ;
end;

destructor cEmutecaSoftManager.Destroy;
begin
  FVisibleList.Free;
  FFullList.Free;
  inherited Destroy;
end;

procedure cEmutecaSoftManager.ClearData;
begin
  VisibleList.Clear;
  FullList.Clear;
end;

procedure cEmutecaSoftManager.SetFilterGroup(AValue: caEmutecaCustomGroup);
var
  i: integer;
  aSoft: cEmutecaSoftware;
begin
  if FFilterGroup = AValue then
    Exit;
  FFilterGroup := AValue;

  VisibleList.Clear;

  // Filter by FilterGroup
  if Assigned(FilterGroup) then
  begin
    i := 0;
    while (i < FullList.Count) do
    begin
      aSoft := FullList[i];
      if aSoft.CachedGroup = FilterGroup then
        VisibleList.Add(aSoft);
      Inc(i);
    end;
  end
  else
  begin
    VisibleList.Assign(FullList);
  end;
end;

procedure cEmutecaSoftManager.ActLoadStrLst(aSoftLst: cEmutecaSoftList;
  aTxtFile: TStrings);
var
  aSoft: cEmutecaSoftware;
  i: integer;
begin
  // aSoftLst.BeginUpdate;
  aSoftLst.Capacity := aSoftLst.Count + aTxtFile.Count; // Speed Up?
  i := 1; // Skipping Header
  while i < aTxtFile.Count do
  begin
    aSoft := cEmutecaSoftware.Create(nil);
    aSoft.CommaText := aTxtFile[i];
    aSoft.CachedSystem := System;
    aSoftLst.Add(aSoft);

    if Assigned(ProgressCallBack) then
      ProgressCallBack(rsLoadingSoftList, aSoft.Title, i,
        aTxtFile.Count, False);

    Inc(i);
  end;
  // aSoftLst.EndUpdate;

  if Assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

procedure cEmutecaSoftManager.SetSystem(AValue: caEmutecaCustomSystem);
var
  aSoft: cEmutecaSoftware;
  i: integer;

begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;

  i := 0;
  while i < FullList.Count do
  begin
    aSoft := cEmutecaSoftware(FullList[i]);
    aSoft.CachedSystem := System;
  end;
end;

procedure cEmutecaSoftManager.ImportFromStrLst(aTxtFile: TStrings);
var
  ImpSoftList: cEmutecaSoftList;
  i, j, aComp: integer;
  aSoft, aImpSoft: cEmutecaSoftware;
begin
  if not Assigned(aTxtFile) then
    Exit;

  ImpSoftList := cEmutecaSoftList.Create(True);
  try
    // Loading import soft list
    ActLoadStrLst(ImpSoftList, aTxtFile);

    // Sorting by ID both lists
    ImpSoftList.Sort(@EmutecaCompareSoftByID);
    FullList.Sort(@EmutecaCompareSoftByID);

    // Dragons... Backwards
    i := ImpSoftList.Count - 1;
    if i >= 0 then
      aImpSoft := ImpSoftList[i]
    else
      aImpSoft := nil;
    j := FullList.Count;
    while j > 0 do
    begin
      Dec(j);
      aSoft := FullList[j];

      if assigned(ProgressCallBack) then
        ProgressCallBack(rsImportingSoftList, aSoft.Title,
          FullList.Count - j, FullList.Count, False);

      if assigned(aImpSoft) then
        aComp := aSoft.CompareID(aImpSoft.ID)
      else
        aComp := 1; // aSoft.CompareSoftKey('');

      // aSoft < aImpSoft -> Try Previous Soft2
      while aComp < 0 do
      begin
        Dec(i);
        if i >= 0 then
          aImpSoft := ImpSoftList[i]
        else
          aImpSoft := nil;

        if assigned(aImpSoft) then
          aComp := aSoft.CompareID(aImpSoft.ID)
        else
          aComp := 1; // aSoft.CompareSoftKey('');
      end;
      // aSoft > aImpSoft -> Not found.
      // aSoft = aImpSoft -> Match.
      if aComp = 0 then
        aSoft.ImportFrom(aImpSoft);
    end;

  finally
    ImpSoftList.Free;
  end;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

procedure cEmutecaSoftManager.ExportToStrLst(aTxtFile: TStrings);
var
  ExpSoftList: cEmutecaSoftList;
  i, j, aComp: integer;
  aSoft, aExpSoft: cEmutecaSoftware;
begin
  if not Assigned(aTxtFile) then
    Exit;

  ExpSoftList := cEmutecaSoftList.Create(True);
  try
    // Loading export soft in file
    ActLoadStrLst(ExpSoftList, aTxtFile);

    // Sorting by ID both lists
    FullList.Sort(@EmutecaCompareSoftByID);
    ExpSoftList.Sort(@EmutecaCompareSoftByID);

    // Dragons...
    i := 0; // FullList item.
    j := 0; // ExportList item.

    while i < FullList.Count do
    begin
      aSoft := FullList[i];

      if Assigned(aSoft) then
      begin
        // Searching a match (0) or ExportList > FullList (1)
        repeat
          if j < ExpSoftList.Count then
            aExpSoft := ExpSoftList[j]
          else
            aExpSoft := nil;

          if Assigned(aExpSoft) then
            aComp := aExpSoft.CompareID(aSoft.ID)
          else
            aComp := 1;

          if aComp < 0 then
            Inc(j);
        until aComp >= 0;

        // Exporting data
        if aComp = 0 then // Match
        begin
          // Importing first match
          aExpSoft.ImportFrom(aSoft);
          // aExpSoft.CachedSystem := aSoft.CachedSystem;
        end
        else
        begin // Creating new soft
          aExpSoft := cEmutecaSoftware.Create(nil);
          aExpSoft.ID := aSoft.ID;
          aExpSoft.ImportFrom(aSoft);
          // aExpSoft.CachedSystem := aSoft.CachedSystem;
          ExpSoftList.Add(aExpSoft);
        end;

        Inc(i); // Next FullList soft

        // Cherry picking repeated FullList items
        aComp := 0;
        while (i < FullList.Count) and (aComp = 0) do
        begin
          aSoft := FullList[i];
          if Assigned(aSoft) then
          begin
            aComp := aExpSoft.CompareID(aSoft.ID);
            if (aComp = 0) then
            begin
              //if (aExpSoft.GetActualTitle = '') then
              //  aExpSoft.Title := aSoft.GetActualTitle;
              if (aExpSoft.GetActualSortTitle = '') then
                aExpSoft.SortTitle := aSoft.GetActualSortTitle;
              if (aExpSoft.Version = '') then
                aExpSoft.Version := aSoft.Version;
              if (aExpSoft.Date = '') then
                aExpSoft.Date := aSoft.Date;
              if (aExpSoft.Publisher = '') then
                aExpSoft.Publisher := aSoft.Publisher;
              if (aExpSoft.Zone = '') then
                aExpSoft.Zone := aSoft.Zone;
              if (aExpSoft.DumpInfo = '') then
                aExpSoft.DumpInfo := aSoft.DumpInfo;
              if (aExpSoft.Fixed = '') then
                aExpSoft.Fixed := aSoft.Fixed;
              if (aExpSoft.Trainer = '') then
                aExpSoft.Trainer := aSoft.Trainer;
              if (aExpSoft.Translation = '') then
                aExpSoft.Translation := aSoft.Translation;
              if (aExpSoft.Pirate = '') then
                aExpSoft.Pirate := aSoft.Pirate;
              if (aExpSoft.Cracked = '') then
                aExpSoft.Cracked := aSoft.Cracked;
              if (aExpSoft.Modified = '') then
                aExpSoft.Modified := aSoft.Modified;
              if (aExpSoft.Hack = '') then
                aExpSoft.Hack := aSoft.Hack;
              if (aExpSoft.ExtraParameters.Count = 0) then
                aExpSoft.ExtraParameters.AddStrings(aSoft.ExtraParameters);

              Inc(i);
            end;
          end
          else
            Inc(i);
        end;
      end
      else // Not assigned aSoft?
        Inc(i);
    end;

    // Actually saving to file
    ExpSoftList.Sort(@EmutecaCompareSoftByID);
    aTxtFile.Clear; // Clearing to export merged list
    aTxtFile.BeginUpdate;

    aTxtFile.Capacity := ExpSoftList.Count + 1; // Speed up?
    aTxtFile.Add(krsCSVSoftHeader);

    i := 0;
    while i < ExpSoftList.Count do
    begin
      aSoft := ExpSoftList[i];
      if Assigned(ProgressCallBack) then
        ProgressCallBack(rsSavingSoftList, aSoft.Title, i,
          ExpSoftList.Count, False);

      aTxtFile.Add(aSoft.ExportCommaText);

      Inc(i);

      // Cleaning file, skip repeated IDs and merging fields if empty.
      aComp := 0;
      while (i < ExpSoftList.Count) and (aComp = 0) do
      begin
        aComp := aSoft.CompareID(ExpSoftList[i].ID);
        if (aComp = 0) then
          Inc(i);
      end;
    end;

  finally
    aTxtFile.EndUpdate;
    ExpSoftList.Free;
  end;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

procedure cEmutecaSoftManager.LoadFromStrLst(aTxtFile: TStrings);
begin
  if not Assigned(aTxtFile) then
    Exit;

  ActLoadStrLst(FullList, aTxtFile);

  VisibleList.Assign(FullList);
end;

procedure cEmutecaSoftManager.SaveToStrLst(aTxtFile: TStrings);
var
  i: integer;
  aSoft: cEmutecaSoftware;
begin
  if not Assigned(aTxtFile) then
    Exit;

  aTxtFile.BeginUpdate;
  try
    aTxtFile.Capacity := FullList.Count + 1; // Speed up?
    aTxtFile.Add(krsCSVSoftStatsHeader);

    i := 0;
    while i < FullList.Count do
    begin
      aSoft := FullList[i];

      if Assigned(ProgressCallBack) then
        ProgressCallBack(rsSavingSoftList, aSoft.Title, i,
          FullList.Count, False);


      aTxtFile.Add(aSoft.CommaText);
      Inc(i);
    end;
  finally
    aTxtFile.EndUpdate;
  end;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

initialization
  RegisterClass(cEmutecaSoftManager);

finalization
  UnRegisterClass(cEmutecaSoftManager);
end.
