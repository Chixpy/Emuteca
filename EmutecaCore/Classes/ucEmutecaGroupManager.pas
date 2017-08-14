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

  public
    property System: caEmutecaCustomSystem read FSystem write SetSystem;

    property FullList: cEmutecaGroupList read FFullList;
    {< Actual list where the parents are stored. }
    property VisibleList: cEmutecaGroupList read FVisibleList;
    {< Parents with soft. Updated by System thread on loading. }

    function AddGroup(aID: string): integer;

    procedure LoadFromStrLst(TxtFile: TStrings); override;
    procedure ImportFromIni(aIniFile: TMemIniFile); override;
    procedure SaveToStrLst(TxtFile: TStrings; const ExportMode: boolean);
      override;
    procedure LoadFromIni(aIniFile: TMemIniFile); override;
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

procedure cEmutecaGroupManager.ImportFromIni(aIniFile: TMemIniFile);
begin

end;

procedure cEmutecaGroupManager.ImportFromStrLst(aTxtFile: TStrings);
begin


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

procedure cEmutecaGroupManager.LoadFromIni(aIniFile: TMemIniFile);
begin

end;

procedure cEmutecaGroupManager.SaveToIni(aIniFile: TMemIniFile;
  const ExportMode: boolean);
begin

end;

procedure cEmutecaGroupManager.LoadFromStrLst(TxtFile: TStrings);
var
  i: integer;
  TempGroup: cEmutecaGroup;
begin
  if not Assigned(TxtFile) then
    Exit;

  //FullList.BeginUpdate;
  FullList.Capacity := FullList.Count + TxtFile.Count; // Speed Up?
  i := 1; // Skipping Header
  while i < TxtFile.Count do
  begin
    TempGroup := cEmutecaGroup.Create(nil);
    TempGroup.TXTString := TxtFile[i];
    TempGroup.CachedSystem := System;

    if Assigned(ProgressCallBack) then
      ProgressCallBack(rsLoadingGroupList, TempGroup.Title, TempGroup.ID,
        i, TxtFile.Count);

    FullList.Add(TempGroup);
    Inc(i);
  end;
  //FullList.EndUpdate;
  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', '', 0, 0);
end;

procedure cEmutecaGroupManager.SaveToStrLst(TxtFile: TStrings;
  const ExportMode: boolean);
var
  i: integer;
  aGroup: cEmutecaGroup;
begin
  if not Assigned(TxtFile) then
    Exit;

  { TODO : cEmutecaGroupManager.SaveToStrLst Export mode }
  TxtFile.Clear;
  TxtFile.BeginUpdate;
  try
    TxtFile.Capacity := FullList.Count + 1; // Speed up?

    if ExportMode then
      TxtFile.Add(krsCSVGroupHeader)
    else
      TxtFile.Add(krsCSVGroupStatsHeader);

    i := 0;
    while i < FullList.Count do
    begin
      aGroup := cEmutecaGroup(FullList[i]);

      if Assigned(ProgressCallBack) then
        ProgressCallBack(rsSavingGroupList, aGroup.Title, aGroup.ID,
          i, FullList.Count);

      TxtFile.Add(aGroup.TXTString);
      Inc(i);
    end;

  finally
    TxtFile.EndUpdate;
  end;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', '', 0, 0);
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
