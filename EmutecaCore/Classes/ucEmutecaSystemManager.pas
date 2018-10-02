unit ucEmutecaSystemManager;
{< cEmutecaSystemManager class unit.

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
  Classes, SysUtils, fgl, LazFileUtils, LazUTF8, IniFiles,
  // CHX units
  uCHXStrUtils,
  // CHX abstracts
  uaCHXStorable,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr,
  // Emuteca Core abstracts
  uaEmutecaCustomManager,
  // Emuteca Core classes
  ucEmutecaSystem, ucEmutecaSystemList, ucEmutecaEmulatorList;

type
  { cEmutecaSystemManager }

  cEmutecaSystemManager = class(caEmutecaCustomManagerIni)
  private
    FFullList: cEmutecaSystemList;
    FEnabledList: cEmutecaSystemList;
    FSysDataFolder: string;
    FTempFolder: string;
    procedure SetSysDataFolder(AValue: string);
    procedure SetTempFolder(AValue: string);

  protected
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack); override;

  public
    property EnabledList: cEmutecaSystemList read FEnabledList;

    property TempFolder: string read FTempFolder write SetTempFolder;
    property SysDataFolder: string read FSysDataFolder write SetSysDataFolder;

    procedure ClearData;
    //< Clears all data WITHOUT saving.
    function AddSystem(aID: string): cEmutecaSystem;
    //< Adds a system to the list.
    procedure UpdateEnabledList;
    procedure UpdateSystemsEmulators(aEmuList: cEmutecaEmulatorList);

    // Inherited abstracts
    // -------------------
    procedure LoadFromIni(aIniFile: TMemIniFile); override;
    procedure SaveToIni(aIniFile: TMemIniFile); override;
    procedure ImportFromIni(aIniFile: TMemIniFile); override;
    {< Updates systems' data from Ini. It don't add any system to the list.
    }
    procedure ExportToIni(aIniFile: TMemIniFile); override;
    {< Saves systems' common data for importing.
    }

    // Soft and group data
    procedure LoadSystemData(aSystem: cEmutecaSystem);
    {< Loads soft and groups data of a system. }
    procedure SaveSystemData(aSystem: cEmutecaSystem; ClearFile: Boolean);
    {< Save soft and groups data of a system. }
    procedure LoadAllEnabledSystemsData;
    {< Loads soft and groups data of all systems. }
    procedure SaveAllEnabledSystemsData;
    {< Saves soft and groups data of all systems. }

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property FullList: cEmutecaSystemList read FFullList;

  end;

implementation

{ cEmutecaSystemManager }

procedure cEmutecaSystemManager.SetSysDataFolder(AValue: string);
begin
  FSysDataFolder := SetAsFolder(AValue);
end;

procedure cEmutecaSystemManager.SetTempFolder(AValue: string);
var
  i: integer;
begin
  FTempFolder := SetAsFolder(AValue);

  // Propaganting Temp folder, (for decompresing media, etc.)
  i := 0;
  while i < FullList.Count do
  begin
    FullList[i].TempFolder := TempFolder;
    Inc(i);
  end;
end;

procedure cEmutecaSystemManager.SetProgressCallBack(AValue: TEmutecaProgressCallBack);
var
  i: integer;
begin
  inherited SetProgressCallBack(AValue);

  // Propagating to systems
  i := 0;
  while i < FullList.Count do
  begin
    FullList[i].ProgressCallBack := ProgressCallBack;
    Inc(i);
  end;
end;

procedure cEmutecaSystemManager.ClearData;
begin
  EnabledList.Clear;
  FullList.Clear;
end;

function cEmutecaSystemManager.AddSystem(aID: string): cEmutecaSystem;
var
  TempSystem: cEmutecaSystem;
begin
  TempSystem := cEmutecaSystem.Create(nil);
  TempSystem.ID := aID;
  TempSystem.Title := aID;
  TempSystem.ListFileName := aID;

  TempSystem.DefaultFileName := DefaultFileName;
  TempSystem.ProgressCallBack := ProgressCallBack;

  FullList.Add(TempSystem);
  Result := TempSystem;
end;

procedure cEmutecaSystemManager.UpdateEnabledList;
var
  i: integer;
  aSys: cEmutecaSystem;
begin
  EnabledList.Clear;
  i := 0;
  while i < FullList.Count do
  begin
    aSys := FullList[i];
    if aSys.Enabled then
      EnabledList.Add(aSys);
    Inc(i);
  end;
end;

procedure cEmutecaSystemManager.UpdateSystemsEmulators(aEmuList: cEmutecaEmulatorList);
var
  i: integer;
begin
  i := 0;
  while i < FullList.Count do
  begin
    FullList[i].LoadEmulatorsFrom(aEmuList);
    Inc(i);
  end;
end;

procedure cEmutecaSystemManager.LoadFromIni(aIniFile: TMemIniFile);
var
  TempList: TStringList;
  TempSys: cEmutecaSystem;
  i: longint;
begin
  if not Assigned(aIniFile) then
    Exit;

  // PB created before, because is showed after loading a sys...
  if assigned(ProgressCallBack) then
    ProgressCallBack(rsLoadingSystemList, '', 0, 100, False);

  TempList := TStringList.Create;
  try
    aIniFile.ReadSections(TempList);
    TempList.Sort;

    i := 0;
    while i < TempList.Count do
    begin
      TempSys := AddSystem(TempList[i]);
      TempSys.LoadFromIni(aIniFile);

      if assigned(ProgressCallBack) then
        ProgressCallBack(rsLoadingSystemList, TempSys.Title,
          i, TempList.Count, False);

      Inc(i);
    end;
  finally
    FreeAndNil(TempList);
  end;

  UpdateEnabledList;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

procedure cEmutecaSystemManager.SaveToIni(aIniFile: TMemIniFile);
var
  i: longint;
  aSystem: cEmutecaSystem;
begin
  if not Assigned(aIniFile) then
    Exit;

  i := 0;
  while i < FullList.Count do
  begin
    aSystem := FullList[i];

    if assigned(ProgressCallBack) then
      ProgressCallBack(rsSavingSystemList, aSystem.Title, i, FullList.Count, False);

      aSystem.SaveToIni(aIniFile);

    Inc(i);
  end;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

procedure cEmutecaSystemManager.ImportFromIni(aIniFile: TMemIniFile);
var
  i: longint;
  aSystem: cEmutecaSystem;
begin
  if not Assigned(aIniFile) then
    Exit;

  i := 0;
  while i < FullList.Count do
  begin
    aSystem := FullList[i];

    if assigned(ProgressCallBack) then
      ProgressCallBack(rsImportingSystemList, aSystem.Title,
        i, FullList.Count, False);

    aSystem.ImportFromIni(aIniFile);

    Inc(i);
  end;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

procedure cEmutecaSystemManager.ExportToIni(aIniFile: TMemIniFile);
var
  i: longint;
  aSystem: cEmutecaSystem;
begin
  if not Assigned(aIniFile) then
    Exit;

  i := 0;
  while i < FullList.Count do
  begin
    aSystem := FullList[i];

    if assigned(ProgressCallBack) then
      ProgressCallBack(rsExportingSystemList, aSystem.Title, i,
        FullList.Count, False);

      aSystem.ExportToIni(aIniFile);

    Inc(i);
  end;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

procedure cEmutecaSystemManager.LoadSystemData(aSystem: cEmutecaSystem);
begin
  if not Assigned(aSystem) then
    Exit;

  aSystem.LoadSoftGroupLists(SysDataFolder + aSystem.ListFileName);
end;

procedure cEmutecaSystemManager.SaveSystemData(aSystem: cEmutecaSystem;
  ClearFile: Boolean);
begin
  if not Assigned(aSystem) then
    Exit;

  aSystem.SaveSoftGroupLists(SysDataFolder + aSystem.ListFileName, ClearFile);
end;

procedure cEmutecaSystemManager.LoadAllEnabledSystemsData;
var
  i: longint;
  aSystem: cEmutecaSystem;
  aPBCB: TEmutecaProgressCallBack;
begin
  i := 0;
  while i < EnabledList.Count do
  begin
    aSystem := EnabledList[i];

    if assigned(aSystem) then
    begin
      // Disabling system progress callback
      aPBCB := aSystem.ProgressCallBack;
      aSystem.ProgressCallBack := nil;

      if assigned(ProgressCallBack) then
        ProgressCallBack(rsLoadingGroupList, aSystem.Title, i, EnabledList.Count, False);

      LoadSystemData(aSystem);

      // Restoring system progress callback
      aSystem.ProgressCallBack := aPBCB;
    end;

    Inc(i);
  end;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

procedure cEmutecaSystemManager.SaveAllEnabledSystemsData;
var
  i: longint;
  aSystem: cEmutecaSystem;
  aPBCB: TEmutecaProgressCallBack;
begin
  i := 0;
  while i < EnabledList.Count do
  begin
    aSystem := EnabledList[i];

    if assigned(aSystem) then
    begin
      // Disabling system progress callback
      aPBCB := aSystem.ProgressCallBack;
      aSystem.ProgressCallBack := nil;

      if assigned(ProgressCallBack) then
        ProgressCallBack(rsSavingGroupList, aSystem.Title, i, EnabledList.Count, False);

      SaveSystemData(aSystem, True);

      // Restoring system progress callback
      aSystem.ProgressCallBack := aPBCB;
    end;

    Inc(i);
  end;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

constructor cEmutecaSystemManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FFullList := cEmutecaSystemList.Create(True);
  FEnabledList := cEmutecaSystemList.Create(False);
end;

destructor cEmutecaSystemManager.Destroy;
begin
  FreeAndNil(FEnabledList);
  FreeAndNil(FFullList);
  inherited Destroy;
end;

initialization
  RegisterClass(cEmutecaSystemManager);

finalization
  UnRegisterClass(cEmutecaSystemManager);
end.
