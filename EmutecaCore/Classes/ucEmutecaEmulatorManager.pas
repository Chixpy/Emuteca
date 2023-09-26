unit ucEmutecaEmulatorManager;
{< cEmutecaEmulatorManager class unit.

  This file is part of Emuteca Core.

  Copyright (C) 2006-2018 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8, IniFiles,
  // CHX abstracts
  uaCHXStorable,
  // Emuteca units
  uEmutecaRscStr,
  // Emuteca abstracts
  uaEmutecaCustomManager,
  // Emuteca classes
  ucEmutecaEmulator, ucEmutecaEmulatorList;

type
  { cEmutecaEmulatorManager }

  cEmutecaEmulatorManager = class(caEmutecaCustomManagerIni)
  private
    FEnabledList: cEmutecaEmulatorList;
    FFullList: cEmutecaEmulatorList;

  public
    property EnabledList: cEmutecaEmulatorList read FEnabledList;
    {< Enabled emulators. }

    procedure ClearData;
    //< Clears all data WITHOUT saving.
    function AddEmulator(aID: string): cEmutecaEmulator;
    //< Adds an emulator to the list.
    procedure UpdateEnabledList;
    //< Updates the Enabled list.

    // Inherited abstracts
    // -------------------
    procedure LoadFromIni(aIniFile: TMemIniFile); override;
    procedure SaveToIni(aIniFile: TMemIniFile); override;
    procedure ImportFromIni(aIniFile: TMemIniFile); override;
    {< Updates emulators' data from Ini. It don't add any emulator to the list.
    }
    procedure ExportToIni(aIniFile: TMemIniFile); override;
    {< Saves emulators' common data for importing.
    }

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property FullList: cEmutecaEmulatorList read FFullList;
    {< All emulators. }
  end;


implementation

{ cEmutecaEmulatorManager }

procedure cEmutecaEmulatorManager.ImportFromIni(aIniFile: TMemIniFile);
var
  i: longint;
  aEmulator: cEmutecaEmulator;
begin
  if not Assigned(aIniFile) then
    Exit;

  i := 0;
  while i < FullList.Count do
  begin
    aEmulator := FullList[i];

    if assigned(ProgressCallBack) then
      ProgressCallBack(rsImportingEmulatorList, aEmulator.Title,
        i, FullList.Count, False);

    aEmulator.ImportFromIni(aIniFile);

    Inc(i);
  end;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;


procedure cEmutecaEmulatorManager.ClearData;
begin
  EnabledList.Clear;
  FullList.Clear;
end;

function cEmutecaEmulatorManager.AddEmulator(aID: string): cEmutecaEmulator;
var
  TempEmulator: cEmutecaEmulator;
begin
  TempEmulator := cEmutecaEmulator.Create(nil);
  TempEmulator.ID := aID;
  TempEmulator.DefaultFileName := DefaultFileName;
  FullList.Add(TempEmulator);
  Result := TempEmulator;
end;

procedure cEmutecaEmulatorManager.UpdateEnabledList;
var
  i: integer;
  aEmu: cEmutecaEmulator;
begin
  EnabledList.Clear;

  i := 0;
  while i < FullList.Count do
  begin
    aEmu := FullList[i];
    if aEmu.Enabled then
      EnabledList.Add(aEmu);
    Inc(i);
  end;
end;

procedure cEmutecaEmulatorManager.LoadFromIni(aIniFile: TMemIniFile);
var
  TempList: TStringList;
  TempEmu: cEmutecaEmulator;
  i: longint;
begin
  if not Assigned(aIniFile) then
    Exit;

  if assigned(ProgressCallBack) then
    ProgressCallBack(rsLoadingEmulatorList, '', 0, 100, False);

  TempList := TStringList.Create;
  try
    aIniFile.ReadSections(TempList);
    TempList.Sort;

    i := 0;
    while i < TempList.Count do
    begin
      TempEmu := AddEmulator(TempList[i]);
      TempEmu.LoadFromIni(aIniFile);

      if assigned(ProgressCallBack) then
        ProgressCallBack(rsLoadingEmulatorList, TempEmu.Title, i,
          TempList.Count, False);

      Inc(i);
    end;
  finally
    FreeAndNil(TempList);
  end;

  UpdateEnabledList;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

procedure cEmutecaEmulatorManager.ExportToIni(aIniFile: TMemIniFile);
var
  i: longint;
  aEmulator: cEmutecaEmulator;
begin
  if not Assigned(aIniFile) then
    Exit;

  i := 0;
  while i < FullList.Count do
  begin
    aEmulator := FullList[i];

    if assigned(ProgressCallBack) then
      ProgressCallBack(rsSavingEmulatorList, aEmulator.Title,
        i, FullList.Count, False);

    aEmulator.ExportToIni(aIniFile);

    Inc(i);
  end;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

procedure cEmutecaEmulatorManager.SaveToIni(aIniFile: TMemIniFile);
var
  i: longint;
  aEmulator: cEmutecaEmulator;
begin
  if not Assigned(aIniFile) then
    Exit;

  i := 0;
  while i < FullList.Count do
  begin
    aEmulator := FullList[i];

    if assigned(ProgressCallBack) then
      ProgressCallBack(rsSavingEmulatorList, aEmulator.Title,
        i, FullList.Count, False);

    aEmulator.SaveToIni(aIniFile);

    Inc(i);
  end;

  if assigned(ProgressCallBack) then
    ProgressCallBack('', '', 0, 0, False);
end;

constructor cEmutecaEmulatorManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FFullList := cEmutecaEmulatorList.Create(True);
  FEnabledList := cEmutecaEmulatorList.Create(False);
end;

destructor cEmutecaEmulatorManager.Destroy;
begin
  EnabledList.Free;
  FullList.Free;

  inherited Destroy;
end;

initialization
  RegisterClass(cEmutecaEmulatorManager);

finalization
  UnRegisterClass(cEmutecaEmulatorManager);
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
