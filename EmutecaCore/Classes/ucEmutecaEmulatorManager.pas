{ This file is part of Emuteca

  Copyright (C) 2006-2016 Chixpy

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

{ cEmulatorManager unit. }
unit ucEmutecaEmulatorManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8, IniFiles,
  ucEmutecaEmulator, uEmutecaCommon, uaEmutecaManager;

resourcestring
  rsLoadingEmulatorList = 'Loading emulator list...';
  rsSavingEmulatorList = 'Saving emulator list...';

type
  { TODO : Create a cEmutecaManager generic (for systems and emulators) }

  { cEmutecaEmulatorManager }

  cEmutecaEmulatorManager = class(caEmutecaManagerIni)
  private
    FFullList: cEmutecaEmulatorMap;

  protected


  public
    property FullList: cEmutecaEmulatorMap read FFullList;

    procedure LoadFromFileIni(IniFile: TCustomIniFile); override;
    procedure SaveToFileIni(IniFile: TCustomIniFile;
      const ExportMode: boolean); override;

    function Add(aId: string): cEmutecaEmulator;
    {< Creates an emulator with aId key, if already exists returns it.

       @Result cEmutecaEmulator created or found.
    }
    function ItemById(aId: string): cEmutecaEmulator;
    {< Return the emulator with have aId key.

       @Result cEmutecaEmulator found.
    }
    function Delete(aId: string): integer;
    {< Deletes a emulator by Id.

       @Result Index of deleted item
    }

    procedure AssingAllTo(aList: TStrings);
    procedure AssingEnabledTo(aList: TStrings);

    function RunEmulator(const EmulatorID, GameFile: string): longword;
    {< Runs software with an emulator (by ID).

       @Result Exit code.
    }

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;


implementation

{ cEmutecaEmulatorManager }

constructor cEmutecaEmulatorManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FFullList := cEmutecaEmulatorMap.Create(True);
  FullList.OnKeyCompare := @AnsiCompareText;
  // TODO: When it works...: FullList.OnKeyCompare := @UTF8CompareText;
end;

destructor cEmutecaEmulatorManager.Destroy;
begin
  FreeAndNil(FFullList);
  inherited Destroy;
end;

function cEmutecaEmulatorManager.RunEmulator(
  const EmulatorID, GameFile: string): longword;
var
  Emu: cEmutecaEmulator;
begin
  Result := 256;
  //Emu := Emulator(EmulatorID);
  //if Emu = nil then
  //  Exit;
  //Result := Emu.Execute(GameFile);

  //// Saving emulator data...
  //if Result = 0 then
  //  Emu.SaveToFile(DataFile, False);
end;

procedure cEmutecaEmulatorManager.AssingAllTo(aList: TStrings);
var
  i: longint;
begin
  if not assigned(aList) then
    aList := TStringList.Create;

  aList.BeginUpdate;
  i := 0;
  while i < FullList.Count do
  begin
    aList.AddObject(FullList.Data[i].EmulatorName, FullList.Data[i]);
    Inc(i);
  end;
  aList.EndUpdate;
end;


procedure cEmutecaEmulatorManager.LoadFromFileIni(IniFile: TCustomIniFile);
var
  TempList: TStringList;
  TempEmu: cEmutecaEmulator;
  i: longint;
begin
  if not Assigned(IniFile) then
    Exit;

  TempList := TStringList.Create;
  try
    IniFile.ReadSections(TempList);
    TempList.Sort;

    i := 0;
    while i < TempList.Count do
    begin
      TempEmu := cEmutecaEmulator.Create(nil);
      TempEmu.ID := TempList[i];
      TempEmu.LoadFromFileIni(IniFile);
      FullList.AddOrSetData(TempEmu.ID, TempEmu);
      Inc(i);

      if ProgressCallBack <> nil then
        ProgressCallBack(rsLoadingEmulatorList, TempEmu.ID,
          TempEmu.EmulatorName, i, TempList.Count);
    end;
  finally
    FreeAndNil(TempList);
  end;
end;

procedure cEmutecaEmulatorManager.SaveToFileIni(IniFile: TCustomIniFile;
  const ExportMode: boolean);
var
  i: longint;
begin
  if not Assigned(IniFile) then
    Exit;

  // if not ExportMode then
  //   IniFile.Clear;  <-- TMemIniFile

  i := 0;
  while i < FullList.Count do
  begin
    FullList.Data[i].SaveToFileIni(IniFile, ExportMode);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsSavingEmulatorList, FullList.Data[i].ID,
        FullList.Data[i].EmulatorName, i + 1, FullList.Count);
    Inc(i);
  end;
end;

function cEmutecaEmulatorManager.Add(aId: string): cEmutecaEmulator;
begin
  Result := ItemById(aId);

  // If already exists, then return it
  if assigned(Result) then
    Exit;

  // Creating new item
  Result := cEmutecaEmulator.Create(Self);
  Result.ID := aId;
  Result.EmulatorName := aId;
  FullList.Add(Result.ID, Result);
end;

function cEmutecaEmulatorManager.ItemById(aId: string): cEmutecaEmulator;
var
  i: integer;
begin
  // FullList.TryGetData(aId, Result); Maybe do this???

  Result := nil;
  i := FullList.IndexOf(aId);

  if i >= 0 then
    Result := FullList.Data[i];
end;

function cEmutecaEmulatorManager.Delete(aId: string): integer;
begin
  Result := FullList.Remove(aId);
end;

procedure cEmutecaEmulatorManager.AssingEnabledTo(aList: TStrings);
var
  i: longint;
begin
  if not assigned(aList) then
    aList := TStringList.Create;

  aList.BeginUpdate;
  i := 0;
  while i < FullList.Count do
  begin
    if FullList.Data[i].Enabled then
      aList.AddObject(FullList.Data[i].EmulatorName, FullList.Data[i]);
    Inc(i);
  end;
  aList.EndUpdate;
end;


end.
