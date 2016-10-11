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
  ucEmutecaEmulator, uaEmutecaManager;

resourcestring
  rsLoadingEmulatorList = 'Loading emulator list...';
  rsSavingEmulatorList = 'Saving emulator list...';

type
  { TODO : Create a cEmutecaManager generic (for systems and emulators) }

  { cEmutecaEmulatorManager }

  cEmutecaEmulatorManager = class(caEmutecaManagerIni)
  private
    FFullList: cEmutecaEmulatorList;

  protected


  public
    property FullList: cEmutecaEmulatorList read FFullList;

    procedure LoadFromFileIni(IniFile: TCustomIniFile); override;
    procedure SaveToFileIni(IniFile: TCustomIniFile;
      const ExportMode: boolean); override;

    function ItemById(aId: string): cEmutecaEmulator;
    {< Returns the emulator with aId key.

       @Result cEmutecaEmulator found or nil.
    }

    procedure AssingAllTo(aList: TStrings); override;
    procedure AssingEnabledTo(aList: TStrings); override;

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
  FFullList := cEmutecaEmulatorList.Create(True);

  // TODO: OnCompare FullList.OnCompare := ;
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
  aEmulator: cEmutecaEmulator;
begin
  if not assigned(aList) then
    aList := TStringList.Create;

  aList.BeginUpdate;
  i := 0;
  while i < FullList.Count do
  begin
    aEmulator := cEmutecaEmulator(FullList[i]);
    aList.AddObject(aEmulator.EmulatorName, aEmulator);
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
      FullList.Add(TempEmu);
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
  aEmulator: cEmutecaEmulator;
begin
  if not Assigned(IniFile) then
    Exit;

  // if not ExportMode then
  //   IniFile.Clear;  <-- TMemIniFile

  i := 0;
  while i < FullList.Count do
  begin
    aEmulator := cEmutecaEmulator(FullList[i]);
    aEmulator.SaveToFileIni(IniFile, ExportMode);
    Inc(i);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsSavingEmulatorList, aEmulator.ID,
        aEmulator.EmulatorName, i, FullList.Count);
  end;
end;

function cEmutecaEmulatorManager.ItemById(aId: string): cEmutecaEmulator;
var
  i: integer;
  aEmulator: cEmutecaEmulator;
begin
  Result := nil;

  i := 0;
  while (Result = nil) and (i < FullList.Count) do
  begin
    aEmulator := cEmutecaEmulator(FullList[i]);
    if UTF8CompareText(aEmulator.ID, aId) = 0 then
      Result := aEmulator;
    inc(i);
  end;
end;

procedure cEmutecaEmulatorManager.AssingEnabledTo(aList: TStrings);
var
  i: longint;
  aEmulator: cEmutecaEmulator;
begin
  { TODO : Change to visible list... and remove procedure }
  if not assigned(aList) then
    aList := TStringList.Create;

  aList.BeginUpdate;
  i := 0;
  while i < FullList.Count do
  begin
    aEmulator := cEmutecaEmulator(FullList[i]);
    if aEmulator.Enabled then
      aList.AddObject(aEmulator.EmulatorName, aEmulator);
    Inc(i);
  end;
  aList.EndUpdate;
end;


end.
