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

{ cEmulatorManager unit. }
unit ucEmutecaEmulatorManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8, IniFiles,
  uaCHXStorable,
  uEmutecaCommon,
  ucEmutecaConfig, ucEmutecaEmulator;

resourcestring
  rsLoadingEmulatorList = 'Loading emulator list...';
  rsSavingEmulatorList = 'Saving emulator list...';

type
  { TODO : Create a cEmutecaManager generic (for systems and emulators) }

  { cEmutecaEmulatorManager }

  cEmutecaEmulatorManager = class(caCHXStorableIni)
  private
    FConfig: cEmutecaConfig;
    FFullList: cEmutecaEmulatorList;
    FProgressCallBack: TEmutecaProgressCallBack;
    procedure SetConfig(AValue: cEmutecaConfig);
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack);

  protected


  public
    property ProgressCallBack: TEmutecaProgressCallBack read FProgressCallBack write SetProgressCallBack;
    //< CallBack function to show the progress in actions.

    property Config: cEmutecaConfig read FConfig write SetConfig;

    procedure ClearData;
    //< Clears all data WITHOUT saving.
    procedure ReloadData;
    //< Reload last data file WITHOUT saving changes.

    procedure LoadFromIni(aIniFile: TCustomIniFile); override;
    procedure SaveToIni(aIniFile: TCustomIniFile;
      const ExportMode: boolean); override;

    function ItemById(aId: string): cEmutecaEmulator;
    {< Returns the emulator with aId key.

       @Result cEmutecaEmulator found or nil.
    }

    procedure AssingAllTo(aList: TStrings);
    procedure AssingEnabledTo(aList: TStrings);

    function RunEmulator(const EmulatorID, GameFile: string): longword;
    {< Runs software with an emulator (by ID).

       @Result Exit code.
    }

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property FullList: cEmutecaEmulatorList read FFullList;

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
//var
 // Emu: cEmutecaEmulator;
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

procedure cEmutecaEmulatorManager.SetConfig(AValue: cEmutecaConfig);
begin
  if FConfig = AValue then Exit;
  FConfig := AValue;
end;

procedure cEmutecaEmulatorManager.SetProgressCallBack(
  AValue: TEmutecaProgressCallBack);
begin
  if FProgressCallBack = AValue then Exit;
  FProgressCallBack := AValue;
end;

procedure cEmutecaEmulatorManager.ClearData;
begin
  FullList.Clear;
end;

procedure cEmutecaEmulatorManager.ReloadData;
begin
  ClearData;

  LoadFromFileIni('');
end;

procedure cEmutecaEmulatorManager.LoadFromIni(aIniFile: TCustomIniFile);
var
  TempList: TStringList;
  TempEmu: cEmutecaEmulator;
  i: longint;
begin
  if not Assigned(aIniFile) then
    Exit;

  TempList := TStringList.Create;
  try
    aIniFile.ReadSections(TempList);
    TempList.Sort;

    i := 0;
    while i < TempList.Count do
    begin
      TempEmu := cEmutecaEmulator.Create(nil);
      TempEmu.ID := TempList[i];
      TempEmu.LoadFromIni(aIniFile);
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

procedure cEmutecaEmulatorManager.SaveToIni(aIniFile: TCustomIniFile;
  const ExportMode: boolean);
var
  i: longint;
  aEmulator: cEmutecaEmulator;
begin
  if not Assigned(aIniFile) then
    Exit;

  // if not ExportMode then
  //   aIniFile.Clear;  <-- TMemIniFile

  i := 0;
  while i < FullList.Count do
  begin
    aEmulator := cEmutecaEmulator(FullList[i]);
    aEmulator.SaveToIni(aIniFile, ExportMode);
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
