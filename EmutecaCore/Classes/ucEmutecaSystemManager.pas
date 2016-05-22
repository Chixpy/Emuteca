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

{ cEmutecaSystemManager unit. }
unit ucEmutecaSystemManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8, IniFiles,
  uEmutecaCommon, uaEmutecaManager, ucEmutecaSystem;

type
  { cEmutecaSystemManager }

  cEmutecaSystemManager = class(caEmutecaManagerIni)
  private
    FFullList: cEmutecaSystemMap;

  protected

  public
    property FullList: cEmutecaSystemMap read FFullList;

    procedure LoadFromFileIni(IniFile: TCustomIniFile); override;
    procedure SaveToFileIni(IniFile: TCustomIniFile;
      const ExportMode: boolean); override;

    function Add(aId: string): cEmutecaSystem;
    {< Creates a system with aId key, if already exists returns it.

       @Result cEmutecaSystem created or found.
    }
    function ItemById(aId: string): cEmutecaSystem;
    {< Return the system with have aId key.

       @Result cEmutecaSystem found.
    }
    function Delete(aId: string): integer;
    {< Deletes a system by Id.

       @Result Index of deleted item
    }

    procedure AssingAll(aList: TStrings);
    procedure AssingEnabledTo(aList: TStrings);

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published

  end;

implementation

{ cEmutecaSystemManager }


procedure cEmutecaSystemManager.LoadFromFileIni(IniFile: TCustomIniFile);
var
  TempList: TStringList;
  TempSys: cEmutecaSystem;
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
      TempSys := cEmutecaSystem.Create(nil);
      TempSys.ID := TempList[i];
      TempSys.LoadFromFileIni(IniFile);
      FullList.AddOrSetData(TempSys.ID, TempSys);
      Inc(i);
      if ProgressCallBack <> nil then
        ProgressCallBack(rsLoadingSystemList, TempSys.ID,
          TempSys.Model, i + 1, TempList.Count);
    end;
  finally
    FreeAndNil(TempList);
  end;
end;

procedure cEmutecaSystemManager.SaveToFileIni(IniFile: TCustomIniFile;
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
      ProgressCallBack(rsSavingSystemList, FullList.Data[i].ID,
        FullList.Data[i].Model, i + 1, FullList.Count);
    Inc(i);
  end;
end;

function cEmutecaSystemManager.Add(aId: string): cEmutecaSystem;
begin
  Result := ItemById(aId);

  // If allready exists, then return it
  if assigned(result) then
    Exit;

  // Creating new system
  Result := cEmutecaSystem.Create(Self);
  Result.ID := aId;
  Result.Model := aId;
  FullList.Add(Result.ID, Result);
end;

function cEmutecaSystemManager.ItemById(aId: string): cEmutecaSystem;
var
  i: integer;
begin
  // FullList.TryGetData(aId, Result); Maybe do this???

  Result := nil;
  i := FullList.IndexOf(aId);

  if i >= 0 then
    Result := FullList.Data[i];
end;

function cEmutecaSystemManager.Delete(aId: string): integer;
begin
  Result := FullList.Remove(aId);
end;

procedure cEmutecaSystemManager.AssingAll(aList: TStrings);
begin

end;

procedure cEmutecaSystemManager.AssingEnabledTo(aList: TStrings);
var
  i: longint;
begin
  if not assigned(aList) then
    aList := TStringList.Create;

  aList.BeginUpdate;
  i := 0;
  while i < FullList.Count do
  begin
    aList.AddObject(FullList.Data[i].Model, FullList.Data[i]);
    Inc(i);
  end;
  aList.EndUpdate;
end;

constructor cEmutecaSystemManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FFullList := cEmutecaSystemMap.Create(True);
  FullList.OnKeyCompare := @AnsiCompareText;
  // TODO: When it works...: FullList.OnKeyCompare := @UTF8CompareText;
end;

destructor cEmutecaSystemManager.Destroy;
begin
  FreeAndNil(FFullList);
  inherited Destroy;
end;

end.
