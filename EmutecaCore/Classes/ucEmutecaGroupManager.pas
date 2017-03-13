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

{ cGameManager unit. }
unit ucEmutecaGroupManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils,
  LazUTF8, LConvEncoding,
  LResources,
  // Emuteca core
  uaEmutecaManager, ucEmutecaGroup;

resourcestring
  rsLoadingGroupList = 'Loading parent list...';
  rsSavingGroupList = 'Saving parent list...';

type
  { cEmutecaGroupManager }

  cEmutecaGroupManager = class(caEmutecaManager)
  private
    FVisibleList: cEmutecaGroupList;
    FFullList: cEmutecaGroupList;

  protected

  public
    property FullList: cEmutecaGroupList read FFullList;
    {< Actual list where the parents are stored. }
    property VisibleList: cEmutecaGroupList read FVisibleList;
    {< Filtered parent list. }

    procedure LoadFromStrLst(TxtFile: TStrings); override;
    procedure SaveToStrLst(TxtFile: TStrings; const ExportMode: boolean);
      override;

    function ItemById(aId: string; Autocreate: boolean): cEmutecaGroup;
    {< Returns the parent with aId key.

       @Result cEmutecaGroup found or nil.
    }
    {
    procedure FilterBySystem(aSystemKey: string);
    }

    procedure Clear;

    procedure AssingAllTo(aList: TStrings); override;
    procedure AssingEnabledTo(aList: TStrings); override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ cEmutecaGroupManager }

function cEmutecaGroupManager.ItemById(aId: string;
  Autocreate: boolean): cEmutecaGroup;
var
  i: integer;
  aGroup: cEmutecaGroup;
begin
  Result := nil;

  // Inverse search can be faster
  i := FullList.Count;
  while (Result = nil) and (i > 0) do
  begin
    Dec(i);
    aGroup := cEmutecaGroup(FullList[i]);
    if UTF8CompareText(aGroup.ID, aId) = 0 then
      Result := aGroup;
  end;

  // Opps, creating it
  if Autocreate and (not assigned(Result)) then
  begin
    Result := cEmutecaGroup.Create(nil);
    Result.ID := aId;
    Result.Title := aId;
    Self.FullList.Add(Result);
  end;
end;

procedure cEmutecaGroupManager.Clear;
begin
  VisibleList.Clear;
  FullList.Clear;
end;

procedure cEmutecaGroupManager.AssingAllTo(aList: TStrings);
var
  i: longint;
  aGroup: cEmutecaGroup;
begin
  if not assigned(aList) then
    aList := TStringList.Create;

  aList.BeginUpdate;
  aList.Capacity := aList.Count + FullList.Count; // Speed up?
  i := 0;
  while i < FullList.Count do
  begin
    aGroup := cEmutecaGroup(FullList[i]);
    aList.AddObject(aGroup.Title, aGroup);
    Inc(i);
  end;
  aList.EndUpdate;
end;

procedure cEmutecaGroupManager.AssingEnabledTo(aList: TStrings);
var
  i: longint;
  aGroup: cEmutecaGroup;
begin
  if not assigned(aList) then
    aList := TStringList.Create;

  aList.BeginUpdate;
  aList.Capacity := aList.Count + VisibleList.Count; // Speed up?
  i := 0;
  while i < VisibleList.Count do
  begin
    aGroup := cEmutecaGroup(VisibleList[i]);
    aList.AddObject(aGroup.Title, aGroup);
    Inc(i);
  end;
  aList.EndUpdate;
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
    TempGroup.DataString := TxtFile[i];

    FullList.Add(TempGroup);
    Inc(i);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsLoadingGroupList, TempGroup.Title,
        TempGroup.Developer, i, TxtFile.Count);
  end;
  //FullList.EndUpdate;
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
    TxtFile.Add('"ID","Title","System","Year","Developer"');

    i := 0;
    while i < FullList.Count do
    begin
      aGroup := cEmutecaGroup(FullList[i]);
      TxtFile.Add(aGroup.DataString);
      Inc(i);

      if ProgressCallBack <> nil then
        ProgressCallBack(rsSavingGroupList, aGroup.Title,
          aGroup.Developer, i, FullList.Count);
    end;

  finally
    TxtFile.EndUpdate;
  end;
end;

constructor cEmutecaGroupManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FFullList := cEmutecaGroupList.Create(True);
  FVisibleList := cEmutecaGroupList.Create(False);
  // TODO: OnCompare FullList.OnCompare := ;
end;

destructor cEmutecaGroupManager.Destroy;
begin
  FreeAndNil(FVisibleList);
  FreeAndNil(FFullList);
  inherited Destroy;
end;

end.
