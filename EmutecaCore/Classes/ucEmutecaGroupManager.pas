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

  cEmutecaGroupManager = class(caEmutecaManagerTxt)
  private
    FVisibleList: cEmutecaGroupList;
    FFullList: cEmutecaGroupList;

  protected


  public
    property FullList: cEmutecaGroupList read FFullList;
    {< Actual list where the parents are stored. }
    property VisibleList: cEmutecaGroupList read FVisibleList;
    {< Filtered parent list. }

    procedure LoadFromFileTxt(TxtFile: TStrings); override;
    procedure SaveToFileTxt(TxtFile: TStrings; const ExportMode: boolean);
      override;

    function ItemById(aId: string): cEmutecaGroup;
    {< Returns the parent with aId key.

       @Result cEmutecaGroup found or nil.
    }

    procedure FilterBySystem(aSystemKey: string);

    procedure AssingAllTo(aList: TStrings); override;
    procedure AssingEnabledTo(aList: TStrings); override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ cEmutecaGroupManager }

function cEmutecaGroupManager.ItemById(aId: string): cEmutecaGroup;
var
  i: integer;
  aGroup: cEmutecaGroup;
begin
  Result := nil;

  i := 0;
  while (Result = nil) and (i < FullList.Count) do
  begin
    aGroup := cEmutecaGroup(FullList[i]);
    if UTF8CompareText(aGroup.ID, aId) = 0 then
      Result := aGroup;
    Inc(i);
  end;
end;

procedure cEmutecaGroupManager.FilterBySystem(aSystemKey: string);
var
  i: longint;
  aGroup: cEmutecaGroup;
begin
  VisibleList.Clear;

  if aSystemKey = '' then
  begin
    VisibleList.Assign(FullList);
  end
  else
  begin
    i := 0;
    while i < FullList.Count do
    begin
      aGroup := cEmutecaGroup(FullList[i]);
      if UTF8CompareText(aGroup.SystemKey, aSystemKey) = 0 then
      begin
        if VisibleList.Capacity = VisibleList.Count then
          VisibleList.Capacity := VisibleList.Capacity * 2; // Speed up?
        VisibleList.Add(aGroup);
      end;
      Inc(i);
    end;
  end;
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
    aList.AddObject(aGroup.Title + ' (' + aGroup.SystemKey + ')', aGroup);
    Inc(i);
  end;
  aList.EndUpdate;
end;

procedure cEmutecaGroupManager.LoadFromFileTxt(TxtFile: TStrings);
var
  i: integer;
  TempGroup: cEmutecaGroup;
begin
  if not Assigned(TxtFile) then
    Exit;

  FullList.Capacity := FullList.Count + TxtFile.Count; // Speed Up?
  TxtFile.BeginUpdate;
  i := 1; // Skipping Header
  while i < TxtFile.Count do
  begin
    TempGroup := cEmutecaGroup.Create(nil);
    TempGroup.DataString := TxtFile[i];
    FullList.Add(TempGroup);
    Inc(i);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsLoadingGroupList, TempGroup.SystemKey,
        TempGroup.Title, i, TxtFile.Count);
  end;
  TxtFile.EndUpdate;
  VisibleList.Assign(FullList);
end;

procedure cEmutecaGroupManager.SaveToFileTxt(TxtFile: TStrings;
  const ExportMode: boolean);
var
  i: integer;
  aGroup: cEmutecaGroup;
begin
  if not Assigned(TxtFile) then
    Exit;

  { TODO : cEmutecaGroupManager.SaveToFileTxt Export mode }
  TxtFile.Clear;
  TxtFile.BeginUpdate;
  TxtFile.Capacity := FullList.Count + 1; // Speed up?
  TxtFile.Add('"ID/Sort Name","System","Title"');

  i := 0;
  while i < FullList.Count do
  begin
    aGroup := cEmutecaGroup(FullList[i]);
    TxtFile.Add(aGroup.DataString);
    Inc(i);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsSavingGroupList, aGroup.SystemKey,
        aGroup.Title, i, FullList.Count);
  end;
  TxtFile.EndUpdate;
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
