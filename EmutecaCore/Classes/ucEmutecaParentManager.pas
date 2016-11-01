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
unit ucEmutecaParentManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils,
  LazUTF8, LConvEncoding,
  LResources,
  // Emuteca core
  uaEmutecaManager, ucEmutecaParent;

resourcestring
  rsLoadingParentList = 'Loading parent list...';
  rsSavingParentList = 'Saving parent list...';

type
  { cEmutecaParentManager }

  cEmutecaParentManager = class(caEmutecaManagerTxt)
  private
    FVisibleList: cEmutecaParentList;
    FFullList: cEmutecaParentList;

  protected


  public
    property FullList: cEmutecaParentList read FFullList;
    {< Actual list where the parents are stored. }
    property VisibleList: cEmutecaParentList read FVisibleList;
    {< Filtered parent list. }

    procedure LoadFromFileTxt(TxtFile: TStrings); override;
    procedure SaveToFileTxt(TxtFile: TStrings; const ExportMode: boolean);
      override;

    function ItemById(aId: string): cEmutecaParent;
    {< Returns the parent with aId key.

       @Result cEmutecaParent found or nil.
    }

    procedure FilterBySystem(aSystemKey: string);

    procedure AssingAllTo(aList: TStrings); override;
    procedure AssingEnabledTo(aList: TStrings); override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ cEmutecaParentManager }

function cEmutecaParentManager.ItemById(aId: string): cEmutecaParent;
var
  i: integer;
  aParent: cEmutecaParent;
begin
  Result := nil;

  i := 0;
  while (Result = nil) and (i < FullList.Count) do
  begin
    aParent := cEmutecaParent(FullList[i]);
    if UTF8CompareText(aParent.ID, aId) = 0 then
      Result := aParent;
    Inc(i);
  end;
end;

procedure cEmutecaParentManager.FilterBySystem(aSystemKey: string);
var
  i: longint;
  aParent: cEmutecaParent;
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
      aParent := cEmutecaParent(FullList[i]);
      if UTF8CompareText(aParent.System, aSystemKey) = 0 then
      begin
        if VisibleList.Capacity = VisibleList.Count then
          VisibleList.Capacity := VisibleList.Capacity * 2; // Speed up?
        VisibleList.Add(aParent);
      end;
      Inc(i);
    end;
  end;
end;

procedure cEmutecaParentManager.AssingAllTo(aList: TStrings);
var
  i: longint;
  aParent: cEmutecaParent;
begin
  if not assigned(aList) then
    aList := TStringList.Create;

  aList.BeginUpdate;
  aList.Capacity := aList.Count + FullList.Count; // Speed up?
  i := 0;
  while i < FullList.Count do
  begin
    aParent := cEmutecaParent(FullList[i]);
    aList.AddObject(aParent.Title, aParent);
    Inc(i);
  end;
  aList.EndUpdate;
end;

procedure cEmutecaParentManager.AssingEnabledTo(aList: TStrings);
var
  i: longint;
  aParent: cEmutecaParent;
begin
  if not assigned(aList) then
    aList := TStringList.Create;

  aList.BeginUpdate;
  aList.Capacity := aList.Count + VisibleList.Count; // Speed up?
  i := 0;
  while i < VisibleList.Count do
  begin
    aParent := cEmutecaParent(VisibleList[i]);
    aList.AddObject(aParent.Title + ' (' + aParent.System + ')', aParent);
    Inc(i);
  end;
  aList.EndUpdate;
end;

procedure cEmutecaParentManager.LoadFromFileTxt(TxtFile: TStrings);
var
  i: integer;
  TempParent: cEmutecaParent;
begin
  if not Assigned(TxtFile) then
    Exit;

  FullList.Capacity := FullList.Count + TxtFile.Count; // Speed Up?
  TxtFile.BeginUpdate;
  i := 1; // Skipping Header
  while i < TxtFile.Count do
  begin
    TempParent := cEmutecaParent.Create(nil);
    TempParent.DataString := TxtFile[i];
    FullList.Add(TempParent);
    Inc(i);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsLoadingParentList, TempParent.System,
        TempParent.Title, i, TxtFile.Count);
  end;
  TxtFile.EndUpdate;
  VisibleList.Assign(FullList);
end;

procedure cEmutecaParentManager.SaveToFileTxt(TxtFile: TStrings;
  const ExportMode: boolean);
var
  i: integer;
  aParent: cEmutecaParent;
begin
  if not Assigned(TxtFile) then
    Exit;

  { TODO : cEmutecaParentManager.SaveToFileTxt Export mode }
  TxtFile.Clear;
  TxtFile.BeginUpdate;
  TxtFile.Capacity := FullList.Count + 1; // Speed up?
  TxtFile.Add('"ID/Sort Name","System","Title"');

  i := 0;
  while i < FullList.Count do
  begin
    aParent := cEmutecaParent(FullList[i]);
    TxtFile.Add(aParent.DataString);
    Inc(i);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsSavingParentList, aParent.System,
        aParent.Title, i, FullList.Count);
  end;
  TxtFile.EndUpdate;
end;

constructor cEmutecaParentManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FFullList := cEmutecaParentList.Create(True);
  FVisibleList := cEmutecaParentList.Create(False);
  // TODO: OnCompare FullList.OnCompare := ;
end;

destructor cEmutecaParentManager.Destroy;
begin
  FreeAndNil(FVisibleList);
  FreeAndNil(FFullList);
  inherited Destroy;
end;

end.
