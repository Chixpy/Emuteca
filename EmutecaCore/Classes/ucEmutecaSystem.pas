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

{ cSystem unit. }
unit ucEmutecaSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LazFileUtils, LazUTF8,
  uEmutecaCommon,
  uaEmutecaCustomSystem,
  ucEmutecaGroupManager, ucEmutecaSoftManager;

type

  { cEmutecaCacheSystemThread }

  cEmutecaCacheSystemThread = class(TThread)
  private
    FGroupManager: cEmutecaGroupManager;
    FSoftManager: cEmutecaSoftManager;
    procedure SetGroupManager(AValue: cEmutecaGroupManager);
    procedure SetSoftManager(AValue: cEmutecaSoftManager);

  protected
    procedure Execute; override;

  public
    property GroupManager: cEmutecaGroupManager
      read FGroupManager write SetGroupManager;
    property SoftManager: cEmutecaSoftManager
      read FSoftManager write SetSoftManager;

    constructor Create;
  end;

  { cEmutecaSystem }

  cEmutecaSystem = class(caEmutecaCustomSystem)
  private
    FCacheDataThread: cEmutecaCacheSystemThread;
    FGroupManager: cEmutecaGroupManager;
    FSoftManager: cEmutecaSoftManager;
    procedure SetCacheDataThread(AValue: cEmutecaCacheSystemThread);

  protected
    property CacheDataThread: cEmutecaCacheSystemThread
      read FCacheDataThread write SetCacheDataThread;

  public

    procedure LoadLists(aFile: string);
    procedure SaveLists(aFile: string; ExportMode: boolean);

    procedure CacheData;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property GroupManager: cEmutecaGroupManager read FGroupManager;
    property SoftManager: cEmutecaSoftManager read FSoftManager;

  end;

  TEmutecaReturnSystemCB = function(aSystem: cEmutecaSystem): boolean of
    object;

{< For CallBack functions }

function Str2EmutecaFileKey(aString: string): TEmutecaFileKey;
// Result := EmutecaFileKeyStrsK[TEmutecaFileKey];

implementation

uses ucEmutecaGroup, ucEmutecaSoftware;

function Str2EmutecaFileKey(aString: string): TEmutecaFileKey;
begin
  // In Emuteca <= 0.7, True => CRC32 / False => FileName
  aString := UTF8UpperCase(aString);

  // I don't like this "else if" format but it's clearer...
  if (aString = UTF8UpperCase(krsCRC32)) or
    (StrToBoolDef(aString, False)) then
    Result := TEFKCRC32
  else if (aString = UTF8UpperCase(krsFileName)) or
    (not StrToBoolDef(aString, True)) then
    Result := TEFKFileName
  else if (aString = UTF8UpperCase(krsSHA1)) then
    Result := TEFKSHA1
  else if (aString = UTF8UpperCase(krsCustom)) then
    Result := TEFKCustom
  else // Default
    Result := TEFKSHA1;
end;

{ cEmutecaCacheSystemThread }

procedure cEmutecaCacheSystemThread.SetGroupManager(AValue:
  cEmutecaGroupManager);
begin
  if FGroupManager = AValue then
    Exit;
  FGroupManager := AValue;
end;

procedure cEmutecaCacheSystemThread.SetSoftManager(AValue:
  cEmutecaSoftManager);
begin
  if FSoftManager = AValue then
    Exit;
  FSoftManager := AValue;
end;

procedure cEmutecaCacheSystemThread.Execute;
var
  aSoft: cEmutecaSoftware;
  aGroup: cEmutecaGroup;
  SoftPos, GroupPos: integer;
  Found: boolean;
begin
  if (not assigned(SoftManager)) or (not assigned(GroupManager)) then
    Exit;

  // Caching groups for soft
  aGroup := nil;
  SoftPos := 0;
  while (not Terminated) and (SoftPos < SoftManager.FullList.Count) do
  begin
    aSoft := SoftManager.FullList[SoftPos];

    // Try last used group.
    if not aSoft.MatchGroup(aGroup) then
    begin
      // Search Group
      // Don't use GroupManager.FullList.ItemById(), because when want to
      //   test Terminated;
      GroupPos := 0;
      Found := False;
      while (not Terminated) and (not Found) and (GroupPos < GroupManager.FullList.Count)
        do
      begin
        aGroup := GroupManager.FullList[GroupPos];
        Found := aSoft.MatchGroup(aGroup);
        Inc(GroupPos);
      end;
    end
    else
      Found := True;

    if (not Terminated) and (not Found) then
    begin
      // OOpps, not found; creating it.
      aGroup := GroupManager.FullList[GroupManager.AddGroup(aSoft.GroupKey)];
    end;

    if (not Terminated) then
    begin
      // Finally caching
      aSoft.CachedGroup := aGroup;
      aGroup.SoftList.Add(aSoft);
    end;

    Inc(SoftPos);
  end;

  if (not Terminated) then
    GroupManager.VisibleList.Clear;

// Adding to visible list groups with soft
GroupPos := 0;
while (not Terminated) and (GroupPos < GroupManager.FullList.Count)  do
begin
  aGroup := GroupManager.FullList[GroupPos];
  if (not Terminated) and (aGroup.SoftList.Count > 0) then
    GroupManager.VisibleList.Add(aGroup);
  Inc(GroupPos);
end;
end;

constructor cEmutecaCacheSystemThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

{ cEmutecaSystem }

procedure cEmutecaSystem.SetCacheDataThread(AValue: cEmutecaCacheSystemThread);
begin
  if FCacheDataThread = AValue then
    Exit;
  FCacheDataThread := AValue;
end;

procedure cEmutecaSystem.LoadLists(aFile: string);
begin
  GroupManager.ClearData;
  if FileExistsUTF8(aFile + krsEmutecaGroupFileExt) then
    GroupManager.LoadFromFileTxt(aFile + krsEmutecaGroupFileExt);

  SoftManager.ClearData;
  if FileExistsUTF8(aFile + krsEmutecaSoftFileExt) then
    SoftManager.LoadFromFileTxt(aFile + krsEmutecaSoftFileExt);

  CacheData;
end;

procedure cEmutecaSystem.SaveLists(aFile: string; ExportMode: boolean);
begin
  if aFile = '' then
    Exit;
  if not DirectoryExistsUTF8(ExtractFileDir(aFile)) then
    ForceDirectoriesUTF8(ExtractFileDir(aFile));
  GroupManager.SaveToFileTxt(aFile + krsEmutecaGroupFileExt, ExportMode);
  SoftManager.SaveToFileTxt(aFile + krsEmutecaSoftFileExt, ExportMode);
end;

procedure cEmutecaSystem.CacheData;
begin
  if Assigned(CacheDataThread) then
    CacheDataThread.Terminate;
  // FreeOnTerminate = true, so we don't need to destroy it.

  // Caching data in background
  FCacheDataThread := cEmutecaCacheSystemThread.Create;
  if Assigned(CacheDataThread.FatalException) then
    raise CacheDataThread.FatalException;
  CacheDataThread.SoftManager := SoftManager;
  CacheDataThread.GroupManager := GroupManager;
  CacheDataThread.Start;
end;

constructor cEmutecaSystem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FGroupManager := cEmutecaGroupManager.Create(Self);
  GroupManager.System := Self;
  FSoftManager := cEmutecaSoftManager.Create(Self);
  SoftManager.System := Self;
end;

destructor cEmutecaSystem.Destroy;
begin
  if Assigned(CacheDataThread) then
    CacheDataThread.Terminate;
  // FreeOnTerminate = true, so we don't need to destroy it.

  SoftManager.Free;
  GroupManager.Free;

  inherited Destroy;
end;

initialization
  RegisterClass(cEmutecaSystem);

finalization
  UnRegisterClass(cEmutecaSystem);
end.
