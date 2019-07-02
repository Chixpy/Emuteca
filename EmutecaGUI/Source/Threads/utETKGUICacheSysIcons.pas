unit utETKGUICacheSysIcons;
{< ctEGUICacheSysIcons thread unit.

  This file is part of Emuteca GUI.

  Copyright (C) 2017-2019 Chixpy

  This source is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by
  writing to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Graphics, LazFileUtils,
  // CHX classes
  ucCHXImageList,
  // Emuteca Core classes
  ucEmutecaSystemManager, ucEmutecaSystem;

type

  { ctEGUICacheSysIcons

    This Thread loads system icons in background.

  }

  ctEGUICacheSysIcons = class(TThread)
  private
    FDefSoftIcon: TPicture;
    FDefSysIcon: TPicture;
    FIconList: cCHXImageList;
    FSystemManager: cEmutecaSystemManager;
    procedure SetDefSoftIcon(AValue: TPicture);
    procedure SetDefSysIcon(AValue: TPicture);
    procedure SetIconList(AValue: cCHXImageList);
    procedure SetSystemManager(AValue: cEmutecaSystemManager);

  protected
    procedure Execute; override;

  public
    property DefSysIcon: TPicture read FDefSysIcon write SetDefSysIcon;
    property DefSoftIcon: TPicture read FDefSoftIcon write SetDefSoftIcon;
    property IconList: cCHXImageList read FIconList write SetIconList;
    property SystemManager: cEmutecaSystemManager
      read FSystemManager write SetSystemManager;

    constructor Create;
  end;

implementation

{ ctEGUICacheSysIcons }

procedure ctEGUICacheSysIcons.SetDefSysIcon(AValue: TPicture);
begin
  if FDefSysIcon = AValue then
    Exit;
  FDefSysIcon := AValue;
end;

procedure ctEGUICacheSysIcons.SetDefSoftIcon(AValue: TPicture);
begin
  if FDefSoftIcon = AValue then
    Exit;
  FDefSoftIcon := AValue;
end;

procedure ctEGUICacheSysIcons.SetIconList(AValue: cCHXImageList);
begin
  if FIconList = AValue then
    Exit;
  FIconList := AValue;
end;

procedure ctEGUICacheSysIcons.SetSystemManager(
  AValue: cEmutecaSystemManager);
begin
  if FSystemManager = AValue then
    Exit;
  FSystemManager := AValue;
end;

procedure ctEGUICacheSysIcons.Execute;
var
  i: integer;
  aSystem: cEmutecaSystem;
  aIcon: TPicture;
begin
  if not assigned(SystemManager) then
    Exit;
  if not assigned(IconList) then
    Exit;
  // if not assigned(DefSysIcon) then Exit; // Can be nil
  // if not assigned(DefSoftIcon) then Exit; // Can be nil

  i := 0;
  while (not Terminated) and (i < SystemManager.FullList.Count) do
  begin
    aSystem := SystemManager.FullList[i];

    if FileExistsUTF8(aSystem.IconFile) then
    begin
      if Terminated then
        Exit;
      aIcon := IconList[IconList.AddImageFile(aSystem.IconFile)];
    end
    else
    begin
      aIcon := DefSysIcon;
    end;

    if Terminated then
      Exit;
    aSystem.Stats.Icon := aIcon;

    if FileExistsUTF8(aSystem.SoftIconFile) then
    begin
      if Terminated then
        Exit;
      aIcon := IconList[IconList.AddImageFile(aSystem.SoftIconFile)];
    end
    else
    begin
      aIcon := DefSoftIcon;
    end;

    if Terminated then
      Exit;
    aSystem.Stats.SysSoftIcon := aIcon;

    Inc(i);
  end;
end;

constructor ctEGUICacheSysIcons.Create;
begin
  inherited Create(True);
  DefSoftIcon := nil; // Just to be sure...
  DefSysIcon := nil;
  FreeOnTerminate := True;
end;

end.
