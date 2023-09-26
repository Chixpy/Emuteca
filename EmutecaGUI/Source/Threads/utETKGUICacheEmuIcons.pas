unit utETKGUICacheEmuIcons;

{< ctETKGUICacheEmuIcons thread unit.

  This file is part of Emuteca GUI.

  Copyright (C) 2017-2019 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Graphics, LazFileUtils,
  // CHX classes
  ucCHXImageList,
  // Emuteca Core classes
  ucEmutecaEmulatorManager, ucEmutecaEmulator;

type

  { ctETKGUICacheEmuIcons

    This Thread loads system icons in background.

  }

  ctETKGUICacheEmuIcons = class(TThread)
  private
    FDefEmuIcon: TPicture;
    FEmuManager: cEmutecaEmulatorManager;
    FIconList: cCHXImageList;
    procedure SetDefEmuIcon(const AValue: TPicture);
    procedure SetEmuManager(const AValue: cEmutecaEmulatorManager);
    procedure SetIconList(const AValue: cCHXImageList);
  protected
    procedure Execute; override;

  public
    property DefEmuIcon: TPicture read FDefEmuIcon write SetDefEmuIcon;
    property IconList: cCHXImageList read FIconList write SetIconList;
    property EmuManager: cEmutecaEmulatorManager
      read FEmuManager write SetEmuManager;

    constructor Create;
  end;

implementation

{ ctETKGUICacheEmuIcons }

procedure ctETKGUICacheEmuIcons.SetDefEmuIcon(const AValue: TPicture);
begin
  if FDefEmuIcon = AValue then
    Exit;
  FDefEmuIcon := AValue;
end;

procedure ctETKGUICacheEmuIcons.SetEmuManager(
  const AValue: cEmutecaEmulatorManager);
begin
  if FEmuManager = AValue then
    Exit;
  FEmuManager := AValue;
end;

procedure ctETKGUICacheEmuIcons.SetIconList(const AValue: cCHXImageList);
begin
  if FIconList = AValue then
    Exit;
  FIconList := AValue;
end;

procedure ctETKGUICacheEmuIcons.Execute;
var
  i: Integer;
  aEmu: cEmutecaEmulator;
  aIcon: TPicture;
begin
  if not assigned(EmuManager) then
    Exit;
  if not assigned(IconList) then
    Exit;
  // if not assigned(DefEmuIcon) then Exit; // Can be nil

  i := 0;
  while (not Terminated) and (i < EmuManager.FullList.Count) do
  begin
    aEmu := EmuManager.FullList[i];

    if FileExistsUTF8(aEmu.IconFile) then
    begin
      if Terminated then
        Exit;
      aIcon := IconList[IconList.AddImageFile(aEmu.IconFile)];
    end
    else
    begin
      aIcon := DefEmuIcon;
    end;

    if Terminated then
      Exit;
    aEmu.Stats.Icon := aIcon;

    Inc(i);
  end;
end;

constructor ctETKGUICacheEmuIcons.Create;
begin
  inherited Create(True);
  DefEmuIcon := nil; // Just to be sure...
  FreeOnTerminate := True;
end;

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
