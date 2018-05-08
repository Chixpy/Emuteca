{ About form of Emuteca GUI

  Copyright (C) 2006-2018 Chixpy

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
unit ufrETKGUIAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ucCHXImageList,
  ufCHXAbout,
  ucEmuteca;

type

  { TfrmETKGUIAbout }

  TfrmETKGUIAbout = class(TfrmCHXAbout)
  private
    FEmuteca: cEmuteca;
    FCachedIcons: cCHXImageList;
    FVersionIcons: cCHXImageList;
    FZoneIcons: cCHXImageMap;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetCachedIcons(AValue: cCHXImageList);
    procedure SetVersionIcons(AValue: cCHXImageList);
    procedure SetZoneIcons(AValue: cCHXImageMap);

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
    property CachedIcons: cCHXImageList read FCachedIcons write SetCachedIcons;
    property ZoneIcons: cCHXImageMap read FZoneIcons write SetZoneIcons;
    property VersionIcons: cCHXImageList read FVersionIcons
      write SetVersionIcons;

    procedure UpdateInfo;

  end;

var
  frmETKGUIAbout: TfrmETKGUIAbout;

implementation

{$R *.lfm}

{ TfrmETKGUIAbout }

procedure TfrmETKGUIAbout.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;
end;

procedure TfrmETKGUIAbout.SetCachedIcons(AValue: cCHXImageList);
begin
  if FCachedIcons = AValue then
    Exit;
  FCachedIcons := AValue;
end;

procedure TfrmETKGUIAbout.SetVersionIcons(AValue: cCHXImageList);
begin
  if FVersionIcons = AValue then
    Exit;
  FVersionIcons := AValue;
end;

procedure TfrmETKGUIAbout.SetZoneIcons(AValue: cCHXImageMap);
begin
  if FZoneIcons = AValue then
    Exit;
  FZoneIcons := AValue;
end;

procedure TfrmETKGUIAbout.UpdateInfo;
var
  i, NGroups, TGroups, NSoft, TSoft: integer;
begin
  mAditional.Clear;

  if assigned(Emuteca) then
  begin
    mAditional.Lines.Add('EMUTECA INFO');
    mAditional.Lines.Add('------------');
    mAditional.Lines.Add(Format('Systems (Enabled/Total): %0:d / %1:d',
      [Emuteca.SystemManager.EnabledList.Count,
      Emuteca.SystemManager.FullList.Count]));
    mAditional.Lines.Add(Format('Emulators (Enabled/Total): %0:d / %1:d',
      [Emuteca.EmulatorManager.EnabledList.Count,
      Emuteca.EmulatorManager.FullList.Count]));

    NGroups := 0;
    TGroups := 0;
    NSoft := 0;
    TSoft := 0;
    i := 0;
    while i < Emuteca.SystemManager.EnabledList.Count do
    begin
      // Only to show how submerge in Emuteca's tree... ;-D
      NGroups := NGroups + Emuteca.SystemManager.EnabledList[
        i].GroupManager.VisibleList.Count;
      TGroups := TGroups + Emuteca.SystemManager.EnabledList[
        i].GroupManager.FullList.Count;
      NSoft := NSoft + Emuteca.SystemManager.EnabledList[
        i].SoftManager.VisibleList.Count;
      TSoft := TSoft + Emuteca.SystemManager.EnabledList[
        i].SoftManager.FullList.Count;
      Inc(i);
    end;

    mAditional.Lines.Add(Format('Groups (Visible/Total): %0:d / %1:d',
      [NGroups, TGroups]));
    mAditional.Lines.Add(Format('Soft (Visible/Total): %0:d / %1:d',
      [NSoft, TSoft]));

    mAditional.Lines.Add('');

  end;

  mAditional.Lines.Add('GUI INFO');
  mAditional.Lines.Add('--------');

  if assigned(CachedIcons) then
  begin
    mAditional.Lines.Add(Format('Cached icons: %0:d', [CachedIcons.Count]));
  end;

  if assigned(ZoneIcons) then
  begin
    mAditional.Lines.Add(Format('Zone icons: %0:d', [ZoneIcons.Count]));
  end;
  if assigned(VersionIcons) then
  begin
    mAditional.Lines.Add(Format('Version icons: %0:d', [VersionIcons.Count]));
  end;
end;

end.
