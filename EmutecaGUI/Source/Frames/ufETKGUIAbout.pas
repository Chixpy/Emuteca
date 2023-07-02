unit ufETKGUIAbout;

{< TfmETKGUIAbout frame of Emuteca GUI

  Copyright (C) 2006-2023 Chixpy

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
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  // CHX classes
  ucCHXImageList,
  // CHX frames
  ufCHXAbout,
  // Emuteca Core units
  uEmutecaConst,
  // Emuteca Core classes
  ucEmuteca;

type

  { TfmETKGUIAbout }

  TfmETKGUIAbout = class(TfmCHXAbout)
  private
    FCachedIcons: cCHXImageList;
    FEmuteca: cEmuteca;
    FVersionIcons: cCHXImageList;
    FZoneIcons: cCHXImageMap;
    procedure SetCachedIcons(const AValue: cCHXImageList);
    procedure SetEmuteca(const AValue: cEmuteca);
    procedure SetVersionIcons(const AValue: cCHXImageList);
    procedure SetZoneIcons(const AValue: cCHXImageMap);

  protected


  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
    property CachedIcons: cCHXImageList read FCachedIcons write SetCachedIcons;
    property ZoneIcons: cCHXImageMap read FZoneIcons write SetZoneIcons;
    property VersionIcons: cCHXImageList read FVersionIcons
      write SetVersionIcons;

    procedure LoadFrameData; override;
    procedure ClearFrameData; override;

    class function SimpleModalForm(aEmuteca: cEmuteca;
      aCachedIcons, aVersionIcons: cCHXImageList; aZoneIcons: cCHXImageMap;
      const aGUIConfigIni, aGUIIconsIni: string): integer;
    //< Creates a form with TfmEmutecaActAddSoft frame.

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmETKGUIAbout }

procedure TfmETKGUIAbout.SetCachedIcons(const AValue: cCHXImageList);
begin
  if FCachedIcons = AValue then
    Exit;
  FCachedIcons := AValue;
end;

procedure TfmETKGUIAbout.SetEmuteca(const AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;
end;

procedure TfmETKGUIAbout.SetVersionIcons(const AValue: cCHXImageList);
begin
  if FVersionIcons = AValue then
    Exit;
  FVersionIcons := AValue;
end;

procedure TfmETKGUIAbout.SetZoneIcons(const AValue: cCHXImageMap);
begin
  if FZoneIcons = AValue then
    Exit;
  FZoneIcons := AValue;
end;

procedure TfmETKGUIAbout.LoadFrameData;
var
  i, NGroups, TGroups, NSoft, TSoft: integer;
begin
  inherited LoadFrameData;

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

  Enabled := True;
end;

procedure TfmETKGUIAbout.ClearFrameData;
begin
  inherited  ClearFrameData;
  mAditional.Clear;
end;

class function TfmETKGUIAbout.SimpleModalForm(aEmuteca: cEmuteca;
  aCachedIcons, aVersionIcons: cCHXImageList; aZoneIcons: cCHXImageMap;
  const aGUIConfigIni, aGUIIconsIni: string): integer;
var
  aFrame: TfmETKGUIAbout;
begin
  aFrame := TfmETKGUIAbout.Create(nil);

  aFrame.Emuteca := aEmuteca;
  aFrame.CachedIcons := aCachedIcons;
  aFrame.ZoneIcons := aZoneIcons;
  aFrame.VersionIcons := aVersionIcons;
  aFrame.LoadFrameData;

  Result := GenSimpleModalForm(aFrame, 'frmETKGUIAbout',
    Format(krsFmtWindowCaption, [Application.Title, 'About... Emuteca GUI']),
    aGUIConfigIni, aGUIIconsIni);
end;

constructor TfmETKGUIAbout.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmETKGUIAbout.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmETKGUIAbout);

finalization
  UnRegisterClass(TfmETKGUIAbout);
end.
