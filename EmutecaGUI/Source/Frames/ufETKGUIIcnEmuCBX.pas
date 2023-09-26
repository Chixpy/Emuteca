unit ufETKGUIIcnEmuCBX;

{< TfmETKGUIIcnEmuCBX frame unit.

  This file is part of Emuteca GUI.

  Copyright (C) 2019-2019 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Types,
  StdCtrls, LCLType,
  // CHX units
  uCHXImageUtils,
  // Emuteca clases
  ucEmutecaEmulator,
  // Emuteca Core frames
  ufEmutecaEmulatorCBX;

type

  { TfmETKGUIIcnEmuCBX }

  TfmETKGUIIcnEmuCBX = class(TfmEmutecaEmulatorCBX)
    procedure cbxEmulatorDrawItem(Control: TWinControl;
      Index: integer; ARect: TRect; State: TOwnerDrawState);
  private
    FDefEmuIcon: TPicture;
    procedure SetDefEmuIcon(const AValue: TPicture);

  public

    property DefEmuIcon: TPicture read FDefEmuIcon write SetDefEmuIcon;

  end;

implementation

{$R *.lfm}

{ TfmETKGUIIcnEmuCBX }

procedure TfmETKGUIIcnEmuCBX.cbxEmulatorDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  aCBXS: TComboBox;
  aEmu: cEmutecaEmulator;
  IconRect: TRect;
  aIcon: TPicture;
begin
  if odInactive in State then
    Exit;

  if not (Control is TComboBox) then
    Exit;

  aCBXS := TComboBox(Control);

  aEmu := cEmutecaEmulator(aCBXS.Items.Objects[Index]);

  // Icon
  // aCBX.Canvas.FillRect(ARect);
  IconRect := ARect;
  IconRect.Left := IconRect.Left + 1;
  IconRect.Right := IconRect.Left + ARect.Bottom - ARect.Top;

  aIcon := nil;
  if assigned(aEmu) then
    aIcon := aEmu.Stats.Icon;

  if not assigned(aIcon) then
    aIcon := DefEmuIcon;

  if assigned(aIcon) then
    aCBXS.Canvas.StretchDraw(CorrectAspectRatio(IconRect, aIcon),
      aIcon.Graphic);

  // Text
  aCBXS.Canvas.TextOut(IconRect.Right + 4, ARect.Top, aCBXS.Items[Index]);
end;

procedure TfmETKGUIIcnEmuCBX.SetDefEmuIcon(const AValue: TPicture);
begin
  if FDefEmuIcon = AValue then
    Exit;
  FDefEmuIcon := AValue;
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
