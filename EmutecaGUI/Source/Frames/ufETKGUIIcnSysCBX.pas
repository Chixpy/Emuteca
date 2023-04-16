unit ufETKGUIIcnSysCBX;

{< TfmETKGUIIcnSysCBX frame unit.

  This file is part of Emuteca GUI.

  Copyright (C) 2011-2019 Chixpy

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Types, StdCtrls, LCLType,
  // CHX units
  uCHXImageUtils,
  // CHX frames
  ufCHXPropEditor,
  // CHX forms
  ufrCHXForm,
  // Emuteca Core classes
  ucEmutecaSystem, ucEmutecaSystemList,
  // Emuteca Core frames
  ufEmutecaSystemCBX;

type

  { TfmETKGUIIcnSysCBX }

  TfmETKGUIIcnSysCBX = class(TfmEmutecaSystemCBX)
  private
    FDefSysIcon: TPicture;
    procedure SetDefSysIcon(AValue: TPicture);

  protected

  public
    class function SimpleDialog(aSystemList: cEmutecaSystemList):
      cEmutecaSystem;

  published
    property DefSysIcon: TPicture read FDefSysIcon write SetDefSysIcon;

    procedure cbxSystemDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  end;

implementation

{$R *.lfm}

{ TfmETKGUIIcnSysCBX }

procedure TfmETKGUIIcnSysCBX.SetDefSysIcon(AValue: TPicture);
begin
  if FDefSysIcon = AValue then
    Exit;
  FDefSysIcon := AValue;
end;

class function TfmETKGUIIcnSysCBX.SimpleDialog(aSystemList:
  cEmutecaSystemList): cEmutecaSystem;
var
  aETKGUIIcnSysCBX: TfmETKGUIIcnSysCBX;
  aDialogFrame: TfmCHXPropEditor;
  aForm: TfrmCHXForm;
begin
  Result := nil;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := 'frmETKGUIIcnSysCBX';
    aForm.Caption := 'System selection';
    aForm.AutoSize := True;

    aDialogFrame := TfmCHXPropEditor.Create(aForm);
    aDialogFrame.Align := alClient;
    aDialogFrame.Parent := aForm;
    aDialogFrame.chkCloseOnSave.Visible := False;
    aDialogFrame.Enabled := True;

    // If aDialogFrame is not the owner of aETKGUIIcnSysCBX then
    //   it isn't freed when aForm is closed, so we can read SelectedSystem
    // aETKGUIIcnSysCBX := TfmETKGUIIcnSysCBX.Create(aDialogFrame);
    aETKGUIIcnSysCBX := TfmETKGUIIcnSysCBX.Create(nil);
    aETKGUIIcnSysCBX.Align := alClient;
    aETKGUIIcnSysCBX.Parent := aDialogFrame;

    aETKGUIIcnSysCBX.FirstItem := ETKSysCBXFISelect;
    aETKGUIIcnSysCBX.SystemList := aSystemList;
    aETKGUIIcnSysCBX.SelectedSystem := nil;

    if aForm.ShowModal = mrOk then
      Result := aETKGUIIcnSysCBX.SelectedSystem;

  finally
    aForm.Free;
    aETKGUIIcnSysCBX.Free;
  end;
end;

procedure TfmETKGUIIcnSysCBX.cbxSystemDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  IconRect: TRect;
  aIcon: TPicture;
  aSystem: cEmutecaSystem;
  aCBXS: TComboBox;

begin
  if odInactive in State then
    Exit;

  if not (Control is TComboBox) then Exit;

  aCBXS := TComboBox(Control);

  aSystem := cEmutecaSystem(aCBXS.Items.Objects[Index]);

  // Icon
  // aCBX.Canvas.FillRect(ARect);
  IconRect := ARect;
  IconRect.Left := IconRect.Left + 1;
  IconRect.Right := IconRect.Left + ARect.Bottom - ARect.Top;

  aIcon := nil;
  if assigned(aSystem) then
    aIcon := aSystem.Stats.Icon;

  if not assigned(aIcon) then
    aIcon := DefSysIcon;

  if assigned(aIcon) then
    aCBXS.Canvas.StretchDraw(CorrectAspectRatio(IconRect, aIcon),
      aIcon.Graphic);

  // Text
  aCBXS.Canvas.TextOut(IconRect.Right + 4, ARect.Top, aCBXS.Items[Index]);
end;

end.
