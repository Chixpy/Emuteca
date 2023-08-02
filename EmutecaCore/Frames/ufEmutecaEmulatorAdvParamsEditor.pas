unit ufEmutecaEmulatorAdvParamsEditor;
{< TfmEmutecaEmulatorAdvParamsEditor  frame unit.

  This file is part of Emuteca GUI.

  Copyright (C) 2011-2023 Chixpy

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ActnList,
  // CHX units
  uCHXStrUtils, uCHXDlgUtils,
  // CHX frames
  ufCHXPropEditor,
  // Emuteca Core abstract
  uaEmutecaCustomEmu,
  // Emuteca Core classes
  ucEmutecaEmulator;

type

  { TfmEmutecaEmulatorAdvParamsEditor }

  TfmEmutecaEmulatorAdvParamsEditor = class(TfmCHXPropEditor)
    eCoreIDParamFmt: TEdit;
    eMultiEmuID: TEdit;
    gbxCoreID: TGroupBox;
    gbxExtParams: TGroupBox;
    gbxExtraSoftParameters: TGroupBox;
    lCoreParamFmt: TLabel;
    lMultiEmuID: TLabel;
    mExtensionParameters: TMemo;
    mExtraParameters: TMemo;
    Splitter1: TSplitter;

  private
    FEmulator: cEmutecaEmulator;
    procedure SetEmulator(const AValue: cEmutecaEmulator);

  protected

  public
    property Emulator: cEmutecaEmulator read FEmulator write SetEmulator;

    procedure ClearFrameData; override;
    procedure LoadFrameData; override;
    procedure SaveFrameData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaEmulatorAdvParamsEditor }

procedure TfmEmutecaEmulatorAdvParamsEditor.SetEmulator(
  const AValue: cEmutecaEmulator);
begin
  if FEmulator = AValue then
    Exit;
  FEmulator := AValue;
  LoadFrameData;
end;

procedure TfmEmutecaEmulatorAdvParamsEditor.ClearFrameData;
begin
  inherited ClearFrameData;

  eMultiEmuID.Clear;
  eCoreIDParamFmt.Clear;
  mExtensionParameters.Clear;
  mExtraParameters.Clear;
end;

procedure TfmEmutecaEmulatorAdvParamsEditor.LoadFrameData;
begin
  inherited LoadFrameData;

  Enabled := assigned(Emulator);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  eMultiEmuID.Text := Emulator.CoreIDKey;
  eCoreIDParamFmt.Text := Emulator.CoreIDParamFormat;
  mExtensionParameters.Lines.Assign(Emulator.ExtensionParamFormat);
  mExtraParameters.Lines.Assign(Emulator.ExtraParamFormat);
end;

procedure TfmEmutecaEmulatorAdvParamsEditor.SaveFrameData;
begin
  inherited SaveFrameData;

  Emulator.CoreIDKey := eMultiEmuID.Text;
  Emulator.CoreIDParamFormat := eCoreIDParamFmt.Text;
  Emulator.ExtensionParamFormat.Assign(mExtensionParameters.Lines);
  Emulator.ExtraParamFormat.Assign(mExtraParameters.Lines);
end;

constructor TfmEmutecaEmulatorAdvParamsEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmEmutecaEmulatorAdvParamsEditor.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmEmutecaEmulatorAdvParamsEditor);

finalization
  UnRegisterClass(TfmEmutecaEmulatorAdvParamsEditor);

end.
