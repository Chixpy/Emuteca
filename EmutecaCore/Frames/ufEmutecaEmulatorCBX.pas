{ Generic emulator combobox frame of LazEmuteca

  Copyright (C) 2011-2017 Chixpy

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
unit ufEmutecaEmulatorCBX;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ucEmutecaEmulator, ucEmutecaEmulatorList,
  ufCHXFrame;

type

  { TfmEmutecaEmulatorCBX }

  TfmEmutecaEmulatorCBX = class(TfmCHXFrame)
    cbxEmulator: TComboBox;
    procedure cbxEmulatorSelect(Sender: TObject);
  private
    FEmulatorList: cEmutecaEmulatorList;
    FOnSelectEmulator: TEmutecaReturnEmulatorCB;
    FSelectedEmulator: cEmutecaEmulator;
    procedure SetEmulatorList(AValue: cEmutecaEmulatorList);
    procedure SetOnSelectEmulator(AValue: TEmutecaReturnEmulatorCB);
    procedure SetSelectedEmulator(AValue: cEmutecaEmulator);

  protected
    procedure DoClearFrameData;
    procedure DoLoadFrameData;

  public

    property EmulatorList: cEmutecaEmulatorList
      read FEmulatorList write SetEmulatorList;
    {< List of emulators. }

    property SelectedEmulator: cEmutecaEmulator
      read FSelectedEmulator write SetSelectedEmulator;
    {< Returns current selected emulator or select it in cbx. }

    property OnSelectEmulator: TEmutecaReturnEmulatorCB
      read FOnSelectEmulator write SetOnSelectEmulator;
    {< Callback when selecting a group. }

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaEmulatorCBX }

procedure TfmEmutecaEmulatorCBX.cbxEmulatorSelect(Sender: TObject);
begin
  if cbxEmulator.ItemIndex <> -1 then
    SelectedEmulator := cEmutecaEmulator(
      cbxEmulator.Items.Objects[cbxEmulator.ItemIndex])
  else
    SelectedEmulator := nil;
end;

procedure TfmEmutecaEmulatorCBX.SetEmulatorList(AValue: cEmutecaEmulatorList);
begin
  if FEmulatorList = AValue then
    Exit;
  FEmulatorList := AValue;

  LoadFrameData;
end;

procedure TfmEmutecaEmulatorCBX.SetOnSelectEmulator(
  AValue: TEmutecaReturnEmulatorCB);
begin
  if FOnSelectEmulator = AValue then
    Exit;
  FOnSelectEmulator := AValue;
end;

procedure TfmEmutecaEmulatorCBX.SetSelectedEmulator(AValue: cEmutecaEmulator);
var
  aPos: integer;
begin
  if FSelectedEmulator = AValue then
    Exit;
  FSelectedEmulator := AValue;

  if not assigned(SelectedEmulator) then
  begin
    cbxEmulator.ItemIndex := -1;
    Exit;
  end;

  aPos := cbxEmulator.Items.IndexOfObject(SelectedEmulator);
  if aPos = -1 then
  begin
    // Uhm.... not exists in list, adding it.
    aPos := cbxEmulator.Items.AddObject(SelectedEmulator.Title,
      SelectedEmulator);
  end;
  // TODO: Test if this creates an infinity loop...
  cbxEmulator.ItemIndex := aPos;

  if Assigned(OnSelectEmulator) then
    {Var := } OnSelectEmulator(SelectedEmulator);
end;

procedure TfmEmutecaEmulatorCBX.DoClearFrameData;
begin
  cbxEmulator.Clear;
end;

procedure TfmEmutecaEmulatorCBX.DoLoadFrameData;
begin
  Enabled := Assigned(EmulatorList);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  cbxEmulator.Clear;
  EmulatorList.AssignToStrLst(cbxEmulator.Items);

  if cbxEmulator.Items.Count = 0 then
  begin
    cbxEmulator.ItemIndex := -1;
    Exit;
  end;

  cbxEmulator.ItemIndex := cbxEmulator.Items.IndexOfObject(SelectedEmulator);
end;

constructor TfmEmutecaEmulatorCBX.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
end;

destructor TfmEmutecaEmulatorCBX.Destroy;
begin
  inherited Destroy;
end;

end.
