unit ufEmutecaSystemCBX;
{< TfmEmutecaEmulatorCBX frame unit.

  This file is part of Emuteca Core.

  Copyright (C) 2011-2023 Chixpy

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  // CHX frames
  ufCHXFrame,
  // Emuteca units
  uETKGUIRscStr,
  // Emuteca Core classes
  ucEmutecaSystemList, ucEmutecaSystem;

type
  TETKSysCBXFirstItem = (ETKSysCBXFINone, ETKSysCBXFISelect, ETKSysCBXFIAll);

  { TfmEmutecaSystemCBX }

  TfmEmutecaSystemCBX = class(TfmCHXFrame)
    cbxSystem: TComboBox;
    procedure cbxSystemChange(Sender: TObject);

  private
    FFirstItem: TETKSysCBXFirstItem;
    FOnSelectSystem: TEmutecaReturnSystemCB;
    FSelectedSystem: cEmutecaSystem;
    FSystemList: cEmutecaSystemList;
    procedure SetFirstItem(AValue: TETKSysCBXFirstItem);
    procedure SetOnSelectSystem(AValue: TEmutecaReturnSystemCB);
    procedure SetSelectedSystem(AValue: cEmutecaSystem);
    procedure SetSystemList(AValue: cEmutecaSystemList);

  protected
    procedure DoClearFrameData;
    procedure DoLoadFrameData;

  public
    property SystemList: cEmutecaSystemList
      read FSystemList write SetSystemList;
    property SelectedSystem: cEmutecaSystem
      read FSelectedSystem write SetSelectedSystem;
    property OnSelectSystem: TEmutecaReturnSystemCB
      read FOnSelectSystem write SetOnSelectSystem;

    property FirstItem: TETKSysCBXFirstItem read FFirstItem write SetFirstItem;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaSystemCBX }

procedure TfmEmutecaSystemCBX.cbxSystemChange(Sender: TObject);
begin
  // We don't need to call SetSelectedSystem
  if cbxSystem.ItemIndex <> -1 then
    FSelectedSystem := cEmutecaSystem(
      cbxSystem.Items.Objects[cbxSystem.ItemIndex])
  else
    FSelectedSystem := nil;

  if Assigned(OnSelectSystem) then
    {Var := } OnSelectSystem(SelectedSystem);
end;

procedure TfmEmutecaSystemCBX.SetOnSelectSystem(AValue:
  TEmutecaReturnSystemCB);
begin
  if FOnSelectSystem = AValue then
    Exit;
  FOnSelectSystem := AValue;
end;

procedure TfmEmutecaSystemCBX.SetFirstItem(AValue: TETKSysCBXFirstItem);
begin
  if FFirstItem = AValue then
    Exit;
  FFirstItem := AValue;

  if cbxSystem.Items.Count = 0 then
    Exit;

  if not assigned(cbxSystem.Items.Objects[0]) then
  begin  // Already have rsSelectSystem or rsAllSystems
    case FirstItem of
      ETKSysCBXFISelect: cbxSystem.Items[0] := rsSelectSystem;
      ETKSysCBXFIAll: cbxSystem.Items[0] := rsAllSystems;
      else
        cbxSystem.Items.Delete(0);
    end;
  end
  else
  begin // Don't have rsSelectSystem or rsAllSystems
    case FirstItem of
      ETKSysCBXFISelect: cbxSystem.Items.Insert(0, rsSelectSystem);
      ETKSysCBXFIAll: cbxSystem.Items.Insert(0, rsAllSystems);
      else
        ;
    end;
  end;
end;

procedure TfmEmutecaSystemCBX.SetSelectedSystem(AValue: cEmutecaSystem);
var
  aPos: integer;
begin
  if FSelectedSystem = AValue then
    Exit;
  FSelectedSystem := AValue;

  if not assigned(SelectedSystem) then
  begin
    if cbxSystem.Items.Count = 0 then Exit;

    cbxSystem.ItemIndex := 0;
    if FirstItem = ETKSysCBXFINone then
    begin
      FSelectedSystem := cEmutecaSystem(cbxSystem.Items.Objects[0]);
    end;
    Exit;
  end;

  aPos := cbxSystem.Items.IndexOfObject(SelectedSystem);

  if aPos = -1 then
  begin
    // Uhm....
    aPos := cbxSystem.Items.AddObject(SelectedSystem.Title, SelectedSystem);
  end;

  cbxSystem.ItemIndex := aPos;
end;

procedure TfmEmutecaSystemCBX.SetSystemList(AValue: cEmutecaSystemList);
begin
  if FSystemList = AValue then
    Exit;
  FSystemList := AValue;

  LoadFrameData;
end;

procedure TfmEmutecaSystemCBX.DoClearFrameData;
begin
  cbxSystem.Clear;
end;

procedure TfmEmutecaSystemCBX.DoLoadFrameData;
begin
  Enabled := Assigned(SystemList);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  cbxSystem.Clear;
  SystemList.AssignToStrLst(cbxSystem.Items);

  if cbxSystem.Items.Count = 0 then
  begin
    cbxSystem.ItemIndex := -1;
    Exit;
    end;

  case FirstItem of
    ETKSysCBXFISelect: cbxSystem.Items.Insert(0, rsSelectSystem);
    ETKSysCBXFIAll: cbxSystem.Items.Insert(0, rsAllSystems);
    else
      ;
  end;

  cbxSystem.ItemIndex := cbxSystem.Items.IndexOfObject(SelectedSystem);
end;

constructor TfmEmutecaSystemCBX.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmEmutecaSystemCBX.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmEmutecaSystemCBX);

finalization
  UnRegisterClass(TfmEmutecaSystemCBX);
end.
