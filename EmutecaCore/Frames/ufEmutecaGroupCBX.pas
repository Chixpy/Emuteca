unit ufEmutecaGroupCBX;
{< TfmEmutecaGroupCBX frame unit.

  This file is part of Emuteca Core.

  Copyright (C) 2006-2023 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, ucEmutecaGroupList, ucEmutecaGroup, ufCHXFrame;

type

  { TfmEmutecaGroupCBX }

  // TODO: Think about do it like TfmEmutecaEmulatorCBX

  TfmEmutecaGroupCBX = class(TfmCHXFrame)
    cbxGroup: TComboBox;
    procedure cbxGroupChange(Sender: TObject);

  private
    FGroupList: cEmutecaGroupList;
    FOnSelectGroup: TEmutecaReturnGroupCB;
    FSelectedGroup: cEmutecaGroup;
    procedure SetGroupList(AValue: cEmutecaGroupList);
    procedure SetOnSelectGroup(AValue: TEmutecaReturnGroupCB);
    procedure SetSelectedGroup(AValue: cEmutecaGroup);

  protected
    procedure LoadFrameData; override;

  public

    property GroupList: cEmutecaGroupList read FGroupList write SetGroupList;
    {< List of groups. }

    property SelectedGroup: cEmutecaGroup
      read FSelectedGroup write SetSelectedGroup;
    {< Returns current selected group or select it in cbx. }

    property OnSelectGroup: TEmutecaReturnGroupCB
      read FOnSelectGroup write SetOnSelectGroup;
    {< Callback when selecting a group. }

    procedure ClearFrameData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaGroupCBX }

procedure TfmEmutecaGroupCBX.cbxGroupChange(Sender: TObject);
begin
  // We don't need to call SetSelectedSystem
  if cbxGroup.ItemIndex <> -1 then
    FSelectedGroup := cEmutecaGroup(
      cbxGroup.Items.Objects[cbxGroup.ItemIndex])
  else
    FSelectedGroup := nil;

  if Assigned(OnSelectGroup) then
    {Var := } OnSelectGroup(SelectedGroup);
end;

procedure TfmEmutecaGroupCBX.SetOnSelectGroup(AValue: TEmutecaReturnGroupCB);
begin
  if FOnSelectGroup = AValue then
    Exit;
  FOnSelectGroup := AValue;
end;

procedure TfmEmutecaGroupCBX.SetGroupList(AValue: cEmutecaGroupList);
begin
  if FGroupList = AValue then
    Exit;
  FGroupList := AValue;
  LoadFrameData;
end;

procedure TfmEmutecaGroupCBX.SetSelectedGroup(AValue: cEmutecaGroup);
var
  aPos: integer;
begin
  if FSelectedGroup = AValue then
    Exit;
  FSelectedGroup := AValue;

  if not assigned(SelectedGroup) then
  begin
    cbxGroup.ItemIndex := -1;
    Exit;
  end;

  aPos := cbxGroup.Items.IndexOfObject(SelectedGroup);
  if aPos = -1 then
  begin
    // Uhm....
    aPos := cbxGroup.Items.AddObject(SelectedGroup.Title, SelectedGroup);
  end;
  cbxGroup.ItemIndex := aPos;
end;

procedure TfmEmutecaGroupCBX.ClearFrameData;
begin
  inherited ClearFrameData;

  cbxGroup.Clear;
end;

procedure TfmEmutecaGroupCBX.LoadFrameData;
begin
  inherited LoadFrameData;

  Enabled := Assigned(GroupList);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  cbxGroup.Clear;
  GroupList.AssignToStrLst(cbxGroup.Items);

  if cbxGroup.Items.Count = 0 then
  begin
    cbxGroup.ItemIndex := -1;
    Exit;
  end;

  cbxGroup.ItemIndex := cbxGroup.Items.IndexOfObject(SelectedGroup);
end;

constructor TfmEmutecaGroupCBX.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmEmutecaGroupCBX.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmEmutecaGroupCBX);

finalization
  UnRegisterClass(TfmEmutecaGroupCBX);

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
