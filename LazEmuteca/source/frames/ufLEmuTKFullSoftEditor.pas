{ Full software editor of LazEmuteca.

  Copyright (C) 2011-2018 Chixpy

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
unit ufLEmuTKFullSoftEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList,
  ufCHXPropEditor,
  ucEmutecaSystem, ucEmutecaGroup, ucEmutecaSoftware,
  ufEmutecaGroupCBX, ufEmutecaGroupEditor, ufEmutecaSoftEditor,
  uLEmuTKCommon;

type

  { TfmLEmuTKFullSoftEditor }

  TfmLEmuTKFullSoftEditor = class(TfmCHXPropEditor)
    actAddNewGroup: TAction;
    bAddNewGroup: TSpeedButton;
    gbxGroup: TGroupBox;
    gbxSoft: TGroupBox;
    pGroupCBX: TPanel;
    pGroupEditor: TPanel;
    ScrollBox: TScrollBox;
    procedure actAddNewGroupExecute(Sender: TObject);

  private
    FfmGroupCBX: TfmEmutecaGroupCBX;
    FfmGroupEditor: TfmEmutecaGroupEditor;
    FfmSoftEditor: TfmEmutecaSoftEditor;
    FGroup: cEmutecaGroup;
    FSoftware: cEmutecaSoftware;
    procedure SetGroup(AValue: cEmutecaGroup);
    procedure SetSoftware(AValue: cEmutecaSoftware);

  protected
    property fmGroupCBX: TfmEmutecaGroupCBX read FfmGroupCBX;
    property fmGroupEditor: TfmEmutecaGroupEditor read FfmGroupEditor;
    property fmSoftEditor: TfmEmutecaSoftEditor read FfmSoftEditor;

    function SelectGroup(aGroup: cEmutecaGroup): boolean;

    // procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
    property Group: cEmutecaGroup read FGroup write SetGroup;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKFullSoftEditor }

procedure TfmLEmuTKFullSoftEditor.actAddNewGroupExecute(Sender: TObject);
var
  GroupTitle: string;
  aGroup: cEmutecaGroup;
  aSystem: cEmutecaSystem;
begin
  if not Assigned(Software) then Exit;
  if not Assigned(Software.CachedSystem) then Exit;

  fmGroupCBX.GroupList := nil;

  GroupTitle := Software.Title;

  if InputQuery('New group', 'Name of the new group.', GroupTitle) = false then Exit;

  aGroup := cEmutecaGroup.Create(nil);
  aGroup.ID := GroupTitle;
  aGroup.Title := GroupTitle;

  aSystem := cEmutecaSystem(Software.CachedSystem);

  aSystem.AddGroup(aGroup);

  fmGroupCBX.GroupList := aSystem.GroupManager.FullList;
  fmGroupCBX.SelectedGroup := aGroup;

  Software.CachedGroup := aGroup;
  fmGroupEditor.Group := aGroup;
end;

procedure TfmLEmuTKFullSoftEditor.SetGroup(AValue: cEmutecaGroup);
begin
  if FGroup = AValue then
    Exit;
  FGroup := AValue;
  FSoftware := nil;

  fmSoftEditor.Software := nil;
  fmGroupEditor.Group := Group;

  if Assigned(Group) then
  begin
    fmGroupCBX.GroupList :=
      cEmutecaSystem(Group.CachedSystem).GroupManager.FullList;
    fmGroupCBX.SelectedGroup := Group;
  end
  else
    fmGroupCBX.GroupList := nil;

  LoadFrameData;
end;

procedure TfmLEmuTKFullSoftEditor.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;
  FSoftware := AValue;
  FGroup := nil;

  fmSoftEditor.Software := Software;

  if Assigned(Software) then
  begin
    fmGroupCBX.GroupList :=
      cEmutecaSystem(Software.CachedSystem).GroupManager.FullList;

    if Assigned(Software.CachedGroup) then
    begin
      FGroup := cEmutecaGroup(Software.CachedGroup);
      fmGroupEditor.Group := Group;
      fmGroupCBX.SelectedGroup := Group;
    end
    else
    begin
      fmGroupEditor.Group := nil;
    end;
  end
  else
  begin
    fmGroupCBX.GroupList := nil;
  end;

  LoadFrameData;
end;

function TfmLEmuTKFullSoftEditor.SelectGroup(aGroup: cEmutecaGroup): boolean;
begin
  Result := True;
  if assigned(Software) then
    Software.CachedGroup := aGroup;
  fmGroupEditor.Group := aGroup;
end;

procedure TfmLEmuTKFullSoftEditor.DoLoadFrameData;
begin
  Enabled := Assigned(Software) or Assigned(Group);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  pGroupCBX.Enabled := Assigned(Software);
  gbxSoft.Enabled := Assigned(Software);
end;

procedure TfmLEmuTKFullSoftEditor.DoSaveFrameData;
begin
  if Assigned(Group) then
    fmGroupEditor.SaveFrameData;

  if Assigned(Software) then
    fmSoftEditor.SaveFrameData;
end;

constructor TfmLEmuTKFullSoftEditor.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FfmGroupCBX := TfmEmutecaGroupCBX.Create(pGroupCBX);
    fmGroupCBX.Align := alClient;
    fmGroupCBX.OnSelectGroup := @SelectGroup;
    fmGroupCBX.Parent := pGroupCBX;

    FfmGroupEditor := TfmEmutecaGroupEditor.Create(pGroupEditor);
    fmGroupEditor.Align := alTop;
    fmGroupEditor.SaveButtons := False;
    fmGroupEditor.ButtonClose := False;
    fmGroupEditor.Parent := pGroupEditor;

    FfmSoftEditor := TfmEmutecaSoftEditor.Create(gbxSoft);
    fmSoftEditor.Align := alTop;
    fmSoftEditor.SaveButtons := False;
    fmSoftEditor.ButtonClose := False;
    fmSoftEditor.Parent := gbxSoft;
end;

begin
  inherited Create(TheOwner);

  CreateFrames;

  // OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmLEmuTKFullSoftEditor.Destroy;
begin
  inherited Destroy;
end;

end.
