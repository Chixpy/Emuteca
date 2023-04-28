unit ufETKGUIFullSoftEditor;

{< TfmETKGUIFullSoftEditor frame unit.

  This file is part of Emuteca GUI.

  Copyright (C) 2011-2022 Chixpy

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList,
  // CHX units
  uCHXStrUtils,
  // CHX frames
  ufCHXPropEditor,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr,
  // Emuteca Core classes
  ucEmutecaSystem, ucEmutecaGroup, ucEmutecaSoftware,
  // Emuteca Core frames
  ufEmutecaGroupCBX, ufEmutecaGroupEditor, ufEmutecaSoftEditor;

type

  { TfmETKGUIFullSoftEditor }

  TfmETKGUIFullSoftEditor = class(TfmCHXPropEditor)
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

    procedure SelectGroup(aGroup: cEmutecaGroup);

    // procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public
    procedure ChangeSoftGroup(aGroup: cEmutecaGroup);
    {< Changes the group of the soft.

      Change is not saved until OK button is pressed (or cascade saved);
    }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    class function SimpleModalForm(aSoft: cEmutecaSoftware;
      NewTitle, aGUIConfigIni, aGUIIconsIni: string): integer;

  published
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
    {< Current software displayed.

       If assigned, Group property will be assigned to software's group.
    }
    property Group: cEmutecaGroup read FGroup write SetGroup;
    {< Current group displayed, or Current software's group.

       If assigned, Software property will be @nil.
    }
  end;

implementation

{$R *.lfm}

{ TfmETKGUIFullSoftEditor }

procedure TfmETKGUIFullSoftEditor.actAddNewGroupExecute(Sender: TObject);
var
  GroupTitle: string;
  aGroup: cEmutecaGroup;
  aSystem: cEmutecaSystem;
begin
  if not Assigned(Software) then
    Exit;
  if not Assigned(Software.CachedSystem) then
    Exit;

  fmGroupCBX.GroupList := nil;

  GroupTitle := Software.Title;

  if InputQuery('New group', 'Name of the new group.', GroupTitle) = False then
    Exit;

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

procedure TfmETKGUIFullSoftEditor.SetGroup(AValue: cEmutecaGroup);
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

procedure TfmETKGUIFullSoftEditor.SetSoftware(AValue: cEmutecaSoftware);
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

procedure TfmETKGUIFullSoftEditor.SelectGroup(aGroup: cEmutecaGroup);
begin
  fmGroupEditor.Group := aGroup;
end;

procedure TfmETKGUIFullSoftEditor.DoLoadFrameData;
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

procedure TfmETKGUIFullSoftEditor.DoSaveFrameData;
var
  OldTitle, OldSortKey: string;
begin
  if Assigned(Software) and fmGroupEditor.chkSortMultigameTitles.Checked then
    fmSoftEditor.SaveFrameData;

  fmGroupEditor.SaveFrameData;

  if Assigned(Software) then
  begin
    // Loading if title in group was sorted.
    if fmGroupEditor.chkSortMultigameTitles.Checked then
      fmSoftEditor.LoadFrameData;

    fmSoftEditor.SaveFrameData;

    // If group is changed assign it to software; and keep Title if empty.
    if fmGroupEditor.Group <> Software.CachedGroup then
    begin
      OldTitle := '';
      OldSortKey := '';

      // If empty, keep old title
      if Software.GetActualTitle = '' then
      begin
        OldTitle := Software.CachedGroup.Title;
        OldSortKey := Software.CachedGroup.GetActualSortTitle
      end;

      // Assign new group
      Software.CachedGroup := fmGroupEditor.Group;

      // Restore old title and sort
      if OldTitle <> '' then
      begin
         Software.Title := OldTitle;
         Software.SortTitle := OldSortKey;
      end;
    end;

    // Load automatic changes: i.e. changing ': ' to ' - ' in SortTitle
    fmSoftEditor.LoadFrameData;
  end;

  // Load automatic changes: i.e. changing ': ' to ' - ' in SortTitle
  fmGroupEditor.LoadFrameData;

end;

procedure TfmETKGUIFullSoftEditor.ChangeSoftGroup(aGroup: cEmutecaGroup);
begin
  fmGroupCBX.SelectedGroup := aGroup;
  SelectGroup(aGroup);
end;

constructor TfmETKGUIFullSoftEditor.Create(TheOwner: TComponent);

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

destructor TfmETKGUIFullSoftEditor.Destroy;
begin
  inherited Destroy;
end;

class function TfmETKGUIFullSoftEditor.SimpleModalForm(aSoft: cEmutecaSoftware;
  NewTitle, aGUIConfigIni, aGUIIconsIni: string): integer;
var
  fmFullSoftEditor: TfmETKGUIFullSoftEditor;
begin
  Result := mrNone;

  if not Assigned(aSoft) then
    Exit;

  fmFullSoftEditor := TfmETKGUIFullSoftEditor.Create(nil);
  fmFullSoftEditor.Software := aSoft;

  fmFullSoftEditor.ButtonClose := True;
  fmFullSoftEditor.chkCloseOnSave.Visible := False;


  if NewTitle <> '' then
    fmFullSoftEditor.fmSoftEditor.eTitle.Text :=
      UTF8TextReplace(NewTitle, ' - ', ': ');

  Result := GenSimpleModalForm(fmFullSoftEditor, 'frmETKGUIFullSoftEditor',
    Format(krsFmtWindowCaption, [Application.Title, 'Software Editor']),
    aGUIConfigIni, aGUIIconsIni);

  // Autofreed? FreeAndNil(fmGroupEditor);
end;

end.
