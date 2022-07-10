unit ufEmutecaGroupEditor;
{< TfmEmutecaGroupEditor form unit.

  This file is part of Emuteca Core.

  Copyright (C) 2011-2020 Chixpy

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
  // CHX forms
  ufCHXPropEditor,
  // Emuteca Core units
  uEmutecaConst,
  // Emuteca Core classes
  ucEmutecaGroup;

type

  { TfmEmutecaGroupEditor }

  TfmEmutecaGroupEditor = class(TfmCHXPropEditor, IFPObserver)
    eDeveloper: TComboBox;
    eGroupID: TEdit;
    eMediaFile: TEdit;
    eSortTitle: TEdit;
    eTitle: TEdit;
    eYear: TEdit;
  private
    FGroup: cEmutecaGroup;
    procedure SetGroup(AValue: cEmutecaGroup);

  protected
    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);

    class function SimpleModalForm(aGroup: cEmutecaGroup;
      NewTitle, aGUIConfigIni, aGUIIconsIni: string): integer;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Group: cEmutecaGroup read FGroup write SetGroup;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaGroupEditor }

procedure TfmEmutecaGroupEditor.SetGroup(AValue: cEmutecaGroup);
begin
  if FGroup = AValue then
    Exit;

  // Observed
  if Assigned(FGroup) then
    FGroup.FPODetachObserver(Self);

  FGroup := AValue;

  if Assigned(Group) then
    Group.FPOAttachObserver(Self);

  LoadFrameData;
end;

procedure TfmEmutecaGroupEditor.DoClearFrameData;
begin
  eGroupID.Clear;
  eTitle.Clear;
  eSortTitle.Clear;
  // eDeveloper.Clear; We don't want to clear item list.
  eDeveloper.Text := '';
  eYear.Clear;
  eMediaFile.Clear;
end;

procedure TfmEmutecaGroupEditor.DoLoadFrameData;
begin
  Enabled := Assigned(Group);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  eGroupID.Text := Group.ID;
  eTitle.Text := Group.Title;
  eSortTitle.Text := Group.GetActualSortTitle;

  eDeveloper.Text := Group.Developer;
  // Adding to ComboBox List
  if (eDeveloper.ItemIndex = -1) and (Group.Developer <> '') then
    eDeveloper.ItemIndex := eDeveloper.Items.Add(Group.Developer);

  eYear.Text := Group.Date;
  eMediaFile.Text := Group.GetActualMediaFilename;
end;

procedure TfmEmutecaGroupEditor.DoSaveFrameData;
begin
  if not Assigned(Group) then
    Exit;

  Group.ID := eGroupID.Text;
  Group.Title := eTitle.Text;
  Group.SortTitle := eSortTitle.Text;

  Group.Developer := eDeveloper.Text;
  // Adding to ComboBox List
  if (Group.Developer <> '') and
    (eDeveloper.Items.IndexOf(Group.Developer) = -1) then
    eDeveloper.AddItem(Group.Developer, nil);

  Group.Date := eYear.Text;
  Group.MediaFileName := eMediaFile.Text;
end;

procedure TfmEmutecaGroupEditor.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if ASender = Group then
    case Operation of
      ooFree: Group := nil
      else
        ;
    end;
end;

class function TfmEmutecaGroupEditor.SimpleModalForm(aGroup: cEmutecaGroup;
  NewTitle, aGUIConfigIni, aGUIIconsIni: string): integer;
var
  fmGroupEditor: TfmEmutecaGroupEditor;
begin
  if not Assigned(aGroup) then
    Exit;

  fmGroupEditor := TfmEmutecaGroupEditor.Create(nil);
  fmGroupEditor.Group := aGroup;

  fmGroupEditor.ButtonClose := True;
  fmGroupEditor.chkCloseOnSave.Visible := False;

  if NewTitle <> '' then
     fmGroupEditor.eTitle.Text := NewTitle;

  Result := GenSimpleModalForm(fmGroupEditor, 'frmETKGroupEditor',
    Format(krsFmtWindowCaption, [Application.Title, 'Group Editor']),
    aGUIConfigIni, aGUIIconsIni);

  // Autofreed? FreeAndNil(fmGroupEditor);
end;

constructor TfmEmutecaGroupEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmEmutecaGroupEditor.Destroy;
begin
    if Assigned(FGroup) then
    FGroup.FPODetachObserver(Self);

  inherited Destroy;
end;

end.
