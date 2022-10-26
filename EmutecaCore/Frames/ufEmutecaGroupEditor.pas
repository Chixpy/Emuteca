unit ufEmutecaGroupEditor;

{< TfmEmutecaGroupEditor form unit.

  This file is part of Emuteca Core.

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
  StdCtrls, Buttons, ActnList, LazUTF8,
  // CHX units
  uCHXStrUtils,
  // CHX forms
  ufCHXPropEditor,
  // Emuteca Core units
  uEmutecaConst,
  // Emuteca Core classes
  ucEmutecaGroup, ucEmutecaSoftware;

type

  { TfmEmutecaGroupEditor }

  TfmEmutecaGroupEditor = class(TfmCHXPropEditor, IFPObserver)
    chkSortMultigameTitles: TCheckBox;
    eDeveloper: TComboBox;
    eGroupID: TEdit;
    eSortTitle: TEdit;
    eTitle: TEdit;
    eYear: TEdit;
    pDateSort: TPanel;
  private
    FGroup: cEmutecaGroup;
    procedure SetGroup(AValue: cEmutecaGroup);

  protected
    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

    procedure SortMultigame;

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
end;

procedure TfmEmutecaGroupEditor.DoLoadFrameData;
begin
  Enabled := Assigned(Group);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  eGroupID.Text := Group.GetActualID;
  eTitle.Text := Group.GetActualTitle;
  eSortTitle.Text := Group.GetActualSortTitle;

  eDeveloper.Text := Group.Developer;
  // Adding to ComboBox List
  if (eDeveloper.ItemIndex = -1) and (Group.Developer <> '') then
    eDeveloper.ItemIndex := eDeveloper.Items.Add(Group.Developer);

  eYear.Text := Group.Date;
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

  if chkSortMultigameTitles.Checked then
  begin
    SortMultigame;
    chkSortMultigameTitles.Checked := False; // Auto-Uncheck
  end;
end;

procedure TfmEmutecaGroupEditor.SortMultigame;
var
  sID, sTitle, sSortTitle, sLowest: string;
  sOldTitle, sOldSortTitle: string;
  slSortTitle, slTitle, slID, slSorter: TStringList;
  i, iLowest: integer;
  EmptyTitle, EmptySort: boolean;
  aSoft: cEmutecaSoftware;
begin
  if not Assigned(Group) then
    Exit;

  if Pos(' + ', Group.ID) <= 0 then
    Exit;

  // ID can't be empty
  EmptyTitle := Trim(Group.GetActualTitle) = '';
  EmptySort := Trim(Group.GetActualSortTitle) = '';

  sID := UTF8TextReplace(Group.ID, ' + ', '|');
  sTitle := UTF8TextReplace(Group.GetActualTitle, ' + ', '|');
  sSortTitle := UTF8TextReplace(Group.GetActualSortTitle, ' + ', '|');

  slID := TStringList.Create;
  slTitle := TStringList.Create;
  slSortTitle := TStringList.Create;

  slID.Sorted := False;
  slTitle.Sorted := False;
  slSortTitle.Sorted := False;
  slID.CaseSensitive := False;
  slTitle.CaseSensitive := False;
  slSortTitle.CaseSensitive := False;

  slID.AddDelimitedtext(sID, '|', True);
  if not EmptyTitle then
    slTitle.AddDelimitedtext(sTitle, '|', True);
  if not EmptySort then
    slSortTitle.AddDelimitedtext(sSortTitle, '|', True);

  sID := '';
  sTitle := '';
  sSortTitle := '';

  if EmptySort then
    slSorter := slID
  else
    slSorter := slSortTitle;

  if ((not EmptyTitle) and (slSorter.Count <> slTitle.Count)) or
    ((not EmptySort) and (slSorter.Count <> slID.Count)) then
  begin
    // There is not the same number of games in Title or Sortitle than ID
    slSortTitle.Free;
    slTitle.Free;
    slID.Free;

    Exit;
  end;

  while slSorter.Count > 0 do
  begin
    sLowest := slSorter[0];
    iLowest := 0;

    i := 1;
    while i < slSorter.Count do
    begin
      if UTF8CompareText(sLowest, slSorter[i]) > 0 then
      begin
        sLowest := slSorter[i];
        iLowest := i;
      end;

      Inc(i);
    end;

    if sID = '' then
    begin
      sID := slID[iLowest];
      if not EmptyTitle then
        sTitle := slTitle[iLowest];
      if not EmptySort then
        sSortTitle := slSortTitle[iLowest];
    end
    else
    begin
      sID := sID + ' + ' + slID[iLowest];
      if not EmptyTitle then
        sTitle := sTitle + ' + ' + slTitle[iLowest];
      if not EmptySort then
        sSortTitle := sSortTitle + ' + ' + slSortTitle[iLowest];
    end;


    slID.Delete(iLowest);
    if not EmptyTitle then
      slTitle.Delete(iLowest);
    if not EmptySort then
      slSortTitle.Delete(iLowest);
  end;

  // We don't need StringList
  slSortTitle.Free;
  slTitle.Free;
  slID.Free;

  // Keep old data for soft childrens
  sOldTitle := Group.Title; // Title or ID (if empty)
  sOldSortTitle := Group.GetActualSortTitle;

  Group.ID := sID;
  if not EmptyTitle then
    Group.Title := sTitle;
  if not EmptySort then
    Group.SortTitle := sSortTitle;

  i := 0;
  while i < Group.SoftList.Count do
  begin
    aSoft := Group.SoftList[i];

    // if title is empty, copy old group data to keep game order in software
    if aSoft.GetActualTitle = '' then
    begin
      aSoft.Title := sOldTitle;
      aSoft.SortTitle := sOldSortTitle;
    end;

    Inc(i);
  end;

  LoadFrameData;
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
  Result := mrAbort;

  if not Assigned(aGroup) then
    Exit;

  fmGroupEditor := TfmEmutecaGroupEditor.Create(nil);
  fmGroupEditor.Group := aGroup;

  fmGroupEditor.ButtonClose := True;
  fmGroupEditor.chkCloseOnSave.Visible := False;

  if NewTitle <> '' then
    fmGroupEditor.eTitle.Text := UTF8TextReplace(NewTitle, ' - ', ': ');

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
