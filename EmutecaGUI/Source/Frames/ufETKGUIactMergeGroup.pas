unit ufETKGUIactMergeGroup;
{< TfmEEGUIactMergeGroup frame unit of Emuteca GUI.

  This file is part of Emuteca GUI.

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList, EditBtn, CheckLst,
  // CHX forms
  ufrCHXForm,
  // CHX frames
  ufCHXPropEditor,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr,
  // Emuteca Core classes
  ucEmutecaGroup, ucEmutecaSoftware;

type

  { TfmETKGUIactMergeGroup }

  TfmETKGUIactMergeGroup = class(TfmCHXPropEditor)
    chkAddTargetToList: TCheckBox;
    chkDeleteFiles: TCheckBox;
    chkRemoveSource: TCheckBox;
    chxIgnoreRedundant: TCheckBox;
    clbSourceFiles: TCheckListBox;
    eTargetFolder: TDirectoryEdit;
    gbxSource: TGroupBox;
    gbxTarget: TGroupBox;
    lTargetFolder: TLabel;
    rgpFormat: TRadioGroup;
    procedure chkRemoveSourceChange(Sender: TObject);
  private
    FGroup: cEmutecaGroup;
    procedure SetGroup(AValue: cEmutecaGroup);

  protected

  public
    property Group: cEmutecaGroup read FGroup write SetGroup;

    procedure LoadFrameData; override;
    procedure SaveFrameData; override;
      procedure ClearFrameData; override;

    // Creates a form with AddSoft frame.
    class function SimpleForm(aGroup: cEmutecaGroup; aGUIIconsIni: string;
      aGUIConfigIni: string): integer;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmETKGUIactMergeGroup }

procedure TfmETKGUIactMergeGroup.chkRemoveSourceChange(Sender: TObject);
begin
  if chkRemoveSource.Checked then
  begin
    chkDeleteFiles.Enabled := True;
  end
  else
  begin
    chkDeleteFiles.Checked := False;
    chkDeleteFiles.Enabled := False;
  end;
end;

procedure TfmETKGUIactMergeGroup.SetGroup(AValue: cEmutecaGroup);
begin
  if FGroup=AValue then Exit;
  FGroup:=AValue;

  LoadFrameData;
end;

procedure TfmETKGUIactMergeGroup.LoadFrameData;
var
  i: Integer;
  aSoft: cEmutecaSoftware;
begin
  inherited LoadFrameData;

  Enabled := Assigned(Group);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  i := 0;
  while i < Group.SoftList.Count do
  begin
    aSoft := Group.SoftList[i];
    clbSourceFiles.AddItem(aSoft.Folder + aSoft.FileName, aSoft);
    inc(i);
  end;

  // Checking all
  clbSourceFiles.CheckAll(cbChecked, False, True);

  // Have all soft cached its SHA1? Or another IDType?
  chxIgnoreRedundant.Enabled := Group.IsSoftSHA1Cached in [0,-2];
end;

procedure TfmETKGUIactMergeGroup.SaveFrameData;
var
  i: integer;
begin
  inherited SaveFrameData;

  if eTargetFolder.Directory = '' then
  begin
    ShowMessage('No output folder selected.');
    Exit;
  end;

  // Removing unchecked from list
  i := clbSourceFiles.Count;
  while i > 0 do
  begin
    Dec(i);
    if not clbSourceFiles.Checked[i] then
      clbSourceFiles.Items.Delete(i);
  end;



  i := 0;
  while i < clbSourceFiles.Count do
  begin
    // Extract/Copy Files to Temp folder

    Etc.

    // Remove Sourcefiles (If inside a zip/7z remove it too)
  end;
end;

procedure TfmETKGUIactMergeGroup.ClearFrameData;
begin
  inherited ClearFrameData;

  clbSourceFiles.Clear;
end;

class function TfmETKGUIactMergeGroup.SimpleForm(aGroup: cEmutecaGroup;
  aGUIIconsIni: string; aGUIConfigIni: string): integer;
var
  aFrame: TfmETKGUIactMergeGroup;
begin
  Result := mrNone;

  aFrame := TfmETKGUIactMergeGroup.Create(nil);
  aFrame.SaveButtons := True;
  aFrame.ButtonClose := True;
  aFrame.Align := alClient;

  aFrame.Group := aGroup;

  Result := GenSimpleModalForm(aFrame, 'frmETKGUIactMergeGroup',
    Format(krsFmtWindowCaption, [Application.Title, 'Merge group files...']),
    aGUIConfigIni, aGUIIconsIni);
end;

constructor TfmETKGUIactMergeGroup.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmETKGUIactMergeGroup.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmETKGUIactMergeGroup);

finalization
  UnRegisterClass(TfmETKGUIactMergeGroup);
end.

