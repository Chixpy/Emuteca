unit ufETKGUICompareSG;

{< TfmETKGUICompareSG frame unit.

  This file is part of Emuteca GUI.

  Copyright (C) 2023 Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ActnList,
  // CHX frames
  ufCHXPropEditor,
  // Emuteca Core units
  uEmutecaConst,
  // Emuteca Core abstracts
  uaEmutecaCustomSGItem,
  // Emuteca Core clases
  ucEmutecaSoftware, ucEmutecaGroup,
  // Emuteca GUI frames
  ufETKGUIFullSoftEditor;

resourcestring
  rsETKGUICSGGroupMerged = '!!! Please use "System/Update group list" in main menu.';
  rsETKGUICSGGroup = 'Soft Count: %0:d';

type

  { TfmETKGUICompareSG }

  TfmETKGUICompareSG = class(TfmCHXPropEditor)
    actG2ToG1: TAction;
    actG1ToG2: TAction;
    actMergeToG2: TAction;
    actMergeToG1: TAction;
    bG1ToG2: TButton;
    bG2ToG1: TButton;
    bMergeToG1: TButton;
    bMergeToG2: TButton;
    lSoftCount1: TLabel;
    lSoftCount2: TLabel;
    lWarning: TLabel;
    pLeft: TPanel;
    pLeftUp: TPanel;
    pRight: TPanel;
    pRightUp: TPanel;
    sbxMain: TPanel;
    Splitter1: TSplitter;
    procedure actG1ToG2Execute(Sender: TObject);
    procedure actG2ToG1Execute(Sender: TObject);
    procedure actMergeToG1Execute(Sender: TObject);
    procedure actMergeToG2Execute(Sender: TObject);
  private
    FfmSGEditorLeft: TfmETKGUIFullSoftEditor;
    FfmSGEditorRight: TfmETKGUIFullSoftEditor;
    FSGLeft: caEmutecaCustomSGItem;
    FSGRight: caEmutecaCustomSGItem;
    procedure SetSGLeft(AValue: caEmutecaCustomSGItem);
    procedure SetSGRight(AValue: caEmutecaCustomSGItem);

  protected
    property fmSGEditorLeft: TfmETKGUIFullSoftEditor read FfmSGEditorLeft;
    property fmSGEditorRight: TfmETKGUIFullSoftEditor read FfmSGEditorRight;

    procedure MergeGroups(SourceGroup, TargetGroup: cEmutecaGroup);

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure ClearFrameData; override;
    procedure LoadFrameData; override;

    class function SimpleModalForm(aSG1, aSG2: caEmutecaCustomSGItem;
      aGUIConfigIni, aGUIIconsIni: string): integer;

  published
    property SGLeft: caEmutecaCustomSGItem read FSGLeft write SetSGLeft;
    property SGRight: caEmutecaCustomSGItem read FSGRight write SetSGRight;

  end;

implementation

{$R *.lfm}

{ TfmETKGUICompareSG }

procedure TfmETKGUICompareSG.actG2ToG1Execute(Sender: TObject);
begin
  fmSGEditorLeft.ChangeSoftGroup(fmSGEditorRight.Group);
end;

procedure TfmETKGUICompareSG.actMergeToG1Execute(Sender: TObject);
var
  aGroup: cEmutecaGroup;
begin
  aGroup := fmSGEditorRight.Group;
  MergeGroups(aGroup, fmSGEditorLeft.Group);

  // Reseting Right Panel
  aGroup.Title := rsETKGUICSGGroupMerged;
  aGroup.SortTitle := '';
  fmSGEditorRight.Group := nil;
  fmSGEditorRight.Group := aGroup;
end;

procedure TfmETKGUICompareSG.actMergeToG2Execute(Sender: TObject);
var
  aGroup: cEmutecaGroup;
begin
  aGroup := fmSGEditorLeft.Group;

  MergeGroups(aGroup, fmSGEditorRight.Group);

  // Reseting Left Panel
  aGroup.Title := rsETKGUICSGGroupMerged;
  aGroup.SortTitle := '';
  fmSGEditorLeft.Group := nil;
  fmSGEditorLeft.Group := aGroup;
end;

procedure TfmETKGUICompareSG.actG1ToG2Execute(Sender: TObject);
begin
  fmSGEditorRight.ChangeSoftGroup(fmSGEditorLeft.Group);
end;

procedure TfmETKGUICompareSG.SetSGLeft(AValue: caEmutecaCustomSGItem);
begin
  if FSGLeft = AValue then Exit;
  FSGLeft := AValue;

  LoadFrameData;
end;

procedure TfmETKGUICompareSG.SetSGRight(AValue: caEmutecaCustomSGItem);
begin
  if FSGRight = AValue then Exit;
  FSGRight := AValue;

  LoadFrameData;
end;

procedure TfmETKGUICompareSG.ClearFrameData;
begin
  inherited ClearFrameData;

  fmSGEditorLeft.Group := nil;
  fmSGEditorRight.Group := nil;
end;

procedure TfmETKGUICompareSG.LoadFrameData;

  procedure AssignSGItem(aFrame: TfmETKGUIFullSoftEditor;
    aSG: caEmutecaCustomSGItem; CountLabel: TLabel);
  var
    aSoft: cEmutecaSoftware;
    aGroup: cEmutecaGroup;
    i: integer;
  begin
    if aSG is cEmutecaGroup then
    begin
      aGroup := cEmutecaGroup(aSG);
      CountLabel.Caption := Format(rsETKGUICSGGroup, [aGroup.SoftList.Count]);

      // Displaying a example soft of the group with same filename
      i := 0;
      aSoft := nil;
      while i < aGroup.SoftList.Count do
      begin
        aSoft := aGroup.SoftList[i];

        if aSoft.MatchGroupFile then
          i := aGroup.SoftList.Count; // Exit loop

        Inc(i);
      end;

      if i = aGroup.SoftList.Count then aSoft := nil;

      if assigned(aSoft) then
        aFrame.Software := aSoft
      else
        aFrame.Group := aGroup;
    end
    else if aSG is cEmutecaSoftware then
    begin
      aSoft := cEmutecaSoftware(aSG);
      aGroup := cEmutecaGroup(aSoft.CachedGroup);
      CountLabel.Caption := Format(rsETKGUICSGGroup, [aGroup.SoftList.Count]);
      aFrame.Software := aSoft;
    end
    else
      Exit;
  end;

begin
  inherited LoadFrameData;

  Enabled := Assigned(SGLeft) and Assigned(SGRight) and (SGLeft <> SGRight);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  AssignSGItem(fmSGEditorLeft, SGLeft, lSoftCount1);
  bG2ToG1.Enabled := SGLeft is cEmutecaSoftware;
  // fmSGEditorLeft.fmSoftEditor.Enabled := SGLeft is cEmutecaSoftware;

  AssignSGItem(fmSGEditorRight, SGRight, lSoftCount2);
  bG1ToG2.Enabled := SGRight is cEmutecaSoftware;
  // fmSGEditorRight.fmSoftEditor.Enabled := SGRight is cEmutecaSoftware;
end;

procedure TfmETKGUICompareSG.MergeGroups(SourceGroup, TargetGroup: cEmutecaGroup
  );
var
  i: Integer;
  aSoft: cEmutecaSoftware;
begin
  i := SourceGroup.SoftList.Count - 1;
  while i >= 0 do
  begin
    aSoft := SourceGroup.SoftList[i];

    aSoft.CachedGroup := TargetGroup;

    Dec(i);
  end;
end;

constructor TfmETKGUICompareSG.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FfmSGEditorLeft := TfmETKGUIFullSoftEditor.Create(pLeft);
    fmSGEditorLeft.Align := alClient;
    // fmGroupEditor.SaveButtons := False;
    // fmGroupEditor.ButtonClose := False;
    fmSGEditorLeft.Parent := pLeft;

    FfmSGEditorRight := TfmETKGUIFullSoftEditor.Create(pRight);
    fmSGEditorRight.Align := alClient;
    // fmGroupEditor.SaveButtons := False;
    // fmGroupEditor.ButtonClose := False;
    fmSGEditorRight.Parent := pRight;
  end;

begin
  inherited Create(TheOwner);

  CreateFrames;
end;

destructor TfmETKGUICompareSG.Destroy;
begin
  inherited Destroy;
end;

class function TfmETKGUICompareSG.SimpleModalForm(aSG1, aSG2:
  caEmutecaCustomSGItem; aGUIConfigIni, aGUIIconsIni: string): integer;
var
  fmGUICompareSG: TfmETKGUICompareSG;
begin
  Result := mrNone;

  if (not Assigned(aSG1)) or (not Assigned(aSG2)) then
    Exit;

  fmGUICompareSG := TfmETKGUICompareSG.Create(nil);
  fmGUICompareSG.SGLeft := aSG1;
  fmGUICompareSG.SGRight := aSG2;

  fmGUICompareSG.ButtonClose := True;
  fmGUICompareSG.chkCloseOnSave.Visible := False;

  Result := GenSimpleModalForm(fmGUICompareSG, 'frmGUICompareSG',
    Format(krsFmtWindowCaption, [Application.Title, 'Comparing Soft / Group']),
    aGUIConfigIni, aGUIIconsIni);
end;

initialization
  RegisterClass(TfmETKGUICompareSG);

finalization
  UnRegisterClass(TfmETKGUICompareSG);
  
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
