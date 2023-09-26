unit ufEmutecaSoftEditor;

{< TfmEmutecaSoftEditor form unit.

  This file is part of Emuteca Core.

  Copyright (C) 2011-2023 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls,
  // CHX frames
  ufCHXPropEditor,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr,
  // Emuteca Core classes
  ucEmutecaSystem, ucEmutecaSoftware;

type

  { TfmEmutecaSoftEditor }

  TfmEmutecaSoftEditor = class(TfmCHXPropEditor, IFPObserver)
    cbxDumpType: TComboBox;
    eCracked: TEdit;
    eDumpInfo: TEdit;
    eFixed: TEdit;
    eHack: TEdit;
    eModified: TEdit;
    ePirate: TEdit;
    ePublisher: TComboBox;
    eSortKey: TEdit;
    eTitle: TEdit;
    eTrainer: TEdit;
    eTranslated: TEdit;
    eVersion: TEdit;
    eYear: TEdit;
    eZone: TEdit;
    gbxDumpTags: TGroupBox;
    gbxExtraParameters: TGroupBox;
    gbxTitle: TGroupBox;
    gbxVersion: TGroupBox;
    lCracked: TLabel;
    lDumpInfo: TLabel;
    lDumpType: TLabel;
    lFixed: TLabel;
    lHack: TLabel;
    lModified: TLabel;
    lPirate: TLabel;
    lTrainer: TLabel;
    lTranslated: TLabel;
    mExtraParameters: TMemo;
    pYearZone: TPanel;

  private
    FSoftware: cEmutecaSoftware;
    procedure SetSoftware(AValue: cEmutecaSoftware);

  protected

  public
    procedure ClearFrameData; override;
    procedure LoadFrameData; override;
    procedure SaveFrameData; override;

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaSoftEditor }

procedure TfmEmutecaSoftEditor.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;

  // Observed
  if Assigned(FSoftware) then
    FSoftware.FPODetachObserver(Self);

  FSoftware := AValue;

  if Assigned(Software) then
    Software.FPOAttachObserver(Self);

  LoadFrameData;
end;

procedure TfmEmutecaSoftEditor.ClearFrameData;
begin
  inherited ClearFrameData;

  eTitle.Clear;
  eSortKey.Clear;

  eVersion.Clear;
  eYear.Clear;
  // ePublisher.Clear; We don't want to clear item list.
  ePublisher.Text := '';
  eZone.Clear;

  // cbxDumpType.Clear; We want keep DumpType list.
  cbxDumpType.Text := '';
  eDumpInfo.Clear;

  eFixed.Clear;
  eTrainer.Clear;
  eTranslated.Clear;
  ePirate.Clear;
  eCracked.Clear;
  eModified.Clear;
  eHack.Clear;

  mExtraParameters.Clear;
end;

procedure TfmEmutecaSoftEditor.LoadFrameData;
var
  i: integer;
begin
  inherited LoadFrameData;

  Enabled := assigned(Software);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  eTitle.Text := Software.GetActualTitle;
  eSortKey.Text := Software.GetActualSortTitle;

  eVersion.Text := Software.Version;
  eYear.Text := Software.Date;

  i := ePublisher.Items.IndexOf(Software.Publisher);
  if i >= 0 then
    ePublisher.ItemIndex := i
  else if Software.Publisher <> '' then // Adding to ComboBox List
    ePublisher.ItemIndex := ePublisher.Items.Add(Software.Publisher)
  else
    ePublisher.Text := '';

  eZone.Text := Software.Zone;

  cbxDumpType.ItemIndex := Ord(Software.DumpStatus);

  eDumpInfo.Text := Software.DumpInfo;

  eFixed.Text := Software.Fixed;
  eTrainer.Text := Software.Trainer;
  eTranslated.Text := Software.Translation;
  ePirate.Text := Software.Pirate;
  eCracked.Text := Software.Cracked;
  eModified.Text := Software.Modified;
  eHack.Text := Software.Hack;
  mExtraParameters.Lines.Assign(Software.ExtraParameters);

end;

procedure TfmEmutecaSoftEditor.SaveFrameData;
begin
  inherited SaveFrameData;

  if not assigned(Software) then
    Exit;

  Software.Title := eTitle.Text;
  Software.SortTitle := eSortKey.Text;

  Software.Version := eVersion.Text;
  Software.Date := eYear.Text;

  Software.Publisher := ePublisher.Text;
  // Adding to ComboBox List
  if (Software.Publisher <> '') and
    (ePublisher.Items.IndexOf(Software.Publisher) = -1) then
    ePublisher.AddItem(Software.Publisher, nil);

  Software.Zone := eZone.Text;

  if TEmutecaDumpStatus(cbxDumpType.ItemIndex) <> edsKeepValue then
    Software.DumpStatus := TEmutecaDumpStatus(cbxDumpType.ItemIndex);

  Software.DumpInfo := eDumpInfo.Text;

  Software.Fixed := eFixed.Text;
  Software.Trainer := eTrainer.Text;
  Software.Translation := eTranslated.Text;
  Software.Pirate := ePirate.Text;
  Software.Cracked := eCracked.Text;
  Software.Modified := eModified.Text;
  Software.Hack := eHack.Text;
  Software.ExtraParameters.Assign(mExtraParameters.Lines);

  // Load automatic changes: i.e. changing ': ' to ' - ' in SortTitle
  LoadFrameData;
end;

procedure TfmEmutecaSoftEditor.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if ASender = Software then
    case Operation of
      ooFree: Software := nil
      else
        ;
    end;
end;

constructor TfmEmutecaSoftEditor.Create(TheOwner: TComponent);
var
  i: string;
begin
  inherited Create(TheOwner);

  // Adding DumpTypes
  for i in EmutecaDumpStatusStr do
    cbxDumpType.AddItem(i, nil);
  cbxDumpType.ItemIndex := 1;
end;

destructor TfmEmutecaSoftEditor.Destroy;
begin
  if Assigned(Software) then
    Software.FPODetachObserver(Self);

  inherited Destroy;
end;

initialization
  RegisterClass(TfmEmutecaSoftEditor);

finalization
  UnRegisterClass(TfmEmutecaSoftEditor);
  
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
