unit ufEMCImagePropEditor;

{< ETK Magazine Cutter properties editor frame.

  This file is part of ETK Magazine.

  Copyright (C) 2022 Chixpy

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
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  EditBtn, Spin, LazFileUtils,
  // CHX units
  uCHXStrUtils,
  // CHX frames
  ufCHXFrame,
  // EMC classes
  ucEMCConfig;

type
  TEMCOnSaveObjCB = procedure(aFilename: string;
    Resize2048: boolean) of object;
  TEMCOnProcedureObjCB = procedure() of object;
  TEMCProcRectObjCB = procedure(aRect: TRect) of object;
  TEMCOnPointModeChange = procedure(PMEnabled: boolean) of object;

  { TfmEMCImagePropEditor }

  TfmEMCImagePropEditor = class(TfmCHXFrame)
    bClearSelection: TButton;
    bDelete: TButton;
    bJoinNextFile: TButton;
    bSave: TButton;
    bSaveAndDelete: TButton;
    cbxGame: TComboBox;
    cbxLanguage: TComboBox;
    cbxMagazine: TComboBox;
    cbxSection: TComboBox;
    cbxSystem: TComboBox;
    cbxType: TComboBox;
    chkMultipage: TCheckBox;
    chkPointMode: TCheckBox;
    chkResize2048: TCheckBox;
    eBaseOutFolder: TDirectoryEdit;
    eHeight: TSpinEdit;
    eFilename: TEdit;
    eLeft: TSpinEdit;
    eOutputFolder: TEdit;
    eIssue: TEdit;
    eOutputFile: TEdit;
    ePMBorder: TSpinEdit;
    eWidth: TSpinEdit;
    eTop: TSpinEdit;
    gbxCutRectangle: TGroupBox;
    gbxImageProperties: TGroupBox;
    gbxOutputFile: TGroupBox;
    gbxMagazineInfo: TGroupBox;
    lBaseOutFolder: TLabel;
    lPMBorder: TLabel;
    lHeight: TLabel;
    lFilename: TLabel;
    lGame: TLabel;
    lIssue: TLabel;
    lLanguage: TLabel;
    lLeft: TLabel;
    lMagazine: TLabel;
    lOutputFile: TLabel;
    lOutputFolder: TLabel;
    lWidth: TLabel;
    lSection: TLabel;
    lSystem: TLabel;
    lTop: TLabel;
    lType: TLabel;
    pButtons: TPanel;
    pEmpty7: TPanel;
    pPointMode: TPanel;
    pRect: TPanel;
    procedure bClearSelectionClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure bJoinNextFileClick(Sender: TObject);
    procedure bSaveAndDeleteClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure cbxSystemEditingDone(Sender: TObject);
    procedure chkPointModeChange(Sender: TObject);
    procedure eTRectChange(Sender: TObject);
    procedure TComboBoxEditingDone(Sender: TObject);
    procedure chkMultipageEditingDone(Sender: TObject);
    procedure TEditEditingDone(Sender: TObject);

  private
    FEMCConfig: cEMCConfig;
    FImageFile: string;
    FOnChangeRect: TEMCProcRectObjCB;
    FOnRectClear: TEMCOnProcedureObjCB;
    FOnDelete: TEMCOnProcedureObjCB;
    FOnJoinNextFile: TEMCOnProcedureObjCB;
    FOnPMChange: TEMCOnPointModeChange;
    FOnSave: TEMCOnSaveObjCB;
    procedure SetEMCConfig(AValue: cEMCConfig);
    procedure SetImageFile(AValue: string);
    procedure SetOnChangeRect(AValue: TEMCProcRectObjCB);
    procedure SetOnRectClear(AValue: TEMCOnProcedureObjCB);
    procedure SetOnDelete(AValue: TEMCOnProcedureObjCB);
    procedure SetOnJoinNextFile(AValue: TEMCOnProcedureObjCB);
    procedure SetOnPMChange(AValue: TEMCOnPointModeChange);
    procedure SetOnSave(AValue: TEMCOnSaveObjCB);

  protected
    procedure MakeFileName;

    procedure DoSave(DeleteOriginal: boolean);
    procedure DoDelete;

  public
    property EMCConfig: cEMCConfig read FEMCConfig write SetEMCConfig;

    property OnSave: TEMCOnSaveObjCB read FOnSave write SetOnSave;
    property OnDelete: TEMCOnProcedureObjCB read FOnDelete write SetOnDelete;
    property OnChangeRect: TEMCProcRectObjCB
      read FOnChangeRect write SetOnChangeRect;
    property OnRectClear: TEMCOnProcedureObjCB
      read FOnRectClear write SetOnRectClear;
    property OnPMChange: TEMCOnPointModeChange
      read FOnPMChange write SetOnPMChange;
    property OnJoinNextFile: TEMCOnProcedureObjCB
      read FOnJoinNextFile write SetOnJoinNextFile;

    procedure SetRect(aRect: TRect);

    procedure SaveEMCConfig;

    procedure ClearFrameData; override;
    procedure LoadFrameData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published
    property ImageFile: string read FImageFile write SetImageFile;
  end;

implementation

{$R *.lfm}

{ TfmEMCImagePropEditor }

procedure TfmEMCImagePropEditor.TComboBoxEditingDone(Sender: TObject);
var
  aCBX: TComboBox;
begin
  // TComboBox.Text is defined in TControl as protected, so we
  //   can't merge TEditEditingDone with TComboBoxEditingDone.

  if not (Sender is TComboBox) then Exit;

  aCBX := TComboBox(Sender);

  aCBX.Text := CleanFileName(aCBX.Text);

  MakeFileName;
end;

procedure TfmEMCImagePropEditor.bSaveClick(Sender: TObject);
begin
  DoSave(False);
end;

procedure TfmEMCImagePropEditor.cbxSystemEditingDone(Sender: TObject);
var
  aCBX: TComboBox;
begin
  // This time PathAware

  if not (Sender is TComboBox) then Exit;

  aCBX := TComboBox(Sender);

  aCBX.Text := CleanFileName(aCBX.Text, True, True);

  MakeFileName;
end;

procedure TfmEMCImagePropEditor.chkPointModeChange(Sender: TObject);
begin
  lPMBorder.Enabled := chkPointMode.Checked;
  ePMBorder.Enabled := chkPointMode.Checked;
  if Assigned(OnPMChange) then
    OnPMChange(chkPointMode.Checked);
end;

procedure TfmEMCImagePropEditor.eTRectChange(Sender: TObject);
begin
  if Assigned(OnChangeRect) then
    OnChangeRect(TRect.Create(eLeft.Value, eTop.Value,
      eLeft.Value + eWidth.Value, eTop.Value + eHeight.Value));
end;

procedure TfmEMCImagePropEditor.bSaveAndDeleteClick(Sender: TObject);
begin
  DoSave(True);
end;

procedure TfmEMCImagePropEditor.bDeleteClick(Sender: TObject);
begin
  DoDelete;
end;

procedure TfmEMCImagePropEditor.bClearSelectionClick(Sender: TObject);
begin
  if assigned(OnRectClear) then
    OnRectClear;
end;

procedure TfmEMCImagePropEditor.bJoinNextFileClick(Sender: TObject);
begin
  if assigned(OnJoinNextFile) then
    OnJoinNextFile;
end;

procedure TfmEMCImagePropEditor.chkMultipageEditingDone(Sender: TObject);
begin
  MakeFileName;
end;

procedure TfmEMCImagePropEditor.TEditEditingDone(Sender: TObject);
var
  aEdit: TEdit;
begin
  // TEdit.Text is defined in TControl as protected, so we
  //   can't merge TEditEditingDone with TComboBoxEditingDone.

  if not (Sender is TEdit) then Exit;

  aEdit := TEdit(Sender);

  aEdit.Text := CleanFileName(aEdit.Text);

  MakeFileName;
end;

procedure TfmEMCImagePropEditor.SetImageFile(AValue: string);
begin
  FImageFile := (AValue);

  LoadFrameData;
end;

procedure TfmEMCImagePropEditor.SetEMCConfig(AValue: cEMCConfig);
begin
  if FEMCConfig = AValue then Exit;
  FEMCConfig := AValue;

  if not assigned(EMCConfig) then Exit;

  eBaseOutFolder.Text := EMCConfig.BaseOutFolder;

  cbxMagazine.Items.AddStrings(EMCConfig.Magazines);
  cbxSection.Items.AddStrings(EMCConfig.Sections);
  cbxLanguage.Items.AddStrings(EMCConfig.Languages);
  cbxType.Items.AddStrings(EMCConfig.Types);
  cbxSystem.Items.AddStrings(EMCConfig.Systems);
  cbxGame.Items.AddStrings(EMCConfig.Videogames);
end;

procedure TfmEMCImagePropEditor.SetOnChangeRect(AValue: TEMCProcRectObjCB);
begin
  if FOnChangeRect = AValue then Exit;
  FOnChangeRect := AValue;
end;

procedure TfmEMCImagePropEditor.SetOnRectClear(AValue: TEMCOnProcedureObjCB);
begin
  if FOnRectClear = AValue then Exit;
  FOnRectClear := AValue;
end;

procedure TfmEMCImagePropEditor.SetOnDelete(AValue: TEMCOnProcedureObjCB);
begin
  if FOnDelete = AValue then Exit;
  FOnDelete := AValue;
end;

procedure TfmEMCImagePropEditor.SetOnJoinNextFile(AValue:
  TEMCOnProcedureObjCB);
begin
  if FOnJoinNextFile = AValue then Exit;
  FOnJoinNextFile := AValue;

  bJoinNextFile.Enabled := Assigned(FOnJoinNextFile);
end;

procedure TfmEMCImagePropEditor.SetOnPMChange(AValue: TEMCOnPointModeChange);
begin
  if FOnPMChange = AValue then Exit;
  FOnPMChange := AValue;
end;

procedure TfmEMCImagePropEditor.SetOnSave(AValue: TEMCOnSaveObjCB);
begin
  if FOnSave = AValue then Exit;
  FOnSave := AValue;
end;

procedure TfmEMCImagePropEditor.ClearFrameData;
begin
  inherited;

  eFilename.Clear;
  eOutputFolder.Clear;
  eOutputFile.Clear;

  // It's not the same as Clear (it olny empties de edit, but value is keep)
  eLeft.Value := 0;
  eTop.Value := 0;
  eWidth.Value := 0;
  eHeight.Value := 0;

  Enabled := False;
end;

procedure TfmEMCImagePropEditor.LoadFrameData;
begin
  inherited;

  if (ImageFile = '') or not FileExistsUTF8(ImageFile) then
  begin
    ClearFrameData;
    Exit;
  end;

  eFilename.Text := ExtractFileNameOnly(ImageFile);

  // It's not the same as Clear (it olny empties de edit, but value is keep)
  eLeft.Value := 0;
  eTop.Value := 0;
  eWidth.Value := 0;
  eHeight.Value := 0;

  MakeFileName;

  Enabled := True;
end;

procedure TfmEMCImagePropEditor.MakeFileName;
var
  FullFileName, TempStr: string;
  i: integer;
begin

  // Testing obligatory fields
  if (cbxSystem.Text = '') or (cbxType.Text = '') or
    (cbxGame.Text = '') then
  begin
    eOutputFolder.Clear;
    eOutputFile.Clear;
    bSave.Enabled := False;
    bSaveAndDelete.Enabled := False;
    Exit;
  end;

  eOutputFolder.Text := SetAsFolder(cbxSystem.Text) +
    SetAsFolder(cbxType.Text);

  FullFileName := SetAsFolder(eBaseOutFolder.Text) +
    SetAsFolder(eOutputFolder.Text) + cbxGame.Text;

  if cbxMagazine.Text <> '' then
  begin
    FullFileName += ' (' + cbxMagazine.Text;

    if eIssue.Text <> '' then
    begin
      if TryStrToInt(eIssue.Text, i) then
        FullFileName += Format(' %.3d', [i])
      else
        FullFileName += eIssue.Text;
    end;

    if cbxSection.Text <> '' then
    begin
      FullFileName += ' - ' + cbxSection.Text;
    end;

    FullFileName += ')';
  end;

  if cbxLanguage.Text <> '' then
    FullFileName += ' [' + cbxLanguage.Text + ']';

  TempStr := FullFileName + '.jpg';

  if chkMultipage.Checked or FileExistsUTF8(TempStr) then
  begin
    // El fichero ya existe o son múltiples páginas para el mismo contenido
    i := 0;
    repeat
      Inc(i);
      TempStr := FullFileName + Format(' (%.2d).jpg', [i]);
    until not FileExistsUTF8(TempStr);
  end;

  FullFileName := TempStr;

  eOutputFile.Text := ExtractFileName(FullFileName);

  bSave.Enabled := True;
  bSaveAndDelete.Enabled := True;
end;

procedure TfmEMCImagePropEditor.DoSave(DeleteOriginal: boolean);
begin
  // Testing obligatory fields
  if (eOutputFolder.Text = '') or (eOutputFile.Text = '') then
  begin
    bSave.Enabled := False;
    bSaveAndDelete.Enabled := False;
    Exit;
  end;

  if assigned(OnSave) then
    OnSave(SetAsFolder(eBaseOutFolder.Text) +
      SetAsFolder(eOutputFolder.Text) + eOutputFile.Text,
      chkResize2048.Checked);

  if cbxMagazine.Items.IndexOf(cbxMagazine.Text) = -1 then
    cbxMagazine.Items.Add(cbxMagazine.Text);
  if cbxSection.Items.IndexOf(cbxSection.Text) = -1 then
    cbxSection.Items.Add(cbxSection.Text);
  if cbxLanguage.Items.IndexOf(cbxLanguage.Text) = -1 then
    cbxLanguage.Items.Add(cbxLanguage.Text);
  if cbxType.Items.IndexOf(cbxType.Text) = -1 then
    cbxType.Items.Add(cbxType.Text);
  if cbxSystem.Items.IndexOf(cbxSystem.Text) = -1 then
    cbxSystem.Items.Add(cbxSystem.Text);
  if cbxGame.Items.IndexOf(cbxGame.Text) = -1 then
    cbxGame.Items.Add(cbxGame.Text);

  if DeleteOriginal then
    DoDelete;

  if not chkMultipage.Checked then
  begin
    cbxGame.Text := '';
  end;

  MakeFileName;
end;

procedure TfmEMCImagePropEditor.DoDelete;
begin
  if assigned(OnDelete) then
    OnDelete
  else
    // If onDelete is handled, we don't want autoclear the frame.
    // Maybe another component autoload other file in it, and it's
    //   cleared again
    ImageFile := '';
end;

procedure TfmEMCImagePropEditor.SetRect(aRect: TRect);
begin
  // HACK: Don't callback again
  eLeft.OnChange := nil;
  eTop.OnChange := nil;
  eWidth.OnChange := nil;
  eHeight.OnChange := nil;

  eLeft.Value := aRect.Left;
  eTop.Value := aRect.Top;
  eWidth.Value := aRect.Width;
  eHeight.Value := aRect.Height;

  eLeft.OnChange := @eTRectChange;
  eTop.OnChange := @eTRectChange;
  eWidth.OnChange := @eTRectChange;
  eHeight.OnChange := @eTRectChange;
end;

procedure TfmEMCImagePropEditor.SaveEMCConfig;
begin
  if assigned(EMCConfig) then
  begin
    EMCConfig.BaseOutFolder := eBaseOutFolder.Text;

    EMCConfig.Magazines.AddStrings(cbxMagazine.Items, True);
    EMCConfig.Sections.AddStrings(cbxSection.Items, True);
    EMCConfig.Languages.AddStrings(cbxLanguage.Items, True);
    EMCConfig.Types.AddStrings(cbxType.Items, True);
    EMCConfig.Systems.AddStrings(cbxSystem.Items, True);
    EMCConfig.Videogames.AddStrings(cbxGame.Items, True);
  end;
end;

constructor TfmEMCImagePropEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  // Enabling lPMBorder and ePMBorder if chkPointMode.Enabled;
  chkPointModeChange(chkPointMode);
end;

destructor TfmEMCImagePropEditor.Destroy;
begin
  inherited Destroy;
end;

end.
