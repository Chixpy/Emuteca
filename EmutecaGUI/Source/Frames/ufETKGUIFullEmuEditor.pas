unit ufETKGUIFullEmuEditor;

{< TfmETKGUIFullEmuEditor frame unit.

  ----

  This file is part of Emuteca GUI.

  Copyright (C) 2006-2018 Chixpy

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, ComCtrls, StdCtrls, LCLIntf, LazFileUtils,
  // CHX unit
  uCHXStrUtils,
  // CHX frames
  ufCHXPropEditor,
  // CHX forms
  ufrCHXForm,
  // Emuteca units
  uEmutecaConst, uEmutecaRscStr,
  // Emuteca clases
  ucEmutecaEmulator,
  // Emuteca frames
  ufEmutecaEmulatorEditor, ufEmutecaEmulatorAdvParamsEditor,
  // Emuteca GUI units
  uETKGUIConst, uETKGUIRscStr;

type

  { TfmETKGUIFullEmuEditor }

  TfmETKGUIFullEmuEditor = class(TfmCHXPropEditor)
    actOpenEmulatorFolder: TAction;
    pcProperties: TPageControl;
    ToolBar1: TToolBar;
    bOpenEmulatorFolder: TToolButton;
    procedure actOpenEmulatorFolderExecute(Sender: TObject);
  private
    FEmuEditor: TfmEmutecaEmulatorEditor;
    FEmulator: cEmutecaEmulator;
    FEmuParamsEditor: TfmEmutecaEmulatorAdvParamsEditor;
    FSHA1Folder: string;
    procedure SetEmulator(AValue: cEmutecaEmulator);
    procedure SetSHA1Folder(AValue: string);
    { private declarations }

  protected
    property EmuEditor: TfmEmutecaEmulatorEditor read FEmuEditor;
    property EmuParamsEditor: TfmEmutecaEmulatorAdvParamsEditor
      read FEmuParamsEditor;

    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public
    { public declarations }
    property Emulator: cEmutecaEmulator read FEmulator write SetEmulator;

    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    class function SimpleModalForm(aEmulator: cEmutecaEmulator;
      const aSHA1Folder: string; const aGUIIconsIni: string;
      const aGUIConfigIni: string): integer;
    //< Creates a form with Emulatoe Editor.

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmETKGUIFullEmuEditor }

procedure TfmETKGUIFullEmuEditor.actOpenEmulatorFolderExecute(Sender: TObject);
var
  aFolder: string;
begin
  if not Assigned(Emulator) then
    Exit;

  aFolder := ExtractFilePath(Emulator.ExeFile);
  if not DirectoryExistsUTF8(aFolder) then
    Exit;

  OpenDocument(aFolder);
end;

procedure TfmETKGUIFullEmuEditor.SetEmulator(AValue: cEmutecaEmulator);
begin
  if FEmulator = AValue then
    Exit;
  FEmulator := AValue;
  EmuEditor.Emulator := Emulator;
  EmuParamsEditor.Emulator := Emulator;

  LoadFrameData;
end;

procedure TfmETKGUIFullEmuEditor.SetSHA1Folder(AValue: string);
begin
  FSHA1Folder := SetAsFolder(AValue);
  //fmEmuImgEditor.SHA1Folder := SHA1Folder;
end;

procedure TfmETKGUIFullEmuEditor.DoClearFrameData;
begin
  EmuEditor.ClearFrameData;
  EmuParamsEditor.ClearFrameData;
end;

procedure TfmETKGUIFullEmuEditor.DoLoadFrameData;
begin
  Enabled := Assigned(Emulator);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

procedure TfmETKGUIFullEmuEditor.DoSaveFrameData;
begin
  EmuEditor.SaveFrameData;
  EmuParamsEditor.SaveFrameData;
end;

class function TfmETKGUIFullEmuEditor.SimpleModalForm(
  aEmulator: cEmutecaEmulator; const aSHA1Folder: string;
  const aGUIIconsIni: string; const aGUIConfigIni: string): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmETKGUIFullEmuEditor;
begin
  Result := mrNone;

  if not assigned(aEmulator) then
  begin
    Result := mrAbort;
    Exit;
  end;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := krsETKGUIEmuEditorID;
    aForm.Caption := Format(krsFmtWindowCaption,
      [Application.Title, rsETKGUIEmuEditorTitle]);

    aFrame := TfmETKGUIFullEmuEditor.Create(aForm);
    aFrame.SaveButtons := True;
    aFrame.ButtonClose := True;
    aFrame.Align := alClient;

    aFrame.SHA1Folder := aSHA1Folder;
    aFrame.Emulator := aEmulator;

    aForm.LoadGUIConfig(aGUIConfigIni);
    aForm.LoadGUIIcons(aGUIIconsIni);
    aFrame.Parent := aForm;

    Result := aForm.ShowModal;
  finally
    aForm.Free;
  end;
end;

constructor TfmETKGUIFullEmuEditor.Create(TheOwner: TComponent);

  procedure CreatePages;
  var
    aTabSheet: TTabSheet;
  begin
    aTabSheet := pcProperties.AddTabSheet;
    aTabSheet.Caption := rsBasicCaption;
    FEmuEditor := TfmEmutecaEmulatorEditor.Create(aTabSheet);
    EmuEditor.SaveButtons := False;
    EmuEditor.Align := alClient;
    EmuEditor.Parent := aTabSheet;

    aTabSheet := pcProperties.AddTabSheet;
    aTabSheet.Caption := rsAdvancedCaption;
    FEmuParamsEditor :=
      TfmEmutecaEmulatorAdvParamsEditor.Create(aTabSheet);
    EmuParamsEditor.SaveButtons := False;
    EmuParamsEditor.Align := alClient;
    EmuParamsEditor.Parent := aTabSheet;
  end;

begin
  inherited Create(TheOwner);

  CreatePages;

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmETKGUIFullEmuEditor.Destroy;
begin
  inherited Destroy;
end;

end.
