{ Full emulator editor frame of Emuteca GUI.

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
unit ufETKGUIFullEmuEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, ComCtrls, StdCtrls, LCLIntf, LazFileUtils,
  uCHXStrUtils,
  ufCHXPropEditor,
  ucEmutecaEmulator,
  ufEmutecaEmulatorEditor;

type

  { TfmEEGUIFullEmuEditor }

  TfmEEGUIFullEmuEditor = class(TfmCHXPropEditor)
    actOpenEmulatorFolder: TAction;
    pcProperties: TPageControl;
    ToolBar1: TToolBar;
    bOpenEmulatorFolder: TToolButton;
    procedure actOpenEmulatorFolderExecute(Sender: TObject);
  private
    FEmuEditor: TfmEmutecaEmulatorEditor;
    FEmulator: cEmutecaEmulator;
    FSHA1Folder: string;
    procedure SetEmulator(AValue: cEmutecaEmulator);
    procedure SetSHA1Folder(AValue: string);
    { private declarations }

  protected
    property EmuEditor: TfmEmutecaEmulatorEditor read FEmuEditor;

    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public
    { public declarations }
    property Emulator: cEmutecaEmulator read FEmulator write SetEmulator;

    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEEGUIFullEmuEditor }

procedure TfmEEGUIFullEmuEditor.actOpenEmulatorFolderExecute(Sender: TObject);
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

procedure TfmEEGUIFullEmuEditor.SetEmulator(AValue: cEmutecaEmulator);
begin
  if FEmulator = AValue then
    Exit;
  FEmulator := AValue;
  EmuEditor.Emulator := Emulator;

  LoadFrameData;
end;

procedure TfmEEGUIFullEmuEditor.SetSHA1Folder(AValue: string);
begin
  FSHA1Folder := SetAsFolder(AValue);
  //fmEmuImgEditor.SHA1Folder := SHA1Folder;
end;

procedure TfmEEGUIFullEmuEditor.DoClearFrameData;
begin

end;

procedure TfmEEGUIFullEmuEditor.DoLoadFrameData;
begin
  Enabled := Assigned(Emulator);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

procedure TfmEEGUIFullEmuEditor.DoSaveFrameData;
begin
  EmuEditor.SaveFrameData;
end;

constructor TfmEEGUIFullEmuEditor.Create(TheOwner: TComponent);

  procedure CreatePages;
  var
    aTabSheet: TTabSheet;
  begin
    aTabSheet := pcProperties.AddTabSheet;
    FEmuEditor := TfmEmutecaEmulatorEditor.Create(aTabSheet);
    EmuEditor.SaveButtons := False;
    EmuEditor.Align := alClient;
    EmuEditor.Parent := aTabSheet;

    aTabSheet := pcProperties.AddTabSheet;
  end;

begin
  inherited Create(TheOwner);

  CreatePages;

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmEEGUIFullEmuEditor.Destroy;
begin
  inherited Destroy;
end;

end.
