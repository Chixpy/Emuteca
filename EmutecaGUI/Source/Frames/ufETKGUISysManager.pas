unit ufETKGUISysManager;

{< TfmETKGUISysManager frame unit.

  This file is part of Emuteca GUI.

  Copyright (C) 2006-2023 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, CheckLst, ActnList, Menus,
  // CHX units
  uCHXStrUtils,
  // CHX forms
  ufrCHXForm,
  // CHX frames
  ufCHXChkLstPropEditor, ufCHXProgressBar,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr,
  // Emuteca Core clases
  ucEmuteca, ucEmutecaSystem,
  // Emuteca GUI frames
  ufETKGUIFullSysEditor;

resourcestring
  rsSystemNameModel = 'System name [Company: Model (extra)].';
{< Title of Add System dialog}

type
  { TfmETKGUISysManager }

  TfmETKGUISysManager = class(TfmCHXChkLstPropEditor)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;

  private
    FEmuteca: cEmuteca;
    FfmProgressBar: TfmCHXProgressBar;
    FSHA1Folder: string;
    FfmSysEditor: TfmETKGUIFullSystemEditor;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetSHA1Folder(AValue: string);

  protected

    property fmSysEditor: TfmETKGUIFullSystemEditor read FfmSysEditor;
    property fmProgressBar: TfmCHXProgressBar read FfmProgressBar;

    procedure AddItemToList; override;
    procedure DeleteItemFromList; override;
    procedure ExportList; override;
    procedure ImportList; override;
    procedure OnListClick(aObject: TObject); override;
    procedure OnListClickCheck(aObject: TObject; aBool: boolean); override;
    procedure SetCheckedAll(aBool: boolean); override;

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
    //< Needed by fmSysEditor.
    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;
    //< Folder with image statistiscs files.

    class function SimpleForm(aEmuteca: cEmuteca; const aSHA1Folder,
      aGUIConfigIni, aGUIIconsIni: string): integer;
    //< Creates a form with System Manager frame.

    procedure LoadFrameData; override;
    procedure SaveFrameData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{< Frame for System Manager. }


implementation

{$R *.lfm}

{ TfmETKGUISysManager }
procedure TfmETKGUISysManager.SetCheckedAll(aBool: boolean);
begin
  // DO NOTHING, ENABLING SYSTEMS IS DONE ON SAVING LIST TO TEST
  //   ONLY STATE CHANGED SYSTEMS:
  //     - UNCHECKED MUST BE SAVED AND UNLOADED.
  //     - CHECKED MUST BE LOADED.
end;

procedure TfmETKGUISysManager.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  fmSysEditor.Emuteca := Emuteca;

  LoadFrameData;
end;

procedure TfmETKGUISysManager.SetSHA1Folder(AValue: string);
begin
  FSHA1Folder := SetAsFolder(AValue);
  fmSysEditor.SHA1Folder := SHA1Folder;
end;

procedure TfmETKGUISysManager.OnListClick(aObject: TObject);
begin
  fmSysEditor.System := cEmutecaSystem(aObject);
end;

procedure TfmETKGUISysManager.OnListClickCheck(aObject: TObject;
  aBool: boolean);
begin
  // DO NOTHING, ENABLING SYSTEMS IS DONE ON SAVING LIST TO TEST
  //   ONLY STATE CHANGED SYSTEMS:
  //     - UNCHECKED MUST BE SAVED AND UNLOADED.
  //     - CHECKED MUST BE LOADED.
end;

procedure TfmETKGUISysManager.AddItemToList;
var
  SystemID: string;
  aSystem: cEmutecaSystem;
begin
  if not assigned(Emuteca) then
    Exit;

  SystemID := Trim(InputBox(actAddItem.Caption, rsSystemNameModel, ''));
  if SystemID = '' then
    Exit;

  aSystem := Emuteca.SystemManager.AddSystem(SystemID);
  aSystem.Enabled := True;

  LoadFrameData;

  fmSysEditor.System := aSystem;
end;

procedure TfmETKGUISysManager.DeleteItemFromList;
var
  aSystem: cEmutecaSystem;
begin
  if not assigned(Emuteca) then
    Exit;

  if clbPropItems.ItemIndex = -1 then
    Exit;

  fmSysEditor.System := nil;

  aSystem := cEmutecaSystem(
    clbPropItems.Items.Objects[clbPropItems.ItemIndex]);
  try
    // If already in enabled list remove here too.
    Emuteca.SystemManager.EnabledList.Remove(aSystem);

    // FullList frees the object too.
    Emuteca.SystemManager.FullList.Remove(aSystem);
    //aSystem.Free;
  finally
    LoadFrameData;
  end;
end;

procedure TfmETKGUISysManager.ExportList;
begin
  if not assigned(Emuteca) then
    Exit;

  if not SaveDialog1.Execute then
    Exit;

  Emuteca.SystemManager.ExportToFile(SaveDialog1.FileName, False);
end;

procedure TfmETKGUISysManager.ImportList;
begin
  if not assigned(Emuteca) then
    Exit;

  if not OpenDialog1.Execute then
    Exit;

  Emuteca.SystemManager.ImportFromFile(OpenDialog1.FileName);
end;

procedure TfmETKGUISysManager.LoadFrameData;
var
  i: integer;
begin
  inherited LoadFrameData;

  Enabled := assigned(Emuteca);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  clbPropItems.Clear;
  Emuteca.SystemManager.FullList.AssignToStrLst(clbPropItems.Items);
  i := 0;
  while i < clbPropItems.Items.Count do
  begin
    clbPropItems.Checked[i] :=
      cEmutecaSystem(clbPropItems.Items.Objects[i]).Enabled;
    Inc(i);
  end;
end;

procedure TfmETKGUISysManager.SaveFrameData;
var
  i: integer;
  aSystem: cEmutecaSystem;
  aPBCB: TEmutecaProgressCallBack;
begin
  inherited SaveFrameData;

  if not assigned(Emuteca) then
    Exit;

  // Saving current system data
  if assigned(fmSysEditor.System) then fmSysEditor.SaveFrameData;

  // HACK: Preventing lost data from changed systems,
  //   saving current data or reloading from disk.

  i := 0;
  while i < clbPropItems.Items.Count do
  begin
    aSystem := cEmutecaSystem(clbPropItems.Items.Objects[i]);
    fmProgressBar.UpdTextAndBar('Saving/Loading state changed systems',
      aSystem.Title, i, clbPropItems.Items.Count, False);

    if aSystem.Enabled <> clbPropItems.Checked[i] then
    begin
      if aSystem.ListFileName = '' then
        aSystem.ListFileName := aSystem.Title;

      aPBCB := aSystem.ProgressCallBack;
      aSystem.ProgressCallBack := nil; // Disabling system callback

      if aSystem.Enabled then
      begin
        // Saving soft of previously enabled systems ...
        // ... if not loaded its not saved

        Emuteca.SystemManager.SaveSystemData(aSystem, True);
        // Unloading System Data
        aSystem.UnloadSoftGroupLists;
      end
      else
      begin
        // Loading soft of previously disabled systems
        // Actually it can be loaded on demand, but loading here don't hurts.
        Emuteca.SystemManager.LoadSystemData(aSystem);
      end;

      aSystem.ProgressCallBack := aPBCB; // Reenabling system callback

      aSystem.Enabled := clbPropItems.Checked[i];
    end;

    Inc(i);
  end;
  fmProgressBar.Finish;

  Emuteca.SystemManager.UpdateEnabledList;
end;

class function TfmETKGUISysManager.SimpleForm(aEmuteca: cEmuteca;
  const aSHA1Folder, aGUIConfigIni, aGUIIconsIni: string): integer;
var
  aFrame: TfmETKGUISysManager;
begin
  Result := mrNone;

  if not assigned(aEmuteca) then
    Exit;

  aFrame := TfmETKGUISysManager.Create(nil);
  aFrame.SaveButtons := True;
  aFrame.ButtonClose := True;
  aFrame.Align := alClient;

  aFrame.SHA1Folder := aSHA1Folder;
  aFrame.Emuteca := aEmuteca;

  Result := GenSimpleModalForm(aFrame, 'frmETKGUISysManager',
    Format(krsFmtWindowCaption, [Application.Title, 'System Manager']),
    aGUIConfigIni, aGUIIconsIni);
end;

constructor TfmETKGUISysManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FfmSysEditor := TfmETKGUIFullSystemEditor.Create(Self);
  fmSysEditor.SaveButtons := True;
  fmSysEditor.ButtonClose := False;
  fmSysEditor.Align := alClient;
  fmSysEditor.Parent := Self;

  FfmProgressBar := TfmCHXProgressBar.SimpleForm('');
end;

destructor TfmETKGUISysManager.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmETKGUISysManager);

finalization
  UnRegisterClass(TfmETKGUISysManager);
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
