unit ufETKGUIFullSysEditor;

{< TfmETKGUIFullSystemEditor frame unit.

  This file is part of Emuteca GUI.

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
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, ActnList, LazFileUtils, LCLIntf, StdCtrls,
  // CHX units
  uCHXStrUtils,
  // CHX forms
  ufrCHXForm,
  // CHX frames
  ufCHXPropEditor,
  // Emuteca Core common
  uEmutecaConst, uEmutecaRscStr,
  // Emuteca Core clases
  ucEmuteca, ucEmutecaSystem,
  // Emuteca Core frames
  ufEmutecaSystemEditor, ufEmutecaSystemImgEditor,
  ufEmutecaSystemITFEditor, ufEmutecaSystemMVFEditor,
  // Emteca GUI units
  uETKGUIConst, uETKGUIRscStr;

type

  { TfmETKGUIFullSystemEditor }

  TfmETKGUIFullSystemEditor = class(TfmCHXPropEditor)
    actCreateFolders: TAction;
    actOpenSystemFolder: TAction;
    pcProperties: TPageControl;
    SysToolBar: TToolBar;
    bCreateFolders: TToolButton;
    bOpenSystemFolder: TToolButton;
    bSaveSystem: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure actCreateFoldersExecute(Sender: TObject);
    procedure actOpenSystemFolderExecute(Sender: TObject);

  private
    FEmuteca: cEmuteca;
    FfmSysITFEditor: TfmEmutecaSystemITFEditor;
    FfmSysEditor: TfmEmutecaSystemEditor;
    FfmSysImgEditor: TfmEmutecaSystemImgEditor;
    FfmSysMVFEditor: TfmEmutecaSystemMVFEditor;
    FSHA1Folder: string;
    FSystem: cEmutecaSystem;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetSHA1Folder(AValue: string);
    procedure SetSystem(AValue: cEmutecaSystem);

  protected
    property fmSysEditor: TfmEmutecaSystemEditor read FfmSysEditor;
    property fmSysImgEditor: TfmEmutecaSystemImgEditor read FfmSysImgEditor;
    property fmSysITFEditor: TfmEmutecaSystemITFEditor read FfmSysITFEditor;
    property fmSysMVFEditor: TfmEmutecaSystemMVFEditor read FfmSysMVFEditor;

    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DOSaveFrameData;


  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
    property System: cEmutecaSystem read FSystem write SetSystem;

    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    class function SimpleForm(aEmuteca: cEmuteca;
      aSystem: cEmutecaSystem; aSHA1Folder: string; aGUIIconsIni: string;
      aGUIConfigIni: string): integer;
    //< Creates a form with System Editor.

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmETKGUIFullSystemEditor }

procedure TfmETKGUIFullSystemEditor.actCreateFoldersExecute(Sender: TObject);

  procedure CreateFolder(aSystem: cEmutecaSystem; aLine: TStringList);
  var
    aFolder: string;
    aTitle: string;
  begin
    if aLine.Count = 0 then
      Exit;

    if aLine[0] = '' then
      Exit;

    aFolder := SetAsFolder(aSystem.BaseFolder) + SetAsFolder(aLine[0]);
    ForceDirectoriesUTF8(aFolder);

    if aLine.Count = 1 then
      Exit;

    if aLine[1] = '' then
      Exit;

    if (aLine.Count = 2) or (aLine[2] = '') then
      aTitle := ExtractFileNameOnly(ExcludeTrailingPathDelimiter(aLine[0]))
    else
      aTitle := aLine[2];

    case AnsiLowerCase(aLine[1]) of
      'i': // Images
      begin
        aSystem.ImageFolders.Add(aFolder);
        aSystem.ImageCaptions.Add(aTitle);
      end;
      't': // Texts
      begin
        aSystem.TextFolders.Add(aFolder);
        aSystem.TextCaptions.Add(aTitle);
      end;
      'm': // Music
      begin
        aSystem.MusicFolders.Add(aFolder);
        aSystem.MusicCaptions.Add(aTitle);
      end;
      'v': // Video
      begin
        aSystem.VideoFolders.Add(aFolder);
        aSystem.VideoCaptions.Add(aTitle);
      end;
      'c': // Icon
      begin
        aSystem.IconFolder := aFolder;
      end;
      'l': // Logo
      begin
        aSystem.LogoFolder := aFolder;
      end;

      else
        ;
    end;
  end;

var
  FolderList, aLine: TStringList;
  i: integer;
  TmpSys: cEmutecaSystem;
begin
  if (not assigned(Emuteca)) or (not Assigned(System)) then
    Exit;

  // TODO: May be change the logic here:
  //  1. Test if system have changes
  //  2. Ask for save them

  // System will change internally, so we unload it first.
  SaveFrameData; // We must save first
  TmpSys := System;
  System := nil;

  if (TmpSys.BaseFolder = '') or not DirectoryExistsUTF8(
    TmpSys.BaseFolder) then
  begin
    { TODO : Exception :-P... Ask for Base folder. }
    ShowMessageFmt(rsFmtNotFound, [GetCurrentDirUTF8, TmpSys.BaseFolder]);
    Exit;
  end;
  if not FileExistsUTF8(Emuteca.Config.AutoSysFolder) then
  begin
    { TODO : Exception :-P }
    ShowMessageFmt(rsFmtNotFound, [GetCurrentDirUTF8,
      Emuteca.Config.AutoSysFolder]);
    Exit;
  end;

  if MessageDlg(rsWarning, Format(rsAutoFolderWarning, [TmpSys.BaseFolder]),
    mtWarning, [mbOK, mbCancel], 'CreateSystemFolders') = mrCancel then
    Exit;

  TmpSys.IconFolder := '';
  TmpSys.LogoFolder := '';
  TmpSys.ImageFolders.Clear;
  TmpSys.ImageCaptions.Clear;
  TmpSys.TextFolders.Clear;
  TmpSys.TextCaptions.Clear;
  TmpSys.MusicFolders.Clear;
  TmpSys.MusicCaptions.Clear;
  TmpSys.VideoFolders.Clear;
  TmpSys.VideoCaptions.Clear;


  aLine := TStringList.Create;
  FolderList := TStringList.Create;
  try
    FolderList.LoadFromFile(Emuteca.Config.AutoSysFolder);
    i := 1; //Skip header
    while i < FolderList.Count do
    begin
      aLine.Clear;
      aLine.CommaText := FolderList[i];
      CreateFolder(TmpSys, aLine);

      Inc(i);
    end;

  finally
    FreeAndNil(FolderList);
    FreeAndNil(aLine);
  end;

  System := TmpSys;
end;

procedure TfmETKGUIFullSystemEditor.actOpenSystemFolderExecute(
  Sender: TObject);
begin
  if (not Assigned(System)) or
    (not DirectoryExistsUTF8(System.BaseFolder)) then
    Exit;

  OpenDocument(System.BaseFolder);
end;

procedure TfmETKGUIFullSystemEditor.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  if not Assigned(Emuteca) then
  begin
    fmSysEditor.EmuManager := nil;
  end
  else
  begin
    fmSysEditor.EmuManager := Emuteca.EmulatorManager;
  end;

  LoadFrameData;
end;

procedure TfmETKGUIFullSystemEditor.SetSHA1Folder(AValue: string);
begin
  FSHA1Folder := SetAsFolder(AValue);
  fmSysImgEditor.SHA1Folder := SHA1Folder;
end;

procedure TfmETKGUIFullSystemEditor.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;

  fmSysEditor.System := System;
  fmSysImgEditor.System := System;
  fmSysITFEditor.System := System;
  fmSysMVFEditor.System := System;

  LoadFrameData;
end;

procedure TfmETKGUIFullSystemEditor.DoClearFrameData;
begin
end;

procedure TfmETKGUIFullSystemEditor.DOSaveFrameData;
begin
  fmSysEditor.SaveFrameData;
  fmSysImgEditor.SaveFrameData;
  fmSysITFEditor.SaveFrameData;
  fmSysMVFEditor.SaveFrameData;
end;

class function TfmETKGUIFullSystemEditor.SimpleForm(aEmuteca: cEmuteca;
  aSystem: cEmutecaSystem; aSHA1Folder: string; aGUIIconsIni: string;
  aGUIConfigIni: string): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmETKGUIFullSystemEditor;
begin
  Result := mrNone;

  if (not assigned(aEmuteca)) or (not assigned(aSystem)) then
  begin
    Result := mrAbort;
    Exit;
  end;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := krsETKGUISystemEditorID;
    aForm.Caption := Format(krsFmtWindowCaption,
      [Application.Title, rsETKGUISystemEditorTitle]);

    aFrame := TfmETKGUIFullSystemEditor.Create(aForm);
    aFrame.SaveButtons := True;
    aFrame.ButtonClose := True;
    aFrame.Align := alClient;

    aFrame.SHA1Folder := aSHA1Folder;
    aFrame.Emuteca := aEmuteca;
    aFrame.System := aSystem;

    aForm.LoadGUIConfig(aGUIConfigIni);
    aForm.LoadGUIIcons(aGUIIconsIni);
    aFrame.Parent := aForm;

    Result := aForm.ShowModal;
  finally
    aForm.Free;
  end;
end;

procedure TfmETKGUIFullSystemEditor.DoLoadFrameData;
begin
  Enabled := Assigned(Emuteca) and Assigned(System);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

constructor TfmETKGUIFullSystemEditor.Create(TheOwner: TComponent);

  procedure CreatePages;
  var
    aTabSheet: TTabSheet;
  begin
    aTabSheet := pcProperties.AddTabSheet;
    aTabSheet.Caption := 'Basic config';
    FfmSysEditor := TfmEmutecaSystemEditor.Create(aTabSheet);
    fmSysEditor.SaveButtons := False;
    fmSysEditor.ButtonClose := False;
    fmSysEditor.Align := alClient;
    fmSysEditor.Parent := aTabSheet;

    aTabSheet := pcProperties.AddTabSheet;
    aTabSheet.Caption := 'Basic images';
    FfmSysImgEditor := TfmEmutecaSystemImgEditor.Create(aTabSheet);
    fmSysImgEditor.SaveButtons := False;
    fmSysImgEditor.ButtonClose := False;
    fmSysImgEditor.Align := alClient;
    fmSysImgEditor.Parent := aTabSheet;

    aTabSheet := pcProperties.AddTabSheet;
    FfmSysITFEditor := TfmEmutecaSystemITFEditor.Create(aTabSheet);
    aTabSheet.Caption := 'Software images';
    fmSysITFEditor.SaveButtons := False;
    fmSysITFEditor.ButtonClose := False;
    fmSysITFEditor.Align := alClient;
    fmSysITFEditor.Parent := aTabSheet;

    aTabSheet := pcProperties.AddTabSheet;
    aTabSheet.Caption := 'Music & Video';
    FfmSysMVFEditor := TfmEmutecaSystemMVFEditor.Create(aTabSheet);
    fmSysMVFEditor.SaveButtons := False;
    fmSysMVFEditor.ButtonClose := False;
    fmSysMVFEditor.Align := alClient;
    fmSysMVFEditor.Parent := aTabSheet;
  end;

begin
  inherited Create(TheOwner);

  CreatePages;

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmETKGUIFullSystemEditor.Destroy;
begin
  inherited Destroy;
end;

end.
