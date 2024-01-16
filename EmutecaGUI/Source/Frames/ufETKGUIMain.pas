unit ufETKGUIMain;

{< TfmETKGUIMain frame unit.

  This file is part of Emuteca GUI.

  Copyright (C) 2006-2023 Chixpy
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, IniFiles, Menus, LazFileUtils,
  // CHX
  uCHXStrUtils, ucCHXImageList,
  // CHX frames
  ufCHXFrame,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr,
  // Emuteca Core clases
  ucEmuteca, ucEmutecaSystem, ucEmutecaGroupList, ucEmutecaGroup,
  ucEmutecaSoftware, ucEmutecaEmulator,
  // Emuteca Core frames
  ufEmutecaSystemCBX, ufEmutecaTagTree,
  // Emuteca GUI units
  uETKGUIConst, uETKGUIRscStr,
  // Emuteca GUI classes
  ucETKGUIConfig,
  // Emuteca GUI frames
  ufETKGUIFullSoftEditor, ufETKGUIIcnSysCBX, ufETKGUISoftMedia,
  ufETKGUIIcnSoftTree, ufETKGUISysPreview;

const
  krsIniMainFrameSection = 'MainFrame';
  krsIniMainFrameLeftPanelWidth = 'LeftPanel_Width';
  krsIniMainFrameRigthPanelWidth = 'RightPanel_Width';

type

  { TfmETKGUIMain }

  TfmETKGUIMain = class(TfmCHXFrame)
    bSearch: TButton;
    cCancelSearch: TButton;
    eSearch: TEdit;
    pcLeft: TPageControl;
    pcSoftware: TPageControl;
    pMain: TPanel;
    pMiddle: TPanel;
    pSearch: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure bSearchClick(Sender: TObject);
    procedure cCancelSearchClick(Sender: TObject);

  private
    FCurrentGroup: cEmutecaGroup;
    FCurrentSoft: cEmutecaSoftware;
    FCurrentSystem: cEmutecaSystem;
    FDumpIcons: cCHXImageList;
    FEmuteca: cEmuteca;
    FfmCHXTagTree: TfmEmutecaTagTree;
    FfmEmutecaSystemCBX: TfmETKGUIIcnSysCBX;
    FfmSoftEditor: TfmETKGUIFullSoftEditor;
    FfmSoftMedia: TfmETKGUISoftMedia;
    FfmSoftTree: TfmETKGUIIcnSoftTree;
    FfmSystemPanel: TfmETKGUISysPreview;
    FGUIConfig: cETKGUIConfig;
    FOnEmulatorChanged: TEmutecaReturnEmulatorCB;
    FOnGroupChanged: TEmutecaReturnGroupCB;
    FOnGrpListChanged: TEmutecaGrpLstCB;
    FOnSoftChanged: TEmutecaSoftCB;
    FOnSoftDblClk: TEmutecaSoftCB;
    FOnSystemChanged: TEmutecaReturnSystemCB;
    FpmGroup: TPopupMenu;
    FpmSoft: TPopupMenu;
    FSHA1Folder: string;
    FZoneIcons: cCHXImageMap;
    procedure SetCurrentGroup(AValue: cEmutecaGroup);
    procedure SetCurrentSoft(AValue: cEmutecaSoftware);
    procedure SetCurrentSystem(AValue: cEmutecaSystem);
    procedure SetDumpIcons(AValue: cCHXImageList);
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetGUIConfig(AValue: cETKGUIConfig);
    procedure SetOnEmulatorChanged(
      const aOnEmulatorChanged: TEmutecaReturnEmulatorCB);
    procedure SetOnGroupChanged(AValue: TEmutecaReturnGroupCB);
    procedure SetOnGrpListChanged(AValue: TEmutecaGrpLstCB);
    procedure SetOnSoftChanged(AValue: TEmutecaSoftCB);
    procedure SetOnSoftDblClk(AValue: TEmutecaSoftCB);
    procedure SetOnSystemChanged(AValue: TEmutecaReturnSystemCB);
    procedure SetpmGroup(AValue: TPopupMenu);
    procedure SetpmSoft(AValue: TPopupMenu);
    procedure SetSHA1Folder(AValue: string);
    procedure SetZoneIcons(AValue: cCHXImageMap);

  protected

    procedure UpdateGroupList(aList: TStrings);

    property CurrentSoft: cEmutecaSoftware
      read FCurrentSoft write SetCurrentSoft;
    property CurrentGroup: cEmutecaGroup read FCurrentGroup
      write SetCurrentGroup;
    property CurrentSystem: cEmutecaSystem
      read FCurrentSystem write SetCurrentSystem;

    // Frames
    property fmEmutecaSystemCBX: TfmETKGUIIcnSysCBX read FfmEmutecaSystemCBX;

    property fmCHXTagTree: TfmEmutecaTagTree read FfmCHXTagTree;

    property fmSystemPanel: TfmETKGUISysPreview read FfmSystemPanel;
    property fmSoftEditor: TfmETKGUIFullSoftEditor read FfmSoftEditor;
    property fmSoftMedia: TfmETKGUISoftMedia read FfmSoftMedia;
    property fmSoftTree: TfmETKGUIIcnSoftTree read FfmSoftTree;

    procedure DoDblClkSoftware(aSoftware: cEmutecaSoftware);
    //< Double click on Software
    procedure DoSelectEmu(aEmulator: cEmutecaEmulator);
    //< Select a emulator

    procedure DoLoadGUIConfig(aIniFile: TIniFile); override;
    procedure DoSaveGUIConfig(aIniFile: TIniFile); override;

  public

    property GUIConfig: cETKGUIConfig read FGUIConfig write SetGUIConfig;

    property DumpIcons: cCHXImageList read FDumpIcons write SetDumpIcons;
    //< Icons for dump info
    property ZoneIcons: cCHXImageMap read FZoneIcons write SetZoneIcons;
    //< Icons of zones

    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    property OnSystemChanged: TEmutecaReturnSystemCB
      read FOnSystemChanged write SetOnSystemChanged;
    property OnGrpListChanged: TEmutecaGrpLstCB
      read FOnGrpListChanged write SetOnGrpListChanged;
    property OnGroupChanged: TEmutecaReturnGroupCB
      read FOnGroupChanged write SetOnGroupChanged;
    {< Callback when selecting a group. }
    property OnSoftChanged: TEmutecaSoftCB
      read FOnSoftChanged write SetOnSoftChanged;
    property OnSoftDblClk: TEmutecaSoftCB
      read FOnSoftDblClk write SetOnSoftDblClk;
    property OnEmulatorChanged: TEmutecaReturnEmulatorCB
      read FOnEmulatorChanged write SetOnEmulatorChanged;

    property pmSoft: TPopupMenu read FpmSoft write SetpmSoft;
    property pmGroup: TPopupMenu read FpmGroup write SetpmGroup;

    procedure LoadFrameData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.lfm}

{ TfmETKGUIMain }

procedure TfmETKGUIMain.SetDumpIcons(AValue: cCHXImageList);
begin
  if FDumpIcons = AValue then
    Exit;
  FDumpIcons := AValue;

  fmSoftTree.DumpIconList := DumpIcons;
end;

procedure TfmETKGUIMain.bSearchClick(Sender: TObject);
begin
  UpdateGroupList(fmCHXTagTree.CheckedList);
end;

procedure TfmETKGUIMain.cCancelSearchClick(Sender: TObject);
begin
  eSearch.Clear;
  UpdateGroupList(fmCHXTagTree.CheckedList);
end;

procedure TfmETKGUIMain.SetCurrentGroup(AValue: cEmutecaGroup);
begin
  CurrentSoft := nil;

  //if FCurrentGroup = AValue then
  //  Exit;
  FCurrentGroup := AValue;

  fmSoftEditor.Group := CurrentGroup;
  fmSoftMedia.Group := CurrentGroup;
  fmCHXTagTree.CurrentGroup := CurrentGroup;

  if assigned(OnGroupChanged) then
    OnGroupChanged(CurrentGroup);

  if Assigned(CurrentGroup) then
    fmSystemPanel.System := cEmutecaSystem(CurrentGroup.CachedSystem);
end;

procedure TfmETKGUIMain.SetCurrentSoft(AValue: cEmutecaSoftware);
begin
  if FCurrentSoft = AValue then
    Exit;
  FCurrentSoft := AValue;

  if assigned(OnSoftChanged) then
    OnSoftChanged(CurrentSoft);

  fmSoftEditor.Software := CurrentSoft;
  fmSoftMedia.Software := CurrentSoft;

  if Assigned(CurrentSoft) then
  begin
    fmSystemPanel.System := cEmutecaSystem(CurrentSoft.CachedSystem);
    fmCHXTagTree.CurrentGroup := cEmutecaGroup(CurrentSoft.CachedGroup);
  end;
end;

procedure TfmETKGUIMain.SetCurrentSystem(AValue: cEmutecaSystem);
begin
  CurrentGroup := nil;

  if FCurrentSystem = AValue then
    Exit;
  FCurrentSystem := AValue;

  if Assigned(CurrentSystem) then
  begin
    GUIConfig.CurrSystem := CurrentSystem.ID;
  end
  else
  begin
    GUIConfig.CurrSystem := '';
  end;

  if assigned(OnSystemChanged) then
    OnSystemChanged(CurrentSystem);

  UpdateGroupList(fmCHXTagTree.CheckedList);

  fmSystemPanel.System := CurrentSystem;
end;

procedure TfmETKGUIMain.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  if assigned(Emuteca) then
  begin
    fmEmutecaSystemCBX.SystemList := Emuteca.SystemManager.EnabledList;

    // TODO: Hack, don't work if all systems is selected
    FCurrentSystem := nil; // Updated by LoadFrameData;

    fmSoftMedia.TempFolder := Emuteca.TempFolder;

    fmCHXTagTree.TagsFolder :=
      SetAsAbsoluteFile(Emuteca.Config.TagsFolder, Emuteca.BaseFolder);
    // fmSoftTree.GroupList set by DoSelectSystem;

  end
  else
  begin
    fmEmutecaSystemCBX.SystemList := nil;
    fmCHXTagTree.TagsFolder := '';
    fmSoftTree.GroupList := nil;
    fmSoftEditor.Software := nil;
    fmSoftMedia.TempFolder := '';
    fmSoftMedia.Software := nil;
    fmSystemPanel.System := nil;
  end;

  LoadFrameData;
end;

procedure TfmETKGUIMain.SetGUIConfig(AValue: cETKGUIConfig);
begin
  if FGUIConfig = AValue then
    Exit;
  FGUIConfig := AValue;

  fmSoftMedia.ImageExt := GUIConfig.ImageExtensions;
  fmSoftMedia.TextExt := GUIConfig.TextExtensions;
  fmSoftMedia.VideoExt := GUIConfig.VideoExtensions;
  fmSoftMedia.MusicExt := GUIConfig.MusicExtensions;
  fmSoftMedia.MPlayerPath :=
    SetAsAbsoluteFile(GUIConfig.mPlayerExe,
    ExtractFileDir(GUIConfig.DefaultFileName));

  LoadFrameData;
end;

procedure TfmETKGUIMain.SetOnEmulatorChanged(
  const aOnEmulatorChanged: TEmutecaReturnEmulatorCB);
begin
  if FOnEmulatorChanged = aOnEmulatorChanged then
    Exit;
  FOnEmulatorChanged := aOnEmulatorChanged;
end;

procedure TfmETKGUIMain.SetOnGroupChanged(AValue: TEmutecaReturnGroupCB);
begin
  if FOnGroupChanged = AValue then
    Exit;
  FOnGroupChanged := AValue;
end;

procedure TfmETKGUIMain.SetOnGrpListChanged(AValue: TEmutecaGrpLstCB);
begin
  if FOnGrpListChanged = AValue then
    Exit;
  FOnGrpListChanged := AValue;
end;

procedure TfmETKGUIMain.SetOnSoftChanged(AValue: TEmutecaSoftCB);
begin
  if FOnSoftChanged = AValue then
    Exit;
  FOnSoftChanged := AValue;
end;

procedure TfmETKGUIMain.SetOnSoftDblClk(AValue: TEmutecaSoftCB);
begin
  if FOnSoftDblClk = AValue then
    Exit;
  FOnSoftDblClk := AValue;
end;

procedure TfmETKGUIMain.SetOnSystemChanged(AValue: TEmutecaReturnSystemCB);
begin
  if FOnSystemChanged = AValue then
    Exit;
  FOnSystemChanged := AValue;
end;

procedure TfmETKGUIMain.SetpmGroup(AValue: TPopupMenu);
begin
  if FpmGroup = AValue then
    Exit;
  FpmGroup := AValue;

  fmSoftTree.pmGroup := pmGroup;
end;

procedure TfmETKGUIMain.SetpmSoft(AValue: TPopupMenu);
begin
  if FpmSoft = AValue then
    Exit;
  FpmSoft := AValue;

  fmSoftTree.pmSoft := pmSoft;
end;

procedure TfmETKGUIMain.SetSHA1Folder(AValue: string);
begin
  if FSHA1Folder = AValue then
    Exit;
  FSHA1Folder := AValue;

  fmSystemPanel.SHA1Folder := SHA1Folder;
  fmSoftMedia.SHA1Folder := SHA1Folder;
end;

procedure TfmETKGUIMain.SetZoneIcons(AValue: cCHXImageMap);
begin
  if FZoneIcons = AValue then
    Exit;
  FZoneIcons := AValue;

  fmSoftTree.ZoneIconMap := ZoneIcons;
end;

procedure TfmETKGUIMain.UpdateGroupList(aList: TStrings);
begin

  Emuteca.UpdateCurrentGroupList(CurrentSystem, eSearch.Text,
    aList);
  fmSoftTree.GroupList := nil; // Forcing clearing tree

  if assigned(OnGrpListChanged) then
    OnGrpListChanged(Emuteca.CurrentGroupList);

  if assigned(Emuteca) then
  begin
    fmSoftTree.GroupList := Emuteca.CurrentGroupList;
  end;
end;

procedure TfmETKGUIMain.DoDblClkSoftware(aSoftware: cEmutecaSoftware);
begin
  if assigned(OnSoftDblClk) then
    OnSoftDblClk(aSoftware);
end;

procedure TfmETKGUIMain.DoSelectEmu(aEmulator: cEmutecaEmulator);
begin
  if Assigned(CurrentSystem) then
    CurrentSystem.CurrentEmulator := aEmulator;

  if Assigned(OnEmulatorChanged) then
    OnEmulatorChanged(aEmulator);
end;


procedure TfmETKGUIMain.LoadFrameData;
begin
  inherited LoadFrameData;

  Enabled := Assigned(Emuteca) and Assigned(GUIConfig);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  fmEmutecaSystemCBX.SelectedSystem :=
    fmEmutecaSystemCBX.SystemList.ItemById(GUIConfig.CurrSystem);

  CurrentSystem := fmEmutecaSystemCBX.SelectedSystem;
end;

procedure TfmETKGUIMain.DoLoadGUIConfig(aIniFile: TIniFile);
begin
  inherited DoLoadGUIConfig(aIniFile);

  pcLeft.Width := aIniFile.ReadInteger(krsIniMainFrameSection,
    krsIniMainFrameLeftPanelWidth, pcLeft.Width);
  pcSoftware.Width := aIniFile.ReadInteger(krsIniMainFrameSection,
    krsIniMainFrameRigthPanelWidth, pcSoftware.Width);
end;

procedure TfmETKGUIMain.DoSaveGUIConfig(aIniFile: TIniFile);
begin
  inherited DoSaveGUIConfig(aIniFile);

  aIniFile.WriteInteger(krsIniMainFrameSection, krsIniMainFrameLeftPanelWidth,
    pcLeft.Width);
  aIniFile.WriteInteger(krsIniMainFrameSection,
    krsIniMainFrameRigthPanelWidth, pcSoftware.Width);
end;

constructor TfmETKGUIMain.Create(TheOwner: TComponent);

  procedure CreateFrames;
  var
    aTabSheet: TTabSheet;
  begin
    // Better create frames in code while developing...

    // Creating and Setting the System ComboBox
    FfmEmutecaSystemCBX := TfmETKGUIIcnSysCBX.Create(pMiddle);
    fmEmutecaSystemCBX.Align := alTop;


    // TODO: Configurable
    fmEmutecaSystemCBX.cbxSystem.Height := 32;
    fmEmutecaSystemCBX.cbxSystem.Font.Height :=
      fmEmutecaSystemCBX.cbxSystem.Height;
    fmEmutecaSystemCBX.cbxSystem.ItemHeight :=
      fmEmutecaSystemCBX.cbxSystem.Height;

    fmEmutecaSystemCBX.FirstItem := ETKSysCBXFIAll;
    fmEmutecaSystemCBX.OnSelectSystem := @SetCurrentSystem;
    fmEmutecaSystemCBX.Parent := pMiddle;

    // Creating System Panel
    aTabSheet := pcLeft.AddTabSheet;
    FfmSystemPanel := TfmETKGUISysPreview.Create(aTabSheet);
    aTabSheet.Caption := rsSystemCaption;
    fmSystemPanel.OnChangeEmulator := @DoSelectEmu;
    fmSystemPanel.Align := alClient;
    fmSystemPanel.Parent := aTabSheet;

    // Creating and Setting Tags frame
    aTabSheet := pcLeft.AddTabSheet;
    FfmCHXTagTree := TfmEmutecaTagTree.Create(aTabSheet);
    aTabSheet.Caption := rsTagsCaption;
    fmCHXTagTree.OnCheckChange := @UpdateGroupList;
    fmCHXTagTree.Align := alClient;
    fmCHXTagTree.Parent := aTabSheet;

    // Creating SoftMedia frame
    aTabSheet := pcSoftware.AddTabSheet;
    FfmSoftMedia := TfmETKGUISoftMedia.Create(aTabSheet);
    aTabSheet.Caption := rsSoftMediaCaption;
    fmSoftMedia.Align := alClient;
    fmSoftMedia.Parent := aTabSheet;

    // Creating SoftEditor frame
    aTabSheet := pcSoftware.AddTabSheet;
    FfmSoftEditor := TfmETKGUIFullSoftEditor.Create(aTabSheet);
    aTabSheet.Caption := rsSoftEditorCaption;
    fmSoftEditor.Align := alClient;
    fmSoftEditor.SaveButtons := True;
    fmSoftEditor.ButtonClose := False;
    fmSoftEditor.Parent := aTabSheet;

    // Creating SoftTree frame
    FfmSoftTree := TfmETKGUIIcnSoftTree.Create(pMain);
    fmSoftTree.OnSelectGroup := @SetCurrentGroup;
    fmSoftTree.OnSelectSoft := @SetCurrentSoft;
    fmSoftTree.OnDblClkSoft := @DoDblClkSoftware;
    fmSoftTree.Align := alClient;
    fmSoftTree.Parent := pMain;
  end;

begin
  inherited Create(TheOwner);

  CreateFrames;
end;

destructor TfmETKGUIMain.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmETKGUIMain);

finalization
  UnRegisterClass(TfmETKGUIMain);
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
