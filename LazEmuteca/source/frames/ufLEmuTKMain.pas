{ This file is part of Emuteca

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
unit ufLEmuTKMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, IniFiles, Menus,
  // CHX
  uCHXStrUtils, ucCHXImageList,
  // CHX frames
  ufCHXFrame, ufCHXTagTree,
  // Emuteca common
  uEmutecaCommon,
  // Emuteca clases
  ucEmuteca, ucEmutecaSystem, ucEmutecaGroupList, ucEmutecaGroup,
  ucEmutecaSoftware,
  // Emuteca frames
  uLEmuTKCommon,
  ufLEmuTKFullSoftEditor, ufEmutecaSystemCBX, ufLEmuTKIcnSysCBX,
  ufLEmuTKSoftMedia,
  ufLEmuTKIcnSoftTree,
  ufLEmuTKSysPreview,
  // GUI
  uGUIConfig;

const
  krsIniMainFrameSection = 'MainFrame';
  krsIniMainFrameLeftPanelWidth = 'LeftPanel_Width';
  krsIniMainFrameRigthPanelWidth = 'RightPanel_Width';

type

  { TfmLEmuTKMain }

  TfmLEmuTKMain = class(TfmCHXFrame)
    eSearch: TEdit;
    pcLeft: TPageControl;
    pcSoftware: TPageControl;
    pMain: TPanel;
    pMiddle: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure eSearchEditingDone(Sender: TObject);

  private
    FCurrentGroup: cEmutecaGroup;
    FCurrentSoft: cEmutecaSoftware;
    FCurrentSystem: cEmutecaSystem;
    FDumpIcons: cCHXImageList;
    FEmuteca: cEmuteca;
    FfmCHXTagTree: TfmCHXTagTree;
    FfmEmutecaSystemCBX: TfmLEmuTKIcnSysCBX;
    FfmSoftEditor: TfmLEmuTKFullSoftEditor;
    FfmSoftMedia: TfmLEmuTKSoftMedia;
    FfmSoftTree: TfmLEmuTKIcnSoftTree;
    FfmSystemPanel: TfmLEmuTKSysPreview;
    FFullGroupList: cEmutecaGroupList;
    FGUIConfig: cGUIConfig;
    FOnGroupChanged: TEmutecaReturnGroupCB;
    FOnGrpListChanged: TEmutecaReturnGrpLstCB;
    FOnSoftChanged: TEmutecaReturnSoftCB;
    FOnSoftDblClk: TEmutecaReturnSoftCB;
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
    procedure SetGUIConfig(AValue: cGUIConfig);
    procedure SetOnGroupChanged(AValue: TEmutecaReturnGroupCB);
    procedure SetOnGrpListChanged(AValue: TEmutecaReturnGrpLstCB);
    procedure SetOnSoftChanged(AValue: TEmutecaReturnSoftCB);
    procedure SetOnSoftDblClk(AValue: TEmutecaReturnSoftCB);
    procedure SetOnSystemChanged(AValue: TEmutecaReturnSystemCB);
    procedure SetpmGroup(AValue: TPopupMenu);
    procedure SetpmSoft(AValue: TPopupMenu);
    procedure SetSHA1Folder(AValue: string);
    procedure SetZoneIcons(AValue: cCHXImageMap);

  protected
    property FullGroupList: cEmutecaGroupList read FFullGroupList;

    procedure UpdateFullGroupList;

    property CurrentSoft: cEmutecaSoftware
      read FCurrentSoft write SetCurrentSoft;
    property CurrentGroup: cEmutecaGroup read FCurrentGroup
      write SetCurrentGroup;
    property CurrentSystem: cEmutecaSystem
      read FCurrentSystem write SetCurrentSystem;

    // Frames
    property fmEmutecaSystemCBX: TfmLEmuTKIcnSysCBX read FfmEmutecaSystemCBX;

    property fmCHXTagTree: TfmCHXTagTree read FfmCHXTagTree;

    property fmSystemPanel: TfmLEmuTKSysPreview read FfmSystemPanel;
    property fmSoftEditor: TfmLEmuTKFullSoftEditor read FfmSoftEditor;
    property fmSoftMedia: TfmLEmuTKSoftMedia read FfmSoftMedia;
    property fmSoftTree: TfmLEmuTKIcnSoftTree read FfmSoftTree;

    function DoSelectSystem(aSystem: cEmutecaSystem): boolean;
    //< Select a system
    function DoSelectGroup(aGroup: cEmutecaGroup): boolean;
    //< Select a group
    function DoSelectSoftware(aSoftware: cEmutecaSoftware): boolean;
    //< Select a software
    function DoDblClkSoftware(aSoftware: cEmutecaSoftware): boolean;
    //< Double click on Software
    procedure CheckTags(aList: TStrings);
    //< Check Tags

    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoLoadGUIConfig(aIniFile: TIniFile);
    procedure DoSaveGUIConfig(aIniFile: TIniFile);

  public

    property GUIConfig: cGUIConfig read FGUIConfig write SetGUIConfig;

    property DumpIcons: cCHXImageList read FDumpIcons write SetDumpIcons;
    //< Icons for dump info
    property ZoneIcons: cCHXImageMap read FZoneIcons write SetZoneIcons;
    //< Icons of zones

    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;

    property OnSystemChanged: TEmutecaReturnSystemCB
      read FOnSystemChanged write SetOnSystemChanged;
    property OnGrpListChanged: TEmutecaReturnGrpLstCB
      read FOnGrpListChanged write SetOnGrpListChanged;
    property OnGroupChanged: TEmutecaReturnGroupCB
      read FOnGroupChanged write SetOnGroupChanged;
    property OnSoftChanged: TEmutecaReturnSoftCB
      read FOnSoftChanged write SetOnSoftChanged;
    property OnSoftDblClk: TEmutecaReturnSoftCB
      read FOnSoftDblClk write SetOnSoftDblClk;

    property pmSoft: TPopupMenu read FpmSoft write SetpmSoft;
    property pmGroup: TPopupMenu read FpmGroup write SetpmGroup;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.lfm}

{ TfmLEmuTKMain }

procedure TfmLEmuTKMain.eSearchEditingDone(Sender: TObject);
begin
  fmSoftTree.TitleFilter := eSearch.Text;
end;

procedure TfmLEmuTKMain.SetDumpIcons(AValue: cCHXImageList);
begin
  if FDumpIcons = AValue then
    Exit;
  FDumpIcons := AValue;

  fmSoftTree.DumpIconList := DumpIcons;
end;

procedure TfmLEmuTKMain.SetCurrentGroup(AValue: cEmutecaGroup);
begin
  if FCurrentGroup = AValue then
    Exit;
  FCurrentGroup := AValue;

  fmSoftEditor.Group := CurrentGroup;
  fmSoftMedia.Group := CurrentGroup;

  if assigned(OnGroupChanged) then
    {var :=} OnGroupChanged(CurrentGroup);

  if Assigned(CurrentGroup) then
    fmSystemPanel.System := cEmutecaSystem(CurrentGroup.CachedSystem);
end;

procedure TfmLEmuTKMain.SetCurrentSoft(AValue: cEmutecaSoftware);
begin
  if FCurrentSoft = AValue then
    Exit;
  FCurrentSoft := AValue;

  fmSoftEditor.Software := CurrentSoft;
  fmSoftMedia.Software := CurrentSoft;

  if assigned(OnSoftChanged) then
    {var :=} OnSoftChanged(CurrentSoft);

  if Assigned(CurrentSoft) then
    fmSystemPanel.System := cEmutecaSystem(CurrentSoft.CachedSystem);
end;

procedure TfmLEmuTKMain.SetCurrentSystem(AValue: cEmutecaSystem);
begin
  if FCurrentSystem = AValue then
    Exit;
  FCurrentSystem := AValue;

  if Assigned(CurrentSystem) then
  begin
    if not CurrentSystem.SoftGroupLoaded then
      Emuteca.SystemManager.LoadSystemData(CurrentSystem);
    GUIConfig.CurrSystem := CurrentSystem.ID;
    fmSoftTree.GroupList := CurrentSystem.GroupManager.VisibleList;
  end
  else
  begin
    Emuteca.SystemManager.LoadAllEnabledSystemsData;
    if FullGroupList.Count = 0 then // If empty populate
      UpdateFullGroupList;
    GUIConfig.CurrSystem := '';
    fmSoftTree.GroupList := FullGroupList;
  end;

  if assigned(Emuteca) then
    Emuteca.CacheData;

  // Using fmSoftTree.GroupList is dirty...
  if assigned(OnGrpListChanged) then
    {var :=} OnGrpListChanged(fmSoftTree.GroupList);

  if assigned(OnSystemChanged) then
    {var :=} OnSystemChanged(CurrentSystem);

  fmSystemPanel.System := CurrentSystem;
end;

procedure TfmLEmuTKMain.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  if assigned(Emuteca) then
  begin
    fmEmutecaSystemCBX.SystemList := Emuteca.SystemManager.EnabledList;

    // TODO: Hack, don't work if all systems is selected
    FCurrentSystem := nil; // Updated by LoadFrameData;

    fmCHXTagTree.Folder :=
      SetAsAbsoluteFile(Emuteca.Config.TagsFolder, Emuteca.BaseFolder);
    // fmSoftTree.GroupList set by DoSelectSystem;
  end
  else
  begin
    fmEmutecaSystemCBX.SystemList := nil;
    fmCHXTagTree.Folder := '';
    fmSoftTree.GroupList := nil;
    fmSoftEditor.Software := nil;
    fmSoftMedia.Software := nil;
    fmSystemPanel.System := nil;
    FullGroupList.Clear; // Clear full list
  end;

  LoadFrameData;
end;

procedure TfmLEmuTKMain.SetGUIConfig(AValue: cGUIConfig);
begin
  if FGUIConfig = AValue then
    Exit;
  FGUIConfig := AValue;

  fmSoftMedia.ImageExt := GUIConfig.ImageExtensions;
  fmSoftMedia.TextExt := GUIConfig.TextExtensions;

  LoadFrameData;
end;

procedure TfmLEmuTKMain.SetOnGroupChanged(AValue: TEmutecaReturnGroupCB);
begin
  if FOnGroupChanged = AValue then
    Exit;
  FOnGroupChanged := AValue;
end;

procedure TfmLEmuTKMain.SetOnGrpListChanged(AValue: TEmutecaReturnGrpLstCB);
begin
  if FOnGrpListChanged = AValue then
    Exit;
  FOnGrpListChanged := AValue;
end;

procedure TfmLEmuTKMain.SetOnSoftChanged(AValue: TEmutecaReturnSoftCB);
begin
  if FOnSoftChanged = AValue then
    Exit;
  FOnSoftChanged := AValue;
end;

procedure TfmLEmuTKMain.SetOnSoftDblClk(AValue: TEmutecaReturnSoftCB);
begin
  if FOnSoftDblClk = AValue then
    Exit;
  FOnSoftDblClk := AValue;
end;

procedure TfmLEmuTKMain.SetOnSystemChanged(AValue: TEmutecaReturnSystemCB);
begin
  if FOnSystemChanged = AValue then
    Exit;
  FOnSystemChanged := AValue;
end;

procedure TfmLEmuTKMain.SetpmGroup(AValue: TPopupMenu);
begin
  if FpmGroup = AValue then
    Exit;
  FpmGroup := AValue;

  fmSoftTree.pmGroup := pmGroup;
end;

procedure TfmLEmuTKMain.SetpmSoft(AValue: TPopupMenu);
begin
  if FpmSoft = AValue then
    Exit;
  FpmSoft := AValue;

  fmSoftTree.pmSoft := pmSoft;
end;

procedure TfmLEmuTKMain.SetSHA1Folder(AValue: string);
begin
  if FSHA1Folder = AValue then
    Exit;
  FSHA1Folder := AValue;

  fmSystemPanel.SHA1Folder := SHA1Folder;
  fmSoftMedia.SHA1Folder := SHA1Folder;
end;

procedure TfmLEmuTKMain.SetZoneIcons(AValue: cCHXImageMap);
begin
  if FZoneIcons = AValue then
    Exit;
  FZoneIcons := AValue;

  fmSoftTree.ZoneIconMap := ZoneIcons;
end;

procedure TfmLEmuTKMain.UpdateFullGroupList;
var
  i, j: Integer;
  aSystem: cEmutecaSystem;
begin
    FullGroupList.Clear;

  i := 0;
  while i < Emuteca.SystemManager.EnabledList.Count do
  begin
    aSystem := Emuteca.SystemManager.EnabledList[i];

    j := 0;
    while j < aSystem.GroupManager.VisibleList.Count do
    begin
      FullGroupList.Add(aSystem.GroupManager.VisibleList[j]);
      Inc(j);
    end;

    Inc(i);
  end;
end;

function TfmLEmuTKMain.DoSelectSystem(aSystem: cEmutecaSystem): boolean;
begin
  Result := DoSelectGroup(nil);

  CurrentSystem := aSystem;
end;

function TfmLEmuTKMain.DoSelectGroup(aGroup: cEmutecaGroup): boolean;
begin
  Result := DoSelectSoftware(nil);

  CurrentGroup := aGroup;
end;

function TfmLEmuTKMain.DoSelectSoftware(aSoftware: cEmutecaSoftware): boolean;
begin
  Result := True;

  CurrentSoft := aSoftware;
end;

function TfmLEmuTKMain.DoDblClkSoftware(aSoftware: cEmutecaSoftware): boolean;
begin
  Result := True;

  if assigned(OnSoftDblClk) then
    Result := OnSoftDblClk(aSoftware);
end;

procedure TfmLEmuTKMain.CheckTags(aList: TStrings);
begin

end;

procedure TfmLEmuTKMain.DoClearFrameData;
begin
  // Nothing
end;

procedure TfmLEmuTKMain.DoLoadFrameData;
begin
  Enabled := Assigned(Emuteca) and Assigned(GUIConfig);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  fmEmutecaSystemCBX.SelectedSystem :=
    fmEmutecaSystemCBX.SystemList.ItemById(GUIConfig.CurrSystem);

  DoSelectSystem(fmEmutecaSystemCBX.SelectedSystem);
end;

procedure TfmLEmuTKMain.DoLoadGUIConfig(aIniFile: TIniFile);
begin
  pcLeft.Width := aIniFile.ReadInteger(krsIniMainFrameSection,
    krsIniMainFrameLeftPanelWidth, pcLeft.Width);
  pcSoftware.Width := aIniFile.ReadInteger(krsIniMainFrameSection,
    krsIniMainFrameRigthPanelWidth, pcSoftware.Width);
end;

procedure TfmLEmuTKMain.DoSaveGUIConfig(aIniFile: TIniFile);
begin
  aIniFile.WriteInteger(krsIniMainFrameSection, krsIniMainFrameLeftPanelWidth,
    pcLeft.Width);
  aIniFile.WriteInteger(krsIniMainFrameSection,
    krsIniMainFrameRigthPanelWidth, pcSoftware.Width);
end;

constructor TfmLEmuTKMain.Create(TheOwner: TComponent);

  procedure CreateFrames;
  var
    aTabSheet: TTabSheet;
  begin
    // Better create frames in code while developing...

    // Creating and Setting the System ComboBox
    FfmEmutecaSystemCBX := TfmLEmuTKIcnSysCBX.Create(pMiddle);
    fmEmutecaSystemCBX.Align := alTop;
    // TODO: Configurable
    //fmEmutecaSystemCBX.cbxSystem.Font.Height := 32;
    //fmEmutecaSystemCBX.cbxSystem.Height := 32;
    //fmEmutecaSystemCBX.cbxSystem.ItemHeight := 32;
    fmEmutecaSystemCBX.FirstItem := ETKSysCBXFIAll;
    fmEmutecaSystemCBX.OnSelectSystem := @DoSelectSystem;
    fmEmutecaSystemCBX.Parent := pMiddle;

    // Creating System Panel
    aTabSheet := pcLeft.AddTabSheet;
    FfmSystemPanel := TfmLEmuTKSysPreview.Create(aTabSheet);
    aTabSheet.Caption := rsSystemCaption;
    fmSystemPanel.Align := alClient;
    fmSystemPanel.Parent := aTabSheet;

    // Creating and Setting Tags frame
    aTabSheet := pcLeft.AddTabSheet;
    FfmCHXTagTree := TfmCHXTagTree.Create(aTabSheet);
    aTabSheet.Caption := rsTagsCaption;
    fmCHXTagTree.OnCheckChange := @CheckTags;
    fmCHXTagTree.Align := alClient;
    fmCHXTagTree.Parent := aTabSheet;

    // Creating SoftMedia frame
    aTabSheet := pcSoftware.AddTabSheet;
    FfmSoftMedia := TfmLEmuTKSoftMedia.Create(aTabSheet);
    aTabSheet.Caption := rsSoftMediaCaption;
    fmSoftMedia.Align := alClient;
    fmSoftMedia.Parent := aTabSheet;

    // Creating SoftEditor frame
    aTabSheet := pcSoftware.AddTabSheet;
    FfmSoftEditor := TfmLEmuTKFullSoftEditor.Create(aTabSheet);
    aTabSheet.Caption := rsSoftEditorCaption;
    fmSoftEditor.Align := alClient;
    fmSoftEditor.SaveButtons := True;
    fmSoftEditor.ButtonClose := False;
    fmSoftEditor.Parent := aTabSheet;

    // Creating SoftTree frame
    FfmSoftTree := TfmLEmuTKIcnSoftTree.Create(pMain);
    // TODO: Configurable
    //fmSoftTree.VDT.Font.Height := 24;
    fmSoftTree.OnSelectGroup := @DoSelectGroup;
    fmSoftTree.OnSelectSoft := @DoSelectSoftware;
    fmSoftTree.OnDblClkSoft := @DoDblClkSoftware;
    fmSoftTree.Align := alClient;
    fmSoftTree.Parent := pMain;
  end;

begin
  inherited Create(TheOwner);

  FFullGroupList := cEmutecaGroupList.Create(False);

  CreateFrames;

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;

  OnLoadGUIConfig := @DoLoadGUIConfig;
  OnSaveGUIConfig := @DoSaveGUIConfig;
end;

destructor TfmLEmuTKMain.Destroy;
begin
  FFullGroupList.Free;

  inherited Destroy;
end;

end.
