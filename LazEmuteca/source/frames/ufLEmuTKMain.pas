unit ufLEmuTKMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, IniFiles,
  // CHX
  uCHXStrUtils, ucCHXImageList,
  // CHX frames
  ufCHXFrame, ufCHXTagTree,
  // Emuteca common
  uEmutecaCommon,
  // Emuteca clases
  ucEmuteca, ucEmutecaSystem, ucEmutecaGroupList, ucEmutecaGroup,
  ucEmutecaSoftList, ucEmutecaSoftware,
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

  private
    FDumpIcons: cCHXImageList;
    FEmuteca: cEmuteca;
    FfmCHXTagTree: TfmCHXTagTree;
    FfmEmutecaSystemCBX: TfmLEmuTKIcnSysCBX;
    FfmSoftEditor: TfmLEmuTKFullSoftEditor;
    FfmSoftMedia: TfmLEmuTKSoftMedia;
    FfmSoftTree: TfmLEmuTKIcnSoftTree;
    FfmSystemPanel: TfmLEmuTKSysPreview;
    FFullGroupList: cEmutecaGroupList;
    FFullSoftlist: cEmutecaSoftList;
    FGUIConfig: cGUIConfig;
    FIconList: cCHXImageList;
    FOnGroupChanged: TEmutecaReturnGroupCB;
    FOnGrpListChanged: TEmutecaReturnGrpLstCB;
    FOnSoftChanged: TEmutecaReturnSoftCB;
    FOnSoftDblClk: TEmutecaReturnSoftCB;
    FOnSystemChanged: TEmutecaReturnSystemCB;
    FSHA1Folder: string;
    FZoneIcons: cCHXImageMap;
    procedure SetDumpIcons(AValue: cCHXImageList);
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetGUIConfig(AValue: cGUIConfig);
    procedure SetIconList(AValue: cCHXImageList);
    procedure SetOnGroupChanged(AValue: TEmutecaReturnGroupCB);
    procedure SetOnGrpListChanged(AValue: TEmutecaReturnGrpLstCB);
    procedure SetOnSoftChanged(AValue: TEmutecaReturnSoftCB);
    procedure SetOnSoftDblClk(AValue: TEmutecaReturnSoftCB);
    procedure SetOnSystemChanged(AValue: TEmutecaReturnSystemCB);
    procedure SetSHA1Folder(AValue: string);
    procedure SetZoneIcons(AValue: cCHXImageMap);

  protected
    property FullGroupList: cEmutecaGroupList read FFullGroupList;
    property FullSoftlist: cEmutecaSoftList read FFullSoftlist;

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

    property IconList: cCHXImageList read FIconList write SetIconList;
    //< Icons for parents, soft, systems and emulators
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
    property OnSoftDblClk: TEmutecaReturnSoftCB read FOnSoftDblClk write SetOnSoftDblClk;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.lfm}

{ TfmLEmuTKMain }

procedure TfmLEmuTKMain.SetDumpIcons(AValue: cCHXImageList);
begin
  if FDumpIcons = AValue then
    Exit;
  FDumpIcons := AValue;

  fmSoftTree.DumpIconList := DumpIcons;
end;

procedure TfmLEmuTKMain.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  if assigned(Emuteca) then
  begin
    fmEmutecaSystemCBX.SystemList := Emuteca.SystemManager.EnabledList;
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

  fmSoftTree.ImageExt := GUIConfig.ImageExtensions;

  LoadFrameData;
end;

procedure TfmLEmuTKMain.SetIconList(AValue: cCHXImageList);
begin
  if FIconList = AValue then
    Exit;
  FIconList := AValue;


  if Assigned(IconList) and (IconList.Count > 2) then
  begin
    { Icons for games parents and software, first default one
      0: Default for software
      1: Default for parent
      2: Default for system
      3: Default for emulator
    }
    fmEmutecaSystemCBX.DefSysIcon := IconList[2];

    fmSoftTree.DefSoftIcon := IconList[0];
    fmSoftTree.DefGrpIcon := IconList[1];
    fmSoftTree.DefSysIcon := IconList[2];
  end
  else
  begin
    fmEmutecaSystemCBX.DefSysIcon := nil;
    fmSoftTree.DefSoftIcon := nil;
    fmSoftTree.DefGrpIcon := nil;
    fmSoftTree.DefSysIcon := nil;
  end;
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
  if FOnSoftDblClk=AValue then Exit;
  FOnSoftDblClk:=AValue;
end;

procedure TfmLEmuTKMain.SetOnSystemChanged(AValue: TEmutecaReturnSystemCB);
begin
  if FOnSystemChanged = AValue then
    Exit;
  FOnSystemChanged := AValue;
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

function TfmLEmuTKMain.DoSelectSystem(aSystem: cEmutecaSystem): boolean;
begin
  Result := DoSelectGroup(nil);

  if Assigned(aSystem) then
  begin
    GUIConfig.CurrSystem := aSystem.ID;
    fmSoftTree.GroupList := aSystem.GroupManager.VisibleList;
  end
  else
  begin
    GUIConfig.CurrSystem := '';
    fmSoftTree.GroupList := FullGroupList;
  end;

  // Using fmSoftTree.GroupList is dirty...
  // Catching icons
  if assigned(OnGrpListChanged) then
    OnGrpListChanged(fmSoftTree.GroupList);

  if assigned(OnSystemChanged) then
    OnSystemChanged(aSystem);

  fmSystemPanel.System := aSystem;
end;

function TfmLEmuTKMain.DoSelectGroup(aGroup: cEmutecaGroup): boolean;
begin
  Result := DoSelectSoftware(nil);

  fmSoftEditor.Group := aGroup;
  fmSoftMedia.Group := aGroup;

  if assigned(OnGroupChanged) then
    OnGroupChanged(aGroup);

  if Assigned(aGroup) then
    fmSystemPanel.System := cEmutecaSystem(aGroup.CachedSystem);
end;

function TfmLEmuTKMain.DoSelectSoftware(aSoftware: cEmutecaSoftware): boolean;
begin
  Result := True;

  fmSoftEditor.Software := aSoftware;
  fmSoftMedia.Software := aSoftware;

  if assigned(OnSoftChanged) then
    Result := OnSoftChanged(aSoftware);

  if Assigned(aSoftware) then
    fmSystemPanel.System := cEmutecaSystem(aSoftware.CachedSystem)
  else
    fmSystemPanel.System := nil;
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
var
  i, j: integer;
  aSystem: cEmutecaSystem;
begin
  FullGroupList.Clear;
  FullSoftlist.Clear;

  Enabled := Assigned(Emuteca) and Assigned(GUIConfig);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

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

    j := 0;
    while j < aSystem.SoftManager.FullList.Count do
    begin
      FullSoftlist.Add(aSystem.SoftManager.FullList[j]);
      Inc(j);
    end;
    Inc(i);
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
  aIniFile.WriteInteger(krsIniMainFrameSection, krsIniMainFrameLeftPanelWidth, pcLeft.Width);
  aIniFile.WriteInteger(krsIniMainFrameSection, krsIniMainFrameRigthPanelWidth, pcSoftware.Width);
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
    fmEmutecaSystemCBX.cbxSystem.Font.Height := 32;
    fmEmutecaSystemCBX.cbxSystem.Height := 32;
    fmEmutecaSystemCBX.cbxSystem.ItemHeight := 32;
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
    fmSoftTree.VDT.Font.Height := 24;
    fmSoftTree.OnSelectGroup := @DoSelectGroup;
    fmSoftTree.OnSelectSoft := @DoSelectSoftware;
    fmSoftTree.OnDblClkSoft := @DoDblClkSoftware;
    fmSoftTree.Align := alClient;
    fmSoftTree.Parent := pMain;
  end;

begin
  inherited Create(TheOwner);

  FFullGroupList := cEmutecaGroupList.Create(False);
  FFullSoftlist := cEmutecaSoftList.Create(False);

  CreateFrames;

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;

    OnLoadGUIConfig := @DoLoadGUIConfig;
  OnSaveGUIConfig := @DoSaveGUIConfig;
end;

destructor TfmLEmuTKMain.Destroy;
begin
  FFullGroupList.Free;
  FFullSoftlist.Free;

  inherited Destroy;
end;

end.
