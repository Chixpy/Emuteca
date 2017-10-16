unit ufLEmuTKMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls,
  // CHX
  uCHXStrUtils, ucCHXImageList,
  // CHX frames
  ufCHXFrame, ufCHXTagTree,
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
    stbInfo: TStatusBar;

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
    FZoneIcons: cCHXImageMap;
    procedure SetDumpIcons(AValue: cCHXImageList);
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetGUIConfig(AValue: cGUIConfig);
    procedure SetIconList(AValue: cCHXImageList);
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

    function SelectSystem(aSystem: cEmutecaSystem): boolean;
    //< Select a system
    function SelectGroup(aGroup: cEmutecaGroup): boolean;
    //< Select a group
    function SelectSoftware(aSoftware: cEmutecaSoftware): boolean;
    //< Select a software
    procedure CheckTags(aList: TStrings);
    //< Check Tags
    function RunSoftware(aSoftware: cEmutecaSoftware): boolean;
    //< Run a software

    procedure ClearFrameData; override;
    procedure LoadFrameData; override;
  public

    property GUIConfig: cGUIConfig read FGUIConfig write SetGUIConfig;

    property IconList: cCHXImageList read FIconList write SetIconList;
    //< Icons for parents, soft, systems and emulators
    property DumpIcons: cCHXImageList read FDumpIcons write SetDumpIcons;
    //< Icons for dump info
    property ZoneIcons: cCHXImageMap read FZoneIcons write SetZoneIcons;
    //< Icons of zones

    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;


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
    // fmSoftTree.GroupList set by SelectSystem;
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

  GUIConfigIni := GUIConfig.ConfigFile;

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

  fmEmutecaSystemCBX.SysIcons := IconList;
  fmSoftTree.SoftIconList := IconList;
end;

procedure TfmLEmuTKMain.SetZoneIcons(AValue: cCHXImageMap);
begin
  if FZoneIcons = AValue then
    Exit;
  FZoneIcons := AValue;

  fmSoftTree.ZoneIconMap := ZoneIcons;
end;

function TfmLEmuTKMain.SelectSystem(aSystem: cEmutecaSystem): boolean;
begin
  Result := SelectGroup(nil);

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

  fmSystemPanel.System := aSystem;
end;

function TfmLEmuTKMain.SelectGroup(aGroup: cEmutecaGroup): boolean;
begin
  Result := SelectSoftware(nil);

  fmSoftEditor.Group := aGroup;
  fmSoftMedia.Group := aGroup;

  if Assigned(aGroup) then
    fmSystemPanel.System := cEmutecaSystem(aGroup.CachedSystem);
end;

function TfmLEmuTKMain.SelectSoftware(aSoftware: cEmutecaSoftware): boolean;
begin
  Result := True;

  fmSoftEditor.Software := aSoftware;
  fmSoftMedia.Software := aSoftware;

  if Assigned(aSoftware) then
    fmSystemPanel.System := cEmutecaSystem(aSoftware.CachedSystem);
end;

procedure TfmLEmuTKMain.CheckTags(aList: TStrings);
begin

end;

function TfmLEmuTKMain.RunSoftware(aSoftware: cEmutecaSoftware): boolean;
begin
  Result := Emuteca.RunSoftware(aSoftware) = 0;
end;

procedure TfmLEmuTKMain.ClearFrameData;
begin

end;

procedure TfmLEmuTKMain.LoadFrameData;
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

  SelectSystem(fmEmutecaSystemCBX.SelectedSystem);
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
    fmEmutecaSystemCBX.OnSelectSystem := @SelectSystem;
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
    fmSoftTree.OnSelectGroup := @SelectGroup;
    fmSoftTree.OnSelectSoft := @SelectSoftware;
    fmSoftTree.OnDblClkSoft := @RunSoftware;
    fmSoftTree.Align := alClient;
    fmSoftTree.Parent := pMain;
  end;

begin
  inherited Create(TheOwner);

  FFullGroupList := cEmutecaGroupList.Create(False);
  FFullSoftlist := cEmutecaSoftList.Create(False);

  CreateFrames;
end;

destructor TfmLEmuTKMain.Destroy;
begin
  FFullGroupList.Free;
  FFullSoftlist.Free;

  inherited Destroy;
end;

end.
