unit ufLEmuTKMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls,
  ActnList,
  // CHX
  uCHXStrUtils, uCHXImageUtils, ucCHXImageList,
  // CHX frames
  ufCHXTagTree,
  // Emuteca clases
  ucEmuteca, ucEmutecaSystem, ucEmutecaGroupList, ucEmutecaGroup,
  ucEmutecaSoftList, ucEmutecaSoftware,
  // Emuteca frames
  ufEmutecaSoftEditor, ufLEmuTKIcnSysCBX, ufLEmuTKSoftMediaOld, ufLEmuTKIcnSoftTree,
  ufEmutecaSystemPanel,
  // GUI
  uGUIConfig;

type

  { TfmLEmuTKMain }

  TfmLEmuTKMain = class(TFrame)
    ActionList1: TActionList;
    eSearch: TEdit;
    ilActImages: TImageList;
    pMain: TPanel;
    pcLeft: TPageControl;
    pcSoftware: TPageControl;
    pMiddle: TPanel;
    pSystems: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    stbInfo: TStatusBar;

  private
    FEmuteca: cEmuteca;
    FfmCHXTagTree: TfmCHXTagTree;
    FfmEmutecaSoftEditor: TfmEmutecaSoftEditor;
    FfmEmutecaSystemCBX: TfmLEmuTKIcnSysCBX;
    FfmSoftMedia: TfmLEmuTKSoftMedia;
    FfmSoftTree: TfmLEmuTKIcnSoftTree;
    FfmSystemPanel: TfmEmutecaSystemPanel;
    FFullGroupList: cEmutecaGroupList;
    FFullSoftlist: cEmutecaSoftList;
    FGUIConfig: cGUIConfig;
    FGUIIconsIni: string;
    FIconList: cCHXImageList;
    FDumpIcons: cCHXImageList;
    FZoneIcons: cCHXImageMap;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetGUIConfig(AValue: cGUIConfig);
    procedure SetGUIIconsIni(AValue: string);
    procedure SetIconList(AValue: cCHXImageList);
    procedure SetDumpIcons(AValue: cCHXImageList);
    procedure SetZoneIcons(AValue: cCHXImageMap);

  protected
    property FullGroupList: cEmutecaGroupList read FFullGroupList;
    property FullSoftlist: cEmutecaSoftList read FFullSoftlist;

    // Frames
    property fmEmutecaSystemCBX: TfmLEmuTKIcnSysCBX read FfmEmutecaSystemCBX;

    property fmCHXTagTree: TfmCHXTagTree read FfmCHXTagTree;

    property fmSystemPanel: TfmEmutecaSystemPanel read FfmSystemPanel;
    property fmSoftEditor: TfmEmutecaSoftEditor
      read FfmEmutecaSoftEditor;
    property fmSoftMedia: TfmLEmuTKSoftMedia read FfmSoftMedia;
    property fmSoftTree: TfmLEmuTKIcnSoftTree read FfmSoftTree;

    procedure LoadFullLists;

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

  public
    property GUIIconsIni: string read FGUIIconsIni write SetGUIIconsIni;
    property GUIConfig: cGUIConfig read FGUIConfig write SetGUIConfig;
    property IconList: cCHXImageList read FIconList write SetIconList;
    //< Icons for parents, soft, systems and emulators
    property DumpIcons: cCHXImageList read FDumpIcons write SetDumpIcons;
    //< Icons for dump info
    property ZoneIcons: cCHXImageMap read FZoneIcons write SetZoneIcons;
    //< Icons of zones

    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    procedure ClearData;
    procedure LoadData;
    procedure SaveData;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKMain }

procedure TfmLEmuTKMain.SetGUIIconsIni(AValue: string);
begin
  FGUIIconsIni := SetAsFile(AValue);

  ReadActionsIcons(GUIIconsIni, Name, ilActImages, ActionList1);

  fmSoftMedia.IconsIni := GUIIconsIni;
end;

procedure TfmLEmuTKMain.SetIconList(AValue: cCHXImageList);
begin
  if FIconList = AValue then
    Exit;
  FIconList := AValue;

  fmEmutecaSystemCBX.SysIcons := IconList;
  fmSoftTree.SoftIconList := IconList;
end;

procedure TfmLEmuTKMain.SetDumpIcons(AValue: cCHXImageList);
begin
  if FDumpIcons = AValue then
    Exit;
  FDumpIcons := AValue;

  fmSoftTree.DumpIconList := DumpIcons;
end;

procedure TfmLEmuTKMain.SetZoneIcons(AValue: cCHXImageMap);
begin
  if FZoneIcons = AValue then
    Exit;
  FZoneIcons := AValue;

  fmSoftTree.ZoneIconMap := ZoneIcons;
end;

procedure TfmLEmuTKMain.LoadFullLists;
var
  i, j, k: integer;
  aSystem: cEmutecaSystem;
begin
  FullGroupList.Clear;
  FullSoftlist.Clear;

  if not Assigned(Emuteca) then
    Exit;

  i := 0;
  while i < Emuteca.SystemManager.EnabledList.Count do
  begin
    aSystem := Emuteca.SystemManager.EnabledList[i];

    j := 0;
    while j < aSystem.GroupManager.FullList.Count do
    begin
      FullGroupList.Add(aSystem.GroupManager.FullList[j]);

      k := 0;
      while k < aSystem.GroupManager.FullList.Count do
      begin
        FullSoftlist.Add(aSystem.SoftManager.FullList[k]);
        Inc(k);
      end;
      Inc(j);
    end;
    Inc(i);
  end;

end;

function TfmLEmuTKMain.SelectSystem(aSystem: cEmutecaSystem): boolean;
begin
  Result := SelectGroup(nil);

  if Assigned(aSystem) then
  begin
    GUIConfig.CurrSystem := aSystem.ID;
    fmSoftTree.GroupList := aSystem.GroupManager.FullList;
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

  // fmSoftEditor.Group := aGroup;
  fmSoftMedia.Group := aGroup;
end;

function TfmLEmuTKMain.SelectSoftware(aSoftware: cEmutecaSoftware): boolean;
begin
  Result := True;
  fmSoftEditor.Software := aSoftware;
  fmSoftMedia.Software := aSoftware;
end;

procedure TfmLEmuTKMain.CheckTags(aList: TStrings);
begin

end;

function TfmLEmuTKMain.RunSoftware(aSoftware: cEmutecaSoftware): boolean;
begin
  Result := Emuteca.RunSoftware(aSoftware) = 0;
end;

procedure TfmLEmuTKMain.ClearData;
begin
  fmSoftTree.ClearData;
  fmEmutecaSystemCBX.SystemList := nil;
  fmEmutecaSystemCBX.ClearData;
  fmSoftMedia.ClearData;

  FullGroupList.Clear;
end;

procedure TfmLEmuTKMain.LoadData;
begin
  Enabled := Assigned(Emuteca) and assigned(GUIConfig);

  if not Enabled then
  begin
    ClearData;
    Exit;
  end;

  LoadFullLists;

  fmSoftMedia.Emuteca := Emuteca;

  fmEmutecaSystemCBX.SystemList := Emuteca.SystemManager.EnabledList;
  fmEmutecaSystemCBX.SelectedSystem :=
    fmEmutecaSystemCBX.SystemList.ItemById(GUIConfig.CurrSystem);

  fmCHXTagTree.Folder := SetAsAbsoluteFile(Emuteca.Config.TagsFolder,
    Emuteca.BaseFolder);

  SelectSystem(fmEmutecaSystemCBX.SelectedSystem);
end;

procedure TfmLEmuTKMain.SaveData;
begin

end;

procedure TfmLEmuTKMain.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  fmSoftTree.Emuteca := Emuteca;

  LoadData;
end;

procedure TfmLEmuTKMain.SetGUIConfig(AValue: cGUIConfig);
begin
  if FGUIConfig = AValue then
    Exit;
  FGUIConfig := AValue;

  fmSoftMedia.GUIConfig := GUIConfig;
  fmSoftTree.GUIConfig := GUIConfig;

  LoadData;
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
    fmEmutecaSystemCBX.OnSelectSystem := @SelectSystem;
    fmEmutecaSystemCBX.Parent := pMiddle;

    // Creating System Panel
    aTabSheet := pcLeft.AddTabSheet;
    FfmSystemPanel := TfmEmutecaSystemPanel.Create(aTabSheet);
    aTabSheet.Caption := fmSystemPanel.Name;  // TODO: Add Caption
    fmSystemPanel.Align := alClient;
    fmSystemPanel.Parent := aTabSheet;

    // Creating and Setting Tags frame
    aTabSheet := pcLeft.AddTabSheet;
    FfmCHXTagTree := TfmCHXTagTree.Create(aTabSheet);
    aTabSheet.Caption := fmCHXTagTree.Name;  // TODO: Add Caption
    fmCHXTagTree.OnCheckChange := @CheckTags;
    fmCHXTagTree.Align := alClient;
    fmCHXTagTree.Parent := aTabSheet;

    // Creating SoftMedia frame
    aTabSheet := pcSoftware.AddTabSheet;
    FfmSoftMedia := TfmLEmuTKSoftMedia.Create(aTabSheet);
    aTabSheet.Caption := fmSoftMedia.Name;  // TODO: Add Caption
    fmSoftMedia.Align := alClient;
    fmSoftMedia.Parent := aTabSheet;

    // Creating SoftEditor frame
    aTabSheet := pcSoftware.AddTabSheet;
    FfmEmutecaSoftEditor := TfmEmutecaSoftEditor.Create(aTabSheet);
    aTabSheet.Caption := fmSoftEditor.Name;  // TODO: Add Caption
    fmSoftEditor.Align := alClient;
    fmSoftEditor.SaveButtons := True;
    fmSoftEditor.ButtonClose := False;
    fmSoftEditor.Parent := aTabSheet;

    // Creating SoftTree frame
    FfmSoftTree := TfmLEmuTKIcnSoftTree.Create(pMain);
    fmSoftTree.OnSelectGroup := @SelectGroup;
    fmSoftTree.OnSelectSoft := @SelectSoftware;
    fmSoftTree.OnDblClkSoft := @RunSoftware;
    fmSoftTree.Align := alClient;
    fmSoftTree.Parent := pMain;

  end;

begin
  inherited Create(TheOwner);

  Enabled := False;

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
