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
  ucEmuteca, ucEmutecaSystem, ucEmutecaGroup, ucEmutecaSoftware,
  // Emuteca frames
  ufLEmuTKIcnGrpList, ufLEmuTKIcnSoftList,
  ufEmutecaSystemCBX, ufEmutecaSoftEditor, ufLEmuTKSoftMedia,
  uGUIConfig;

type

  { TfmLEmuTKMain }

  TfmLEmuTKMain = class(TFrame)
    ActionList1: TActionList;
    eSearch: TEdit;
    ilActImages: TImageList;
    pBottom: TPanel;
    pcLeft: TPageControl;
    pcSoftware: TPageControl;
    pMiddle: TPanel;
    pSystems: TPanel;
    pTop: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    stbInfo: TStatusBar;

  private
    FEmuteca: cEmuteca;
    FfmCHXTagTree: TfmCHXTagTree;
    FfmEmutecaGroupList: TfmLEmuTKIcnGrpList;
    FfmEmutecaSoftEditor: TfmEmutecaSoftEditor;
    FfmEmutecaSoftList: TfmLEmuTKIcnSoftList;
    FfmEmutecaSystemCBX: TfmEmutecaSystemCBX;
    FfmSoftMedia: TfmLEmuTKSoftMedia;
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
    // Frames
    property fmEmutecaSystemCBX: TfmEmutecaSystemCBX read FfmEmutecaSystemCBX;
    property fmEmutecaGroupList: TfmLEmuTKIcnGrpList read FfmEmutecaGroupList;
    property fmEmutecaSoftList: TfmLEmuTKIcnSoftList read FfmEmutecaSoftList;

    property fmCHXTagTree: TfmCHXTagTree read FfmCHXTagTree;

    property fmEmutecaSoftEditor: TfmEmutecaSoftEditor
      read FfmEmutecaSoftEditor;
    property fmSoftMedia: TfmLEmuTKSoftMedia read FfmSoftMedia;

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

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKMain }

procedure TfmLEmuTKMain.SetGUIIconsIni(AValue: string);
begin
  FGUIIconsIni := SetAsFile(AValue);

  ReadActionsIcons(GUIIconsIni, Self.Name, ilActImages, ActionList1);

  fmSoftMedia.IconsIni := GUIIconsIni;
end;

procedure TfmLEmuTKMain.SetIconList(AValue: cCHXImageList);
begin
  if FIconList = AValue then
    Exit;
  FIconList := AValue;

  fmEmutecaSoftList.SoftIconList := IconList;
  fmEmutecaGroupList.GroupIconList := IconList;
end;

procedure TfmLEmuTKMain.SetDumpIcons(AValue: cCHXImageList);
begin
  if FDumpIcons = AValue then
    Exit;
  FDumpIcons := AValue;

  fmEmutecaSoftList.DumpIconList := DumpIcons;
end;

procedure TfmLEmuTKMain.SetZoneIcons(AValue: cCHXImageMap);
begin
  if FZoneIcons = AValue then
    Exit;
  FZoneIcons := AValue;

  fmEmutecaSoftList.ZoneIconMap := ZoneIcons;
end;

function TfmLEmuTKMain.SelectSystem(aSystem: cEmutecaSystem): boolean;
begin
  Emuteca.SoftManager.VisibleSystem := aSystem;

  Result := SelectGroup(nil);

  if Assigned(aSystem) then
  begin
    GUIConfig.CurrSystem := aSystem.ID;
    if fmEmutecaGroupList.GroupList =
      aSystem.GroupManager.VisibleList then
      fmEmutecaGroupList.UpdateList
    else
      fmEmutecaGroupList.GroupList := aSystem.GroupManager.VisibleList;
  end
  else
  begin
    GUIConfig.CurrSystem := '';

    // TODO: List all groups
    fmEmutecaGroupList.GroupList := nil;
  end;

  fmEmutecaGroupList.UpdateList;
end;

function TfmLEmuTKMain.SelectGroup(aGroup: cEmutecaGroup): boolean;
begin
  Emuteca.SoftManager.VisibleGroup := aGroup;
  fmEmutecaSoftList.UpdateList;

  Result := SelectSoftware(nil);

  fmEmutecaSoftEditor.Group := aGroup;
  fmSoftMedia.Group := aGroup;

end;

function TfmLEmuTKMain.SelectSoftware(aSoftware: cEmutecaSoftware): boolean;
begin
  Result := True;
  fmEmutecaSoftEditor.Software := aSoftware;
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
  fmEmutecaSoftList.SoftList := nil;
  fmEmutecaGroupList.GroupList := nil;
  fmEmutecaSystemCBX.SystemList := nil;
  fmEmutecaSystemCBX.SelectedSystem := nil;
end;

procedure TfmLEmuTKMain.LoadData;
begin
  Self.Enabled := Assigned(Emuteca) and assigned(GUIConfig);

  if not Self.Enabled then
  begin
    ClearData;
    Exit;
  end;

  fmEmutecaSoftList.SoftList := Emuteca.SoftManager.VisibleList;

  fmEmutecaSystemCBX.SystemList := Emuteca.SystemManager.EnabledList;
  fmEmutecaSystemCBX.SelectedSystem :=
    Emuteca.SystemManager.ItemById(GUIConfig.CurrSystem, False);

  //fmCHXTagTree.Folder := GUIConfig.TagSubFolder;

  SelectSystem(fmEmutecaSystemCBX.SelectedSystem);
end;

procedure TfmLEmuTKMain.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  fmEmutecaGroupList.Emuteca := Emuteca;
  fmEmutecaSoftList.Emuteca := Emuteca;
  fmSoftMedia.Emuteca := Emuteca;

  LoadData;
end;


procedure TfmLEmuTKMain.SetGUIConfig(AValue: cGUIConfig);
begin
  if FGUIConfig = AValue then
    Exit;
  FGUIConfig := AValue;

  fmEmutecaGroupList.GUIConfig := GUIConfig;
  fmEmutecaSoftList.GUIConfig := GUIConfig;
  fmSoftMedia.GUIConfig := GUIConfig;

  LoadData;
end;

constructor TfmLEmuTKMain.Create(TheOwner: TComponent);

  procedure CreateFrames;
  var
    aTabSheet: TTabSheet;
  begin
    // Better create frames in code while developing...

    // Creating and Setting the System ComboBox
    FfmEmutecaSystemCBX := TfmEmutecaSystemCBX.Create(pMiddle);
    fmEmutecaSystemCBX.Align := alTop;
    fmEmutecaSystemCBX.OnSelectSystem := @Self.SelectSystem;
    fmEmutecaSystemCBX.Parent := pMiddle;

    // Creating and setting the parent list frame
    FfmEmutecaGroupList := TfmLEmuTKIcnGrpList.Create(pTop);
    fmEmutecaGroupList.OnItemSelect := @Self.SelectGroup;
    fmEmutecaGroupList.Parent := pTop;

    // Creating and Setting the software list frame
    FfmEmutecaSoftList := TfmLEmuTKIcnSoftList.Create(pBottom);
    fmEmutecaSoftList.OnItemSelect := @Self.SelectSoftware;
    fmEmutecaSoftList.OnDblClick := @Self.RunSoftware;
    fmEmutecaSoftList.Parent := pBottom;

    // Creating and Setting Tags frame
    aTabSheet := pcLeft.AddTabSheet;
    FfmCHXTagTree := TfmCHXTagTree.Create(aTabSheet);
    aTabSheet.Caption := fmCHXTagTree.Name;  // TODO: Add Caption
    fmCHXTagTree.OnCheckChange := @self.CheckTags;
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
    aTabSheet.Caption := fmEmutecaSoftEditor.Name;  // TODO: Add Caption
    fmEmutecaSoftEditor.Align := alClient;
    fmEmutecaSoftEditor.SaveButtons := True;
    fmEmutecaSoftEditor.ButtonClose := False;
    fmEmutecaSoftEditor.Parent := aTabSheet;
  end;

begin
  inherited Create(TheOwner);

  Self.Enabled := False;

  CreateFrames;
end;

destructor TfmLEmuTKMain.Destroy;
begin
  inherited Destroy;
end;

end.
