unit ufEmutecaSoftEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls,
  ufCHXPropEditor,
  uEmutecaRscStr, uaEmutecaCustomSoft,
  ucEmutecaSystem, ucEmutecaGroup, ucEmutecaSoftware,
  ufEmutecaGroupCBXOld;

type

  { TfmEmutecaSoftEditor }

  TfmEmutecaSoftEditor = class(TfmCHXPropEditor, IFPObserver)
    cbxDumpType: TComboBox;
    eCracked: TEdit;
    eDumpInfo: TEdit;
    eFixed: TEdit;
    eHack: TEdit;
    eModified: TEdit;
    ePirate: TEdit;
    ePublisher: TEdit;
    eSortKey: TEdit;
    eTitle: TEdit;
    eTrainer: TEdit;
    eTranslated: TEdit;
    eTransTitle: TEdit;
    eVersion: TEdit;
    eYear: TEdit;
    eZone: TEdit;
    gbxDumpTags: TGroupBox;
    gbxGroup: TGroupBox;
    gbxSystem: TGroupBox;
    gbxTitle: TGroupBox;
    gbxVersion: TGroupBox;
    lCracked: TLabel;
    lDumpInfo: TLabel;
    lDumpType: TLabel;
    lFixed: TLabel;
    lHack: TLabel;
    lModified: TLabel;
    lPirate: TLabel;
    lPublisher: TLabel;
    lSystem: TLabel;
    lTitle: TLabel;
    lTrainer: TLabel;
    lTranslated: TLabel;
    lVersion: TLabel;
    lYear: TLabel;
    lZone: TLabel;

  private
    FcbxGroup: TfmEmutecaGroupCBX;
    FSoftware: cEmutecaSoftware;
    procedure SetSoftware(AValue: cEmutecaSoftware);

  protected
    property cbxGroup: TfmEmutecaGroupCBX read FcbxGroup;

    function SelectGroup(aGroup: cEmutecaGroup): boolean;

  public
    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);

    procedure ClearData; override;
    procedure LoadData; override;
    procedure SaveData; override;

    procedure SelectGroupByID(aGroupKey: string);

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaSoftEditor }

procedure TfmEmutecaSoftEditor.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;

  if Assigned(FSoftware) then
    FSoftware.FPODetachObserver(Self);

  FSoftware := AValue;

  if Assigned(Software) then
    Software.FPOAttachObserver(Self);

  LoadData;
end;

function TfmEmutecaSoftEditor.SelectGroup(aGroup: cEmutecaGroup): boolean;
begin
  Result := True;
end;

procedure TfmEmutecaSoftEditor.ClearData;
begin
  lSystem.Caption := rsUnknown;
  cbxGroup.SelectedGroup := nil;
  cbxGroup.GroupManager := nil;

  eTitle.Clear;
  eSortKey.Clear;
  eTransTitle.Clear;

  eVersion.Clear;
  eYear.Clear;
  ePublisher.Clear;
  eZone.Clear;

  cbxDumpType.ItemIndex := -1;
  cbxDumpType.Text := '';
  eDumpInfo.Clear;

  eFixed.Clear;
  eTrainer.Clear;
  eTranslated.Clear;
  ePirate.Clear;
  eCracked.Clear;
  eModified.Clear;
  eHack.Clear;
end;

procedure TfmEmutecaSoftEditor.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  case Operation of
    ooFree: Software := nil
    else
      ;
  end;

  LoadData;
end;

procedure TfmEmutecaSoftEditor.LoadData;
begin
  Enabled := assigned(Software);

  if not Enabled then
  begin
    ClearData;
    Exit;
  end;

  //
  if assigned(Software.CachedSystem) then
  begin
  lSystem.Caption := Software.CachedSystem.Title;

  cbxGroup.GroupManager := cEmutecaSystem(
    Software.CachedSystem).GroupManager;
  end
  else
  begin
    lSystem.Caption := '- NO SYSTEM -';
    cbxGroup.GroupManager := nil;
  end;

  if assigned(Software.CachedGroup) then
    cbxGroup.SelectedGroup := cEmutecaGroup(Software.CachedGroup)
  else
    cbxGroup.SelectGroupByID(Software.GroupKey);

  eTitle.Text := Software.Title;
  eSortKey.Text := Software.GetActualSortTitle;
  eTransTitle.Text := Software.GetActualTranslitTitle;

  eVersion.Text := Software.Version;
  eYear.Text := Software.Year;
  ePublisher.Text := Software.Publisher;
  eZone.Text := Software.Zone;

  case Software.DumpStatus of
    edsVerified: cbxDumpType.ItemIndex := 0;
    edsGood: cbxDumpType.ItemIndex := 1;
    edsAlternate: cbxDumpType.ItemIndex := 2;
    edsOverDump: cbxDumpType.ItemIndex := 3;
    edsBadDump: cbxDumpType.ItemIndex := 4;
    edsUnderDump: cbxDumpType.ItemIndex := 5;
    else
      cbxDumpType.ItemIndex := 1;
  end;
  eDumpInfo.Text := Software.DumpInfo;

  eFixed.Text := Software.Fixed;
  eTrainer.Text := Software.Trainer;
  eTranslated.Text := Software.Translation;
  ePirate.Text := Software.Pirate;
  eCracked.Text := Software.Cracked;
  eModified.Text := Software.Modified;
  eHack.Text := Software.Hack;
end;

procedure TfmEmutecaSoftEditor.SaveData;
var
  aSystem: cEmutecaSystem;
begin
  aSystem := cEmutecaSystem(Software.CachedSystem);

  if (not assigned(Software)) or (not assigned(aSystem)) then
    Exit;

  if Assigned(cbxGroup.SelectedGroup) then
  begin
    Software.CachedGroup := cbxGroup.SelectedGroup;
  end
  else
  begin
    // Search group
    Software.CachedGroup :=
      aSystem.GroupManager.FullList.ItemById(cbxGroup.cbxGroup.Text);

    if not assigned(Software.CachedGroup) then
      // Create group
      aSystem.GroupManager.AddGroup(cbxGroup.cbxGroup.Text);
  end;

  Software.Title := eTitle.Text;
  Software.SortTitle := eSortKey.Text;
  Software.TranslitTitle := eTransTitle.Text;

  Software.Version := eVersion.Text;
  Software.Year := eYear.Text;
  Software.Publisher := ePublisher.Text;
  Software.Zone := eZone.Text;

  case cbxDumpType.ItemIndex of
    0: Software.DumpStatus := edsVerified;
    1: Software.DumpStatus := edsGood;
    2: Software.DumpStatus := edsAlternate;
    3: Software.DumpStatus := edsOverDump;
    4: Software.DumpStatus := edsBadDump;
    5: Software.DumpStatus := edsUnderDump;
    else
      Software.DumpStatus := edsGood;
  end;
  Software.DumpInfo := eDumpInfo.Text;

  Software.Fixed := eFixed.Text;
  Software.Trainer := eTrainer.Text;
  Software.Translation := eTranslated.Text;
  Software.Pirate := ePirate.Text;
  Software.Cracked := eCracked.Text;
  Software.Modified := eModified.Text;
  Software.Hack := eHack.Text;
end;

procedure TfmEmutecaSoftEditor.SelectGroupByID(aGroupKey: string);
begin
  cbxGroup.SelectGroupByID(aGroupKey);
end;

constructor TfmEmutecaSoftEditor.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FcbxGroup := TfmEmutecaGroupCBX.Create(gbxGroup);
    cbxGroup.Align := alTop;
    cbxGroup.OnSelectGroup := @SelectGroup;
    cbxGroup.Parent := gbxGroup;
  end;

var
  i: string;
begin
  inherited Create(TheOwner);

  Enabled := False;

  CreateFrames;
  lSystem.Caption := rsUnknown;

  // Adding DumpTypes
  for i in EmutecaDumpStatusStrs do
    cbxDumpType.AddItem(i, nil);
  cbxDumpType.ItemIndex := 1;
end;

destructor TfmEmutecaSoftEditor.Destroy;
begin
  if Assigned(Software) then
    Software.FPODetachObserver(Self);

  inherited Destroy;
end;

end.
