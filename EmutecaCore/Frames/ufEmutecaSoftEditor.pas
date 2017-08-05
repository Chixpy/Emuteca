unit ufEmutecaSoftEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls,
  ufCHXPropEditor,
  uLEmuTKCommon,
  uaEmutecaCustomSoft,
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
    FfmGroupCBX: TfmEmutecaGroupCBX;
    FSoftware: cEmutecaSoftware;
    procedure SetSoftware(AValue: cEmutecaSoftware);

  protected
    property fmGroupCBX: TfmEmutecaGroupCBX read FfmGroupCBX;

    function SelectGroup(aGroup: cEmutecaGroup): boolean;

    procedure ClearFrameData; override;
    procedure LoadFrameData; override;

  public
    procedure SaveFrameData; override;

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);

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

  // Observed
  if Assigned(FSoftware) then
    FSoftware.FPODetachObserver(Self);

  FSoftware := AValue;

  if Assigned(Software) then
    Software.FPOAttachObserver(Self);

  // Subframes
  if Assigned(Software) then
  begin
    if assigned(Software.CachedSystem) then
    begin
      fmGroupCBX.GroupManager :=
        cEmutecaSystem(Software.CachedSystem).GroupManager;
    end
    else
      fmGroupCBX.GroupManager := nil;
  end
  else
  begin
    fmGroupCBX.GroupManager := nil;
  end;

  LoadFrameData;
end;

function TfmEmutecaSoftEditor.SelectGroup(aGroup: cEmutecaGroup): boolean;
begin
  Result := True;
end;

procedure TfmEmutecaSoftEditor.ClearFrameData;
begin
  lSystem.Caption := rsUnknown;
  eTitle.Clear;
  eSortKey.Clear;
  eTransTitle.Clear;

  eVersion.Clear;
  eYear.Clear;
  ePublisher.Clear;
  eZone.Clear;

  // We want keep DumpType list, so don't cbxDumpType.Clear;
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

procedure TfmEmutecaSoftEditor.LoadFrameData;
begin
  Enabled := assigned(Software);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  if assigned(Software.CachedSystem) then
    lSystem.Caption := Software.CachedSystem.Title
  else
    lSystem.Caption := '- NO SYSTEM -';

  if assigned(Software.CachedGroup) then
    fmGroupCBX.SelectedGroup := cEmutecaGroup(Software.CachedGroup)
  else
    fmGroupCBX.SelectGroupByID(Software.GroupKey);


  eTitle.Text := Software.Title;
  eSortKey.Text := Software.GetActualSortTitle;
  eTransTitle.Text := Software.GetActualTranslitTitle;

  eVersion.Text := Software.Version;
  eYear.Text := Software.Year;
  ePublisher.Text := Software.Publisher;
  eZone.Text := Software.Zone;

  case Software.DumpStatus of
    edsVerified: cbxDumpType.ItemIndex := 0;
    // Default: edsGood: cbxDumpType.ItemIndex := 1;
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

procedure TfmEmutecaSoftEditor.SaveFrameData;
var
  aSystem: cEmutecaSystem;
begin
  aSystem := cEmutecaSystem(Software.CachedSystem);

  if (not assigned(Software)) or (not assigned(aSystem)) then
  begin
    // TODO: Exception
    ShowMessage('TfmEmutecaSoftEditor: Can''t save Software data.');
    Exit;
  end;

  if Assigned(fmGroupCBX.SelectedGroup) then
  begin
    Software.CachedGroup := fmGroupCBX.SelectedGroup;
  end
  else
  begin
    // Search group
    Software.CachedGroup :=
      aSystem.GroupManager.FullList.ItemById(fmGroupCBX.cbxGroup.Text);

    if not assigned(Software.CachedGroup) then
      // Create group
      aSystem.GroupManager.AddGroup(fmGroupCBX.cbxGroup.Text);
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

procedure TfmEmutecaSoftEditor.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  case Operation of
    ooFree: Software := nil
    else
      ;
  end;
end;

procedure TfmEmutecaSoftEditor.SelectGroupByID(aGroupKey: string);
begin
  fmGroupCBX.SelectGroupByID(aGroupKey);
end;

constructor TfmEmutecaSoftEditor.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FfmGroupCBX := TfmEmutecaGroupCBX.Create(gbxGroup);
    fmGroupCBX.Align := alTop;
    fmGroupCBX.OnSelectGroup := @SelectGroup;
    fmGroupCBX.Parent := gbxGroup;
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
