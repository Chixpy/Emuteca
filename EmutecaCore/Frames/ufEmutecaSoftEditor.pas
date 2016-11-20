unit ufEmutecaSoftEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls,
  ufCHXPropEditor,
  ucEmuteca, ucEmutecaSystem, ucEmutecaSoftware, ucEmutecaGroup,
  ufEmutecaSystemCBX, ufEmutecaGroupCBX;

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
    lTitle: TLabel;
    lTrainer: TLabel;
    lTranslated: TLabel;
    lVersion: TLabel;
    lYear: TLabel;
    lZone: TLabel;

  private
    FcbxGroup: TfmEmutecaGroupCBX;
    FcbxSystem: TfmEmutecaSystemCBX;
    FEmuteca: cEmuteca;
    FSoftware: cEmutecaSoftware;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetSoftware(AValue: cEmutecaSoftware);
    { private declarations }

  protected
    property cbxSystem: TfmEmutecaSystemCBX read FcbxSystem;
    property cbxGroup: TfmEmutecaGroupCBX read FcbxGroup;

    function SelectSystem(aSystem: cEmutecaSystem): boolean;
    function SelectGroup(aGroup: cEmutecaGroup): boolean;

    procedure ClearData; override;

  public
    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);

    procedure LoadData; override;
    procedure SaveData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaSoftEditor }

procedure TfmEmutecaSoftEditor.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  if assigned(Emuteca) then
  begin
    cbxSystem.SystemList := Emuteca.SystemManager.VisibleList;
    cbxGroup.GroupList := Emuteca.GroupManager.VisibleList;
    if Assigned(Software) then
      self.Enabled := True
    else
      self.Enabled := False;
  end
  else
  begin
    cbxSystem.SystemList := nil;
    cbxGroup.GroupList := nil;
    self.Enabled := False;
  end;
end;

procedure TfmEmutecaSoftEditor.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;

  if Assigned(FSoftware) then
    FSoftware.FPODetachObserver(Self);

  FSoftware := AValue;

  if Assigned(Software) then
  begin
    Software.FPOAttachObserver(Self);
    if assigned(Emuteca) then
      self.Enabled := True;
  end
  else
    self.Enabled := False;

  LoadData;
end;

function TfmEmutecaSoftEditor.SelectSystem(aSystem: cEmutecaSystem): boolean;
begin
  // TODO: We need to update Parent list
  Result := False;
end;

function TfmEmutecaSoftEditor.SelectGroup(aGroup: cEmutecaGroup): boolean;
begin
  Result := False;
end;

procedure TfmEmutecaSoftEditor.ClearData;
begin
  cbxSystem.SelectedSystem := nil;
  cbxGroup.SelectedGroup := nil;

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
    ooChange: LoadData;
    ooFree: Software := nil;
    ooAddItem: LoadData;
    ooDeleteItem: LoadData;
    ooCustom: LoadData;
  end;
end;

procedure TfmEmutecaSoftEditor.LoadData;
begin
  if not assigned(Software) then
  begin
    ClearData;
    Exit;
  end;

  // Don't use Emuteca.CacheSoft(Software);
  // We don't want autocreate a Group now until Soft is saved,
  //   so we want find it by hand;

  if assigned(Software.System) then
    cbxSystem.SelectedSystem := Software.System
  else
  begin
  if assigned(Emuteca) then
    cbxSystem.SelectedSystem := Emuteca.SearchSystem(Software.SystemKey)
  else
    cbxSystem.SelectedSystem := nil;

  end;

  if assigned(Software.Group) then
    cbxGroup.SelectedGroup := Software.Group
  else
  begin
    if assigned(Emuteca) then
      cbxGroup.SelectedGroup := Emuteca.SearchGroup(Software.GroupKey)
    else
       cbxGroup.SelectedGroup := nil;

    if cbxGroup.SelectedGroup = nil then
      // Forcing Software.GroupKey text
      cbxGroup.cbxGroup.Text := Software.GroupKey;
  end;



  eTitle.Text := Software.Title;
  eSortKey.Text := Software.SortTitle;
  eTransTitle.Text := Software.TranslitTitle;

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
begin
  if not assigned(Software) then
    Exit;

  if assigned(cbxSystem.SelectedSystem) then
  begin
    Software.System := cbxSystem.SelectedSystem;
    Software.SystemKey := cbxSystem.SelectedSystem.ID;
  end
  else
    // I hope never enter this branch ...
    Software.SystemKey := cbxSystem.cbxSystem.Text; //LOLWUT

  if assigned(cbxGroup.SelectedGroup) then
  begin
    Software.Group := cbxGroup.SelectedGroup;
    Software.GroupKey := cbxGroup.SelectedGroup.ID;
  end
  else
  begin
    Software.GroupKey := cbxGroup.cbxGroup.Text; //LOLWUT^2
    Emuteca.CacheSoft(Software); // Auto creating Group
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

constructor TfmEmutecaSoftEditor.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FcbxGroup := TfmEmutecaGroupCBX.Create(gbxGroup);
    cbxGroup.Align := alTop;
    cbxGroup.OnSelectGroup := @SelectGroup;
    cbxGroup.Parent := gbxGroup;

    FcbxSystem := TfmEmutecaSystemCBX.Create(gbxSystem);
    cbxSystem.Align := alTop;
    cbxSystem.OnSelectSystem := @SelectSystem;
    cbxSystem.Parent := gbxSystem;
  end;

var
  i: string;
begin
  inherited Create(TheOwner);

  CreateFrames;

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
