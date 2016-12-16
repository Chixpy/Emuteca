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

    procedure SelectGroupByID(aGroupKey: string);

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
  end
  else
  begin
    cbxSystem.SystemList := nil;
    cbxGroup.GroupList := nil;
  end;

  self.Enabled := Assigned(Software) and assigned(Emuteca);
end;

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

  self.Enabled := Assigned(Software) and assigned(Emuteca);
end;

function TfmEmutecaSoftEditor.SelectSystem(aSystem: cEmutecaSystem): boolean;
begin
  Result := True;
end;

function TfmEmutecaSoftEditor.SelectGroup(aGroup: cEmutecaGroup): boolean;
begin
  Result := True;
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
    ooFree: Software := nil
    else
      LoadData;
  end;
end;

procedure TfmEmutecaSoftEditor.LoadData;
begin
  if not assigned(Software) then
  begin
    ClearData;
    Exit;
  end;

  cbxSystem.SelectedSystem := Software.System;
  if assigned(Software.Group) then
    cbxGroup.SelectedGroup := Software.Group
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
  aGroup: cEmutecaGroup;
begin
  if not assigned(Software) then
    Exit;

  Software.System := cbxSystem.SelectedSystem;

  if Assigned(cbxGroup.SelectedGroup) then
  begin
    Software.Group := cbxGroup.SelectedGroup;
  end
  else
  begin
    Software.Group := Emuteca.SearchGroup(cbxGroup.cbxGroup.Text, True);
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

    FcbxSystem := TfmEmutecaSystemCBX.Create(gbxSystem);
    cbxSystem.Align := alTop;
    cbxSystem.OnSelectSystem := @SelectSystem;
    cbxSystem.Parent := gbxSystem;
  end;

var
  i: string;
begin
  inherited Create(TheOwner);

  Self.Enabled := False;

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
