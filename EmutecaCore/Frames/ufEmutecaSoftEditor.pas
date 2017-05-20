unit ufEmutecaSoftEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls,
  ufCHXPropEditor,
  uEmutecaRscStr,
  ucEmutecaSystem,  ucEmutecaGroup, ucEmutecaSoftware,
  ufEmutecaGroupCBX;

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
    FGroup: cEmutecaGroup;
    FSoftware: cEmutecaSoftware;
    procedure SetGroup(AValue: cEmutecaGroup);
    procedure SetSoftware(AValue: cEmutecaSoftware);

  protected
    property cbxGroup: TfmEmutecaGroupCBX read FcbxGroup;

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
    property Group: cEmutecaGroup read FGroup write SetGroup;
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

procedure TfmEmutecaSoftEditor.SetGroup(AValue: cEmutecaGroup);
begin
  if FGroup = AValue then
    Exit;

  if Assigned(FGroup) then
    FGroup.FPODetachObserver(Self);

  FGroup := AValue;

  if Assigned(Group) then
    Group.FPOAttachObserver(Self);

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
  if not assigned(Software) or not Assigned(Software.System) then
  begin
    ClearData;
    Self.Enabled := False;
    Exit;
  end
  else
    Self.Enabled := True;

  lSystem.Caption := Software.System.Title;

  cbxGroup.GroupList := Software.System.GroupManager.FullList;


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
begin
  if not assigned(Software) or not assigned(Software.System) then
    Exit;

  if Assigned(cbxGroup.SelectedGroup) then
  begin
    Software.Group := cbxGroup.SelectedGroup;
  end
  else
  begin
    Software.Group := Software.System.GroupManager.ItemById(
      cbxGroup.cbxGroup.Text, True);
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

  Self.Enabled := False;

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
