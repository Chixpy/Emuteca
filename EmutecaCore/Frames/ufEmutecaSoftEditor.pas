unit ufEmutecaSoftEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls,
  ufCHXPropEditor,
  uEmutecaCommon,
  ucEmutecaSystem, ucEmutecaGroup, ucEmutecaSoftware,
  ufEmutecaGroupEditor;

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
    ePublisher: TComboBox;
    eSortKey: TEdit;
    eTitle: TEdit;
    eTrainer: TEdit;
    eTranslated: TEdit;
    eTransTitle: TEdit;
    eVersion: TEdit;
    eYear: TEdit;
    eZone: TEdit;
    gbxDumpTags: TGroupBox;
    gbxExtraParameters: TGroupBox;
    gbxVersion: TGroupBox;
    lCracked: TLabel;
    lDumpInfo: TLabel;
    lDumpType: TLabel;
    lFixed: TLabel;
    lHack: TLabel;
    lModified: TLabel;
    lPirate: TLabel;
    lPublisher: TLabel;
    lTrainer: TLabel;
    lTranslated: TLabel;
    lVersion: TLabel;
    lYear: TLabel;
    lZone: TLabel;
    mExtraParameters: TMemo;

  private
    FfmGroupEditor: TfmEmutecaGroupEditor;
    FGroup: cEmutecaGroup;
    FSoftware: cEmutecaSoftware;
    procedure SetGroup(AValue: cEmutecaGroup);
    procedure SetSoftware(AValue: cEmutecaSoftware);

  protected
    property fmGroupEditor: TfmEmutecaGroupEditor read FfmGroupEditor;

    function SelectGroup(aGroup: cEmutecaGroup): boolean;

    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);

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

  // Observed
  if Assigned(FSoftware) then
    FSoftware.FPODetachObserver(Self);

  FSoftware := AValue;

  if Assigned(Software) then
    Software.FPOAttachObserver(Self);

  LoadFrameData;
end;

procedure TfmEmutecaSoftEditor.SetGroup(AValue: cEmutecaGroup);
begin
  if FGroup = AValue then
    Exit;
  FGroup := AValue;
end;

function TfmEmutecaSoftEditor.SelectGroup(aGroup: cEmutecaGroup): boolean;
begin
  Result := True;
end;

procedure TfmEmutecaSoftEditor.DoClearFrameData;
begin
  eTitle.Clear;
  eSortKey.Clear;
  eTransTitle.Clear;

  eVersion.Clear;
  eYear.Clear;
  // ePublisher.Clear; We don't want to clear item list.
  ePublisher.Text := '';
  eZone.Clear;

  // cbxDumpType.Clear; We want keep DumpType list.
  cbxDumpType.Text := '';
  eDumpInfo.Clear;

  eFixed.Clear;
  eTrainer.Clear;
  eTranslated.Clear;
  ePirate.Clear;
  eCracked.Clear;
  eModified.Clear;
  eHack.Clear;

  mExtraParameters.Clear;
end;

procedure TfmEmutecaSoftEditor.DoLoadFrameData;
begin
  Enabled := assigned(Software);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  eTitle.Text := Software.GetActualTitle;
  eSortKey.Text := Software.GetActualSortTitle;
  eTransTitle.Text := Software.GetActualTranslitTitle;

  eVersion.Text := Software.Version;
  eYear.Text := Software.Year;

  ePublisher.Text := Software.Publisher;
  // Adding to ComboBox List
  if (ePublisher.ItemIndex = -1) and (Software.Publisher <> '') then
    ePublisher.ItemIndex := ePublisher.Items.Add(Software.Publisher);

  eZone.Text := Software.Zone;

  cbxDumpType.ItemIndex := Ord(Software.DumpStatus);

  eDumpInfo.Text := Software.DumpInfo;

  eFixed.Text := Software.Fixed;
  eTrainer.Text := Software.Trainer;
  eTranslated.Text := Software.Translation;
  ePirate.Text := Software.Pirate;
  eCracked.Text := Software.Cracked;
  eModified.Text := Software.Modified;
  eHack.Text := Software.Hack;
  mExtraParameters.Lines.Assign(Software.ExtraParameters);

end;

procedure TfmEmutecaSoftEditor.DoSaveFrameData;
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

  Software.Title := eTitle.Text;
  Software.SortTitle := eSortKey.Text;
  Software.TranslitTitle := eTransTitle.Text;

  Software.Version := eVersion.Text;
  Software.Year := eYear.Text;

  Software.Publisher := ePublisher.Text;
  // Adding to ComboBox List
  if (Software.Publisher <> '') and
    (ePublisher.Items.IndexOf(Software.Publisher) = -1) then
    ePublisher.AddItem(Software.Publisher, nil);

  Software.Zone := eZone.Text;

  if TEmutecaDumpStatus(cbxDumpType.ItemIndex) <> edsKeepValue then
    Software.DumpStatus := TEmutecaDumpStatus(cbxDumpType.ItemIndex);

  Software.DumpInfo := eDumpInfo.Text;

  Software.Fixed := eFixed.Text;
  Software.Trainer := eTrainer.Text;
  Software.Translation := eTranslated.Text;
  Software.Pirate := ePirate.Text;
  Software.Cracked := eCracked.Text;
  Software.Modified := eModified.Text;
  Software.Hack := eHack.Text;
  Software.ExtraParameters.Assign(mExtraParameters.Lines);
end;

procedure TfmEmutecaSoftEditor.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if ASender = Software then
    case Operation of
      ooFree: Software := nil
      else
        ;
    end;
end;

constructor TfmEmutecaSoftEditor.Create(TheOwner: TComponent);
var
  i: string;
begin
  inherited Create(TheOwner);

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;

  // Adding DumpTypes
  for i in EmutecaDumpStatusStr do
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
