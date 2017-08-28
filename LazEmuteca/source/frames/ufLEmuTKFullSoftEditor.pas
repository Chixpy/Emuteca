unit ufLEmuTKFullSoftEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList,
  ufCHXPropEditor,
  ucEmutecaSystem, ucEmutecaGroup, ucEmutecaSoftware,
  ufEmutecaGroupCBX, ufEmutecaGroupEditor, ufEmutecaSoftEditor,
  uLEmuTKCommon;

type

  { TfmLEmuTKFullSoftEditor }

  TfmLEmuTKFullSoftEditor = class(TfmCHXPropEditor)
    gbxGroup: TGroupBox;
    gbxSoft: TGroupBox;
    pGroupCBX: TPanel;
    sbxSoft: TScrollBox;

  private
    FfmGroupCBX: TfmEmutecaGroupCBX;
    FfmGroupEditor: TfmEmutecaGroupEditor;
    FfmSoftEditor: TfmEmutecaSoftEditor;
    FGroup: cEmutecaGroup;
    FSoftware: cEmutecaSoftware;
    procedure SetGroup(AValue: cEmutecaGroup);
    procedure SetSoftware(AValue: cEmutecaSoftware);

  protected
    property fmGroupCBX: TfmEmutecaGroupCBX read FfmGroupCBX;
    property fmGroupEditor: TfmEmutecaGroupEditor read FfmGroupEditor;
    property fmSoftEditor: TfmEmutecaSoftEditor read FfmSoftEditor;

    function SelectGroup(aGroup: cEmutecaGroup): boolean;

    procedure ClearFrameData; override;
    procedure LoadFrameData; override;

  public
    procedure SaveFrameData; override;

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

{ TfmLEmuTKFullSoftEditor }

procedure TfmLEmuTKFullSoftEditor.SetGroup(AValue: cEmutecaGroup);
begin
  if FGroup = AValue then
    Exit;
  FGroup := AValue;
  FSoftware := nil;

  fmSoftEditor.Software := nil;
  fmGroupEditor.Group := Group;

  if Assigned(Group) then
  begin
    fmGroupCBX.GroupList :=
      cEmutecaSystem(Group.CachedSystem).GroupManager.FullList;
    fmGroupCBX.SelectedGroup := Group;
  end
  else
    fmGroupCBX.GroupList := nil;

  LoadFrameData;
end;

procedure TfmLEmuTKFullSoftEditor.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;
  FSoftware := AValue;
  FGroup := nil;

  fmSoftEditor.Software := Software;

  if Assigned(Software) then
  begin
    fmGroupCBX.GroupList :=
      cEmutecaSystem(Software.CachedSystem).GroupManager.FullList;

    if Assigned(Software.CachedGroup) then
    begin
      fmGroupEditor.Group := cEmutecaGroup(Software.CachedGroup);
      fmGroupCBX.SelectedGroup := fmGroupEditor.Group;
    end
    else
    begin
      fmGroupEditor.Group := nil;
    end;
  end
  else
  begin
    fmGroupCBX.GroupList := nil;
  end;

  LoadFrameData;
end;

function TfmLEmuTKFullSoftEditor.SelectGroup(aGroup: cEmutecaGroup): boolean;
begin
  Result := True;
  if assigned(Software) then
    Software.CachedGroup := aGroup;
  fmGroupEditor.Group := aGroup;
end;

procedure TfmLEmuTKFullSoftEditor.ClearFrameData;
begin


end;

procedure TfmLEmuTKFullSoftEditor.LoadFrameData;
begin
  Enabled := Assigned(Software) or Assigned(Group);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  pGroupCBX.Enabled := Assigned(Software);
  gbxSoft.Enabled := Assigned(Software);
end;

procedure TfmLEmuTKFullSoftEditor.SaveFrameData;
begin
  if (not assigned(Software)) and (not assigned(Group)) then
  begin
    // TODO: Exception
    ShowMessage('TfmLEmuTKFullSoftEditor: Can''t save Soft/Group data.');
    Exit;
  end;

  fmGroupEditor.SaveFrameData;

  if Assigned(Software) then
    fmSoftEditor.SaveFrameData;
end;

procedure TfmLEmuTKFullSoftEditor.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin

end;

constructor TfmLEmuTKFullSoftEditor.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FfmGroupCBX := TfmEmutecaGroupCBX.Create(pGroupCBX);
    fmGroupCBX.Align := alTop;
    fmGroupCBX.OnSelectGroup := @SelectGroup;
    fmGroupCBX.Parent := pGroupCBX;

    FfmGroupEditor := TfmEmutecaGroupEditor.Create(gbxGroup);
    fmGroupEditor.Align := alClient;
    FfmGroupEditor.SaveButtons := False;
    FfmGroupEditor.ButtonClose := False;
    fmGroupEditor.Parent := gbxGroup;

    FfmSoftEditor := TfmEmutecaSoftEditor.Create(sbxSoft);
    //fmSoftEditor.Align := alTop;
    fmSoftEditor.SaveButtons := False;
    fmSoftEditor.ButtonClose := False;
    fmSoftEditor.Parent := sbxSoft;
  end;

var
  i: string;
begin
  inherited Create(TheOwner);

  CreateFrames;
end;

destructor TfmLEmuTKFullSoftEditor.Destroy;
begin
  inherited Destroy;
end;

end.
