unit ufEmutecaSoftEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  ucEmuteca, ucEmutecaSystem, ucEmutecaSoftware, ucEmutecaParent,
  ufEmutecaSystemCBX, ufEmutecaParentCBX;

type

  { TfmEmutecaSoftEditor }

  TfmEmutecaSoftEditor = class(TFrame, IFPObserver)
    eDescription: TEdit;
    eTitle: TEdit;
    lDescription: TLabel;
    lParent: TLabel;
    lSystem: TLabel;
    lTitle: TLabel;

  private
    FcbxParent: TfmEmutecaParentCBX;
    FcbxSystem: TfmEmutecaSystemCBX;
    FEmuteca: cEmuteca;
    FSoftware: cEmutecaSoftware;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetSoftware(AValue: cEmutecaSoftware);

  protected
    property cbxSystem: TfmEmutecaSystemCBX read FcbxSystem;
    property cbxParent: TfmEmutecaParentCBX read FcbxParent;

    function SelectSystem(aSystem: cEmutecaSystem): boolean;
    function SelectParent(aParent: cEmutecaParent): boolean;

  public
    { public declarations }
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    procedure SaveData;
    procedure UpdateData;
    procedure ClearData;

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
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

  UpdateData;
end;

procedure TfmEmutecaSoftEditor.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  cbxSystem.SystemList := nil;
  cbxParent.ParentList := nil;

  if not assigned(Emuteca) then
  begin
  cbxSystem.SystemList := Emuteca.SystemManager.VisibleList;
  cbxParent.ParentList := Emuteca.ParentManager.VisibleList;
  end
  else
  begin
      cbxSystem.SystemList := nil;
  cbxParent.ParentList := nil;
  end;
end;

procedure TfmEmutecaSoftEditor.SaveData;
begin
  if not assigned(Software) then
    Exit;

  if assigned(cbxSystem.CurrentSystem) then
    Software.System := cbxSystem.CurrentSystem.ID
  else
    Software.System := cbxSystem.cbxSystem.Text; //LOLWUT

  if assigned(cbxParent.CurrentParent) then
    Software.Parent := cbxParent.CurrentParent.ID
  else
    Software.Parent := cbxParent.cbxParent.Text; //LOLWUT^2

  Software.Title := eTitle.Text;
  Software.Description := eDescription.Text;
end;

procedure TfmEmutecaSoftEditor.UpdateData;
var
  aParent: cEmutecaParent;
  aSystem: cEmutecaSystem;
begin
  if not assigned(Software) then
  begin
    ClearData;
    Exit;
  end;

  aSystem := nil;
  aParent := nil;

  if assigned(Emuteca) then
  begin
    aSystem :=  Emuteca.SystemManager.ItemById(Software.System);
    aParent := Emuteca.ParentManager.ItemById(Software.Parent);
  end;

  cbxSystem.CurrentSystem := aSystem;
  cbxParent.CurrentParent  := aParent;
  eTitle.Text := Software.Title;
  eDescription.Text := Software.Description;
end;

procedure TfmEmutecaSoftEditor.ClearData;
begin
  cbxSystem.CurrentSystem := nil;
  cbxParent.CurrentParent  := nil;
  eTitle.Clear;
  eDescription.Clear;
end;

procedure TfmEmutecaSoftEditor.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  case Operation of
    ooChange: UpdateData;
    ooFree: Software := nil;
    ooAddItem: UpdateData;
    ooDeleteItem: UpdateData;
    ooCustom: UpdateData;
  end;
end;

function TfmEmutecaSoftEditor.SelectSystem(aSystem: cEmutecaSystem): boolean;
begin
  // TODO: We need to update Parent list
  Result := False;
end;

function TfmEmutecaSoftEditor.SelectParent(aParent: cEmutecaParent): boolean;
begin

  Result := False;
end;

constructor TfmEmutecaSoftEditor.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FcbxSystem := TfmEmutecaSystemCBX.Create(Self);
    cbxSystem.Parent := Self;
    cbxSystem.Align := alTop;
    cbxSystem.Top := 15;
    cbxSystem.OnSelectSystem := @SelectSystem;

    FcbxParent := TfmEmutecaParentCBX.Create(Self);
    cbxParent.Parent := Self;
    cbxParent.Align := alTop;
     cbxSystem.Top := 0;
    cbxParent.OnSelectParent := @SelectParent;
  end;

begin
  inherited Create(TheOwner);

  CreateFrames;
end;

destructor TfmEmutecaSoftEditor.Destroy;
begin
  if Assigned(Software) then
    Software.FPODetachObserver(Self);

  inherited Destroy;
end;

end.
