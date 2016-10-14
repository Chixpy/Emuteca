unit ufEmutecaSoftEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  ucEmutecaSoftware, ucEmutecaParent, ucEmuteca;

type

  { TfmEmutecaSoftEditor }

  TfmEmutecaSoftEditor = class(TFrame)
    cbxParent: TComboBox;
    eDescription: TEdit;
    eTitle: TEdit;
    lDescription: TLabel;
    lParent: TLabel;
    lTitle: TLabel;
  private
    FEmuteca: cEmuteca;
    FSoftware: cEmutecaSoftware;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetSoftware(AValue: cEmutecaSoftware);
    { private declarations }

  public
    { public declarations }
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    procedure SaveData;
    procedure UpdateData;
    procedure ClearData;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaSoftEditor }

procedure TfmEmutecaSoftEditor.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;
  FSoftware := AValue;
end;

procedure TfmEmutecaSoftEditor.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;


  cbxParent.Clear;
  if not assigned(Emuteca) then
    Exit;
  Emuteca.ParentManager.AssingEnabledTo(cbxParent.Items);
end;

procedure TfmEmutecaSoftEditor.SaveData;
begin
  if not assigned(Software) then
    Exit;
  { TODO : Think about how to add a new parent }
  if cbxParent.ItemIndex <> -1 then
    Software.Parent := cEmutecaParent(
      cbxParent.Items.Objects[cbxParent.ItemIndex]).ID
  else
    Software.Parent := cbxParent.Text;

  Software.Title := eTitle.Text;
  Software.Description := eDescription.Text;
end;

procedure TfmEmutecaSoftEditor.UpdateData;
begin
  ClearData;

  if not assigned(Software) then
    Exit;

  { TODO : Search parent from the list and show full title }
  cbxParent.Text := Software.Parent;

  eTitle.Text := Software.Title;
  eDescription.Text := Software.Description;
end;

procedure TfmEmutecaSoftEditor.ClearData;
begin
  cbxParent.ItemIndex := -1;
  eTitle.Clear;
  eDescription.Clear;
end;

end.
