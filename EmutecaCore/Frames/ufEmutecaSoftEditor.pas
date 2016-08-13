unit ufEmutecaSoftEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  ucEmutecaSoftware, ucEmutecaParent, ucEmuteca;

type

  { TfmEmutecaVersionEditor }

  TfmEmutecaVersionEditor = class(TFrame)
    cbxParent: TComboBox;
    eDescription: TEdit;
    eTitle: TEdit;
    lDescription: TLabel;
    lParent: TLabel;
    lTitle: TLabel;
  private
    FEmuteca: cEmuteca;
    FVersion: cEmutecaVersion;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetVersion(AValue: cEmutecaVersion);
    { private declarations }

  public
    { public declarations }
    property Version: cEmutecaVersion read FVersion write SetVersion;
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    procedure SaveData;
    procedure UpdateData;
    procedure ClearData;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaVersionEditor }

procedure TfmEmutecaVersionEditor.SetVersion(AValue: cEmutecaVersion);
begin
  if FVersion = AValue then
    Exit;
  FVersion := AValue;
end;

procedure TfmEmutecaVersionEditor.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;


  cbxParent.Clear;
  if not assigned(Emuteca) then
    Exit;
  Emuteca.ParentManager.AssingEnabledTo(cbxParent.Items);
end;

procedure TfmEmutecaVersionEditor.SaveData;
begin
  if not assigned(Version) then
    Exit;
  { TODO : Think about how to add a new parent }
  if cbxParent.ItemIndex <> -1 then
    Version.Parent := cEmutecaParent(
      cbxParent.Items.Objects[cbxParent.ItemIndex]).ID
  else
    Version.Parent := cbxParent.Text;

  Version.Title := eTitle.Text;
  Version.Description := eDescription.Text;
end;

procedure TfmEmutecaVersionEditor.UpdateData;
begin
  ClearData;

  if not assigned(Version) then
    Exit;

  { TODO : Search parent from the list and show full title }
  cbxParent.Text := Version.Parent;

  eTitle.Text := Version.Title;
  eDescription.Text := Version.Description;
end;

procedure TfmEmutecaVersionEditor.ClearData;
begin
  cbxParent.ItemIndex := -1;
  eTitle.Clear;
  eDescription.Clear;
end;

end.
