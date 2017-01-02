unit ufrLEmuTKExportData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ufCHXForm,
  ucEmuteca,
  ufLEmuTKExportData;

type

  { TfrmLEmuTKExportData }

  TfrmLEmuTKExportData = class(TfrmCHXForm)
    procedure FormCreate(Sender: TObject);
  private
    FEmuteca: cEmuteca;
    FfmExportData: TfmLEmuTKExportData;
    procedure SetEmuteca(AValue: cEmuteca);

  protected
    property fmExportData: TfmLEmuTKExportData read FfmExportData;

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

  end;

var
  frmLEmuTKExportData: TfrmLEmuTKExportData;

implementation

{$R *.lfm}

{ TfrmLEmuTKExportData }

procedure TfrmLEmuTKExportData.FormCreate(Sender: TObject);
begin
  // Frames
  FfmExportData := TfmLEmuTKExportData.Create(self);
  fmExportData.SaveButtons:=True;
  fmExportData.ButtonClose := True;
  fmExportData.Align := alClient;
  fmExportData.Parent := Self;
end;

procedure TfrmLEmuTKExportData.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then Exit;
  FEmuteca := AValue;
  fmExportData.Emuteca := Emuteca;
end;

end.

