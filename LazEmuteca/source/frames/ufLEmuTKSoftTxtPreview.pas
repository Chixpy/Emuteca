unit ufLEmuTKSoftTxtPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ActnList,
  ucEmuteca, ucEmutecaGroup, ucEmutecaSoftware,
  ufLEmuTKPreviewList;

type

  { TfmLEmuTKSoftTxtPreview }

  TfmLEmuTKSoftTxtPreview = class(TfmLEmuTKPreviewList)
    cbxTextType: TComboBox;
    mSoftText: TMemo;

  private
    FEmuteca: cEmuteca;
    FSoftware: cEmutecaSoftware;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetSoftware(AValue: cEmutecaSoftware);

  public
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKSoftTxtPreview }

procedure TfmLEmuTKSoftTxtPreview.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;
  FSoftware := AValue;

  Self.Enabled := Assigned(Software);
end;

procedure TfmLEmuTKSoftTxtPreview.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then Exit;
  FEmuteca := AValue;
end;

constructor TfmLEmuTKSoftTxtPreview.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmLEmuTKSoftTxtPreview.Destroy;
begin
  inherited Destroy;
end;

end.
