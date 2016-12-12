unit ufLEmuTKSoftTxtPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ActnList,
  ucEmutecaSoftware,
  ufLEmuTKPreviewList;

type

  { TfmLEmuTKSoftTxtPreview }

  TfmLEmuTKSoftTxtPreview = class(TfmLEmuTKPreviewList)
    cbxTextType: TComboBox;
    mSoftText: TMemo;

  private
    FSoftware: cEmutecaSoftware;
    procedure SetSoftware(AValue: cEmutecaSoftware);

  public
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;

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

constructor TfmLEmuTKSoftTxtPreview.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmLEmuTKSoftTxtPreview.Destroy;
begin
  inherited Destroy;
end;

end.
