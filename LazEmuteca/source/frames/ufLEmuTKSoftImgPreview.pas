unit ufLEmuTKSoftImgPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ActnList, ExtCtrls,
  ucEmutecaSoftware, ucEmutecaSystem,
  ufLEmuTKPreviewList;

type

  { TfmLEmuTKSoftImgPreview }

  TfmLEmuTKSoftImgPreview = class(TfmLEmuTKPreviewList)
    cbxImageType: TComboBox;
    iSoftImage: TImage;
    procedure cbxImageTypeSelect(Sender: TObject);

  private
    FCurrCaption: string;
    FCurrSystem: cEmutecaSystem;
    FSoftware: cEmutecaSoftware;
    procedure SetCurrCaption(AValue: string);
    procedure SetCurrSystem(AValue: cEmutecaSystem);
    procedure SetSoftware(AValue: cEmutecaSoftware);

  protected
    property CurrSystem: cEmutecaSystem read FCurrSystem write SetCurrSystem;
    property CurrCaption: string read FCurrCaption write SetCurrCaption;

    procedure UpdateImageList;

  public
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKSoftImgPreview }

procedure TfmLEmuTKSoftImgPreview.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;
  FSoftware := AValue;

  if not Assigned(Software) then
    CurrSystem := nil
  else
    CurrSystem := Software.System;

  UpdateImageList;

  Self.Enabled := Assigned(Software);
end;

procedure TfmLEmuTKSoftImgPreview.UpdateImageList;
begin

end;

procedure TfmLEmuTKSoftImgPreview.SetCurrSystem(AValue: cEmutecaSystem);
var
  aIndex: integer;
begin
  if FCurrSystem = AValue then
    Exit;
  FCurrSystem := AValue;

  if Assigned(CurrSystem) then
  begin
    // Updating captions
    cbxImageType.Items.Assign(CurrSystem.ImageCaptions);

    // Selecting previous one if exists
    aIndex := cbxImageType.Items.IndexOf(CurrCaption);
    if aIndex <> -1 then
    begin
      cbxImageType.ItemIndex := aIndex;
    end
    else
    begin
      if cbxImageType.Items.Count > 0 then
      begin
        cbxImageType.ItemIndex := 0;
        // CurrCaption := ''; Not clearing this maybe is interenting...
      end;
    end;
  end
  else
  begin
    cbxImageType.Clear;
    // CurrCaption := ''; Keep this...
  end;
end;

procedure TfmLEmuTKSoftImgPreview.cbxImageTypeSelect(Sender: TObject);
var
  aIndex: integer;
begin
  aIndex := cbxImageType.ItemIndex;
  if aIndex = -1 then
    CurrCaption := ''
  else
    CurrCaption := cbxImageType.Items[cbxImageType.ItemIndex];
end;

procedure TfmLEmuTKSoftImgPreview.SetCurrCaption(AValue: string);
begin
  if FCurrCaption = AValue then
    Exit;
  FCurrCaption := AValue;
end;

constructor TfmLEmuTKSoftImgPreview.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmLEmuTKSoftImgPreview.Destroy;
begin
  inherited Destroy;
end;

end.
