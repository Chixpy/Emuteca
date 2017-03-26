unit ufLEmuTKSoftImgPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ActnList, ExtCtrls,
  ufCHXForm, ufCHXImgViewer,
  ucEmuteca, ucEmutecaSoftware, ucEmutecaSystem,
  ufLEmuTKPreviewList;

type

  { TfmLEmuTKSoftImgPreview }

  TfmLEmuTKSoftImgPreview = class(TfmLEmuTKPreviewList)
    cbxImageType: TComboBox;
    iSoftImage: TImage;
    procedure cbxImageTypeSelect(Sender: TObject);
    procedure iSoftImageDblClick(Sender: TObject);

  private
    FCurrCaption: string;
    FCurrSystem: cEmutecaSystem;
    FEmuteca: cEmuteca;
    FImageExt: TStringList;
    FImageList: TStringList;
    FSoftware: cEmutecaSoftware;
    procedure SetCurrCaption(AValue: string);
    procedure SetCurrSystem(AValue: cEmutecaSystem);
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetImageExt(AValue: TStringList);
    procedure SetSoftware(AValue: cEmutecaSoftware);

  protected
    property CurrSystem: cEmutecaSystem read FCurrSystem write SetCurrSystem;
    property CurrCaption: string read FCurrCaption write SetCurrCaption;
    property ImageList: TStringList read FImageList;

    procedure OnCurrItemChange; override;
    procedure UpdateImageList;

  public
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
    property ImageExt: TStringList read FImageExt write SetImageExt;

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

procedure TfmLEmuTKSoftImgPreview.OnCurrItemChange;
begin
  if (cbxImageType.ItemIndex = -1) or (CurrItem < 1) or
    (ImageList.Count = 0) then
  begin
    iSoftImage.Picture.Clear;
    exit;
  end;

  iSoftImage.Picture.LoadFromFile(ImageList[CurrItem - 1]);
end;

procedure TfmLEmuTKSoftImgPreview.UpdateImageList;
begin
  ImageList.Clear;
  iSoftImage.Picture.Clear;

  if not assigned(CurrSystem) then
    Exit;

  if not Assigned(Emuteca) then
    Exit;

  if cbxImageType.ItemIndex > -1 then
    Emuteca.SearchSoftFiles(ImageList,
      Software.System.ImageFolders[cbxImageType.ItemIndex],
      Software, ImageExt);

  ItemCount := ImageList.Count;

  if ItemCount > 0 then
    CurrItem := 1
  else
    CurrItem := 0;
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

procedure TfmLEmuTKSoftImgPreview.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;
end;

procedure TfmLEmuTKSoftImgPreview.SetImageExt(AValue: TStringList);
begin
  if FImageExt = AValue then
    Exit;
  FImageExt := AValue;
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

  UpdateImageList;
end;

procedure TfmLEmuTKSoftImgPreview.iSoftImageDblClick(Sender: TObject);
var
  aForm: TfrmCHXForm;
  fmCHXImageViewer: TfmCHXImgViewer;
begin
  Application.CreateForm(TfrmCHXForm, aForm);
  { TODO : Uncomment this }
  // aForm.GUIConfigIni := ;
  aform.WindowState := wsMaximized; // We want view full image
  aForm.GUIIconsIni := Self.IconsIni;

  fmCHXImageViewer := TfmCHXImgViewer.Create(aForm);
  fmCHXImageViewer.IconsIniFile := Self.IconsIni;
  fmCHXImageViewer.AddImages(ImageList, CurrItem);
  fmCHXImageViewer.Align := alClient;
  fmCHXImageViewer.Parent := aForm;

  aForm.ShowModal;

  FreeAndNil(aForm);
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

  FImageList := TStringList.Create;
end;

destructor TfmLEmuTKSoftImgPreview.Destroy;
begin
  ImageList.Destroy;
  inherited Destroy;
end;

end.
