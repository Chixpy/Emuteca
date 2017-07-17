unit ufLEmuTKSoftImgPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ActnList, ExtCtrls,
  ufCHXForm, ufCHXImgViewer,
  uGUIConfig,
  uEmutecaRscStr,
  ucEmuteca, uaEmutecaCustomSystem, ucEmutecaSoftware, ucEmutecaGroup,
  ufLEmuTKPreviewList;

type

  { TfmLEmuTKSoftImgPreview }

  TfmLEmuTKSoftImgPreview = class(TfmLEmuTKPreviewList)
    cbxImageType: TComboBox;
    iSoftImage: TImage;
    procedure cbxImageTypeSelect(Sender: TObject);
    procedure iSoftImageDblClick(Sender: TObject);

  private
    FEmuteca: cEmuteca;
    FGroup: cEmutecaGroup;
    FGUIConfig: cGUIConfig;
    FImageList: TStringList;
    FLastCaption: string;
    FLastSystem: caEmutecaCustomSystem;
    FSoftware: cEmutecaSoftware;
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetGroup(AValue: cEmutecaGroup);
    procedure SetGUIConfig(AValue: cGUIConfig);
    procedure SetLastCaption(AValue: string);
    procedure SetLastSystem(AValue: caEmutecaCustomSystem);
    procedure SetSoftware(AValue: cEmutecaSoftware);

  protected
    property ImageList: TStringList read FImageList;
    property LastSystem: caEmutecaCustomSystem read FLastSystem write SetLastSystem;
    property LastCaption: string read FLastCaption write SetLastCaption;

    procedure OnCurrItemChange; override;
    procedure UpdateImageList;

    procedure SetGUIIconsIni(AValue: string); override;
    procedure SetGUIConfigIni(AValue: string); override;


  public
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
    property Group: cEmutecaGroup read FGroup write SetGroup;
    property GUIConfig: cGUIConfig read FGUIConfig write SetGUIConfig;


    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;

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

  if assigned(Software) then
    LastSystem := Software.CachedSystem
  else
    LastSystem := nil;

  UpdateImageList;
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

  Enabled := (Assigned(Software) or Assigned(Group)) and (Assigned(Emuteca));

  if cbxImageType.ItemIndex > -1 then
    Emuteca.SearchSoftFiles(ImageList,
      Software.CachedSystem.ImageFolders[cbxImageType.ItemIndex],
      Software, GUIConfig.ImageExtensions);

  ItemCount := ImageList.Count;

  if ItemCount > 0 then
    CurrItem := 1
  else
    CurrItem := 0;
end;

procedure TfmLEmuTKSoftImgPreview.SetGUIIconsIni(AValue: string);
begin
  inherited SetGUIIconsIni(AValue);
end;

procedure TfmLEmuTKSoftImgPreview.SetGUIConfigIni(AValue: string);
begin
  inherited SetGUIConfigIni(AValue);
end;

procedure TfmLEmuTKSoftImgPreview.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;
end;

procedure TfmLEmuTKSoftImgPreview.SetGroup(AValue: cEmutecaGroup);
begin
  if FGroup = AValue then
    Exit;
  FGroup := AValue;

  FSoftware := nil;

  // UpdateImageList;
end;

procedure TfmLEmuTKSoftImgPreview.SetGUIConfig(AValue: cGUIConfig);
begin
  if FGUIConfig = AValue then
    Exit;
  FGUIConfig := AValue;

  if assigned(GUIConfig) then
    GUIConfigIni := GUIConfig.ConfigFile
  else
    GUIConfigIni := '';
end;

procedure TfmLEmuTKSoftImgPreview.SetLastCaption(AValue: string);
begin
  if FLastCaption = AValue then
    Exit;
  FLastCaption := AValue;
end;

procedure TfmLEmuTKSoftImgPreview.SetLastSystem(AValue: caEmutecaCustomSystem);
var
  aIndex: integer;
begin
  if FLastSystem = AValue then
    Exit;
  FLastSystem := AValue;

  if Assigned(LastSystem) then
  begin
    // Updating captions
    cbxImageType.Items.Assign(LastSystem.ImageCaptions);

    // Selecting previous one if exists
    aIndex := cbxImageType.Items.IndexOf(LastCaption);
    if aIndex <> -1 then
    begin
      cbxImageType.ItemIndex := aIndex;
    end
    else
    begin
      if cbxImageType.Items.Count > 0 then
      begin
        cbxImageType.ItemIndex := 0;
        // CurrCaption := ''; Keep this...
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
begin
  UpdateImageList;
end;

procedure TfmLEmuTKSoftImgPreview.iSoftImageDblClick(Sender: TObject);
var
  aForm: TfrmCHXForm;
  fmCHXImageViewer: TfmCHXImgViewer;
begin
  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := 'frmCHXImgViewer';
    aForm.GUIConfigIni := GUIConfigIni;
    aForm.Caption := Format(rsFmtWindowCaption,
      [Application.Title, 'Image Viewer']); // TODO: Add more info

    aForm.WindowState := wsMaximized; // We want view full image
    aForm.GUIIconsIni := GUIIconsIni;

    fmCHXImageViewer := TfmCHXImgViewer.Create(aForm);
    fmCHXImageViewer.IconsIniFile := GUIIconsIni;
    fmCHXImageViewer.AddImages(ImageList, CurrItem);
    fmCHXImageViewer.Align := alClient;
    fmCHXImageViewer.Parent := aForm;

    aForm.ShowModal;
  finally
    aForm.Free;
  end;
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
