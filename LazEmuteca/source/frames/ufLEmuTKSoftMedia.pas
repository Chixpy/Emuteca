unit ufLEmuTKSoftMedia;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  uCHXStrUtils,
  ucEmutecaGroup, ucEmutecaSoftware,
  ufLEmuTKSoftImgPreview, ufLEmuTKSoftTxtPreview;

type

  { TfmLEmuTKSoftMedia }

  TfmLEmuTKSoftMedia = class(TFrame)
    pSoftImagPreview: TPanel;
    pSoftTxtPreview: TPanel;
    Splitter1: TSplitter;

  private
    FIconsIni: TFilename;
    FSoftImgPreview: TfmLEmuTKSoftImgPreview;
    FSoftTxtPreview: TfmLEmuTKSoftTxtPreview;
    FSoftware: cEmutecaSoftware;
    procedure SetIconsIni(AValue: TFilename);
    procedure SetSoftware(AValue: cEmutecaSoftware);

  protected
    property SoftTxtPreview: TfmLEmuTKSoftTxtPreview read FSoftTxtPreview;
    property SoftImgPreview: TfmLEmuTKSoftImgPreview read FSoftImgPreview;

  public
    { public declarations }
    property IconsIni: TFilename read FIconsIni write SetIconsIni;

    property Software: cEmutecaSoftware read FSoftware write SetSoftware;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKSoftMedia }

procedure TfmLEmuTKSoftMedia.SetIconsIni(AValue: TFilename);
begin
 FIconsIni := SetAsFile(AValue);
 SoftImgPreview.IconsIni := IconsIni;
 SoftTxtPreview.IconsIni := IconsIni;
end;

procedure TfmLEmuTKSoftMedia.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;
  FSoftware := AValue;

  SoftImgPreview.Software := Software;

  self.Enabled := assigned(Software);
end;

constructor TfmLEmuTKSoftMedia.Create(TheOwner: TComponent);
  procedure CreateFrames;
  begin
    FSoftImgPreview := TfmLEmuTKSoftImgPreview.Create(pSoftImagPreview);
    SoftImgPreview.Align := alClient;
    SoftImgPreview.Parent := pSoftImagPreview;

    FSoftTxtPreview := TfmLEmuTKSoftTxtPreview.Create(pSoftTxtPreview);
    SoftTxtPreview.Align := alClient;
    SoftTxtPreview.Parent := pSoftTxtPreview;
  end;

begin
  inherited Create(TheOwner);

  Self.Enabled := False;

  CreateFrames;
end;

destructor TfmLEmuTKSoftMedia.Destroy;
begin
  inherited Destroy;
end;

end.
